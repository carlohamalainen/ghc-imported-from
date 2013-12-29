{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Instances()
import Data.Function (on)
import Data.Generics hiding (typeOf)
import Data.List
import Data.Maybe
import Data.Typeable()
import Desugar()
import DynFlags
import FastString
import GHC
import GHC.Paths (libdir)
import GHC.SYB.Utils
import HscTypes
import Name
import Outputable
import RdrName
import System.Environment
import System.IO
import System.Process
import TcRnTypes()

import qualified SrcLoc
import qualified Safe

getImports :: FilePath -> String -> IO [SrcLoc.Located (ImportDecl RdrName)]
getImports targetFile targetModuleName =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            -- Set the dynamic flags for the session.
            dflags <- getSessionDynFlags
            -- FIXME Do we need Opt_ImplicitPrelude? Maybe we should check
            -- if the targetFile has an implicit prelude option set?
            let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
            setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

            -- Load the target file (e.g. "Muddle.hs").
            target <- guessTarget targetFile Nothing
            setTargets [target]
            load LoadAllTargets

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]

            -- Extract the module summary and the *textual* imports.
            modSum <- getModSummary $ mkModuleName targetModuleName

            return $ ms_textual_imps modSum

data HaskellModule = HaskellModule { modName          :: String
                                   , modQualifier     :: Maybe String
                                   , modIsImplicit    :: Bool
                                   , modHiding        :: [String]
                                   , modImportedAs    :: String
                                   } deriving (Show)

toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding importedAs
    where idecl'     = SrcLoc.unLoc idecl
          name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = removeBrackets $ parseHiding $ GHC.ideclHiding idecl'
          importedAs = showSDoc tracingDynFlags (ppr $ ideclAs idecl')

          removeBrackets :: [a] -> [a]
          removeBrackets [] = []
          removeBrackets x = reverse . tail . reverse . tail $ x

          grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
          grabNames loc = showSDoc tracingDynFlags (ppr names)
            where names = GHC.ieNames $ SrcLoc.unLoc loc

          parseHiding :: Maybe (Bool, [Located (IE RdrName)]) -> [String]
          parseHiding Nothing = []
          parseHiding (Just (False, _)) = error "This should not happen???"
          parseHiding (Just (True, h))  = map grabNames h

lookupSymbol :: String -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
lookupSymbol targetFile targetModuleName qualifiedSymbol importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets

        -- Bring in the target module and its imports.
        setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)

        -- Get the module summary, then parse it, type check it, and desugar it.
        modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
        p <- parseModule modSummary   :: Ghc ParsedModule
        t <- typecheckModule p        :: Ghc TypecheckedModule
        d <- desugarModule t          :: Ghc DesugaredModule

        -- The "guts" has the global reader environment, which we need.
        let guts = coreModule d            :: ModGuts
            gre = HscTypes.mg_rdr_env guts :: GlobalRdrEnv

        -- Beware that parseName expects an unambiguous symbol otherwise it causes a
        -- GHC panic. A fully qualified name should suffice. Is there a way to
        -- catch this exception?
        names <- parseName qualifiedSymbol
        let occNames = map nameOccName names                        :: [OccName]
            occNamesLookups = map (lookupGlobalRdrEnv gre) occNames :: [[GlobalRdrElt]]

        return $ zip names occNamesLookups

-- Module in which a name is *defined*.
symbolDefinedIn :: Name -> Module
symbolDefinedIn occname = nameModule occname

-- List of possible modules which have resulted in
-- the name being in the current scope. Using a
-- global reader we get the provenance data and then
-- get the list of import specs.
symbolImportedFrom :: GlobalRdrElt -> [ModuleName]
symbolImportedFrom occNameLookup = map importSpecModule whys
  where prov = gre_prov occNameLookup :: Provenance
        Imported whys = prov
        _ = whys :: [ImportSpec] -- dummy binding so we can see that 'whys' has this type, just for documentation


-- http://stackoverflow.com/a/4978733

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep' where
  sep' [] = Nothing
  sep' l  = Just . fmap (drop 1) . break (==chr) $ l


postfixMatch :: String -> String -> Bool
postfixMatch originalSymbol qName = isPrefixOf (reverse endTerm) (reverse qName)
  where endTerm = last $ separateBy '.' originalSymbol

-- FIXME total hack, should deconstruct the name properly?
moduleOfQualifiedName :: String -> Maybe String
moduleOfQualifiedName qn = if moduleBits == []
                                      then Nothing
                                      else Just $ concat $ intersperse "." moduleBits
  where moduleBits = reverse $ drop 1 $ reverse $ separateBy '.' qn -- FIXME is this ok?



-- listifySpans and listifyStaged are copied from ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))


-- qualifiedName :: String -> String -> Int -> Int -> [String] -> IO [String]
qualifiedName targetFile targetModuleName lineNo colNo importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags' { hscTarget = HscInterpreted, ghcLink = LinkInMemory }

        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets

        setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
        p <- parseModule modSummary   :: Ghc ParsedModule
        t <- typecheckModule p        :: Ghc TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNo, colNo) :: [LHsBind Id]
            es = listifySpans tcs (lineNo, colNo) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNo, colNo) :: [LPat Id]

        let blahToString x = showSDoc tracingDynFlags $ ppr x
            bsStrings = map blahToString bs
            esStrings = map blahToString es
            psStrings = map blahToString ps

        return $ bsStrings ++ esStrings ++ psStrings

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof <- hIsEOF h
    if ineof
        then return ""
        else hGetContents h

ghcPkgFindModule :: ModuleName -> IO (Maybe String)
ghcPkgFindModule m = do

    let m' = showSDoc tracingDynFlags (ppr $ m)

    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" ["find-module", m', "--simple-output"]){ std_in = CreatePipe
                                                                                                 , std_out = CreatePipe
                                                                                                 , std_err = CreatePipe
                                                                                                 }

    output <- readRestOfHandle hout
    return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

ghcPkgHaddockUrl :: String -> IO (Maybe String)
ghcPkgHaddockUrl p = do
    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" ["field", p, "haddock-html"]){ std_in = CreatePipe
                                                                                       , std_out = CreatePipe
                                                                                       , std_err = CreatePipe
                                                                                       }

    line <- (reverse . (dropWhile (== '\n')) . reverse) <$> readRestOfHandle hout

    return $ Safe.lastMay $ words line

moduleNameToHtmlFile :: ModuleName -> String
moduleNameToHtmlFile m =  base' ++ ".html"
    where base = (showSDoc tracingDynFlags (ppr m))
          base' = map f base
          f '.' = '-'
          f c   = c


main :: IO ()
main = do
    args <- getArgs

    -- quick and dirty argument parsing, no error checking
    let targetFile     = args !! 0
        targetModule   = args !! 1
        symbol         = args !! 2
        lineNo         = (read $ args !! 3) :: Int
        colNo          = (read $ args !! 4) :: Int

    importList <- (map (modName . toHaskellModule)) <$> getImports targetFile targetModule

    -- importListRaw <- getImports targetFile targetModule
    -- forM_ importListRaw $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))
    -- putStrLn ""

    qnames <- (filter (not . (' ' `elem`))) <$> qualifiedName targetFile targetModule lineNo colNo importList :: IO [String]

    -- putStrLn "<qnames>"
    -- forM_ qnames putStrLn
    -- putStrLn "</qnames>"
    -- putStrLn ""

    let postMatches = filter (postfixMatch symbol) qnames :: [String]

        symbol' = if postMatches == [] then symbol else minimumBy (compare `on` length) postMatches -- Flaky?

    -- putStrLn $ "symbol:  " ++ symbol
    -- putStrLn $ "symbol': " ++ symbol'


    let maybeExtraModule = moduleOfQualifiedName symbol'

        importList' = if symbol == symbol' then importList else importList ++ [fromJust maybeExtraModule]

    -- putStrLn $ "try to match on: " ++ (show (symbol, qnames))
    -- putStrLn $ "postMatches: " ++ (show postMatches)
    -- putStrLn $ "importlist': " ++ (show importList')

    x <- lookupSymbol targetFile targetModule symbol' importList'

    forM_ x $ \(name, lookUp) -> do putStrLn $ "file: " ++ targetFile
                                    putStrLn $ "module: " ++ targetModule
                                    putStrLn $ "supplied symbol: " ++ symbol
                                    putStrLn $ "inferred symbol: " ++ symbol'
                                    -- putStrLn $ "imports: " ++ (show importList')

                                    let definedIn = symbolDefinedIn name
                                        importedFrom = Safe.headMay $ concat $ map symbolImportedFrom lookUp

                                    putStrLn $ showSDoc tracingDynFlags (ppr $ importedFrom)

                                    m' <- maybe (return Nothing) ghcPkgFindModule importedFrom

                                    -- print m'

                                    let base = moduleNameToHtmlFile <$> importedFrom

                                    haddock <- maybe (return Nothing) ghcPkgHaddockUrl m'

                                    if isNothing haddock || isNothing m'
                                        then putStrLn $ "haddock: 111FAIL111"
                                        else putStrLn $ "haddock: " ++ (fromJust haddock) ++ "/" ++ (fromJust base) -- FIXME path separator on Windows?

                                    -- putStrLn $ "defined in: " ++ (showSDoc tracingDynFlags (ppr $ definedIn))

                                    -- if importedFrom == []
                                    --     then putStrLn $ "imported from: 000FAIL000"
                                    --     else putStrLn $ "imported from: " ++ (showSDoc tracingDynFlags (ppr $ head $ importedFrom))
