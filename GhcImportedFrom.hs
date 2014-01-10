{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

{-

Synopsis: Attempt to guess the location of the Haddock HTML
documentation for a given symbol in a particular module, file, and
line/col location.

See the tests directory for some example usage.

Author: Carlo Hamalainen <carlo@carlo-hamalainen.net>

-}

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
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import TcRnTypes()

import qualified DynFlags
import qualified GhcMonad
import qualified MonadUtils
import qualified Packages
import qualified SrcLoc
import qualified Safe

{-

TODO

* Pass in ghc options, e.g. "--package-db" so that a command like this works:

    ghc-pkg --package-db ./.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d field safe haddock-html
    haddock-html: /home/carlo/work/github/ghc-imported-from/.cabal-sandbox/share/doc/x86_64-linux-ghc-7.6.3/safe-0.3.3/html

-}

-- Inconsistency with the package-db option. Sometimes --package-db, sometimes -package-db. See notes
-- at http://www.vex.net/~trebla/haskell/sicp.xhtml

derps = [] -- [ "/home/carlo/work/github/cli-yesod-blog/blog/.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d" ]

-- for GHC API
-- myOptsTmp  = ["-no-user-package-db"] ++ map ("-package-db  " ++) derps
myOptsTmp  = ["-global"] -- ["-no-user-package-db"] ++ map ("-package-db  " ++) derps

-- for ghc-pkg
myOptsTmp' = (concat $ map (\x -> ["--package-db", x]) derps)

data GhcOptions = GhcOptions [String] deriving (Show)

-- setDynamicFlags :: GhcMonad m => GhcOptions -> m DynFlags
setDynamicFlags ghcOpts dflags0 = do
    let argv0 = myOptsTmp

    let dflags1 = foldl xopt_set dflags0 [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash, Opt_MagicHash, Opt_TemplateHaskell, Opt_QuasiQuotes, Opt_OverloadedStrings, Opt_TypeFamilies, Opt_FlexibleContexts, Opt_GADTs, Opt_MultiParamTypeClasses] -- What if the user does not want an implicit prelude?
        dflags2 = dflags1 { hscTarget = HscInterpreted
                          , ghcLink = LinkInMemory
                          }

    (dflags3, _, _) <- GHC.parseDynamicFlags dflags2 (map SrcLoc.noLoc argv0) -- FIXME check for errors/warnings?

    setSessionDynFlags dflags3

    GhcMonad.liftIO $ Packages.initPackages dflags3

    return dflags3

getImports :: FilePath -> String -> IO [SrcLoc.Located (ImportDecl RdrName)]
getImports targetFile targetModuleName =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            getSessionDynFlags >>= setDynamicFlags (GhcOptions [])

            GhcMonad.liftIO $ print "getImports1"

            -- Load the target file (e.g. "Muddle.hs").
            target <- guessTarget targetFile Nothing
            GhcMonad.liftIO $ print "getImports1a"
            setTargets [target]
            GhcMonad.liftIO $ print "getImports1b"
            load LoadAllTargets

            GhcMonad.liftIO $ print "getImports2"

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]

            GhcMonad.liftIO $ print "getImports2"

            -- Extract the module summary and the *textual* imports.
            modSum <- getModSummary $ mkModuleName targetModuleName

            GhcMonad.liftIO $ print "getImports2"

            return $ ms_textual_imps modSum

data HaskellModule = HaskellModule { modName          :: String
                                   , modQualifier     :: Maybe String
                                   , modIsImplicit    :: Bool
                                   , modHiding        :: [String]
                                   , modImportedAs    :: Maybe String
                                   , modSpecifically  :: [String]
                                   } deriving (Show, Eq)

toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding importedAs specifically
    where idecl'     = SrcLoc.unLoc idecl
          name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = map removeBrackets $ (catMaybes . parseHiding . GHC.ideclHiding) idecl'
          -- importedAs = showSDoc tracingDynFlags (ppr $ ideclAs idecl')
          importedAs = ((showSDoc tracingDynFlags) . ppr) <$> (ideclAs idecl')
          specifically = map removeBrackets $ (parseSpecifically . GHC.ideclHiding) idecl'

          removeBrackets :: [a] -> [a]
          removeBrackets [] = []
          removeBrackets x = reverse . tail . reverse . tail $ x

          grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
          grabNames loc = showSDoc tracingDynFlags (ppr names)
            where names = GHC.ieNames $ SrcLoc.unLoc loc

          parseHiding :: Maybe (Bool, [Located (IE RdrName)]) -> [Maybe String]
          parseHiding Nothing = [Nothing]

          -- If we do
          --
          --     import System.Environment ( getArgs )
          --
          -- then we get ["getArgs"] here, but we don't really need it...
          parseHiding (Just (False, h)) = [] -- error $ "This should not happen?" ++ (show (map grabNames h))

          -- Actually hid names, e.g.
          --
          --     import Data.List hiding (map)
          parseHiding (Just (True, h))  = map (Just . grabNames) h

          parseSpecifically :: Maybe (Bool, [Located (IE RdrName)]) -> [String]
          parseSpecifically (Just (False, h)) = map grabNames h
          parseSpecifically _                 = []




lookupSymbol :: String -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
lookupSymbol targetFile targetModuleName qualifiedSymbol importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        getSessionDynFlags >>= setDynamicFlags (GhcOptions [])

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


-- This definition of separateBy is taken
-- from: http://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep' where
  sep' [] = Nothing
  sep' l  = Just . fmap (drop 1) . break (==chr) $ l

postfixMatch :: String -> String -> Bool
postfixMatch originalSymbol qName = isPrefixOf (reverse endTerm) (reverse qName)
  where endTerm = last $ separateBy '.' originalSymbol

-- FIXME Is there a way to deconstruct the name properly instead of this kludge?
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


qualifiedName :: String -> String -> Int -> Int -> [String] -> IO [String]
qualifiedName targetFile targetModuleName lineNo colNo importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        getSessionDynFlags >>= setDynamicFlags (GhcOptions [])

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

    let opts = ["find-module", m', "--simple-output"] ++ myOptsTmp'
    putStrLn $ "ghc-pkg " ++ (unwords opts)

    (_, Just hout, Just herr, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                                       , std_out = CreatePipe
                                                                       , std_err = CreatePipe
                                                                       }

    output <- readRestOfHandle hout
    err    <- readRestOfHandle herr
    putStrLn $ "ghcPkgFindModule stdout: " ++ output
    putStrLn $ "ghcPkgFindModule stderr: " ++ err

    return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

ghcPkgHaddockUrl :: String -> IO (Maybe String)
ghcPkgHaddockUrl p = do
    let opts = ["field", p, "haddock-html"] ++ myOptsTmp'
    print opts

    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
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

-- If symbol == "X.foo" and an element of hmodules is an import of the original form
-- "import Y.Z as X" then we return Just "Y.Z.foo".
--
-- FIXME assert length 1 as well?
expandMatchingAsImport :: String -> [HaskellModule] -> Maybe String
expandMatchingAsImport symbol hmodules = case x of (Just (h, (Just cp))) -> Just $ (modName h) ++ (drop (length cp) symbol)
                                                   _                     -> Nothing
    where x = Safe.headMay $ filter (isJust . snd) $ zip hmodules (map (cmpMod symbol) hmodules)

          cmpMod s (HaskellModule _ _ _ _ (Just impAs) _) = if impAs `isPrefixOf` s
                                                               then Just $ commonPrefix s impAs
                                                               else Nothing
          cmpMod _ _ = Nothing

          -- http://www.haskell.org/pipermail/beginners/2011-April/006856.html
          commonPrefix :: Eq a => [a] -> [a] -> [a]
          commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

specificallyMatches :: String -> [HaskellModule] -> [HaskellModule]
specificallyMatches symbol importList = filter (\h -> symbol `elem` modSpecifically h) importList

toHackageUrl :: String -> String -> String
toHackageUrl f m = "https://hackage.haskell.org/package/" ++ f''
    where x = fromJust $ substringP m f -- FIXME brittle use of fromJust
          f' = drop x f
          f'' = map repl f'

          repl '\\' = '/'
          repl c    = c

          -- http://www.haskell.org/pipermail/haskell-cafe/2010-June/078702.html
          substringP :: String -> String -> Maybe Int
          substringP _ []  = Nothing
          substringP sub str = case isPrefixOf sub str of
            False -> fmap (+1) $ substringP sub (tail str)
            True  -> Just 0

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
    forM_ importList $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))

    importListRaw <- getImports targetFile targetModule
    forM_ importListRaw $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))
    putStrLn "############################################################"

    -- importListRaw <- (map toHaskellModule) <$> getImports targetFile targetModule
    -- forM_ importListRaw $ \x -> putStrLn $ "  " ++ (show x)
    -- putStrLn "############################################################"

    qnames <- (filter (not . (' ' `elem`))) <$> qualifiedName targetFile targetModule lineNo colNo importList :: IO [String]

    putStrLn "<qnames>"
    forM_ qnames putStrLn
    putStrLn "</qnames>"
    putStrLn ""

    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    let aai = expandMatchingAsImport symbol (map toHaskellModule importListRaw)
    putStrLn $ "symbol: " ++ (show symbol)
    putStrLn $ "importListRaw: " ++ (show $ map toHaskellModule importListRaw)
    putStrLn $ "aai: " ++ (show aai)
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

    let postMatches = filter (postfixMatch symbol) qnames :: [String]

        -- symbol' = if postMatches == [] then symbol else minimumBy (compare `on` length) postMatches -- Flaky?
        symbol' = if isJust aai
                    then fromJust aai
                    else if postMatches == []
                            then symbol
                            else minimumBy (compare `on` length) postMatches -- Flaky?

    putStrLn $ "symbol:  " ++ symbol
    putStrLn $ "symbol': " ++ symbol'


    let maybeExtraModule = moduleOfQualifiedName symbol'
        importList' = if symbol == symbol' then importList else importList ++ [fromJust maybeExtraModule]

    -- putStrLn $ "try to match on: " ++ (show (symbol, qnames))
    -- putStrLn $ "postMatches: " ++ (show postMatches)
    putStrLn $ "importlist: " ++ (show importList)
    putStrLn $ "importlist': " ++ (show importList')

    let smatches = specificallyMatches symbol (map toHaskellModule importListRaw)

    putStrLn $ "smatches: " ++ (show smatches)

    -- do this here, earlier, or what?
    let symbol'' = if smatches == []
                        then symbol'
                        else (modName $ head smatches) ++ "." ++ symbol

    x <- lookupSymbol targetFile targetModule symbol'' importList'

    forM_ x $ \(name, lookUp) -> do putStrLn $ "file: " ++ targetFile
                                    putStrLn $ "module: " ++ targetModule
                                    putStrLn $ "supplied symbol: " ++ symbol
                                    putStrLn $ "inferred symbol: " ++ symbol''
                                    putStrLn $ "name: " ++ (showSDoc tracingDynFlags (ppr name))
                                    -- putStrLn $ "imports: " ++ (show importList')

                                    let -- definedIn = symbolDefinedIn name
                                        -- importedFrom = Safe.headMay $ concat $ map symbolImportedFrom lookUp :: Maybe ModuleName
                                        importedFrom = if smatches == []
                                                            then Safe.headMay $ concat $ map symbolImportedFrom lookUp :: Maybe ModuleName
                                                            else (Just . mkModuleName . fromJust . moduleOfQualifiedName) symbol'' -- FIXME dangerous fromJust

                                    putStrLn $ "all imported froms: " ++ (showSDoc tracingDynFlags (ppr $ concat $ map symbolImportedFrom lookUp))

                                    putStrLn $ "importedFrom: " ++ (showSDoc tracingDynFlags (ppr $ importedFrom))

                                    m' <- maybe (return Nothing) ghcPkgFindModule importedFrom
                                    putStrLn $ "ghcPkgFindModule: " ++ (show m')

                                    let base = moduleNameToHtmlFile <$> importedFrom

                                    haddock <- fmap (filter ((/=) '"')) <$> maybe (return Nothing) ghcPkgHaddockUrl m'

                                    print "haddock:"
                                    print haddock
                                    print "m':"
                                    print m'

                                    if isNothing haddock || isNothing m'
                                        then putStrLn $ "haddock: 111FAIL111"
                                        else do let f = (fromJust haddock) </> (fromJust base)
                                                e <- doesFileExist f

                                                if e then putStrLn $ "SUCCESS: " ++ "file://" ++ f
                                                     else putStrLn $ "SUCCESS: " ++ (toHackageUrl f (fromJust m'))
                                    -- putStrLn $ "defined in: " ++ (showSDoc tracingDynFlags (ppr $ definedIn))

                                    -- if importedFrom == []
                                    --     then putStrLn $ "imported from: 000FAIL000"
                                    --     else putStrLn $ "imported from: " ++ (showSDoc tracingDynFlags (ppr $ head $ importedFrom))

