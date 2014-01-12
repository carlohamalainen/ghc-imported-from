{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Synopsis: Attempt to guess the location of the Haddock HTML
documentation for a given symbol in a particular module, file, and
line/col location.

See the tests directory for some example usage.

Author: Carlo Hamalainen <carlo@carlo-hamalainen.net>

-}

module GhcImportedFrom ( QualifiedName
                       , Symbol
                       , GhcOptions (..)
                       , GhcPkgOptions (..)
                       , HaskellModule (..)
                       , setDynamicFlags
                       , getTextualImports
                       , toHaskellModule
                       , symbolDefinedIn
                       , symbolImportedFrom
                       , postfixMatch
                       , moduleOfQualifiedName
                       , qualifiedName
                       , ghcPkgFindModule
                       , ghcPkgHaddockUrl
                       , moduleNameToHtmlFile
                       , lookupSymbol
                       , expandMatchingAsImport
                       , specificallyMatches
                       , guessHaddockUrl
                       ) where

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

import qualified DynFlags()
import qualified GhcMonad
import qualified MonadUtils()
import qualified Packages
import qualified SrcLoc
import qualified Safe

type QualifiedName = String -- ^ A qualified name, e.g. "Foo.bar".

type Symbol = String -- ^ A symbol, possibly qualified, e.g. "bar" or "Foo.bar".

newtype GhcOptions
    -- | List of user-supplied GHC options, refer to @tests@ subdirectory for example usage. Note that
    -- GHC API and ghc-pkg have inconsistencies in the naming of options, see <http://www.vex.net/~trebla/haskell/sicp.xhtml> for more details.
    = GhcOptions [String] deriving (Show)

newtype GhcPkgOptions
    -- | List of user-supplied ghc-pkg options.
    = GhcPkgOptions [String] deriving (Show)

data HaskellModule
    -- | Information about an import of a Haskell module.
    = HaskellModule { modName           :: String
                    , modQualifier      :: Maybe String
                    , modIsImplicit     :: Bool
                    , modHiding         :: [String]
                    , modImportedAs     :: Maybe String
                    , modSpecifically   :: [String]
                    } deriving (Show, Eq)

-- |Set GHC options and run 'Packages.initPackages' in the 'GhcMonad'.
--
-- Typical use:
--
-- > defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
-- >    runGhc (Just libdir) $ do
-- >        getSessionDynFlags >>= setDynamicFlags (GhcOptions myGhcOptionList)
-- >        -- do stuff
setDynamicFlags :: GhcOptions -> DynFlags -> Ghc DynFlags
setDynamicFlags (GhcOptions ghcOpts) dflags0 = do
    -- FIXME actually use ghcOpts, not myOptsTmp!
    -- FIXME parse the language options like Opt_TemplateHaskell from the source file.
    -- let argv0 = myOptsTmp
    let argv0 = ghcOpts

    let dflags1 = foldl xopt_set dflags0 [ Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash, Opt_MagicHash
                                         , Opt_TemplateHaskell, Opt_QuasiQuotes, Opt_OverloadedStrings
                                         , Opt_TypeFamilies, Opt_FlexibleContexts, Opt_GADTs
                                         , Opt_MultiParamTypeClasses] -- FIXME What if the user does not want an implicit prelude?
        dflags2 = dflags1 { hscTarget = HscInterpreted
                          , ghcLink = LinkInMemory
                          }

    (dflags3, _, _) <- GHC.parseDynamicFlags dflags2 (map SrcLoc.noLoc argv0) -- FIXME check for errors/warnings?

    setSessionDynFlags dflags3

    GhcMonad.liftIO $ Packages.initPackages dflags3

    return dflags3

-- |Read the textual imports in a file.
--
-- Example:
--
-- >>> (showSDoc tracingDynFlags) . ppr <$> getTextualImports "tests/Hiding.hs" "Hiding" >>= putStrLn
-- [ import (implicit) Prelude, import qualified Safe
-- , import System.Environment ( getArgs )
-- , import Data.List hiding ( map )
-- ]
--
-- See also 'toHaskellModule'.

getTextualImports :: FilePath -> String -> IO [SrcLoc.Located (ImportDecl RdrName)]
getTextualImports targetFile targetModuleName =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            getSessionDynFlags >>= setDynamicFlags (GhcOptions [])

            -- Load the target file (e.g. "Muddle.hs").
            target <- guessTarget targetFile Nothing
            setTargets [target]
            load LoadAllTargets

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]

            -- Extract the module summary and the *textual* imports.
            modSum <- getModSummary $ mkModuleName targetModuleName

            return $ ms_textual_imps modSum

-- |Convenience function for converting an 'GHC.ImportDecl' to a 'HaskellModule'.
--
-- Example:
--
-- > -- Hiding.hs
-- > module Hiding where
-- > import Data.List hiding (map)
-- > import System.Environment (getArgs)
-- > import qualified Safe
--
-- then:
--
-- >>> map toHaskellModule <$> getTextualImports "tests/Hiding.hs" "Hiding" >>= print
-- [ HaskellModule { modName = "Prelude"
--                 , modQualifier = Nothing
--                 , modIsImplicit = True
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- , HaskellModule {modName = "Safe"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- , HaskellModule { modName = "System.Environment"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = ["getArgs"]
--                 }
-- , HaskellModule { modName = "Data.List"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = ["map"]
--                 , modImportedAs = Nothing
--                 , modSpecifically = []
--                 }
-- ]

toHaskellModule :: SrcLoc.Located (GHC.ImportDecl GHC.RdrName) -> HaskellModule
toHaskellModule idecl = HaskellModule name qualifier isImplicit hiding importedAs specifically
    where idecl'     = SrcLoc.unLoc idecl
          name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName $ idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = map removeBrackets $ (catMaybes . parseHiding . GHC.ideclHiding) idecl'
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
          parseHiding (Just (False, _)) = []

          -- Actually hid names, e.g.
          --
          --     import Data.List hiding (map)
          parseHiding (Just (True, h))  = map (Just . grabNames) h

          parseSpecifically :: Maybe (Bool, [Located (IE RdrName)]) -> [String]
          parseSpecifically (Just (False, h)) = map grabNames h
          parseSpecifically _                 = []

-- |Find all matches for a symbol in a source file. The last parameter is a list of
-- imports.
--
-- Example:
--
-- >>> x <- lookupSymbol "tests/Hiding.hs" "Hiding" "head" ["Prelude", "Safe", "System.Environment", "Data.List"]
-- *GhcImportedFrom> putStrLn . (showSDoc tracingDynFlags) . ppr $ x
-- [(GHC.List.head,
--   [GHC.List.head
--      imported from `Data.List' at tests/Hiding.hs:5:1-29
--      (and originally defined in `base:GHC.List')])]

lookupSymbol :: FilePath -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
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
        let occNames        = map nameOccName names                 :: [OccName]
            occNamesLookups = map (lookupGlobalRdrEnv gre) occNames :: [[GlobalRdrElt]]

        return $ zip names occNamesLookups

-- | Module in which a name is defined.
symbolDefinedIn :: Name -> Module
symbolDefinedIn occname = nameModule occname

-- | List of possible modules which have resulted in
-- the name being in the current scope. Using a
-- global reader we get the provenance data and then
-- get the list of import specs.
symbolImportedFrom :: GlobalRdrElt -> [ModuleName]
symbolImportedFrom occNameLookup = map importSpecModule whys
  where prov = gre_prov occNameLookup :: Provenance
        Imported (whys :: [ImportSpec])  = prov

-- This definition of separateBy is taken
-- from: http://stackoverflow.com/a/4978733
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep' where
  sep' [] = Nothing
  sep' l  = Just . fmap (drop 1) . break (==chr) $ l

-- | Returns True if the 'Symbol' matches the end of the 'QualifiedName'.
--
-- Example:
--
-- >>> postfixMatch "bar" "Foo.bar"
-- True
-- >>> postfixMatch "bar" "Foo.baz"
-- False
-- >>> postfixMatch "bar" "bar"
-- True
postfixMatch :: Symbol -> QualifiedName -> Bool
postfixMatch originalSymbol qName = isPrefixOf (reverse endTerm) (reverse qName)
  where endTerm = last $ separateBy '.' originalSymbol

-- | Get the module part of a qualified name.
--
-- Example:
--
-- >>> moduleOfQualifiedName "Foo.bar"
-- Just "Foo"
-- >>> moduleOfQualifiedName "bar"
-- Nothing
moduleOfQualifiedName :: QualifiedName -> Maybe String
moduleOfQualifiedName qn = if bits == []
                                then Nothing
                                else Just $ concat $ intersperse "." bits
  where bits = reverse $ drop 1 $ reverse $ separateBy '.' qn

-- listifySpans and listifyStaged are copied from ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

-- | Find the possible qualified names for the symbol at line/col in the given Haskell file and module.
--
-- Example:
--
-- >>> x <- qualifiedName "tests/Muddle.hs" "Muddle" 27 5 ["Data.Maybe", "Data.List", "Data.Map", "Safe"]
-- >>> forM_ x print
-- "AbsBinds [] []\n  {Exports: [Muddle.h <= h\n               <>]\n   Exported types: Muddle.h\n                     :: Data.Map.Base.Map GHC.Base.String GHC.Base.String\n                   [LclId]\n   Binds: h = Data.Map.Base.fromList [(\"x\", \"y\")]}"
-- "h = Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList"

qualifiedName :: FilePath -> String -> Int -> Int -> [String] -> IO [String]
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

        let foo x = showSDoc tracingDynFlags $ ppr x
            bs' = map foo bs
            es' = map foo es
            ps' = map foo ps

        return $ bs' ++ es' ++ ps'

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof <- hIsEOF h
    if ineof
        then return ""
        else hGetContents h

-- | Call @ghc-pkg find-module@ to determine that package that provides a module, e.g. @Prelude@ is defined
-- in @base-4.6.0.1@.
ghcPkgFindModule :: GhcPkgOptions -> ModuleName -> IO (Maybe String)
ghcPkgFindModule (GhcPkgOptions ghcPkgOpts) m = do

    let m' = showSDoc tracingDynFlags (ppr $ m)

    let opts = ["find-module", m', "--simple-output"] ++ ghcPkgOpts
    putStrLn $ "ghc-pkg " ++ (show opts)

    (_, Just hout, Just herr, _) <- createProcess (proc "ghc-pkg" opts){ std_in  = CreatePipe
                                                                       , std_out = CreatePipe
                                                                       , std_err = CreatePipe
                                                                       }

    output <- readRestOfHandle hout
    err    <- readRestOfHandle herr
    putStrLn $ "ghcPkgFindModule stdout: " ++ output
    putStrLn $ "ghcPkgFindModule stderr: " ++ err

    return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

-- | Call @ghc-pkg field@ to get the @haddock-html@ field for a package.
ghcPkgHaddockUrl :: GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgHaddockUrl (GhcPkgOptions ghcPkgOpts) p = do
    let opts = ["field", p, "haddock-html"] ++ ghcPkgOpts
    putStrLn $ "ghc-pkg "++ (show opts)

    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                               , std_out = CreatePipe
                                                               , std_err = CreatePipe
                                                               }

    line <- (reverse . (dropWhile (== '\n')) . reverse) <$> readRestOfHandle hout

    return $ Safe.lastMay $ words line

-- | Convert a 'ModuleName' to a 'String', e.g. @Data.List@ to @Data-List.html@.
moduleNameToHtmlFile :: ModuleName -> String
moduleNameToHtmlFile m =  base' ++ ".html"
    where base = showSDoc tracingDynFlags $ ppr m
          base' = map f base

          f :: Char -> Char
          f '.' = '-'
          f c   = c

-- | If the Haskell module has an import like @import qualified Data.List as DL@, convert an
-- occurence @DL.fromList@ to the qualified name using the actual module name: @Data.List.fromList@.
--
-- Example:
--
-- > -- Muddle.hs
-- >
-- > module Muddle where
-- >
-- > import Data.Maybe
-- > import qualified Data.List as DL
-- > import qualified Data.Map as DM
-- > import qualified Safe
--
-- then:
--
-- >>> hmodules <- map toHaskellModule <$> getTextualImports "tests/Muddle.hs" "Muddle"
-- >>> print $ expandMatchingAsImport "DL.fromList" hmodules
-- Just "Data.List.fromList"

expandMatchingAsImport :: QualifiedName -> [HaskellModule] -> Maybe QualifiedName
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

-- | Return list of modules which explicitly import a symbol.
--
-- Example:
--
-- > -- Hiding.hs
-- > module Hiding where
-- > import Data.List hiding (map)
-- > import System.Environment (getArgs)
-- > import qualified Safe
--
-- >>> hmodules <- map toHaskellModule <$> getTextualImports "tests/Hiding.hs" "Hiding"
-- >>> print $ specificallyMatches "getArgs" hmodules
-- [ HaskellModule { modName = "System.Environment"
--                 , modQualifier = Nothing
--                 , modIsImplicit = False
--                 , modHiding = []
--                 , modImportedAs = Nothing
--                 , modSpecifically = ["getArgs"]
--                 }
-- ]

specificallyMatches :: Symbol -> [HaskellModule] -> [HaskellModule]
specificallyMatches symbol importList = filter (\h -> symbol `elem` modSpecifically h) importList

-- | Convert a file path to a Hackage HTML file to its equivalent on @https://hackage.haskell.org@.
toHackageUrl :: FilePath -> String -> String -> String
toHackageUrl filepath package modulename = "https://hackage.haskell.org/package/" ++ package ++ "/" ++ "docs/" ++ modulename''
    where filepath'    = map repl filepath
          modulename'  = head $ separateBy '.' $ head $ separateBy '-' modulename
          modulename'' = drop (fromJust $ substringP modulename' filepath') filepath'

          -- On Windows we get backslashes in the file path; convert
          -- to forward slashes for the URL.
          repl :: Char -> Char
          repl '\\' = '/'
          repl c    = c

          -- http://www.haskell.org/pipermail/haskell-cafe/2010-June/078702.html
          substringP :: String -> String -> Maybe Int
          substringP _ []  = Nothing
          substringP sub str = case isPrefixOf sub str of
            False -> fmap (+1) $ substringP sub (tail str)
            True  -> Just 0

-- | Attempt to guess the Haddock url, either a local file path or url to @hackage.haskell.org@
-- for the symbol in the given file, module, at the specified line and column location.
--
-- Example:
--
-- >>> guessHaddockUrl "tests/Muddle.hs" "Muddle" "Maybe" 11 11
-- (lots of output)
-- SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html
--
guessHaddockUrl :: FilePath -> String -> Symbol -> Int -> Int -> GhcOptions -> GhcPkgOptions -> IO ()
guessHaddockUrl targetFile targetModule symbol lineNo colNo ghcOpts ghcPkgOpts = do
    importList <- (map (modName . toHaskellModule)) <$> getTextualImports targetFile targetModule
    forM_ importList $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))

    importListRaw <- getTextualImports targetFile targetModule
    forM_ importListRaw $ \x -> putStrLn $ "  " ++ (showSDoc tracingDynFlags (ppr $ x))
    putStrLn "############################################################"

    -- importListRaw <- (map toHaskellModule) <$> getTextualImports targetFile targetModule
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

                                    let definedIn = symbolDefinedIn name
                                        -- importedFrom = Safe.headMay $ concat $ map symbolImportedFrom lookUp :: Maybe ModuleName
                                        importedFrom = if smatches == []
                                                            then Safe.headMay $ concat $ map symbolImportedFrom lookUp :: Maybe ModuleName
                                                            else (Just . mkModuleName . fromJust . moduleOfQualifiedName) symbol'' -- FIXME dangerous fromJust

                                    putStrLn $ "defined in: " ++ (showSDoc tracingDynFlags $ ppr definedIn)
                                    putStrLn $ "all imported froms: " ++ (showSDoc tracingDynFlags (ppr $ concat $ map symbolImportedFrom lookUp))

                                    putStrLn $ "importedFrom: " ++ (showSDoc tracingDynFlags (ppr $ importedFrom))

                                    m' <- maybe (return Nothing) (ghcPkgFindModule ghcPkgOpts) importedFrom
                                    putStrLn $ "ghcPkgFindModule: " ++ (show m')

                                    let base = moduleNameToHtmlFile <$> importedFrom

                                    haddock <- fmap (filter ((/=) '"')) <$> maybe (return Nothing) (ghcPkgHaddockUrl ghcPkgOpts) m'

                                    print "haddock:"
                                    print haddock
                                    print "m':"
                                    print m'

                                    if isNothing haddock || isNothing m'
                                        then putStrLn $ "haddock: 111FAIL111"
                                        else do let f = (fromJust haddock) </> (fromJust base)
                                                e <- doesFileExist f

                                                if e then putStrLn $ "SUCCESS: " ++ "file://" ++ f
                                                     else do putStrLn $ "f:  " ++ (show f)
                                                             putStrLn $ "m': " ++ (show (fromJust m'))
                                                             putStrLn $ "pack: " ++ (showSDoc tracingDynFlags (ppr $ fromJust $ importedFrom))
                                                             putStrLn $ "SUCCESS: " ++ (toHackageUrl f (fromJust m') (showSDoc tracingDynFlags (ppr $ fromJust $ importedFrom)))
                                    -- putStrLn $ "defined in: " ++ (showSDoc tracingDynFlags (ppr $ definedIn))

                                    -- if importedFrom == []
                                    --     then putStrLn $ "imported from: 000FAIL000"
                                    --     else putStrLn $ "imported from: " ++ (showSDoc tracingDynFlags (ppr $ head $ importedFrom))
