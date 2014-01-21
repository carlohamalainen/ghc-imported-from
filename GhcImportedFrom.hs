{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GhcImportedFrom
-- Copyright   :  Carlo Hamalainen 2013, 2014
-- License     :  BSD3
--
-- Maintainer  :  carlo@carlo-hamalainen.net
-- Stability   :  experimental
-- Portability :  portable
--
-- Synopsis: Attempt to guess the location of the Haddock HTML
-- documentation for a given symbol in a particular module, file, and
-- line/col location.

module GhcImportedFrom ( QualifiedName(..)
                       , Symbol(..)
                       , GhcOptions(..)
                       , GhcPkgOptions(..)
                       , HaskellModule(..)
                       , ghcOptionToGhcPKg
                       , getGhcOptionsViaGhcMod
                       , getGHCOptionsViaCradle
                       , modifyDFlags
                       , setDynamicFlags
                       , getTextualImports
                       , getSummary
                       , toHaskellModule
                       , lookupSymbol
                       , symbolImportedFrom
                       , postfixMatch
                       , moduleOfQualifiedName
                       , qualifiedName
                       , ghcPkgFindModule
                       , ghcPkgHaddockUrl
                       , moduleNameToHtmlFile
                       , expandMatchingAsImport
                       , specificallyMatches
                       , toHackageUrl
                       , bestPrefixMatches
                       , findHaddockModule
                       , matchToUrl
                       , guessHaddockUrl
                       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Instances()
import Control.Monad.Writer
import Control.Monad.Trans as CMT
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Typeable()
import Desugar()
import DynFlags
import FastString
import GHC
import GHC.Paths (libdir)
import GHC.SYB.Utils()
import HscTypes
import Name
import Outputable
import RdrName
import System.Directory
import System.Environment()
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

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal

import UtilsFromGhcMod

type QualifiedName = String -- ^ A qualified name, e.g. @Foo.bar@.

type Symbol = String -- ^ A symbol, possibly qualified, e.g. @bar@ or @Foo.bar@.

newtype GhcOptions
    -- | List of user-supplied GHC options, refer to @tets@ subdirectory for example usage. Note that
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

-- | Convert a GHC command line option to a @ghc-pkg@ command line option. This function
-- is incomplete; it only handles a few cases at the moment.
ghcOptionToGhcPKg :: [String] -> [String]
ghcOptionToGhcPKg [] = []
ghcOptionToGhcPKg (x:xs) = case x of "-no-user-package-db" -> "--no-user-package-db":ghcOptionToGhcPKg xs
                                     "-package-db"         -> ["--package-db", head xs] ++ ghcOptionToGhcPKg (tail xs)
                                     _                     -> error $ "Unknown GHC option: " ++ show (x:xs) -- FIXME Other cases?

-- | Use ghcmod's API to get the GHC options for a project. This uses 'findCradle', 'cradlePackageDbOpts', and 'GhcOptions'.
getGhcOptionsViaGhcMod :: IO GhcOptions
getGhcOptionsViaGhcMod = GhcOptions . cradlePackageDbOpts <$> findCradle

-- | Use ghcmod's API to get the GHC options for a project. This uses 'findCradle' and 'getGHCOptions.'
getGHCOptionsViaCradle :: IO [GHCOption]
getGHCOptionsViaCradle = do
    c <- findCradle
    pkgDesc <- GhcMonad.liftIO $ parseCabalFile $ fromJust $ cradleCabalFile c
    let binfo = head $ cabalAllBuildInfo pkgDesc
    getGHCOptions [] c (fromJust $ cradleCabalDir c) binfo

-- | Add user-supplied GHC options to those discovered via ghc-mod.
modifyDFlags :: [String] -> DynFlags -> IO ([String], [GHCOption], DynFlags)
modifyDFlags ghcOpts0 dflags0 =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
        runGhc (Just libdir) $ do
            (GhcOptions ghcOpts1) <- GhcMonad.liftIO getGhcOptionsViaGhcMod
            ghcOpts2 <- GhcMonad.liftIO getGHCOptionsViaCradle

            -- FIXME need to add ghcOpts1 and ghcOpts2 to the WriterT, but here
            -- we are inside the GhcMonad, so need to do some transformer stuff.
            -- Instead we laboriously return ghcOpts1 and ghcOpts2 up the call chain.
            -- GhcMonad.liftIO $ putStrLn $ "ghcOpts1: " ++ show ghcOpts1
            -- GhcMonad.liftIO $ putStrLn $ "ghcOpts2: " ++ show ghcOpts2

            (dflags1, _, _) <- GHC.parseDynamicFlags dflags0 (map SrcLoc.noLoc $ ghcOpts1 ++ ghcOpts2 ++ ghcOpts0)

            let dflags2 = dflags1 { hscTarget = HscInterpreted
                                  , ghcLink = LinkInMemory
                                  }

            return (ghcOpts1, ghcOpts2, dflags2)

-- | Set GHC options and run 'initPackages' in 'GhcMonad'.
--
-- Typical use:
--
-- > defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
-- >    runGhc (Just libdir) $ do
-- >        getSessionDynFlags >>= setDynamicFlags (GhcOptions myGhcOptionList)
-- >        -- do stuff
setDynamicFlags :: GhcMonad m => GhcOptions -> DynFlags -> m ([String], [GHCOption], DynFlags)
setDynamicFlags (GhcOptions extraGHCOpts) dflags0 = do
    (ghcOpts1, ghcOpts2, dflags1) <- GhcMonad.liftIO $ modifyDFlags extraGHCOpts dflags0
    void $ setSessionDynFlags dflags1
    _ <- GhcMonad.liftIO $ Packages.initPackages dflags1
    return (ghcOpts1, ghcOpts2, dflags1)

-- |Read the textual imports in a file.
--
-- Example:
--
-- >>> (showSDoc tracingDynFlags) . ppr <$> getTextualImports "test/data/Hiding.hs" "Hiding" >>= putStrLn
-- [ import (implicit) Prelude, import qualified Safe
-- , import System.Environment ( getArgs )
-- , import Data.List hiding ( map )
-- ]
--
-- See also 'toHaskellModule' and 'getSummary'.

getTextualImports :: GhcOptions -> FilePath -> String -> WriterT [String] IO [SrcLoc.Located (ImportDecl RdrName)]
getTextualImports ghcopts targetFile targetModuleName = do
    (ghcOpts1, ghcOpts2, modSum) <- CMT.liftIO $ getSummary ghcopts targetFile targetModuleName

    myTell $ "getTextualImports: ghcOpts1: " ++ show ghcOpts1
    myTell $ "getTextualImports: ghcOpts2: " ++ show ghcOpts2

    return $ ms_textual_imps modSum

-- | Get the module summary for a particular file/module. The first and second components of the
-- return value are @ghcOpts1@ and @ghcOpts2@; see 'setDynamicFlags'.
getSummary :: GhcOptions -> FilePath -> String -> IO ([String], [GHCOption], ModSummary)
getSummary ghcopts targetFile targetModuleName =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
        runGhc (Just libdir) $ do
            (ghcOpts1, ghcOpts2, _) <- getSessionDynFlags >>= setDynamicFlags ghcopts

            -- Load the target file (e.g. "Muddle.hs").
            target <- guessTarget targetFile Nothing
            setTargets [target]
            _ <- load LoadAllTargets

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]

            -- Extract the module summary.
            modSum <- getModSummary (mkModuleName targetModuleName)

            return (ghcOpts1, ghcOpts2, modSum)

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
-- >>> map toHaskellModule <$> getTextualImports "tests/data/data/Hiding.hs" "Hiding" >>= print
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
          name       = showSDoc tracingDynFlags (ppr $ GHC.ideclName idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = map removeBrackets $ (catMaybes . parseHiding . GHC.ideclHiding) idecl'
          importedAs = (showSDoc tracingDynFlags . ppr) <$> ideclAs idecl'
          specifically = map removeBrackets $ (parseSpecifically . GHC.ideclHiding) idecl'

          removeBrackets :: [a] -> [a]
          removeBrackets [] = []
          removeBrackets x = (init . tail) x

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
-- >>> x <- lookupSymbol "tests/data/data/Hiding.hs" "Hiding" "head" ["Prelude", "Safe", "System.Environment", "Data.List"]
-- *GhcImportedFrom> putStrLn . (showSDoc tracingDynFlags) . ppr $ x
-- [(GHC.List.head,
--   [GHC.List.head
--      imported from `Data.List' at tests/data/data/Hiding.hs:5:1-29
--      (and originally defined in `base:GHC.List')])]

lookupSymbol :: GhcOptions -> FilePath -> String -> String -> [String] -> IO [(Name, [GlobalRdrElt])]
lookupSymbol ghcopts targetFile targetModuleName qualifiedSymbol importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
      runGhc (Just libdir) $ do
        _ <- getSessionDynFlags >>= setDynamicFlags ghcopts

        target <- guessTarget targetFile Nothing
        setTargets [target]
        _ <- load LoadAllTargets

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
postfixMatch originalSymbol qName = endTerm `isSuffixOf` qName
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
moduleOfQualifiedName qn = if null bits
                                then Nothing
                                else Just $ intercalate "." bits
  where bits = reverse $ drop 1 $ reverse $ separateBy '.' qn

-- | Find the possible qualified names for the symbol at line/col in the given Haskell file and module.
--
-- Example:
--
-- >>> x <- qualifiedName "tests/data/data/Muddle.hs" "Muddle" 27 5 ["Data.Maybe", "Data.List", "Data.Map", "Safe"]
-- >>> forM_ x print
-- "AbsBinds [] []\n  {Exports: [Muddle.h <= h\n               <>]\n   Exported types: Muddle.h\n                     :: Data.Map.Base.Map GHC.Base.String GHC.Base.String\n                   [LclId]\n   Binds: h = Data.Map.Base.fromList [(\"x\", \"y\")]}"
-- "h = Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList [(\"x\", \"y\")]"
-- "Data.Map.Base.fromList"

qualifiedName :: GhcOptions -> FilePath -> String -> Int -> Int -> [String] -> IO [String]
qualifiedName ghcopts targetFile targetModuleName lineNr colNr importList =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
      runGhc (Just libdir) $ do
        _ <- getSessionDynFlags >>= setDynamicFlags ghcopts

        target <- guessTarget targetFile Nothing
        setTargets [target]
        _ <- load LoadAllTargets

        setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
        p <- parseModule modSummary   :: Ghc ParsedModule
        t <- typecheckModule p        :: Ghc TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
            es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]

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
ghcPkgFindModule :: GhcPkgOptions -> String -> WriterT [String] IO (Maybe String)
ghcPkgFindModule (GhcPkgOptions extraGHCPkgOpts) m = do

    (GhcOptions gopts) <- CMT.liftIO getGhcOptionsViaGhcMod :: WriterT [String] IO GhcOptions

    let opts = ["find-module", m, "--simple-output"] ++ ["--global", "--user"] ++ ghcOptionToGhcPKg gopts ++ extraGHCPkgOpts
    myTell $ "ghc-pkg " ++ show opts

    (_, Just hout, Just herr, _) <- CMT.liftIO $ createProcess (proc "ghc-pkg" opts){ std_in  = CreatePipe
                                                                                    , std_out = CreatePipe
                                                                                    , std_err = CreatePipe
                                                                                    }

    output <- CMT.liftIO $ readRestOfHandle hout
    err    <- CMT.liftIO $ readRestOfHandle herr
    myTell $ "ghcPkgFindModule stdout: " ++ output
    myTell $ "ghcPkgFindModule stderr: " ++ err

    return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

-- | Call @ghc-pkg field@ to get the @haddock-html@ field for a package.
ghcPkgHaddockUrl :: GhcPkgOptions -> String -> WriterT [String] IO (Maybe String)
ghcPkgHaddockUrl (GhcPkgOptions extraGHCPkgOpts) p = do
    (GhcOptions gopts) <- CMT.liftIO getGhcOptionsViaGhcMod :: WriterT [String] IO GhcOptions

    let opts = ["field", p, "haddock-html"] ++ ["--global", "--user"] ++ ghcOptionToGhcPKg gopts ++ extraGHCPkgOpts
    myTell $ "ghc-pkg "++ show opts

    (_, Just hout, _, _) <- CMT.liftIO $ createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                                            , std_out = CreatePipe
                                                                            , std_err = CreatePipe
                                                                            }

    line <- CMT.liftIO $ (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
    return $ Safe.lastMay $ words line

-- | Convert a module name string, e.g. @Data.List@ to @Data-List.html@.
moduleNameToHtmlFile :: String -> String
moduleNameToHtmlFile m =  map f m ++ ".html"
    where f :: Char -> Char
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
-- >>> hmodules <- map toHaskellModule <$> getTextualImports "tests/data/data/Muddle.hs" "Muddle"
-- >>> print $ expandMatchingAsImport "DL.fromList" hmodules
-- Just "Data.List.fromList"

expandMatchingAsImport :: QualifiedName -> [HaskellModule] -> Maybe QualifiedName
expandMatchingAsImport symbol hmodules = case x of (Just (h, Just cp)) -> Just $ modName h ++ drop (length cp) symbol
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
-- >>> hmodules <- map toHaskellModule <$> getTextualImports "tests/data/data/Hiding.hs" "Hiding"
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
specificallyMatches symbol = filter (\h -> symbol `elem` modSpecifically h)

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

          -- Adapted from http://www.haskell.org/pipermail/haskell-cafe/2010-June/078702.html
          substringP :: String -> String -> Maybe Int
          substringP _ []  = Nothing
          substringP sub str = if sub `isPrefixOf` str then Just 0 else fmap (+1) $ substringP sub (tail str)

-- | When we use 'parseName' to convert a 'String' to a 'Name' we get a list of matches instead of
-- a unique match, so we end up having to guess the best match based on the qualified name.
bestPrefixMatches :: Name -> [GlobalRdrElt] -> [String]
bestPrefixMatches name lookUp = x''
    where name' = showSDoc tracingDynFlags $ ppr name
          name'' = fromJust $ moduleOfQualifiedName name' -- FIXME dangerous fromJust
          x   = concatMap symbolImportedFrom lookUp
          x'  = map (showSDoc tracingDynFlags . ppr) x
          x'' = filter (name'' `isPrefixOf`) x'

-- | Find the haddock module. Returns a 4-tuple consisting of: module that the symbol is imported
-- from, haddock url, module, and module's HTML filename.
findHaddockModule :: QualifiedName -> [HaskellModule] -> GhcPkgOptions -> (Name, [GlobalRdrElt]) -> WriterT [String] IO (Maybe String, Maybe String, Maybe String, Maybe String)
findHaddockModule symbol'' smatches ghcPkgOpts (name, lookUp) = do
    myTell $ "name: " ++ showSDoc tracingDynFlags (ppr name)

    let definedIn = nameModule name
        bpms = bestPrefixMatches name lookUp
        importedFrom = if null smatches
                            -- then Safe.headMay $ concatMap symbolImportedFrom lookUp :: Maybe ModuleName
                            -- FIXME should really return *all* of these matches, not just the first one. We
                            -- can't be certain that we're choosing the best one. Ditto for all other
                            -- uses of head or Safe.headMay.
                            then if null bpms then Safe.headMay $ map (showSDoc tracingDynFlags . ppr) $ concatMap symbolImportedFrom lookUp
                                              else Safe.headMay bpms :: Maybe String
                            else (Just . (showSDoc tracingDynFlags . ppr) . mkModuleName . fromJust . moduleOfQualifiedName) symbol'' -- FIXME dangerous fromJust

    myTell $ "definedIn: " ++ showSDoc tracingDynFlags (ppr definedIn)
    myTell $ "bpms: " ++ show bpms
    myTell $ "concat $ map symbolImportedFrom lookUp: " ++ showSDoc tracingDynFlags (ppr $ concatMap symbolImportedFrom lookUp)


    myTell $ "importedFrom: " ++ show importedFrom

    foundModule <- maybe (return Nothing) (ghcPkgFindModule ghcPkgOpts) importedFrom
    myTell $ "ghcPkgFindModule result: " ++ show foundModule

    let base = moduleNameToHtmlFile <$> importedFrom

    myTell $ "base: : " ++ show base

    -- haddock <- fmap (filter ('"' /=)) <$> maybe (return Nothing) (ghcPkgHaddockUrl ghcPkgOpts) foundModule
    haddock <- maybe (return Nothing) (ghcPkgHaddockUrl ghcPkgOpts) foundModule

    myTell $ "haddock: " ++ show haddock
    myTell $ "foundModule: " ++ show foundModule

    return (importedFrom, haddock, foundModule, base)

myTell :: MonadWriter [t] m => t -> m ()
myTell x = tell [x]

-- | Convert our match to a URL, either @file://@ if the file exists, or to @hackage.org@ otherwise.
matchToUrl :: (Maybe String, Maybe String, Maybe String, Maybe String) -> WriterT [String] IO String
matchToUrl (importedFrom, haddock, foundModule, base) = do
    let importedFrom' = fromJust importedFrom
        haddock'      = fromJust haddock
        foundModule'  = fromJust foundModule
        base'         = fromJust base

        f = haddock' </> base'

    e <- CMT.liftIO $ doesFileExist f

    if e then return $ "file://" ++ f
         else do myTell $ "f:  " ++ show f
                 myTell $ "foundModule: " ++ show foundModule'
                 return $ toHackageUrl f foundModule' (showSDoc tracingDynFlags (ppr importedFrom'))


-- | Attempt to guess the Haddock url, either a local file path or url to @hackage.haskell.org@
-- for the symbol in the given file, module, at the specified line and column location.
--
-- Example:
--
-- >>> guessHaddockUrl "tests/data/data/Muddle.hs" "Muddle" "Maybe" 11 11
-- (lots of output)
-- SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html

guessHaddockUrl :: FilePath -> String -> Symbol -> Int -> Int -> GhcOptions -> GhcPkgOptions -> WriterT [String] IO (Either String String)
guessHaddockUrl targetFile targetModule symbol lineNr colNr (GhcOptions ghcOpts0) ghcPkgOpts = do
    myTell $ "targetFile: " ++ targetFile
    myTell $ "targetModule: " ++ targetModule
    myTell $ "symbol: " ++ show symbol
    myTell $ "line nr: " ++ show lineNr
    myTell $ "col nr: " ++ show colNr

    textualImports <- getTextualImports (GhcOptions ghcOpts0) targetFile targetModule

    let haskellModuleNames = map (modName . toHaskellModule) textualImports
    myTell $ "haskellModuleNames: " ++ show haskellModuleNames

    qnames <- CMT.liftIO $ filter (not . (' ' `elem`)) <$> qualifiedName (GhcOptions ghcOpts0) targetFile targetModule lineNr colNr haskellModuleNames

    myTell $ "qualified names: " ++ show qnames

    let matchingAsImport = expandMatchingAsImport symbol (map toHaskellModule textualImports)
    myTell $ "matchingAsImport: " ++ show matchingAsImport

    let postMatches = filter (postfixMatch symbol) qnames :: [String]
        symbol' = fromMaybe (if null postMatches then symbol else minimumBy (compare `on` length) postMatches) matchingAsImport

    myTell $ "postMatches:  " ++ show postMatches
    myTell $ "symbol': " ++ symbol'

    let maybeExtraModule = moduleOfQualifiedName symbol'
        haskellModuleNames' = if symbol == symbol' then haskellModuleNames else haskellModuleNames ++ [fromJust maybeExtraModule]

    myTell $ "maybeExtraModule: " ++ show maybeExtraModule
    myTell $ "haskellModuleNames': " ++ show haskellModuleNames'

    let smatches = specificallyMatches symbol (map toHaskellModule textualImports)
    myTell $ "smatches: " ++ show smatches

    let symbol'' = if null smatches
                        then symbol'
                        else modName (head smatches) ++ "." ++ symbol

    myTell $ "symbol'': " ++ symbol''

    let allJust (a, b, c, d) = isJust a && isJust b && isJust c && isJust d

    final1 <- CMT.liftIO $ lookupSymbol (GhcOptions ghcOpts0) targetFile targetModule symbol'' haskellModuleNames'
    final2 <- filter allJust <$> mapM (findHaddockModule symbol'' smatches ghcPkgOpts) final1

    final3 <- mapM matchToUrl final2

    return (if null final3 then Left "No matches found."
                           else Right $ head final3)
