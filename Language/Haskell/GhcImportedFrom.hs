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
--
-- Latest development version: <https://github.com/carlohamalainen/ghc-imported-from>.

module Language.Haskell.GhcImportedFrom (
     QualifiedName
   , Symbol
   , GhcOptions(..)
   , GhcPkgOptions(..)
   , HaskellModule(..)
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
   , haddockUrl
   , getGhcOptionsViaCabalRepl

   -- Things from Language.Haskell.GhcImportedFrom.Types
   , Options (..)
   , defaultOptions
   , LineSeparator (..)
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Instances()
import Control.Monad.Writer
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

import Language.Haskell.GhcMod (
      findCradle
    , cradleRootDir
    , Cradle(..)
    )

import Language.Haskell.GhcImportedFrom.UtilsFromGhcMod
import Language.Haskell.GhcImportedFrom.Types

import Control.Exception (SomeException)

import qualified Text.Parsec as TP
import Data.Functor.Identity

#if __GLASGOW_HASKELL__ >= 708
import DynFlags ( unsafeGlobalDynFlags )
tdflags = unsafeGlobalDynFlags
#else
import DynFlags ( tracingDynFlags )
tdflags = tracingDynFlags
#endif

type GHCOption = String

getGhcOptionsViaCabalRepl :: IO (Maybe [String])
getGhcOptionsViaCabalRepl = do
    putStrLn $ "getGhcOptionsViaCabalRepl..."

    (Just _, Just hout, Just _, _) <- createProcess (proc "cabal" ["repl", "--with-ghc=fake-ghc-for-ghc-imported-from"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    ineof <- hIsEOF hout

    result <- if ineof
                    then return ""
                    else do firstLine <- hGetLine hout
                            if "GHCi" `isPrefixOf` firstLine
                                 then return "DERP" -- stuffed up, should report error - FIXME change this to an error or something?
                                 else do rest <- readRestOfHandle hout
                                         return $ firstLine ++ "\n" ++ rest

    let result' = filter ("--interactive" `isPrefixOf`) . lines $ result

    case length result' of 1 -> return $ Just $ filterOpts $ words $ head result'
                           _ -> return Nothing

filterOpts :: [String] -> [String]
filterOpts xs = filter (\x -> x /= "--interactive" && x /= "-fbuilding-cabal-package" && x /= "-Wall") $ dropModuleNames xs

dropModuleNames :: [String] -> [String]
dropModuleNames = filter parseHelper

parseHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
parseHaskellModuleName = do
    c <- TP.upper
    cs <- TP.many (TP.choice [TP.lower, TP.upper, TP.char '_', TP.digit])
    return (c:cs)

parseDottedHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
parseDottedHaskellModuleName = do
    TP.char '.'
    cs <- parseHaskellModuleName
    return cs

parseFullHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
parseFullHaskellModuleName = do
    h <- parseHaskellModuleName
    rest <- many parseDottedHaskellModuleName

    return $ intercalate "." (h:rest)

parseHelper :: String -> Bool
parseHelper s = case (TP.parse (parseFullHaskellModuleName <* TP.eof) "" s) of Right _ -> False
                                                                               Left _  -> True

getGhcOptionsViaCabalReplOrEmpty :: IO [String]
getGhcOptionsViaCabalReplOrEmpty =  liftM (fromMaybe []) getGhcOptionsViaCabalRepl

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

-- | Add user-supplied GHC options to those discovered via cabl repl.
modifyDFlags :: [String] -> DynFlags -> IO ([GHCOption], DynFlags)
modifyDFlags ghcOpts0 dflags0 =
    -- defaultErrorHandler defaultFatalMessager defaultFlushOut $
        runGhc (Just libdir) $ do
            ghcOpts1 <- GhcMonad.liftIO getGhcOptionsViaCabalReplOrEmpty

            (dflags1, _, _) <- GHC.parseDynamicFlags dflags0 (map SrcLoc.noLoc $ ghcOpts0 ++ ghcOpts1)

            let dflags2 = dflags1 { hscTarget = HscInterpreted
                                  , ghcLink = LinkInMemory
                                  }

            return (ghcOpts0 ++ ghcOpts1, dflags2)

-- | Set GHC options and run 'initPackages' in 'GhcMonad'.
--
-- Typical use:
--
-- > defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
-- >    runGhc (Just libdir) $ do
-- >        getSessionDynFlags >>= setDynamicFlags (GhcOptions myGhcOptionList)
-- >        -- do stuff
setDynamicFlags :: GhcMonad m => GhcOptions -> DynFlags -> m ([GHCOption], DynFlags)
setDynamicFlags (GhcOptions extraGHCOpts) dflags0 = do
    (allGhcOpts, dflags1) <- GhcMonad.liftIO $ modifyDFlags extraGHCOpts dflags0

    void $ setSessionDynFlags dflags1
    _ <- GhcMonad.liftIO $ Packages.initPackages dflags1

    return (allGhcOpts, dflags1)

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

getTextualImports :: GhcMonad m => GhcOptions -> FilePath -> String -> m ([GHCOption], [SrcLoc.Located (ImportDecl RdrName)])
getTextualImports ghcopts targetFile targetModuleName = do
    GhcMonad.liftIO $ putStrLn $ "getTextualImports: " ++ show (targetFile, targetModuleName)
    (allGhcOpts, modSum) <- getSummary ghcopts targetFile targetModuleName

    GhcMonad.liftIO $ putStrLn $ "getTextualImports: allGhcOpts: " ++ show allGhcOpts

    return (allGhcOpts, ms_textual_imps modSum)

-- | Get the module summary for a particular file/module. The first and second components of the
-- return value are @ghcOpts1@ and @ghcOpts2@; see 'setDynamicFlags'.
getSummary :: GhcMonad m => GhcOptions -> FilePath -> String -> m ([GHCOption], ModSummary)
getSummary ghcopts targetFile targetModuleName = do
            GhcMonad.liftIO $ putStrLn $ "getSummary, setting dynamic flags..."
            (allGhcOpts, _) <- getSessionDynFlags >>= setDynamicFlags ghcopts

            GhcMonad.liftIO $ putStrLn $ "getSummary, allGhcOpts: " ++ show allGhcOpts

            -- Load the target file (e.g. "Muddle.hs").
            GhcMonad.liftIO $ putStrLn $ "getSummary, loading the target file..."
            target <- guessTarget targetFile Nothing
            setTargets [target]

            _ <- load LoadAllTargets

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            GhcMonad.liftIO $ putStrLn $ "getSummary, setting the context..."

            (setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName])
                   `gcatch` (\(e  :: SourceError)   -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a SourceError, trying to continue anyway..." ++ show e))
                   `gcatch` (\(g  :: GhcApiError)   -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g))
                   `gcatch` (\(se :: SomeException) -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a SomeException, trying to continue anyway..." ++ show se))

            -- Extract the module summary.
            GhcMonad.liftIO $ putStrLn $ "getSummary, extracting the module summary..."
            modSum <- getModSummary (mkModuleName targetModuleName)

            -- graph <- GHC.depanal [] False
            -- -- graph <- getModuleGraph
            -- let graph_names = map (GHC.moduleNameString . GHC.ms_mod_name) graph
            -- GhcMonad.liftIO $ print $ "graph_names: " ++ show graph_names

            return (allGhcOpts, modSum)

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
          name       = showSDoc tdflags (ppr $ GHC.ideclName idecl')
          isImplicit = GHC.ideclImplicit idecl'
          qualifier  = unpackFS <$> GHC.ideclPkgQual idecl'
          hiding     = map removeBrackets $ (catMaybes . parseHiding . GHC.ideclHiding) idecl'
          importedAs = (showSDoc tdflags . ppr) <$> ideclAs idecl'
          specifically = map removeBrackets $ (parseSpecifically . GHC.ideclHiding) idecl'

          removeBrackets :: [a] -> [a]
          removeBrackets [] = []
          removeBrackets x = (init . tail) x

          grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
          grabNames loc = showSDoc tdflags (ppr names)
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
-- *GhcImportedFrom> putStrLn . (showSDoc tdflags) . ppr $ x
-- [(GHC.List.head,
--   [GHC.List.head
--      imported from `Data.List' at tests/data/data/Hiding.hs:5:1-29
--      (and originally defined in `base:GHC.List')])]

lookupSymbol :: GhcOptions -> String -> String -> String -> [String] -> Ghc [(Name, [GlobalRdrElt])]
lookupSymbol ghcopts targetFile targetModuleName qualifiedSymbol importList = do
        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: " ++ show (ghcopts, targetFile, targetModuleName, qualifiedSymbol, importList)

        -- Bring in the target module and its imports.
        (setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
           `gcatch` (\(s  :: SourceError)    -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: setContext failed with a SourceError, trying to continue anyway..." ++ show s
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(g  :: GhcApiError)    -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(se :: SomeException)  -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: setContext failed with a SomeException, trying to continue anyway..." ++ show se
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)

        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: continuing1..."

        -- Get the module summary, then parse it, type check it, and desugar it.
        modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
        p <- parseModule modSummary   :: Ghc ParsedModule
        t <- typecheckModule p        :: Ghc TypecheckedModule
        d <- desugarModule t          :: Ghc DesugaredModule

        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: continuing2..."

        -- The "guts" has the global reader environment, which we need.
        let guts = coreModule d            :: ModGuts
            gre = HscTypes.mg_rdr_env guts :: GlobalRdrEnv

        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: continuing3..."

        -- parseName expects an unambiguous symbol otherwise it causes a
        -- GHC panic. A fully qualified name should suffice. If this step
        -- fails we return an empty list.
        names <- (parseName qualifiedSymbol)
           `gcatch` (\(s  :: SourceError)    -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: parseName failed with a SourceError, trying to continue anyway..." ++ show s
                                                   return [])
           `gcatch` (\(g  :: GhcApiError)    -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: parseName failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   return [])
           `gcatch` (\(se :: SomeException)  -> do GhcMonad.liftIO $ putStrLn $ "lookupSymbol: parseName failed with a SomeException, trying to continue anyway..." ++ show se
                                                   return [])

        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: continuing4..."
        let occNames        = map nameOccName names                 :: [OccName]
            occNamesLookups = map (lookupGlobalRdrEnv gre) occNames :: [[GlobalRdrElt]]

        GhcMonad.liftIO $ putStrLn $ "lookupSymbol::: continuing5..."

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
-- >>> moduleOfQualifiedName "Foo"
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

qualifiedName :: GhcOptions -> FilePath -> String -> Int -> Int -> [String] -> Ghc [String]
qualifiedName ghcopts targetFile targetModuleName lineNr colNr importList = do
        (setContext $ map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
           `gcatch` (\(s  :: SourceError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SourceError, trying to continue anyway..." ++ show s
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(g  :: GhcApiError)    -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)
           `gcatch` (\(se :: SomeException)  -> do GhcMonad.liftIO $ putStrLn $ "qualifiedName: setContext failed with a SomeException, trying to continue anyway..." ++ show se
                                                   setContext $ map (IIDecl . simpleImportDecl . mkModuleName) importList)

        modSummary <- getModSummary $ mkModuleName targetModuleName :: Ghc ModSummary
        p <- parseModule modSummary   :: Ghc ParsedModule
        t <- typecheckModule p        :: Ghc TypecheckedModule

        let TypecheckedModule{tm_typechecked_source = tcs} = t
            bs = listifySpans tcs (lineNr, colNr) :: [LHsBind Id]
            es = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            ps = listifySpans tcs (lineNr, colNr) :: [LPat Id]

        let foo x = showSDoc tdflags $ ppr x
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

optsForGhcPkg :: [String] -> [String]
optsForGhcPkg [] = []
optsForGhcPkg ("-no-user-package-db":rest)   = "--no-user-package-db"          : optsForGhcPkg rest
optsForGhcPkg ("-package-db":pd:rest)        = ("--package-db" ++ "=" ++ pd)   : optsForGhcPkg rest
optsForGhcPkg ("-package-conf":pc:rest)      = ("--package-conf" ++ "=" ++ pc) : optsForGhcPkg rest
optsForGhcPkg ("-no-user-package-conf":rest) = "--no-user-package-conf"        : optsForGhcPkg rest
optsForGhcPkg (_:rest) = optsForGhcPkg rest

-- | Call @ghc-pkg find-module@ to determine that package that provides a module, e.g. @Prelude@ is defined
-- in @base-4.6.0.1@.
ghcPkgFindModule :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgFindModule allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m = do
    let opts = ["find-module", m, "--simple-output"] ++ ["--global", "--user"] ++ optsForGhcPkg allGhcOptions ++ extraGHCPkgOpts
    putStrLn $ "ghc-pkg " ++ show opts

    (_, Just hout, Just herr, _) <- createProcess (proc "ghc-pkg" opts){ std_in  = CreatePipe
                                                                       , std_out = CreatePipe
                                                                       , std_err = CreatePipe
                                                                       }

    output <- readRestOfHandle hout
    err    <- readRestOfHandle herr

    putStrLn $ "ghcPkgFindModule stdout: " ++ show output
    putStrLn $ "ghcPkgFindModule stderr: " ++ show err

    return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

-- | Call @ghc-pkg field@ to get the @haddock-html@ field for a package.
ghcPkgHaddockUrl :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgHaddockUrl allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
    let opts = ["field", p, "haddock-html"] ++ ["--global", "--user"] ++ optsForGhcPkg allGhcOptions ++ extraGHCPkgOpts
    putStrLn $ "ghc-pkg "++ show opts

    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                               , std_out = CreatePipe
                                                               , std_err = CreatePipe
                                                               }

    line <- (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
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
          substringP sub str = if sub `isPrefixOf` str then Just 0 else (+1) <$> substringP sub (tail str)

-- | When we use 'parseName' to convert a 'String' to a 'Name' we get a list of matches instead of
-- a unique match, so we end up having to guess the best match based on the qualified name.
bestPrefixMatches :: Name -> [GlobalRdrElt] -> [String]
bestPrefixMatches name lookUp = x''
    where name' = showSDoc tdflags $ ppr name
          name'' = fromJust $ moduleOfQualifiedName name' -- FIXME dangerous fromJust
          x   = concatMap symbolImportedFrom lookUp
          x'  = map (showSDoc tdflags . ppr) x
          x'' = filter (name'' `isPrefixOf`) x'

-- | Find the haddock module. Returns a 4-tuple consisting of: module that the symbol is imported
-- from, haddock url, module, and module's HTML filename.
findHaddockModule :: QualifiedName -> [HaskellModule] -> [String] -> GhcPkgOptions -> (Name, [GlobalRdrElt]) -> IO [(Maybe String, Maybe String, Maybe String, Maybe String)]
findHaddockModule symbol'' smatches allGhcOpts ghcpkgOpts (name, lookUp) = do
 -- FIXME this is messy - the code below has a dodgy fromJust...
 if isJust (moduleOfQualifiedName symbol'')
  then do
    let lastBitOfSymbol = last $ separateBy '.' symbol''
    putStrLn $ "findHaddockModule, symbol'': " ++ symbol''
    putStrLn $ "findHaddockModule, lastBitOfSymbol: " ++ lastBitOfSymbol
    putStrLn $ "name: " ++ showSDoc tdflags (ppr name)

    let definedIn = nameModule name
        bpms = bestPrefixMatches name lookUp
        importedFrom :: [String]
        importedFrom = if null smatches
                            then if null bpms then map (showSDoc tdflags . ppr) $ concatMap symbolImportedFrom lookUp
                                              else catMaybes $ return $ Safe.headMay bpms
                            else return $ ((showSDoc tdflags . ppr) . mkModuleName . fromJust . moduleOfQualifiedName) symbol'' -- FIXME dangerous fromJust

    putStrLn $ "definedIn: " ++ showSDoc tdflags (ppr definedIn)
    putStrLn $ "bpms: " ++ show bpms
    putStrLn $ "concat $ map symbolImportedFrom lookUp: " ++ showSDoc tdflags (ppr $ concatMap symbolImportedFrom lookUp)


    putStrLn $ "importedFrom: " ++ show importedFrom

    forM importedFrom $ \impfrom -> do
        let impfrom' = Just impfrom
        foundModule <- maybe (return Nothing) (ghcPkgFindModule allGhcOpts ghcpkgOpts) impfrom'
        putStrLn $ "ghcPkgFindModule result: " ++ show foundModule

        let base = moduleNameToHtmlFile <$> impfrom'

        putStrLn $ "base: : " ++ show base

        haddock <- maybe (return Nothing) (ghcPkgHaddockUrl allGhcOpts ghcpkgOpts) foundModule

        putStrLn $ "haddock: " ++ show haddock
        putStrLn $ "foundModule1: " ++ show foundModule

        return (impfrom', haddock, foundModule, base)
  else
    return []

-- | Convert our match to a URL, either @file://@ if the file exists, or to @hackage.org@ otherwise.
matchToUrl :: (Maybe String, Maybe String, Maybe String, Maybe String) -> IO String
matchToUrl (importedFrom, haddock, foundModule, base) = do
    when (isNothing importedFrom) $ error "importedFrom is Nothing :("
    when (isNothing haddock) $ error "haddock is Nothing :("
    when (isNothing foundModule) $ error "foundModule is Nothing :("
    when (isNothing base) $ error "base is Nothing :("

    let importedFrom' = fromJust importedFrom
        haddock'      = fromJust haddock
        foundModule'  = fromJust foundModule
        base'         = fromJust base

        f = haddock' </> base'

    e <- doesFileExist f

    if e then return $ "file://" ++ f
         else do putStrLn $ "f:  " ++ show f
                 putStrLn $ "foundModule2: " ++ show foundModule'
                 putStrLn $ "calling toHackageUrl with params: " ++ show (f, foundModule', importedFrom')
                 return $ toHackageUrl f foundModule' importedFrom'


-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary monads.
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

isHidden :: String -> String -> HaskellModule -> Bool
isHidden symbol mname (HaskellModule name qualifier isImplicit hiding importedAs specifically) = mname == name && isNothing importedAs && symbol `elem` hiding

filterMatchingQualifiedImport :: String -> [HaskellModule] -> [HaskellModule]
filterMatchingQualifiedImport symbol hmodules =
    case moduleOfQualifiedName symbol of Nothing    -> []
                                         asBit@(Just _) -> filter (\z -> asBit == modImportedAs z) hmodules

isInHiddenPackage :: GhcMonad m => String -> m Bool
isInHiddenPackage mName =
    (do setContext $ map (IIDecl . simpleImportDecl . mkModuleName) [mName]
        -- We were able to load the package, so it is not hidden.
        return False)
        `gcatch` (\(_  :: SourceError) -> do GhcMonad.liftIO $ putStrLn $ "isInHiddenPackage: module " ++ mName ++ " is in a hidden package."
                                             return True)


finalCase :: [String] -> String -> String -> [Char] -> [[Char]] -> Ghc [String]
finalCase ghcOpts0 targetFile targetModule symbol haskellModuleNames' = do
    blah <- forM haskellModuleNames' $ \hm -> do fff <- lookupSymbol (GhcOptions ghcOpts0) targetFile targetModule (hm ++ "." ++ symbol) haskellModuleNames'
                                                 if (length fff) > 0
                                                    then do GhcMonad.liftIO $ putStrLn $ "finalCase: " ++ hm
                                                            return [hm]
                                                    else return []
    return $ concat blah

actualFinalCase allGhcOpts ghcpkgOptions targetFile targetModule symbol haskellModuleNames' = do
    -- This is getting ridiculous...
    GhcMonad.liftIO $ putStrLn "last bits 1..."
    zzz <- finalCase allGhcOpts targetFile targetModule symbol haskellModuleNames'
    GhcMonad.liftIO $ putStrLn "last bits 2..."
    yyy <- forM zzz $ \r -> do p <- GhcMonad.liftIO $ ghcPkgFindModule allGhcOpts ghcpkgOptions r
                               GhcMonad.liftIO $ print $ "forM_ last bits: " ++ show p
                               case p of Nothing  -> return []
                                         (Just _) -> return [(r, fromJust p)]

    let yyy' = concat yyy

    GhcMonad.liftIO $ putStrLn "last bits 3..."
    -- FIXME why don't we have the full ghc options right now? More than just the user-supplied ones?
    yyy'' <- forM yyy' $ \(mname, pname) -> do haddock <- GhcMonad.liftIO $ ghcPkgHaddockUrl allGhcOpts (GhcPkgOptions allGhcOpts) pname
                                               if isJust haddock
                                                     then do GhcMonad.liftIO $ putStrLn $ "last bits 3 inner loop: " ++ show haddock
                                                             url <- GhcMonad.liftIO $ matchToUrl (Just mname, haddock, Just mname, Just $ moduleNameToHtmlFile mname)
                                                             return $ Just url
                                                     else return Nothing

    GhcMonad.liftIO $ putStrLn $ "yyy'': " ++ show yyy''

    GhcMonad.liftIO $ putStrLn "last bits 4..."
    let yyy''' = catMaybes yyy''
    GhcMonad.liftIO $ print $ "yyy''': " ++ (show yyy''')

    return yyy'''

-- | Attempt to guess the Haddock url, either a local file path or url to @hackage.haskell.org@
-- for the symbol in the given file, module, at the specified line and column location.
--
-- Example:
--
-- >>> guessHaddockUrl "tests/data/data/Muddle.hs" "Muddle" "Maybe" 11 11
-- (lots of output)
-- SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html

guessHaddockUrl :: FilePath -> String -> Symbol -> Int -> Int -> GhcOptions -> GhcPkgOptions -> IO (Either String [String])
guessHaddockUrl _targetFile targetModule symbol lineNr colNr (GhcOptions ghcOpts0) ghcpkgOptions = do
    cradle <- findCradle
    let currentDir = cradleCurrentDir cradle
        workDir = cradleRootDir cradle
    setCurrentDirectory workDir

    let targetFile = currentDir </> _targetFile

    putStrLn $ "currentDir: " ++ currentDir
    putStrLn $ "workDir: " ++ workDir

    putStrLn $ "targetFile: " ++ targetFile
    putStrLn $ "targetModule: " ++ targetModule
    putStrLn $ "symbol: " ++ show symbol
    putStrLn $ "line nr: " ++ show lineNr
    putStrLn $ "col nr: " ++ show colNr

    putStrLn $ "ghcOpts0: " ++ show ghcOpts0
    putStrLn $ "ghcpkgOptions: " ++ show ghcpkgOptions

    -- Put a runGhc up here, then change the types further down???
    runGhc (Just libdir) $ do
        (allGhcOpts, textualImports) <- getTextualImports (GhcOptions ghcOpts0) targetFile targetModule

        let haskellModules0 = map toHaskellModule textualImports
            haskellModuleNames0 = map modName haskellModules0
        GhcMonad.liftIO $ putStrLn $ "haskellModuleNames0: " ++ show haskellModuleNames0
        GhcMonad.liftIO $ putStrLn $ "haskellModuleNames0 (full detail): " ++ show haskellModules0

        -- If symbol is something like DM.lookup, then restrict haskellModuleNames to the
        -- one that has modImportedAs == Just "DM".
        let filterThings = filterMatchingQualifiedImport symbol haskellModules0
        let haskellModules = if null filterThings then haskellModules0 else filterThings
        let haskellModuleNames = if null filterThings then map modName haskellModules0 else map modName filterThings

        qnames <- filter (not . (' ' `elem`)) <$> qualifiedName (GhcOptions ghcOpts0) targetFile targetModule lineNr colNr haskellModuleNames

        GhcMonad.liftIO $ putStrLn $ "qualified names: " ++ show qnames

        let matchingAsImport = expandMatchingAsImport symbol (map toHaskellModule textualImports)
        GhcMonad.liftIO $ putStrLn $ "matchingAsImport: " ++ show matchingAsImport

        let postMatches = filter (postfixMatch symbol) qnames :: [String]
            symbol' = fromMaybe (if null postMatches then symbol else minimumBy (compare `on` length) postMatches) matchingAsImport

        GhcMonad.liftIO $ putStrLn $ "postMatches:  " ++ show postMatches
        GhcMonad.liftIO $ putStrLn $ "symbol': " ++ symbol'

        let maybeExtraModule = moduleOfQualifiedName symbol'

        -- The module maybeExtraModule might be hidden. Check this.
        extraIsHidden <- case maybeExtraModule of Just x  -> isInHiddenPackage x
                                                  Nothing -> return False
        GhcMonad.liftIO $ putStrLn $ "extraIsHidden: " ++ show extraIsHidden

        let maybeExtraModule' = if extraIsHidden
                                    then []
                                    else if isJust maybeExtraModule
                                        then [fromJust maybeExtraModule]
                                        else []

        let haskellModuleNames' = if symbol == symbol' then haskellModuleNames else haskellModuleNames ++ maybeExtraModule'

        GhcMonad.liftIO $ putStrLn $ "maybeExtraModule: " ++ show maybeExtraModule
        GhcMonad.liftIO $ putStrLn $ "maybeExtraModule': " ++ show maybeExtraModule'
        GhcMonad.liftIO $ putStrLn $ "haskellModuleNames': " ++ show haskellModuleNames'

        let smatches = specificallyMatches symbol (map toHaskellModule textualImports)
        GhcMonad.liftIO $ putStrLn $ "smatches: " ++ show smatches

        let symbol'' = if null smatches
                            then symbol'
                            else modName (head smatches) ++ "." ++ symbol

        GhcMonad.liftIO $ putStrLn $ "symbol'': " ++ symbol''

        let allJust (a, b, c, d) = isJust a && isJust b && isJust c && isJust d

        -- Then this does a runGhc as well.
        final1 <- lookupSymbol (GhcOptions ghcOpts0) targetFile targetModule symbol'' haskellModuleNames'

        final1' <- GhcMonad.liftIO $ concatMapM (findHaddockModule symbol'' smatches allGhcOpts ghcpkgOptions) final1
        GhcMonad.liftIO $ putStrLn $ "final1': " ++ show final1'

        -- Remove any modules that have this name hidden.
        -- e.g. import Data.List hiding (map)
        let final1'' = filter (\(a,_,_,_) -> case a of Just a' -> not $ any (isHidden symbol a') haskellModules
                                                       Nothing -> False) final1'
        GhcMonad.liftIO $ putStrLn $ "final1'': " ++ show final1''
        GhcMonad.liftIO $ putStrLn $ show (symbol, haskellModules)

        let final2 = filter allJust final1''
        final3 <- GhcMonad.liftIO $ mapM matchToUrl final2

        GhcMonad.liftIO $ putStrLn "last bits 5..."
        if null final3
            then do yyy''' <- actualFinalCase ghcOpts0 ghcpkgOptions targetFile targetModule symbol haskellModuleNames'
                    if null yyy'''
                            then return $ Left $ "No matches found."
                            else return $ Right yyy'''
                    else return $ Right final3

-- | Top level function; use this one from src/Main.hs.
haddockUrl :: Options -> FilePath -> String -> String -> Int -> Int -> IO String
haddockUrl opt file modstr symbol lineNr colNr = do

    let ghcopts    = GhcOptions    $ ghcOpts    opt
    let ghcpkgopts = GhcPkgOptions $ ghcPkgOpts opt

    res <- (guessHaddockUrl file modstr symbol lineNr colNr ghcopts ghcpkgopts)
    --           `gcatch` (\(s  :: SourceError)   -> return $ Left $ "guessHaddockUrl failed with a SourceError... " ++ show s)
    --           `gcatch` (\(g  :: GhcApiError)   -> return $ Left $ "guessHaddockUrl failed with a GhcApiError... " ++ show g)
    --           `gcatch` (\(se :: SomeException) -> return $ Left $ "guessHaddockUrl failed with a SomeException... " ++ show se)

    case res of Right x  -> return $ (if length x > 1 then "WARNING: Multiple matches! Showing them all.\n" else "")
                                        ++ (concat $ map (\z -> "SUCCESS: " ++ z ++ "\n") (reverse x)) -- Why reverse? To show the first one last, which the vim plugin will get.
                                                                                                       -- This is flaky but will make it behave as earlier versions did, which used
                                                                                                       -- Safe.headMay to get the first result.
                Left err -> return $ "FAIL: " ++ show err ++ "\n"
