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
   , symbolImportedFrom
   , postfixMatch
   , moduleOfQualifiedName
   , qualifiedName
   , ghcPkgFindModule
   , ghcPkgHaddockUrl
   , moduleNameToHtmlFile
   , toHackageUrl
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
import Data.Either (rights)
import Data.Function (on)
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Typeable()
import Desugar()
import FastString
import GHC
import GHC.Paths (libdir)
import GHC.SYB.Utils()
import HscTypes
import Module
import Name
import Outputable
import RdrName
import System.Directory
import System.Environment()
import System.FilePath
import System.IO
import System.Process
import TcRnTypes()
import HsImpExp
import HsTypes
import Type
import HsPat

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

import qualified Data.Map as M

import Language.Haskell.GhcMod.Monad ( runGmOutT )
import qualified Language.Haskell.GhcMod.Types as GhcModTypes

import Language.Haskell.GhcMod.Types       (IOish(..))
import Language.Haskell.GhcMod.Monad.Types (GhcModLog(..), GmOut(..))
import Control.Monad.Trans.Journal (runJournalT)

import Language.Haskell.GhcImportedFrom.UtilsFromGhcMod
import Language.Haskell.GhcImportedFrom.Types

import Control.Exception (SomeException)

import qualified Text.Parsec as TP
import Data.Functor.Identity

import qualified Documentation.Haddock as Haddock

import Debug.Trace

import qualified DynFlags()

#if __GLASGOW_HASKELL__ >= 708
import DynFlags ( unsafeGlobalDynFlags )
tdflags = unsafeGlobalDynFlags
#else
import DynFlags ( tracingDynFlags )
tdflags = tracingDynFlags
#endif

trace' :: Show x => String -> x -> b -> b
trace' m x = trace (m ++ ">>> " ++ show x)

trace'' :: Outputable x => String -> x -> b -> b
trace'' m x = trace (m ++ ">>> " ++ showSDoc tdflags (ppr x))

type GHCOption = String

getGhcOptionsViaCabalRepl :: IO (Maybe [String])
getGhcOptionsViaCabalRepl = do
    putStrLn "getGhcOptionsViaCabalRepl..."

    (Just _, Just hout, Just _, _) <- createProcess (proc "cabal" ["repl", "--with-ghc=fake-ghc-for-ghc-imported-from"]){ std_in = CreatePipe
                                                                                                                        , std_out = CreatePipe
                                                                                                                        , std_err = CreatePipe
                                                                                                                        }

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
parseDottedHaskellModuleName = TP.char '.' >> parseHaskellModuleName

parseFullHaskellModuleName :: TP.ParsecT String u Data.Functor.Identity.Identity String
parseFullHaskellModuleName = do
    h <- parseHaskellModuleName
    rest <- many parseDottedHaskellModuleName

    return $ intercalate "." (h:rest)

parseHelper :: String -> Bool
parseHelper s = case TP.parse (parseFullHaskellModuleName <* TP.eof) "" s of Right _ -> False
                                                                             Left _  -> True

parsePackageAndQualName = TP.choice [TP.try parsePackageAndQualNameWithHash, parsePackageAndQualNameNoHash]

-- Package with no hash (seems to be for internal packages?)
-- base-4.8.2.0:Data.Foldable.length
parsePackageAndQualNameNoHash :: TP.ParsecT String u Data.Functor.Identity.Identity (String, String)
parsePackageAndQualNameNoHash = do
    packageName <- parsePackageName
    qualName    <- parsePackageFinalQualName

    return (packageName, qualName)

  where

    parsePackageName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageName = TP.anyChar `TP.manyTill` TP.char ':'

    parsePackageFinalQualName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageFinalQualName = TP.many1 TP.anyChar

-- Parse the package name "containers-0.5.6.2" from a string like
-- "containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU:Data.Map.Base.fromList"
parsePackageAndQualNameWithHash :: TP.ParsecT String u Data.Functor.Identity.Identity (String, String)
parsePackageAndQualNameWithHash = do
    packageName <- parsePackageName
    _           <- parsePackageHash
    qualName    <- parsePackageFinalQualName

    return (packageName, qualName)

  where

    parsePackageName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageName = TP.anyChar `TP.manyTill` TP.char '@'

    parsePackageHash :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageHash = TP.anyChar `TP.manyTill` TP.char ':'

    parsePackageFinalQualName :: TP.ParsecT String u Data.Functor.Identity.Identity String
    parsePackageFinalQualName = TP.many1 TP.anyChar

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
            GhcMonad.liftIO $ putStrLn "getSummary, setting dynamic flags..."
            (allGhcOpts, _) <- getSessionDynFlags >>= setDynamicFlags ghcopts

            GhcMonad.liftIO $ putStrLn $ "getSummary, allGhcOpts: " ++ show allGhcOpts

            -- Load the target file (e.g. "Muddle.hs").
            GhcMonad.liftIO $ putStrLn "getSummary, loading the target file..."
            target <- guessTarget targetFile Nothing
            setTargets [target]

            _ <- load LoadAllTargets

            -- Set the context by loading the module, e.g. "Muddle" which is in "Muddle.hs".
            GhcMonad.liftIO $ putStrLn "getSummary, setting the context..."

            setContext [(IIDecl . simpleImportDecl . mkModuleName) targetModuleName]
                   `gcatch` (\(e  :: SourceError)   -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a SourceError, trying to continue anyway..." ++ show e))
                   `gcatch` (\(g  :: GhcApiError)   -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a GhcApiError, trying to continue anyway..." ++ show g))
                   `gcatch` (\(se :: SomeException) -> GhcMonad.liftIO (putStrLn $ "getSummary: setContext failed with a SomeException, trying to continue anyway..." ++ show se))

            -- Extract the module summary.
            GhcMonad.liftIO $ putStrLn "getSummary, extracting the module summary..."
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
          hiding     = (catMaybes . parseHiding . GHC.ideclHiding) idecl'
          importedAs = (showSDoc tdflags . ppr) <$> ideclAs idecl'
          specifically = (parseSpecifically . GHC.ideclHiding) idecl'

          grabNames :: GHC.Located (GHC.IE GHC.RdrName) -> String
          grabNames loc = showSDoc tdflags (ppr names)
            where names = GHC.ieNames $ SrcLoc.unLoc loc

          grabNames' :: GHC.Located [GHC.LIE GHC.RdrName] -> [String]
          grabNames' loc = map (showSDoc tdflags . ppr) names
            where names :: [RdrName]
                  names = map (ieName . SrcLoc.unLoc) $ SrcLoc.unLoc loc
                  -- FIXME We are throwing away location info by using unLoc each time?
                  -- Trace these things to see what we are losing.
                  --
          parseHiding :: Maybe (Bool, Located [LIE RdrName]) -> [Maybe String]
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
          parseHiding (Just (True, h))  = map Just $ grabNames' h

          parseSpecifically :: Maybe (Bool, Located [LIE RdrName]) -> [String]
          parseSpecifically (Just (False, h)) = grabNames' h
          parseSpecifically _                 = []

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
        setContext (map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
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

-- Like qualifiedName but uses 'reallyAlwaysQualify' to show the fully qualified name, e.g.
-- "containers-0.5.6.2@conta_2C3ZI8RgPO2LBMidXKTvIU:Data.Map.Base.fromList" instead of
-- "Data.Map.Base.fromList". Will probably replace qualifiedName once more testing has
-- been done. If this works we can also remove 'ghcPkgFindModule' which uses a shell
-- call to try to find the package name.
qualifiedName' :: GhcOptions -> FilePath -> String -> Int -> Int -> String -> [String] -> Ghc [String]
qualifiedName' ghcopts targetFile targetModuleName lineNr colNr symbol importList = do
        setContext (map (IIDecl . simpleImportDecl . mkModuleName) (targetModuleName:importList))
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
            -- ls0 = listifySpans tcs (lineNr, colNr) :: [LHsBindLR Id Id]
            -- ls1 = listifySpans tcs (lineNr, colNr) :: [LIPBind Id]
            -- ls2 = listifySpans tcs (lineNr, colNr) :: [LPat Id]
            -- ls3 = listifySpans tcs (lineNr, colNr) :: [LHsDecl Id]
            -- ls4 = listifySpans tcs (lineNr, colNr) :: [LHsExpr Id]
            -- ls5 = listifySpans tcs (lineNr, colNr) :: [LHsTupArg Id]
            -- ls6 = listifySpans tcs (lineNr, colNr) :: [LHsCmd Id]
            -- ls7 = listifySpans tcs (lineNr, colNr) :: [LHsCmdTop Id]

        let bs' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) bs
            es' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) es
            ps' = map (showSDocForUser tdflags reallyAlwaysQualify . ppr) ps

        return $ filter (postfixMatch symbol) $ concatMap words $ bs' ++ es' ++ ps'

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

ghcPkgFindModule :: Maybe String -> [String] -> GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgFindModule x allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m = do
    sandboxResult <- hcPkgFindModule x allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m

    case sandboxResult of
        Nothing     -> _ghcPkgFindModule x allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m
        s@(Just _)  -> return s

-- | Call @ghc-pkg find-module@ to determine that package that provides a module, e.g. @Prelude@ is defined
-- in @base-4.6.0.1@.
_ghcPkgFindModule :: Maybe String -> [String] -> GhcPkgOptions -> String -> IO (Maybe String)
_ghcPkgFindModule x allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m =
    case x of
        Just x' -> return x -- we found it earlier - this is a hack
        Nothing -> do let opts = ["find-module", m, "--simple-output"] ++ ["--global", "--user"] ++ optsForGhcPkg allGhcOptions ++ extraGHCPkgOpts
                      putStrLn $ "ghc-pkg " ++ show opts

                      (_, Just hout, Just herr, _) <- createProcess (proc "ghc-pkg" opts){ std_in  = CreatePipe
                                                                                         , std_out = CreatePipe
                                                                                         , std_err = CreatePipe
                                                                                         }

                      output <- readRestOfHandle hout
                      err    <- readRestOfHandle herr

                      putStrLn $ "_ghcPkgFindModule stdout: " ++ show output
                      putStrLn $ "_ghcPkgFindModule stderr: " ++ show err

                      return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output

-- | Call @cabal sandbox hc-pkg@ to find the package the provides a module.
hcPkgFindModule :: Maybe String -> [String] -> GhcPkgOptions -> String -> IO (Maybe String)
hcPkgFindModule x allGhcOptions (GhcPkgOptions extraGHCPkgOpts) m =
    case x of
        Just x' -> return x -- we found it earlier - this is a hack
        Nothing -> do let opts = ["sandbox", "hc-pkg", "find-module", m, "--", "--simple-output"]

                      (_, Just hout, Just herr, _) <- createProcess (proc "cabal" opts){ std_in  = CreatePipe
                                                                                       , std_out = CreatePipe
                                                                                       , std_err = CreatePipe
                                                                                       }

                      output <- readRestOfHandle hout
                      err    <- readRestOfHandle herr

                      putStrLn $ "hcPkgFindModule stdout: " ++ show output
                      putStrLn $ "hcPkgFindModule stderr: " ++ show err

                      return $ join $ Safe.lastMay <$> words <$> (Safe.lastMay . lines) output


ghcPkgHaddockUrl :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgHaddockUrl allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
    hc <- hcPkgHaddockUrl p

    case hc of
        Just hc' -> return $ Just hc'
        Nothing  -> _ghcPkgHaddockUrl allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p

-- | Call @ghc-pkg field@ to get the @haddock-html@ field for a package.
_ghcPkgHaddockUrl :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
_ghcPkgHaddockUrl allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
    let opts = ["field", p, "haddock-html"] ++ ["--global", "--user"] ++ optsForGhcPkg allGhcOptions ++ extraGHCPkgOpts
    putStrLn $ "ghc-pkg "++ show opts

    (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                               , std_out = CreatePipe
                                                               , std_err = CreatePipe
                                                               }

    line <- (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
    return $ Safe.lastMay $ words line

-- | Call cabal sandbox hc-pkg to find the haddock url.
hcPkgHaddockUrl :: String -> IO (Maybe String)
hcPkgHaddockUrl p = do
    let opts = ["sandbox", "hc-pkg", "field", p, "haddock-html"]
    putStrLn $ "cabal sandbox hc-pkg field " ++ p ++ " haddock-html"

    (_, Just hout, _, _) <- createProcess (proc "cabal" opts){ std_in = CreatePipe
                                                             , std_out = CreatePipe
                                                             , std_err = CreatePipe
                                                             }

    line <- (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
    print ("line", line)

    if "haddock-html:" `isInfixOf` line
        then do print ("line2", Safe.lastMay $ words line)
                return $ Safe.lastMay $ words line
        else return Nothing













ghcPkgHaddockInterface :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
ghcPkgHaddockInterface allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
    hc <- hcPkgHaddockInterface p

    case hc of
        Just hc' -> return $ Just hc'
        Nothing  -> _ghcPkgHaddockInterface allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p

  where

    _ghcPkgHaddockInterface :: [String] -> GhcPkgOptions -> String -> IO (Maybe String)
    _ghcPkgHaddockInterface allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
        let opts = ["field", p, "haddock-interfaces"] ++ ["--global", "--user"] ++ optsForGhcPkg allGhcOptions ++ extraGHCPkgOpts
        putStrLn $ "ghc-pkg "++ show opts

        (_, Just hout, _, _) <- createProcess (proc "ghc-pkg" opts){ std_in = CreatePipe
                                                                   , std_out = CreatePipe
                                                                   , std_err = CreatePipe
                                                                   }

        line <- (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
        return $ Safe.lastMay $ words line

    -- | Call cabal sandbox hc-pkg to find the haddock Interfaces.
    hcPkgHaddockInterface :: String -> IO (Maybe String)
    hcPkgHaddockInterface p = do
        let opts = ["sandbox", "hc-pkg", "field", p, "haddock-interfaces"]
        putStrLn $ "cabal sandbox hc-pkg field " ++ p ++ " haddock-interfaces"

        (_, Just hout, _, _) <- createProcess (proc "cabal" opts){ std_in = CreatePipe
                                                                 , std_out = CreatePipe
                                                                 , std_err = CreatePipe
                                                                 }

        line <- (reverse . dropWhile (== '\n') . reverse) <$> readRestOfHandle hout
        print $ ("ZZZZZZZZZZZZZ", line)

        return $ if "haddock-interfaces" `isInfixOf` line
            then Safe.lastMay $ words line
            else Nothing


getVisibleExports :: [String] -> GhcPkgOptions -> String -> Ghc (Maybe (M.Map String [String]))
getVisibleExports allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p = do
    haddockInterfaceFile <- GhcMonad.liftIO $ ghcPkgHaddockInterface allGhcOptions (GhcPkgOptions extraGHCPkgOpts) p
    join <$> traverse getVisibleExports' haddockInterfaceFile

  where

    getVisibleExports' :: FilePath -> Ghc (Maybe (M.Map String [String]))
    getVisibleExports' ifile = do
        iface <- Haddock.readInterfaceFile Haddock.nameCacheFromGhc ifile

        case iface of
            Left err        -> error $ "Could not read interface file " ++ ifile ++ " due to error: " ++ err
            Right iface'    -> do let m  = map (\ii -> (Haddock.instMod ii, Haddock.instVisibleExports ii)) $ Haddock.ifInstalledIfaces iface' :: [(Module, [Name])]
                                      m' = map (\(mname, names) -> (showSDoc tdflags $ ppr mname, map (showSDoc tdflags . ppr) names)) m       :: [(String, [String])]
                                  return $ Just $ M.fromList m'

{-
        GhcMonad.liftIO $ print $ showSDoc tdflags $ ppr $ map Haddock.instMod $ Haddock.ifInstalledIfaces x

        let x' = (!!0) $ Haddock.ifInstalledIfaces x
        GhcMonad.liftIO $ print $ showSDoc tdflags $ ppr $ Haddock.instMod x'
        GhcMonad.liftIO $ print $ showSDoc tdflags $ ppr $ Haddock.instVisibleExports x'

        let interfaces :: [Haddock.InstalledInterface]
        interfaces = Haddock.ifInstalledIfaces x

        -- modToVisibleExports :: [(Module, [Name])]
        -- modToVisibleExports = map (\i -> (Haddock.instMod x, Haddock.instVisibleExports x)) interfaces
-}







-- | Convert a module name string, e.g. @Data.List@ to @Data-List.html@.
moduleNameToHtmlFile :: String -> String
moduleNameToHtmlFile m =  map f m ++ ".html"
    where f :: Char -> Char
          f '.' = '-'
          f c   = c

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

filterMatchingQualifiedImport :: String -> [HaskellModule] -> [HaskellModule]
filterMatchingQualifiedImport symbol hmodules =
    case moduleOfQualifiedName symbol of Nothing    -> []
                                         asBit@(Just _) -> filter (\z -> asBit == modImportedAs z) hmodules

-- Copied from ghc-mod-5.5.0.0
findCradleNoLog  :: forall m. (IOish m, GmOut m) => m Cradle
findCradleNoLog = fst <$> (runJournalT findCradle :: m (Cradle, GhcModLog))

getModuleExports :: GhcOptions
                 -> GhcPkgOptions
                 -> HaskellModule
                 -> Ghc (Maybe ([String], String))
getModuleExports (GhcOptions ghcOpts) ghcPkgOpts m = do
    minfo     <- ((findModule (mkModuleName $ modName m) Nothing) >>= getModuleInfo)
                   `gcatch` (\(e  :: SourceError)   -> return Nothing)

    p <- GhcMonad.liftIO $ ghcPkgFindModule Nothing ghcOpts ghcPkgOpts (modName m)

    case (minfo, p) of
        (Nothing, _)            -> return Nothing
        (_, Nothing)            -> return Nothing
        (Just minfo', Just p')  -> return $ Just (map (showSDocForUser tdflags reallyAlwaysQualify . ppr) $ modInfoExports minfo', p')

type UnqualifiedName    = String    -- ^ e.g. "Just"
type FullyQualifiedName = String    -- ^ e.g. e.g. "base-4.8.2.0:Data.Foldable.length"
type StrModuleName      = String    -- ^ e.g. "Data.List"

data MySymbol = MySymbolSysQualified  String  -- ^ e.g. "base-4.8.2.0:Data.Foldable.length"
              | MySymbolUserQualified String  -- ^ e.g. "DL.length" with an import earlier like "import qualified Data.List as DL"
              deriving Show

data ModuleExports = ModuleExports
    { mName            :: StrModuleName            -- ^ e.g. "Data.List"
    , mPackageName     :: String                   -- ^ e.g. "snap-0.14.0.6"
    , mInfo            :: HaskellModule            -- ^ Our parse of the module import, with info like "hiding (map)".
    , qualifiedExports :: [FullyQualifiedName]     -- ^ e.g. [ "base-4.8.2.0:GHC.Base.++"
                                                        --        , "base-4.8.2.0:GHC.List.filter"
                                                        --        , "base-4.8.2.0:GHC.List.zip"
                                                        --        , ...
                                                        --        ]
    }
    deriving Show

pprModuleExports :: ModuleExports -> String
pprModuleExports me = mName me ++ "\n" ++ show (mInfo me) ++ "\n" ++ unwords (map show $ qualifiedExports me)

refineAs :: MySymbol -> [ModuleExports] -> [ModuleExports]

-- User qualified the symbol, so we can filter out anything that doesn't have a matching 'modImportedAs'.
refineAs (MySymbolUserQualified userQualSym) exports = filter f exports
  where
    f export = case modas of
                Nothing     -> False
                Just modas' -> modas' == userQualAs
       where modas = modImportedAs $ mInfo export :: Maybe String

             -- e.g. "DL"
             userQualAs = fromMaybe (error $ "Expected a qualified name like 'DL.length' but got: " ++ userQualSym)
                                    (moduleOfQualifiedName userQualSym)

-- User didn't qualify the symbol, so we have the full system qualified thing, so do nothing here.
refineAs (MySymbolSysQualified _) exports = exports

refineRemoveHiding :: String -> [ModuleExports] -> [ModuleExports]
refineRemoveHiding symbol exports = map (\e -> e { qualifiedExports = f symbol e }) exports
  where
    f symbol export = filter (`notElem` hiding') thisExports
       where hiding = modHiding $ mInfo export :: [String] -- Things that this module hides.
             hiding' = map (qualifyName thisExports) hiding  :: [String]    -- Qualified version of hiding.
             thisExports = qualifiedExports export         -- Things that this module exports.

    qualifyName :: [QualifiedName] -> Symbol -> QualifiedName
    qualifyName qualifiedNames name
        = case filter (postfixMatch name) qualifiedNames of
            [match]     -> match
            _           -> error $ "Could not qualify " ++ name ++ " from these exports: " ++ show qualifiedNames

refineExportsIt :: String -> [ModuleExports] -> [ModuleExports]
refineExportsIt symbol exports = map (\e -> e { qualifiedExports = f symbol e }) exports
  where
    -- f symbol export = filter (symbol ==) thisExports
    f symbol export = filter (postfixMatch symbol) thisExports
       where thisExports = qualifiedExports export         -- Things that this module exports.

refineLeadingDot :: MySymbol -> [ModuleExports] -> [ModuleExports]
refineLeadingDot (MySymbolUserQualified userQualSym) exports = exports
refineLeadingDot (MySymbolSysQualified symb)         exports = map (\e -> e { qualifiedExports = f leadingDot e }) exports
  where
    leadingDot :: String
    leadingDot = '.' : last (separateBy '.' symb)

    -- f symbol export = filter (symbol ==) thisExports
    f symbol export = filter (symbol `isSuffixOf`) thisExports
       where thisExports = qualifiedExports export         -- Things that this module exports.

refineVisibleExports :: [String] -> GhcPkgOptions -> [ModuleExports] -> Ghc [ModuleExports]
refineVisibleExports allGhcOpts ghcpkgOptions exports = mapM f exports
  where
    f :: ModuleExports -> Ghc ModuleExports
    f mexports = do
        let pname          = mPackageName     mexports -- e.g. "base-4.8.2.0"
            thisModuleName = mName            mexports -- e.g. "Prelude"
            qexports       = qualifiedExports mexports -- e.g. ["base-4.8.2.0:GHC.Base.Just", ...]
        visibleExportsMap <- getVisibleExports allGhcOpts ghcpkgOptions pname

        let thisModVisibleExports = fromMaybe
                                        (error $ "Could not get visible exports of " ++ pname)
                                        (join $ traverse (M.lookup thisModuleName) visibleExportsMap)

        let qexports' = filter (hasPostfixMatch thisModVisibleExports) qexports
        return $ mexports { qualifiedExports = qexports' }

    -- hasPostfixMatch "base-4.8.2.0:GHC.Base.Just" ["Just", "True", ...] -> True
    hasPostfixMatch :: [String] -> String -> Bool
    hasPostfixMatch xs s = last (separateBy '.' s) `elem` xs

-- | The last thing with a single export must be the match? Iffy.
getLastMatch :: [ModuleExports] -> Maybe ModuleExports
getLastMatch exports = Safe.lastMay $ filter f exports
  where
    f me = length (qualifiedExports me) == 1

-- | Attempt to guess the Haddock url, either a local file path or url to @hackage.haskell.org@
-- for the symbol in the given file, module, at the specified line and column location.
--
-- Example:
--
-- >>> guessHaddockUrl "tests/data/data/Muddle.hs" "Muddle" "Maybe" 11 11
-- (lots of output)
-- SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html

guessHaddockUrl :: FilePath -> String -> Symbol -> Int -> Int -> GhcOptions -> GhcPkgOptions -> IO (Either String String)
guessHaddockUrl _targetFile targetModule symbol lineNr colNr (GhcOptions ghcOpts0) ghcpkgOptions = do
    cradle <- runGmOutT GhcModTypes.defaultOptions findCradleNoLog
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

        qnames_with_qualified_printing <- filter (not . (' ' `elem`)) <$> qualifiedName' (GhcOptions ghcOpts0) targetFile targetModule lineNr colNr symbol haskellModuleNames :: Ghc [String]
        GhcMonad.liftIO $ putStrLn $ "qualified names with qualified printing: " ++ show qnames_with_qualified_printing

        let parsedPackagesAndQualNames :: [Either TP.ParseError (String, String)]
            parsedPackagesAndQualNames = map (TP.parse parsePackageAndQualName "") qnames_with_qualified_printing

        GhcMonad.liftIO $ putStrLn $ "qqqqqq1: " ++ show parsedPackagesAndQualNames

        let symbolToUse :: String
            symbolToUse = fromMaybe (head qnames) (Safe.headMay qnames_with_qualified_printing) -- FIXME dodgy use of 'head'

        GhcMonad.liftIO $ print ("symbolToUse", symbolToUse)

        -- Possible extra modules...
        let extraModules :: [HaskellModule]
            extraModules = case Safe.headMay parsedPackagesAndQualNames of
                            Just (Right (_, x)) -> case moduleOfQualifiedName x of Just x' -> [ HaskellModule { modName         = x'
                                                                                                              , modQualifier    = Nothing
                                                                                                              , modIsImplicit   = False
                                                                                                              , modHiding       = []
                                                                                                              , modImportedAs   = Nothing
                                                                                                              , modSpecifically = []
                                                                                                              }
                                                                                              ]
                                                                                   Nothing -> []
                            _                   -> []

        GhcMonad.liftIO $ print extraModules

        -- Try to use the qnames_with_qualified_printing case, which has something like "base-4.8.2.0:GHC.Base.map",
        -- which will be more accurate to filter on.

        exports <- mapM (getModuleExports (GhcOptions ghcOpts0) ghcpkgOptions) (haskellModules0 ++ extraModules)

        -- Sometimes the modules in extraModules might be hidden or weird ones like GHC.Base that we can't
        -- load, so filter out the successfully loaded ones.
        let successes :: [(HaskellModule, Maybe ([String], String))]
            successes = filter (isJust . snd) (zip (haskellModules0 ++ extraModules) exports)

            bubble :: (HaskellModule, Maybe ([FullyQualifiedName], String)) -> Maybe (HaskellModule, ([FullyQualifiedName], String))
            bubble (h, Just x)  = Just (h, x)
            bubble (_, Nothing) = Nothing

            successes' :: [(HaskellModule, ([String], String))]
            successes' = mapMaybe bubble successes

            upToNow = map (\(m, (e, p)) -> ModuleExports
                                                { mName             = modName m
                                                , mPackageName      = p
                                                , mInfo             = m
                                                , qualifiedExports  = e
                                                }) successes'

        GhcMonad.liftIO $ forM_ upToNow $ \x -> putStrLn $ pprModuleExports x

        -- Get all "as" imports.
        let asImports :: [String]
            asImports = mapMaybe (modImportedAs . mInfo) upToNow

        -- Can a user do "import xxx as Foo.Bar"??? Check this.

        let mySymbol = case moduleOfQualifiedName symbol of
                        Nothing     -> MySymbolSysQualified symbolToUse
                        Just x      -> if x `elem` asImports
                                            then MySymbolUserQualified symbol
                                            else MySymbolSysQualified symbolToUse

        GhcMonad.liftIO $ print mySymbol

        let upToNow0 = refineAs mySymbol upToNow
        GhcMonad.liftIO $ putStrLn "upToNow0"
        GhcMonad.liftIO $ forM_ upToNow0 $ \x -> putStrLn $ pprModuleExports x

        let upToNow1 = refineRemoveHiding symbolToUse upToNow0
        GhcMonad.liftIO $ putStrLn "upToNow1"
        GhcMonad.liftIO $ forM_ upToNow1 $ \x -> putStrLn $ pprModuleExports x

        let upToNow2 = refineExportsIt symbolToUse upToNow1
        GhcMonad.liftIO $ putStrLn "upToNow2"
        GhcMonad.liftIO $ forM_ upToNow2 $ \x -> putStrLn $ pprModuleExports x

        let upToNow3 = refineLeadingDot mySymbol upToNow2
        GhcMonad.liftIO $ putStrLn "upToNow3"
        GhcMonad.liftIO $ forM_ upToNow3 $ \x -> putStrLn $ pprModuleExports x

        upToNow4 <- refineVisibleExports allGhcOpts ghcpkgOptions upToNow3
        GhcMonad.liftIO $ putStrLn "upToNow4"
        GhcMonad.liftIO $ forM_ upToNow4 $ \x -> putStrLn $ pprModuleExports x

        let lastMatch = getLastMatch upToNow4
        GhcMonad.liftIO $ print $ "last match: " ++ show lastMatch

        -- "last match: Just (ModuleExports {mName = \"Control.Monad\", mInfo = HaskellModule {modName = \"Control.Monad\", modQualifier = Nothing, modIsImplicit = False, modHiding = [], modImportedAs = Nothing, modSpecifically = [\"forM_\",\"liftM\",\"filterM\",\"when\",\"unless\"]}, qualifiedExports = [\"base-4.8.2.0:GHC.Base.when\"]})"

        let matchedModule :: String
            matchedModule = case mName <$> lastMatch of
                                Just mod    -> mod
                                _           -> error $ "No nice match in lastMatch for module: " ++ show lastMatch

        let matchedPackageName :: String
            matchedPackageName = case mPackageName <$> lastMatch of
                                    Just p -> p
                                    _      -> error $ "No nice match in lastMatch for package name: " ++ show lastMatch

        haddock <- GhcMonad.liftIO $ (maybe (return Nothing) (ghcPkgHaddockUrl allGhcOpts ghcpkgOptions) . Just) matchedPackageName

        GhcMonad.liftIO $ putStrLn $ "at the end now: " ++ show (matchedModule, moduleNameToHtmlFile matchedModule, matchedPackageName, haddock)

        url <- GhcMonad.liftIO $ matchToUrl (Just matchedModule, haddock, Just matchedModule, Just $ moduleNameToHtmlFile matchedModule)

        return $ Right url

-- | Top level function; use this one from src/Main.hs.
haddockUrl :: Options -> FilePath -> String -> String -> Int -> Int -> IO String
haddockUrl opt file modstr symbol lineNr colNr = do

    let ghcopts    = GhcOptions    $ ghcOpts    opt
    let ghcpkgopts = GhcPkgOptions $ ghcPkgOpts opt

    res <- guessHaddockUrl file modstr symbol lineNr colNr ghcopts ghcpkgopts
    print ("res", show res)

    case res of Right x  -> return $ "SUCCESS: " ++ x ++ "\n"
                Left err -> return $ "FAIL: " ++ show err ++ "\n"
