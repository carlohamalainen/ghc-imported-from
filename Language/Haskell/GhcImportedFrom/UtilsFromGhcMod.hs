{-# LANGUAGE Rank2Types, OverloadedStrings, CPP #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  UtilsFromGhcMod
-- Copyright   :  Carlo Hamalainen 2013, 2014
-- License     :  BSD3
--
-- Maintainer  :  carlo@carlo-hamalainen.net
-- Stability   :  experimental
-- Portability :  portable
--
-- The ghc-mod project has some very useful functions that are not
-- exported, so here I've pulled out the few that I need. Credit for
-- the code in this file is due to Kazu Yamamoto <kazu@iij.ad.jp>.
--
--      * <http://www.mew.org/~kazu/proj/ghc-mod/en>
--
--      * <https://github.com/kazu-yamamoto/ghc-mod>
--
-- Hopefully this is ok since ghc-mod and this project are both licensed BSD3.
-- If this package ever stabilises I may send a pull request to ghc-mod asking
-- for some of these functions to be exported, perhaps in Language.Haskell.GhcMod.Internal.

module Language.Haskell.GhcImportedFrom.UtilsFromGhcMod where

import Language.Haskell.GhcImportedFrom.UtilsFromCabalInstall

import Control.Applicative
import Data.Generics hiding (typeOf)
import GHC
import GHC.SYB.Utils
import System.Directory
import System.FilePath

import Packages

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Distribution.PackageDescription
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.Text as DistText
import Distribution.Verbosity (silent)

import Control.Exception (throwIO)

import Data.Set (fromList, toList)

import DynFlags

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
getGHCOptions  :: [GHCOption] -> Cradle -> String -> BuildInfo -> IO [GHCOption]
getGHCOptions ghcopts cradle cdir binfo = do
    cabalCpp <- cabalCppOptions cdir
    let cpps = map ("-optP" ++) $ cppOptions binfo ++ cabalCpp

    return $ ghcopts ++ pkgDb ++ exts ++ [lang] ++ libs ++ libDirs ++ cpps
  where
    pkgDb = cradlePackageDbOpts cradle
    lang = maybe "-XHaskell98" (("-X" ++) . DistText.display) $ defaultLanguage binfo
    libDirs = map ("-L" ++) $ extraLibDirs binfo
    exts = map (("-X" ++) . DistText.display) $ usedExtensions binfo
    libs = map ("-l" ++) $ extraLibs binfo

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
-- modification: return $ if ... instead of if .. return
-- Extra modification: add dist-sandbox-[hash] directory.
cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    let sandboxSubDir = sandboxBuildDir $ dir </> ".cabal-sandbox"
        sandboxFullPath = dir </> sandboxSubDir

    opts1 <- makeOpt $ dir </> "dist/build/autogen/cabal_macros.h"
    opts2 <- makeOpt $ sandboxFullPath </> "build/autogen/cabal_macros.h"

    return $ opts1 ++ opts2
  where
    makeOpt :: FilePath -> IO [String]
    makeOpt cabalMacroDotH = do
        exist <- doesFileExist cabalMacroDotH
        return $ if exist then ["-include", cabalMacroDotH]
                          else []

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
getGHCId :: IO CompilerId
getGHCId = CompilerId GHC <$> getGHC

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
getGHC :: IO Version
getGHC = do
    mv <- programFindVersion ghcProgram silent (programName ghcProgram)
    case mv of
        Nothing -> throwIO $ userError "ghc not found"
        Just v  -> return v

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
getCompilerOptions :: [GHCOption] -> Cradle -> PackageDescription -> IO CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- getGHCOptions ghcopts cradle cdir $ head buildInfos
    return $ CompilerOptions gopts idirs depPkgs
  where
    wdir       = cradleCurrentDir cradle
    Just cdir  = cradleCabalDir   cradle
    Just cfile = cradleCabalFile  cradle
    buildInfos = cabalAllBuildInfo pkgDesc
    idirs      = includeDirectories cdir wdir $ cabalSourceDirs buildInfos
    depPkgs    = removeThem problematicPackages $ removeMe cfile $ cabalDependPackages buildInfos

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
removeMe :: FilePath -> [Package] -> [Package]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
removeThem :: [Package] -> [Package] -> [Package]
removeThem badpkgs = filter (`notElem` badpkgs)

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
problematicPackages :: [Package]
problematicPackages = [
    "base-compat" -- providing "Prelude"
  ]

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
cabalBuildDirs :: [FilePath]
cabalBuildDirs = ["dist/build", "dist/build/autogen"]

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
includeDirectories :: FilePath -> FilePath -> [FilePath] -> [FilePath]
includeDirectories cdir wdir dirs = uniqueAndSort (extdirs ++ [cdir,wdir])
  where
    sandboxSubDir = sandboxBuildDir $ wdir </> ".cabal-sandbox"
    sandboxFullPath = wdir </> sandboxSubDir

    extdirs = map expand $ dirs ++ cabalBuildDirs ++ [sandboxFullPath </> "build", sandboxFullPath </> "build/autogen"]:: [FilePath]

    expand :: FilePath -> FilePath
    expand "."    = cdir
    expand subdir = cdir </> subdir

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
uniqueAndSort :: [String] -> [String]
uniqueAndSort = toList . fromList

-- ghc-mod/Language/Haskell/GhcMod/Gap.hs
addDevPkgs :: DynFlags -> [Package] -> DynFlags
addDevPkgs df []   = df
addDevPkgs df pkgs = df''
  where
#if __GLASGOW_HASKELL__ >= 707
    df' = gopt_set df Opt_HideAllPackages
#else
    df' = dopt_set df Opt_HideAllPackages
#endif
    df'' = df' {
        packageFlags = map ExposePackage pkgs ++ packageFlags df
      }


