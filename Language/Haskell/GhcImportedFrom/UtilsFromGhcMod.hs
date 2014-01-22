{-# LANGUAGE Rank2Types, OverloadedStrings #-}

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

module Language.Haskell.GhcImportedFrom.UtilsFromGhcMod where

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
import Distribution.Verbosity (silent)

import Control.Exception (throwIO)

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
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ defaultLanguage binfo
    libDirs = map ("-L" ++) $ extraLibDirs binfo
    exts = map (("-X" ++) . display) $ usedExtensions binfo
    libs = map ("-l" ++) $ extraLibs binfo

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
-- modification: return $ if ... instead of if .. return
cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    return $ if exist then ["-include", cabalMacro]
                      else []
  where
    cabalMacro = dir </> "dist/build/autogen/cabal_macros.h"

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
