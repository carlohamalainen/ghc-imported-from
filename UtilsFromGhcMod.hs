{-# LANGUAGE Rank2Types #-}

{-

The ghc-mod project has some very useful functions that are not
exported, so here I've pulled out the few that I need. Credit for
the code in this file is due to Kazu Yamamoto <kazu@iij.ad.jp>.

* http://www.mew.org/~kazu/proj/ghc-mod/en/
* https://github.com/kazu-yamamoto/ghc-mod

(Hopefully this is ok since ghc-mod is licensed BSD3 as is this project.)

-}


module UtilsFromGhcMod where

import Control.Applicative
import Data.Generics hiding (typeOf)
import Data.Maybe
import GHC
import GHC.SYB.Utils
import System.Directory
import System.FilePath

import Packages

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Internal
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)

import Control.Exception (throwIO)

import Distribution.Package ( Dependency(Dependency)
                            , PackageName(PackageName)
                            , PackageIdentifier(pkgName))


-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

-- ghc-mod/Language/Haskell/GhcMod/CabalApi.hs
getGHCOptions  :: [GHCOption] -> Cradle -> [Char] -> BuildInfo -> IO [GHCOption]
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
cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    if exist then
        return ["-include", cabalMacro]
      else
        return []
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
        Just v  -> return $ v
