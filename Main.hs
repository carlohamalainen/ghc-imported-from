import GhcImportedFrom

import Data.List
import Data.Maybe
import System.Environment
import System.IO()

-- import Control.Applicative ((<$>))
-- import Control.Monad (forM_)
-- import Language.Haskell.GhcMod as GM
-- import Language.Haskell.GhcMod.Internal as GMI
-- import Distribution.PackageDescription as PD
-- import Outputable (ppr, showSDoc)
-- import DynFlags (tracingDynFlags, xopt, ExtensionFlag(..))
-- import qualified GHC

main :: IO ()
main = do
    {-
    -- getSummary :: GhcOptions -> FilePath -> String -> IO ModSummary
    x <- getSummary (GhcOptions ["-no-user-package-db", "-package-db", "/home/carlo/work/github/cli-yesod-blog/blog/.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"])
                    "Handler/Home.hs" "Handler.Home"

    let ons = GHC.extensions $ GHC.ms_hspp_opts x

    print $ length ons

    let eflags = GHC.extensionFlags $ GHC.ms_hspp_opts x

    print $ xopt Opt_Rank2Types $ GHC.ms_hspp_opts x
    print $ xopt Opt_AlternativeLayoutRule $ GHC.ms_hspp_opts x
    print $ xopt Opt_ScopedTypeVariables $ GHC.ms_hspp_opts x

    c <- GM.findCradle

    print $ cradlePackageDbOpts c
    -}


    {-
    let cf = fromJust $ GM.cradleCabalFile c

    buildInfo <- cabalAllBuildInfo <$> GMI.parseCabalFile cf

    let opts = concatMap PD.defaultExtensions buildInfo ++ concatMap PD.otherExtensions buildInfo

    putStrLn $ show opts

    print "done"

    -- cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
    -}


    args <- getArgs

    -- quick and dirty argument parsing, no error checking
    let targetFile     = args !! 0
        targetModule   = args !! 1
        symbol         = args !! 2
        lineNo         = (read $ args !! 3) :: Int
        colNo          = (read $ args !! 4) :: Int
        rest           = drop 5 args

    -- assert: rest !! 0 == "--ghc-options"

    let n = fromJust $ elemIndex "--ghc-pkg-options" rest

    -- assert: rest !! n == "--ghc-pkg-options"

    let ghcOpts    = GhcOptions $ tail $ take n rest
    let ghcPkgOpts = GhcPkgOptions $ drop (n + 1) rest

    print ghcOpts
    print ghcPkgOpts

    guessHaddockUrl targetFile targetModule symbol lineNo colNo ghcOpts ghcPkgOpts
