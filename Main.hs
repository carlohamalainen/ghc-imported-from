import GhcImportedFrom

import Data.List
import Data.Maybe
import System.Environment
import System.IO()

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Writer
import Language.Haskell.GhcMod as GM
import Language.Haskell.GhcMod.Internal as GMI
import Distribution.PackageDescription as PD
import Outputable (ppr, showSDoc)
import DynFlags (tracingDynFlags, xopt, ExtensionFlag(..))
import qualified GHC

main :: IO ()
main = do
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

    (res, log) <- runWriterT $ guessHaddockUrl targetFile targetModule symbol lineNo colNo ghcOpts ghcPkgOpts

    case res of Right x  -> do forM_ log putStrLn
                               putStrLn $ "SUCCESS: " ++ x
                Left err -> putStrLn $ "FAIL"
