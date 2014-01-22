import Language.Haskell.GhcImportedFrom

import Data.List
import Data.Maybe
import System.Environment
import System.IO()

import Control.Monad (forM_)
import Control.Monad.Writer

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

    let ghcopts    = GhcOptions $ tail $ take n rest
    let ghcpkgopts = GhcPkgOptions $ drop (n + 1) rest

    print ghcopts
    print ghcpkgopts

    (res, logMessages) <- runWriterT $ guessHaddockUrl targetFile targetModule symbol lineNo colNo ghcopts ghcpkgopts

    case res of Right x  -> do forM_ logMessages putStrLn
                               putStrLn $ "SUCCESS: " ++ x
                Left err -> putStrLn $ "FAIL: " ++ show err
