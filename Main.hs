import GhcImportedFrom

import Data.List
import Data.Maybe
import System.Environment
import System.IO()

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

    let n = fromJust $ findIndex (== "--ghc-pkg-options") rest

    -- assert: rest !! n == "--ghc-pkg-options"

    let ghcOptions    = GhcOptions $ tail $ take n rest
    let ghcPkgOptions = GhcPkgOptions $ drop (n + 1) rest

    putStrLn $ show ghcOptions
    putStrLn $ show ghcPkgOptions

    guessHaddockUrl targetFile targetModule symbol lineNo colNo ghcOptions ghcPkgOptions
