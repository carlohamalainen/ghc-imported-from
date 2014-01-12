import GhcImportedFrom

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
        ghcOpts        = GhcOptions $ drop 5 args

    putStrLn $ "ghcOpts: " ++ (show ghcOpts)
    guessHaddockUrl targetFile targetModule symbol lineNo colNo
