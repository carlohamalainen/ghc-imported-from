module Main where

-- Fake ghc binary, used to extract the GHC command line options
-- via the "cabal repl" command. Thanks to Herbert Valerio Riedel
-- on haskell-cafe for the tip:
--
--     http://www.haskell.org/pipermail/haskell-cafe/2014-May/114183.html

-- If we are called with --numeric-version or --info then we lie and pretend
-- to be the system's current default ghc binary. Will this cause problems if
-- someone is using --with-ghc elsewhere to choose the ghc binary?

import Data.List (unwords)
import System.Cmd (rawSystem)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs

    if length args == 1
        then case head args of "--numeric-version" -> rawSystem "ghc" ["--numeric-version"] >> return ()
                               "--info"            -> rawSystem "ghc" ["--info"]            >> return ()
                               _                   -> putStrLn $ unwords args
        else putStrLn $ unwords args
