{-# LANGUAGE DeriveDataTypeable #-}

import Language.Haskell.GhcImportedFrom

import Data.List
import Data.Maybe
import Data.Version ( showVersion )
import System.Environment

import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)
import System.Console.GetOpt
import System.Exit

import Control.Exception
import Data.Typeable

import Control.Monad (forM_)
import Control.Monad.Writer

import Paths_ghc_imported_from

-- FIXME adapt these to ghc-imported-from
data GHCImportedFromError = SafeList
                 | TooManyArguments String
                 | NoSuchCommand String
                 | CmdArg [String]
                 | FileNotExist String deriving (Show, Typeable)

instance Exception GHCImportedFromError

ghcOptHelp :: String
ghcOptHelp = " [--ghc-option GHC_opt1 --ghc-option GHC_opt2 ...] "

ghcPkgOptHelp :: String
ghcPkgOptHelp = " [--ghc-pkg-option ghc_pkg_opt1 --ghc-pkg-option ghc_pkg_opt2 ...] "

usage :: String
usage =    "ghc-imported-from version " ++ showVersion version ++ "\n"
        ++ "Usage:\n"
        ++ "\t ghc-imported-from haddock-url" ++ ghcOptHelp ++ ghcPkgOptHelp ++ "<HaskellFile> <module> <symbol> <line-no> <column-no>\n"
        ++ "\t ghc-imported-from help\n"
        ++ "\nExample: ghc-imported-from haddock-url src/Main.hs Main getArgs 160 13\n"

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv
    = case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldr id defaultOptions o, n)
        (_,_,errs) -> throw (CmdArg errs)

argspec :: [OptDescr (Options -> Options)]
argspec = [ Option "g" ["ghc-options"]
            (ReqArg (\g opts -> opts { ghcOpts = g : ghcOpts opts }) "ghc-options")
            "GHC options"
          , Option "p" ["ghc-pkg-options"]
            (ReqArg (\g opts -> opts { ghcPkgOpts = g : ghcPkgOpts opts }) "ghc-pkg-options")
            "ghc-pkg options"
          -- , Option "b" ["boundary"]
          --   (ReqArg (\s opts -> opts { lineSeparator = LineSeparator s }) "sep")
          --   "specify line separator (default is Nul string)"
          ]

main :: IO ()
main = flip catches handlers $ do
-- #if __GLASGOW_HASKELL__ >= 611
    hSetEncoding stdout utf8
-- #endif
    args <- getArgs
    let (opt,cmdArg) = parseArgs argspec args

    -- print opt
    -- print cmdArg

    let cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg2 = cmdArg !. 2
        cmdArg3 = cmdArg !. 3
        cmdArg4 = cmdArg !. 4
        cmdArg5 = cmdArg !. 5
        remainingArgs = tail cmdArg
        nArgs n f = if length remainingArgs == n
                        then f
                        else throw (TooManyArguments cmdArg0)

    res <- case cmdArg0 of
      "haddock-url" -> nArgs 5 $ haddockUrl opt cmdArg1 cmdArg2 cmdArg3 (read cmdArg4) (read cmdArg5)
      "help"        -> return $ usageInfo usage argspec
      cmd           -> throw (NoSuchCommand cmd)
    putStr res

    where
        handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
        handleThenExit handler = \e -> handler e >> exitFailure
        handler1 :: ErrorCall -> IO ()
        handler1 = print -- for debug
        handler2 :: GHCImportedFromError -> IO ()
        handler2 SafeList = printUsage
        handler2 (TooManyArguments cmd) = do
            hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
            printUsage
        handler2 (NoSuchCommand cmd) = do
            hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
            printUsage
        handler2 (CmdArg errs) = do
            mapM_ (hPutStr stderr) errs
            printUsage
        handler2 (FileNotExist file) = do
            hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
            printUsage
        printUsage = hPutStrLn stderr $ '\n' : usageInfo usage argspec

        xs !. idx
          | length xs <= idx = throw SafeList
          | otherwise = xs !! idx

