module Main where

import Language.Haskell.GhcImportedFrom
import Control.Monad.Writer
import Options.Applicative

data CmdOptions = CmdOptions { cmdGhcOptions    :: [String]
                             , cmdGhcPkgOptions :: [String]
                             , cmdHaskellFile   :: FilePath
                             , cmdModuleName    :: String
                             , cmdSymbol        :: String
                             , cmdLineNr        :: Int
                             , cmdColNr         :: Int
                             } deriving Show

parserOptions :: Parser CmdOptions
parserOptions = CmdOptions
    <$> (many $ strOption ( long "ghc-options"     <> help "GHC options" ))
    <*> (many $ strOption ( long "ghc-pkg-options" <> help "ghc-pkg options" ))
    <*> argument str  (metavar "<Haskell file>")
    <*> argument str  (metavar "<module name>")
    <*> argument str  (metavar "<symbol>")
    <*> argument auto (metavar "<line nr>")
    <*> argument auto (metavar "<column nr>")

main :: IO ()
main = do
    (CmdOptions gOpts gpkgOpts hfile mname sym line col) <- execParser opts
    haddockUrl (Options gOpts gpkgOpts (LineSeparator "\0")) hfile mname sym line col >>= putStr
  where
    opts = info (helper <*> parserOptions)
      (fullDesc <> header "ghc-imported-from - find the haddock url for a symbol in a Haskell file.\n\nExample: ghc-imported-from src/Main.hs Main getArgs 160 13\n")
