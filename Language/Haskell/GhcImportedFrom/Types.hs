{-# LANGUAGE FlexibleInstances #-}

-- Adapted from ghc-mod/Language/Haskell/GhcMod/Types.hs

module Language.Haskell.GhcImportedFrom.Types where

-- FIXME We don't support LineSeparator; might be handy for
-- Windows (?) with CRLF encoding?
newtype LineSeparator = LineSeparator String deriving (Show)

data Options = Options {
      ghcOpts       :: [String]
    , ghcPkgOpts    :: [String]
    , lineSeparator :: LineSeparator
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options {
      ghcOpts       = []
    , ghcPkgOpts    = []
    , lineSeparator = LineSeparator "\0"
    }
