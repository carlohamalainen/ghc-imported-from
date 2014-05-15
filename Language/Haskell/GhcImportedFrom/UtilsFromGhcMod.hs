{-# LANGUAGE Rank2Types, OverloadedStrings, CPP #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  UtilsFromGhcMod
-- Copyright   :  Carlo Hamalainen 2013, 2014
-- License     :  BSD3
--
-- Maintainer  :  carlo@carlo-hamalainen.net
-- Stability   :  experimental
-- Portability :  portable
--
-- The ghc-mod project has some very useful functions that are not
-- exported, so here I've pulled out the few that I need. Credit for
-- the code in this file is due to Kazu Yamamoto <kazu@iij.ad.jp>.
--
--      * <http://www.mew.org/~kazu/proj/ghc-mod/en>
--
--      * <https://github.com/kazu-yamamoto/ghc-mod>
--
-- Hopefully this is ok since ghc-mod and this project are both licensed BSD3.
-- If this package ever stabilises I may send a pull request to ghc-mod asking
-- for some of these functions to be exported, perhaps in Language.Haskell.GhcMod.Internal.

module Language.Haskell.GhcImportedFrom.UtilsFromGhcMod where

import Data.Generics hiding (typeOf)
import GHC
import GHC.SYB.Utils

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifySpans :: Typeable a => TypecheckedSource -> (Int, Int) -> [Located a]
listifySpans tcs lc = listifyStaged TypeChecker p tcs
  where
    p (L spn _) = isGoodSrcSpan spn && spn `spans` lc

-- ghcmod/Language/Haskell/GhcMod/Info.hs
listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))
