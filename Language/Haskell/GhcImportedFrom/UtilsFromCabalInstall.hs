-----------------------------------------------------------------------------
-- |
-- Module      :  UtilsFromCabalInstall
-- Copyright   :  Carlo Hamalainen 2014
-- License     :  BSD3
--
-- Maintainer  :  carlo@carlo-hamalainen.net
-- Stability   :  experimental
-- Portability :  portable
--
-- We need a function from cabal-install which is not exported.

module Language.Haskell.GhcImportedFrom.UtilsFromCabalInstall where

import Data.Word                              ( Word32 )
import Numeric                                ( showHex )
import Data.List                              ( foldl' )
import Data.Char                              ( ord )
import Data.Bits                              ( shiftL, shiftR, xor )

-- Required for a temporary workaround for https://github.com/carlohamalainen/ghc-imported-from/issues/10

-- https://github.com/haskell/cabal/blob/master/cabal-install/Distribution/Client/Sandbox.hs#L129
sandboxBuildDir :: FilePath -> FilePath
sandboxBuildDir sandboxDir = "dist/dist-sandbox-" ++ showHex sandboxDirHash ""
  where
    sandboxDirHash = jenkins sandboxDir

    -- See http://en.wikipedia.org/wiki/Jenkins_hash_function
    jenkins :: String -> Word32
    jenkins str = loop_finish $ foldl' loop 0 str
      where
        loop :: Word32 -> Char -> Word32
        loop hash key_i' = hash'''
          where
            key_i   = toEnum . ord $ key_i'
            hash'   = hash + key_i
            hash''  = hash' + (shiftL hash' 10)
            hash''' = hash'' `xor` (shiftR hash'' 6)

        loop_finish :: Word32 -> Word32
        loop_finish hash = hash'''
          where
            hash'   = hash + (shiftL hash 3)
            hash''  = hash' `xor` (shiftR hash' 11)
            hash''' = hash'' + (shiftL hash'' 15)
