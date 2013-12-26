-- Muddle.hs

module Muddle where

import Data.Maybe
import qualified Data.List as DL

f :: a -> Maybe a
f x = Just x

g :: IO ()
g = do
    let (Just _, _) = (Just 3, Just 4)

    return ()

s = "boo" :: String
s' = head s
t = Just 100 :: Maybe Int
r = DL.length [1, 2, 3]

main = print "Hello, World!"
