module Main where

import Bar (barFn, fooFn)


x = barFn
y = fooFn

main :: IO ()
main = print $ fooFn 3
