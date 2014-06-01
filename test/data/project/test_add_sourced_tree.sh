#!/bin/bash

./clean.sh

cd B
cabal sandbox init
cabal sandbox add-source `pwd`/../A
cabal install --dependencies-only
cabal build

ghc-imported-from haddock-url B.hs B fnA 5 5
