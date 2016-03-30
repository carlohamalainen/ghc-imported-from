#!/bin/bash

cabal configure --enable-tests && cabal build && cabal test
cat dist/test/ghc-imported-from-*-spec.log
