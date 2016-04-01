#!/bin/bash

set -e
set -x

cabal update
cabal sandbox init
cabal install --enable-documentation --haddock-hyperlink-source --dependencies-only --verbose
cabal install --verbose
cabal configure --enable-tests --verbose && cabal build --verbose && cabal test
cat dist/test/ghc-imported-from-*-spec.log
