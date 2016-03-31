#!/bin/bash

set -e
set -x

cabal update
cabal install cabal-install
export PATH=$HOME/.cabal/bin:$PATH
cabal sandbox init
cabal install --enable-documentation --haddock-hyperlink-source --dependencies-only
cabal install
cabal configure --enable-tests && cabal build && cabal test
cat dist/test/ghc-imported-from-*-spec.log
