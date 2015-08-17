#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

export sandbox=/scratch/sandboxes/ghc-imported-from

rm -fr $sandbox cabal.sandbox.config dist

cabal sandbox init --sandbox=${sandbox}

cabal install --haddock-hyperlink-source --dependencies-only # Is this necessary? Why not just cabal install?
cabal install
