#!/bin/bash

set -e
set -x

find . -name ghc-imported-from-*.tar.gz -exec rm -v {} \;

cabal sdist
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/debian-cabal/
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/debian-stack/
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/fedora-cabal/
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/fedora-stack/
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/ubuntu-cabal/
cp dist/ghc-imported-from-*.tar.gz docker-testsuite/ubuntu-stack/
