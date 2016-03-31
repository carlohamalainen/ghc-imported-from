#!/bin/bash

set -e
set -x

stack setup     --install-ghc --no-system-ghc
stack build     --install-ghc --no-system-ghc
stack haddock   --install-ghc --no-system-ghc
stack test      --install-ghc --no-system-ghc
