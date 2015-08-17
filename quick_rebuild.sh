#!/bin/bash

cabal install --haddock-hyperlink-source --dependencies-only && cabal install --haddock-hyperlink-source
