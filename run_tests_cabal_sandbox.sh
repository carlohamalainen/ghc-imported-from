#!/bin/bash

cabal configure --enable-tests && cabal build && cabal test
