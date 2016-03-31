#!/bin/bash

set -e
set -x

stack setup
stack build
stack haddock
stack test
