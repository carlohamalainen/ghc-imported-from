#!/bin/bash

bin="../.cabal-sandbox/bin/ghc-imported-from"

# TODO Automate these. HUnit?

$bin Muddle.hs Muddle Maybe     8 11

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just      9 7

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just      13 10

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle String    17 14

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Int       19 23

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DL.length 20 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle print     22 8

