#!/bin/bash

bin="../.cabal-sandbox/bin/ghc-imported-from"

# TODO Automate these. HUnit?

$bin Muddle.hs Muddle Maybe       9 11

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        10 7

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        14 10

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle String      18 14

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Int         20 23

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DL.length   21 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle print       23 8

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DM.fromList 25 5

