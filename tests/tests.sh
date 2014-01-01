#!/bin/bash

bin="../.cabal-sandbox/bin/ghc-imported-from"

# TODO Automate these. HUnit?

$bin Muddle.hs Muddle Maybe       10 11

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        11 7

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        15 10

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle String      19 14

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Int         21 23

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DL.length   22 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle print       24 8

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DM.fromList 26 5

