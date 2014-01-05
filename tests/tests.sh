#!/bin/bash

bin="../.cabal-sandbox/bin/ghc-imported-from"

# TODO Automate these. HUnit?

$bin Muddle.hs Muddle Maybe       11 11

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        12 7

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Just        16 10

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle String      20 14

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Int         22 23

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DL.length   23 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle print       25 8

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle DM.fromList 27 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Muddle.hs Muddle Safe.headMay 29 6

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Hiding.hs Hiding map           12 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin Hiding.hs Hiding head          12 5

echo ""
echo "---------------------------------------------------------"
echo ""

$bin When.hs When when 15 5

