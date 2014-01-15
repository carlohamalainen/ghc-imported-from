If you installed using ```cabal install```:

    ./tests-in-plain-cabal-install.sh  | grep '^SUCCESS' > tests-plain.out

If you installed using a cabal sandbox:

    ./tests-in-sandbox.sh              | grep '^SUCCESS' > tests-sandbox.out
