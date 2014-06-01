TODO: Make this a proper test?

    cd usingreexport
    cabal sandbox init
    cabal sandbox add-source /path/to/reexport
    cabal install --dependencies-only --reinstall
    cabal install

Then check:

    ghc-imported-from haddock-url Main.hs Main barFn 6 5

It should report the haddock for ```Bar```.
