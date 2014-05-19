# ghc-imported-from

For a given Haskell source file, determine the path to the Haddock documentation for a symbol at a particular line/col location.

Example: on the file [src/Main.hs](https://github.com/carlohamalainen/ghc-imported-from/blob/master/src/Main.hs),

    ghc-imported-from haddock-url src/Main.hs Main getArgs 69 13

says

    SUCCESS: file:///home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/System-Environment.html

since the usage of ```getArgs``` at line 160, column 13, is from the ```System.Environment``` module.

Difficulties arise because some symbols are exported from a certain
package but defined in another, for example ```String``` is defined in
```GHC.Base``` but is exported from the standard prelude, the module
```Prelude```. There are other cases to deal with including qualified
imports, selective imports, imports with hidden components, etc.

Preference is given to any locally available Haddock documentation,
and then to the generic url at hackage.org.

## Beware

You may have to run

    cabal build

or

    cabal repl

in a project directory to sort out some of the ```dist/build/autogen```
files. At the moment ```ghc-imported-from``` has no functionality to
do this boot process automatically. To run ```cabal repl``` you might need
the latest Cabal from [https://github.com/haskell/cabal](https://github.com/haskell/cabal).

If you see

    <command line>: cannot satisfy -package hspec
        (use -v for more information)

then you may need the hspec and/or doctest packages:

    cabal install hspec doctest

Feedback and pull requests most welcome!

## Install

### ghc-imported-from

Install into ```~/.cabal```:

    git clone https://github.com/carlohamalainen/ghc-imported-from
    cd ghc-imported-from
    cabal install

Or, install into a sandbox:

    git clone https://github.com/carlohamalainen/ghc-imported-from
    cd ghc-imported-from
    ./build_in_sandbox.sh

Either way, ensure that ```ghc-imported-from``` and ```fake-ghc-for-ghc-imported-from``` are in the current PATH.

### Tests

Run the tests using cabal:

    cabal test --show-details=streaming

As of 2014-05-18 just running ```cabal test``` seems to run the
tests and then hang (waiting on a PID). This seems to be the problem:
https://github.com/haskell/cabal/issues/1810

Running with ```--show-details=streaming``` seems to work ok.

### ghcimportedfrom-vim

Follow the instructions at
[https://github.com/carlohamalainen/ghcimportedfrom-vim](https://github.com/carlohamalainen/ghcimportedfrom-vim)
to install the Vim plugin.

## Usage

See the ```tests``` subdirectory for some examples. Or load your favourite Haskell project and hit F4.

Or watch the screencast (be sure to set 720p HD and then fullscreen):

[http://www.youtube.com/watch?v=VVc8uupYJGs](http://www.youtube.com/watch?v=VVc8uupYJGs)

## Notes

```ghc-imported-from``` uses both GHC and ghc-pkg, which
accept arguments in differing formats.  For example GHC takes
```-package-db``` while ghc-pkg takes ```--package-db=```. For more
details: http://www.vex.net/~trebla/haskell/sicp.xhtml

## Debugging

To see the GHC options that have been automatically detected, change into your project's directory and run:

    $ cd ~/ghc-imported-from
    $ cabal repl --with-ghc=fake-ghc-for-ghc-imported-from
    Preprocessing library ghc-imported-from-0.2.0.2...
    --interactive -fbuilding-cabal-package -O0 -outputdir dist/build -odir dist/build -hidir dist/build -stubdir dist/build -i -idist/build -i. -idist/build/autogen -Idist/build/autogen -Idist/build -optP-include -optPdist/build/autogen/cabal_macros.h -package-name ghc-imported-from-0.2.0.2 -hide-all-packages -no-user-package-db -package-db /home/user/ghc-imported-from/.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d -package-db dist/package.conf.inplace -package-id Cabal-1.16.0-c6e09e008cd04cf255c1ce0c59aba905 -package-id base-4.6.0.1-8aa5d403c45ea59dcd2c39f123e27d57 -package-id containers-0.5.0.0-ab1dae9a94cd3cc84e7b2805636ebfa2 -package-id directory-1.2.0.1-91a788fd88acd7f149f0f10f5f1e23f2 -package-id filepath-1.3.0.1-b12cbe18566fe1532a1fda4c85e31cbe -package-id ghc-7.6.3-18957ddbb817289f604552aa2da2e879 -package-id ghc-mod-4.1.0-a87501f2667239b3f0bef3e0f3753496 -package-id ghc-paths-0.1.0.9-3817f31ae510ed3b58554933ea527b74 -package-id ghc-syb-utils-0.2.1.2-bf72c1e71339c52f0af404a12449c9d2 -package-id mtl-2.2.0.1-ef91e0abcf7a4fb581ecb7fe83cdcba1 -package-id process-1.1.0.2-76e05340eb66705981411022731ca84a -package-id safe-0.3.4-ba52ca348aecad429ba90450e3aba4c4 -package-id syb-0.4.1-9469ffdd9c6a7ebbf035421c915a08ee -package-id transformers-0.4.1.0-42810d723884ebf2a2dd638e5b22e523 -XHaskell2010 Language.Haskell.GhcImportedFrom Language.Haskell.GhcImportedFrom.UtilsFromGhcMod Language.Haskell.GhcImportedFrom.Types -Wall
