# ghc-imported-from

For a given Haskell source file, determine the path to the Haddock documentation for a symbol at a particular line/col location.

Example: on the file [src/Main.hs](https://github.com/carlohamalainen/ghc-imported-from/blob/master/src/Main.hs),

    ghc-imported-from haddock-url src/Main.hs Main getArgs 160 13

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

Either way, ensure that ```ghc-imported-from``` is in the current PATH.

### ghcimportedfrom-vim

Follow the instructions at
[https://github.com/carlohamalainen/ghcimportedfrom-vim](https://github.com/carlohamalainen/ghcimportedfrom-vim)
to install the Vim plugin.

## Usage

See the ```tests``` subdirectory for some examples. Or load your favourite Haskell project and hit F4.

Or watch the screencast (be sure to set 720p HD and then fullscreen):

[http://www.youtube.com/watch?v=VVc8uupYJGs](http://www.youtube.com/watch?v=VVc8uupYJGs)
