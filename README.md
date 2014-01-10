ghc-imported-from
=================

For a given Haskell source file, determine the path to the Haddock documentation for a symbol at a particular line/col location.

Example: on the file [Muddle.hs](https://github.com/carlohamalainen/ghc-imported-from/blob/master/tests/Muddle.hs),

    ../.cabal-sandbox/bin/ghc-imported-from Muddle.hs Muddle Maybe 11 11

says

    SUCCESS: /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/Data-Maybe.html

since the usage of ```Maybe``` at line 11, column 11, is from the ```Data.Maybe``` module.

Difficulties arise because some symbols are exported from a certain
package but defined in another, for example ```String``` is defined in
```GHC.Base``` but is exported from the standard prelude, the module
```Prelude```. There are other cases to deal with including qualified
imports, selective imports, imports with hidden components, etc.

Preference is given to any locally available Haddock documentation,
and then to the generic url at hackage.org.

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

## Beware

If you use Cabal sandboxes, you'll have to hardcode the path to the package config file. This is a known issue: [https://github.com/carlohamalainen/ghc-imported-from/issues/3](https://github.com/carlohamalainen/ghc-imported-from/issues/3)

Feedback and pull requests most welcome!
