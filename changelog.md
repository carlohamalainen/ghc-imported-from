2014-05-19 v0.2.0.3

* Fixed test cases.
* Added alternative heuristic for lookup.

2014-05-16 v0.2.0.2

* Catch GHC panics.
* Verbose debug output.
* Handle case where source file may not have a validly define module name
  but parses correctly regardless. https://github.com/carlohamalainen/ghc-imported-from/issues/15

2014-05-15 v0.2.0.1

* Link to changelog.md.

2014-05-15 v0.2.0.0

* Builds with GHC 7.6.3 and 7.8.2.
* Reduced dependencies on ghc-mod and Cabal internals.
* GHC option discovery using a fake GHC binary.
* Various tidyups via hlint.
* Removed cabal constraints file.

2014-05-02 v0.1.0.4

* Fix for hashed directory in sandbox dist/build; see also
  https://github.com/carlohamalainen/ghc-imported-from/issues/10

2014-03-03 v0.1.0.3

* Fix build failure by pinning all package dependencies.

2014-01-26 v0.1.0.2

* Use more of ghc-mod's API to set the correct GHC command
  line arguments.

2014-01-24 v0.1.0.1

* Change module layout to Language.Haskell.GhcImportedFrom
  to comply with hackage.haskell.org guidelines.

2014-01-21 v0.1.0.0

* First version on hackage.haskell.org
