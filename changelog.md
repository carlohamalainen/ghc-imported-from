2016-04-04 v0.3.0.3

* Bugfix: was parsing stderr instead of stdout for some 'stack path' commands.
* Build against process-streaming-0.9.1.0 instead of process-streaming-0.7.2.2.

2016-03-30 v0.3.0.2

* Bugfix to the bugfix.

2016-03-30 v0.3.0.1

* Bugfix: use process-streaming to avoid deadlock on Fedora 23.

2016-03-26 v0.3.0.0

* New heuristics for resolving symbols.
* Compatability with Stack!

2016-01-20 v0.2.1.1

* Builds against ghc-mod-5.5.0.0.

2016-01-20 v0.2.1.0

* Builds on GHC 7.10.3 with the latest version of ghc-mod
  that is available on Hackage.

2015-08-17 v0.2.0.7

* Added a fall-back case when our resolved qualified name
  does not match anything.

2014-07-05 v0.2.0.6

* Use optparse-applicative for argument parsing.
* Allow digits and underscores in module names.

2014-06-01 v0.2.0.5

* Version bound on ghc-mod.

2014-05-22 v0.2.0.4

* Speedup: factor out calls to getGhcOptionsViaCabalRepl.
* Bug fix: filter out haskell module names from the cabal options list.

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
