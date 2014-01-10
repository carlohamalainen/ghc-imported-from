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

## Install - Linux

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

## Install - Windows

Tested on Windows 7 64bit.

Install the following components:

* [Haskell Platform 2013](http://www.haskell.org/platform/windows.html)
* [GIT](http://git-scm.com/download/win)
* [MingW](http://sourceforge.net/projects/mingw/files/)  (then install all dev tools in the "Basic" tab)
* [Vim](http://www.vim.org/download.php)

Then using a Git Bash shell:

    cd $HOME
    mkdir -p ~/vimfiles/autoload ~/vimfiles/bundle
    curl -Sso ~/vimfiles/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

### vimproc:

    git clone https://github.com/Shougo/vimproc.vim.git ~/vimfiles/bundle/vimproc.vim
    cd ~/vimfiles/bundle/vimproc.vim
    /c/MinGW/bin/mingw32-make.exe -f make_mingw32.mak
    cd

### ghc-imported-from

    git clone https://github.com/carlohamalainen/ghc-imported-from
    cd ghc-imported-from
    cabal install
    cd -

### ghcimportedfrom-vim

    git clone https://github.com/carlohamalainen/ghcimportedfrom-vim ~/vimfiles/bundle/ghcimportedfrom-vim

### vimrc

Make sure that it looks like this:

    $ cat $HOME/.vimrc

    execute pathogen#infect()
    syntax on
    filetype plugin indent on

    au FileType haskell  nnoremap <buffer> <F4> :GhcImportedFromOpenHaddock<CR>
    au FileType lhaskell nnoremap <buffer> <F4> :GhcImportedFromOpenHaddock<CR>

    au FileType haskell  nnoremap <buffer> <F5> :GhcImportedFromEchoUrl<CR>
    au FileType lhaskell nnoremap <buffer> <F5> :GhcImportedFromEchoUrl<CR>

## Usage

See the ```tests``` subdirectory for some examples. Or load your favourite Haskell project and hit F4.

Or watch the screencast (be sure to set 720p HD and then fullscreen):

[http://www.youtube.com/watch?v=VVc8uupYJGs](http://www.youtube.com/watch?v=VVc8uupYJGs)

## Beware

If you use Cabal sandboxes, you'll have to hardcode the path to the package config file. This is a known issue: [https://github.com/carlohamalainen/ghc-imported-from/issues/3](https://github.com/carlohamalainen/ghc-imported-from/issues/3)

Feedback and pull requests most welcome!
