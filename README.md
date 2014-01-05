ghc-imported-from
=================

For a given Haskell source file, determine where a symbol is imported from.

## Install

### ghcmod-vim (forked)

First install prerequisites, following instructions at
[https://github.com/carlohamalainen/ghcmod-vim/blob/master/README.md](https://github.com/carlohamalainen/ghcmod-vim/blob/master/README.md)

Then install the forked version:

    cd ~/.vim/bundle/
    git clone https://github.com/eagletmt/ghcmod-vim.git
    cd

Optionally, add a symlink so that it works with .lhs files as well:

    cd ~/.vim/bundle/ghcmod-vim/after/ftplugin
    ln -s haskell/ lhaskell
    cd

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

### Tweak vimrc

Add to ```~/.vimrc``` to get F4 to show the documentation's url:

    au FileType haskell nnoremap <buffer> <F4> :GhcModDocUrl<CR>
    au FileType lhaskell nnoremap <buffer> <F4> :GhcModDocUrl<CR>

Or to make F4 open the documentation in the systems's default browser:

    au FileType haskell nnoremap <buffer> <F4> :GhcModOpenDoc<CR>
    au FileType lhaskell nnoremap <buffer> <F4> :GhcModOpenDoc<CR>

## Usage

See the ```tests``` subdirectory for some examples. Or load your favourite Haskell project and hit F4.

Feedback and pull requests most welcome!

