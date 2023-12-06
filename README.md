# .emacs.d: Emacs configuration

In december 2023, I started anew with my emacs configuration. I'm
using emacs since around 1996, I think. So over the years I've
accumulated quite some cruft, making a fresh start a bit of a
necessity. And I wanted to modernize my way of working a bit by
including LSP support.


## Inspiration/sources

- https://github.com/renzmann/.emacs.d/

- My previous emacs config, cobbled together over 25+ years.

- Abovementioned emacs config contained quite some "emacs starter kit"
  content.


## Requirements

Of course this repo needs to be checked out in `~/.emacs.d/`.

The python-virtualenv-detector "PET" needs this for json/toml parsing:

    $ brew install dasel

Also needed for LSP:

    $ pipx install pyright  # which actually installs an npm package
    $ pipx install ruff-lsp  # picked up automatically


## Notes

- [use-package
  explanation](https://www.masteringemacs.org/article/spotlight-use-package-a-declarative-configuration-tool)
- [use-package docs](https://github.com/jwiegley/use-package/)

## LSP: python

Ruff checks basic sytax and can format the code.


## Keybindings I want to remember

`M-g i`: "go to index item", runs "imenu".
