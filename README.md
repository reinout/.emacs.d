# `.emacs.d`: Emacs configuration

In December 2023, I started anew with my emacs configuration. I'm
using emacs since around 1996, I think. So over the years I've
accumulated quite some cruft, making a fresh start a bit of a
necessity. And I wanted to modernize my way of working a bit by
including LSP support.

Of course this repo needs to be checked out in `~/.emacs.d/`.


## Inspiration/sources

- https://github.com/renzmann/.emacs.d/

- My previous emacs config, cobbled together over 25+ years.

- Abovementioned emacs config contained quite some "emacs starter kit"
  content.


## LSP python notes

`lsp-mode` was way too slow for me. Perhaps it was `lsp-ui`. Anyway,
I'm using `eglot` now, which works pretty OK.

Needed for python is microsoft's "pyright" language server:

    $ pipx install pyright  # which actually installs an npm package

This takes care of code completion and type hints *if* it can find the
code. Due to the large difference in projects that I work on, I'm just
sticking to adding a `pyrightconfig.json` or adding it to
`pyproject.toml`. If the stuff is in a virtualenv dir, you can do it
in `pyproject.toml`:

    [tool.pyright]
    venv = "venv"

Otherwise, `pyrightconfig.json`:

    {"include": ["my_code_dir"],
     "extraPaths": ["lib/python3.11/site-packages/"]
    }


## (Python) project integration

Formatting, running the tests: I don't want to spend the time to
configure that all in the LSP. In several modes, "projectile" is
enabled, which can keep track of the project's root directory. From
the root dir, I can then kick off some shell scripts (see
https://github.com/reinout/tools/tree/master/shell#projectile-test).

For instance `ctrl-c t` to run the tests, which runs
`projectile-test`, see the documentation linked above for what it
does. Normally, it will find your local `pytest` or it will run `make
test`.

`ctrl-c b`: code beautification.

`ctrl-c c`: code checks.


## Keybindings I want to remember

`M-g i`: "go to index item", runs "imenu".



## TODO

eldoc meer als popup configureren

Misschien flymake in de marge laten vrutten?

yasnippet aanzetten

Projectile niet vergeten: kan ik ruff aanslingeren?

imenu mooier
