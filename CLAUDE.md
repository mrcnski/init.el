# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal Emacs configuration (~4300 lines of elisp) for a single user on macOS. Requires Emacs 29+, currently built with `brew install emacs-plus --with-poll --with-native-comp`. There is no build system or CI — verification mostly means loading the config in Emacs.

## Verifying changes

ERT tests live in `test/` (currently only for select custom functions). Run a test file with:

```sh
emacs -Q --batch -l test/repunctuate-paragraph-tests.el -f ert-run-tests-batch-and-exit
```

Test files stub `use-package` as a no-op macro so an `init/*.el` file can be loaded under `emacs -Q` without package.el — copy that pattern when testing functions from other init modules.

From inside Emacs (these are the workflows documented in init.el's commentary):

- `M-x restart-emacs` — restart to test config changes end-to-end.
- `M-x bug-hunter-init-file` — bisect the init files to locate a load error.
- `M-x profiler-start` / `M-x profiler-report` — profile runtime performance.
- `M-x free-keys` — find unused keybindings before adding a new one.

A quick syntax/load smoke test from the shell only covers part of the config (see the terminal gotcha below):

```sh
emacs --batch -l ~/.emacs.d/init.el
```

## Architecture

`early-init.el` handles native-comp settings and frame/GC startup tweaks. `init.el` then loads modules from `init/` via `require`, **in a deliberate order** — `init-basics` → `init-package` (MELPA available after this) → early utils/keybindings → built-ins → visuals → external packages → `init-org` → `init-mode-line` (deliberately last, to override changes from other packages). When adding a module or moving code between modules, respect this ordering.

**Terminal gotcha:** init.el stops loading early when `display-graphic-p` is nil (a trick that moves the load pointer to the end of the buffer). Terminal Emacs is only used for quick edits, so everything from `init-packages-general` onward — including all external packages, org, and mode-line — only loads in GUI sessions. Batch-mode checks hit the same early exit.

Key module facts not obvious from filenames:

- `init-basics.el` defines the path variables used everywhere (`user-emacs-var-directory` etc.). Package/var/etc state lives outside the repo in `~/.local/emacs/`; the eln-cache lives in `~/.eln-cache/` (set in early-init.el) to avoid cloud-sync churn.
- Packages are managed with `use-package` + MELPA, with `use-package-always-ensure t` — a new `use-package` form auto-installs on next start.
- `customize.el` is loaded at the end of init but is gitignored (machine-local); don't put config there.
- `init-packages-languages.el` pins tree-sitter grammars to exact tags/commits in `treesit-language-source-alist`. Grammars install to `~/.local/emacs/var/treesit/` (no-littering's themed dir). The install loop skips already-installed grammars, so after bumping a pin you must delete the corresponding `~/.local/emacs/var/treesit/*.dylib` (or call `treesit-install-language-grammar`) for it to take effect.

## Vendored packages (`packages/`)

Git submodules for self-authored or forked packages (nimbus-theme, eyebrowse, keys, compnav-eshell, context-clues, promptu, catppuccin-theme), loaded via `use-package :load-path`. Convention: feature work and bug fixes belong in the fork/package repo itself; the `init/*.el` files only contain toggles, keybindings, and glue. Don't grow package logic inside `init/`.

## Conventions

- Every file uses the standard elisp skeleton: `-*- lexical-binding: t; -*-` header, `;;; Commentary:` / `;;; Code:` sections, and a matching `(provide 'feature)` + `;;; file ends here` footer.
- `notes.md` at the repo root is an untracked scratch file for pending upgrade/maintenance plans — read it for context on in-flight work, but it is not part of the config.
