;;; init-packages-languages --- Load language packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-packages-project)

;; Treesitter

;; Built-in treesitter support.
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
               ;; Pinned to a commit since upstream has no version tags.
               (astro . ("https://github.com/virchau13/tree-sitter-astro" "213f6e6973d9b456c6e50e86f19f66877e7ef0ee"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               ;; Split grammar: Emacs 31's built-in markdown-ts-mode
               ;; needs both the block and inline parsers.
               (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3" "tree-sitter-markdown/src"))
               (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3" "tree-sitter-markdown-inline/src"))
               (prisma . ("https://github.com/victorhqc/tree-sitter-prisma" "v1.5.0"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        ;; Keep binaries out of the synced repo.
        (treesit-install-language-grammar
         (car grammar) (no-littering-expand-var-file-name "treesit/")))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             ;; .js maps to `javascript-mode' (alias of `js-mode');
             ;; remap both — `major-mode-remap' doesn't chase aliases.
             (javascript-mode . js-ts-mode)
             (js-mode . js-ts-mode)
             ;; Shell scripts use sh-mode (there is no bash-mode).
             (sh-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             ;; Built-in auto-mode-alist sends .json to `js-json-mode';
             ;; our own entry catches .json first, this catches the rest.
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

;; Astro

(use-package astro-ts-mode
  :after (treesit-auto)

  :config
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe))

  (define-key astro-ts-mode-map (kbd "M-o") nil)

  (setq astro-ts-mode-indent-offset 4)
  )

;; Beancount

(use-package beancount
  :hook (
         (beancount-mode . flymake-bean-check-enable)
         (beancount-mode . outline-minor-mode)
         (beancount-mode . my-beancount-hook)
         )
  :mode ("\\.bean\\(?:count\\)?\\'" . beancount-mode)

  :init
  (defun my-beancount-hook ()
    (outline-hide-sublevels 3)
    )

  :config
  ;; (setq beancount-accounts-files
  ;;       (directory-files "~/Dropbox/beancount/accounts/"
  ;;                        'full
  ;;                        (rx ".bean" eos)))
  )

;; Caddy

(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode))
  )

;; CSS / SCSS

(use-package css-mode
  :ensure nil
  :config
  (setopt
   css-indent-offset 2
   )
  )

;; Docker

(use-package dockerfile-ts-mode
  :ensure nil
  ;; Same files the dockerfile-mode package matched.
  :mode ("[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'"
         "\\.dockerfile\\'")
  )

;; Emmet

(use-package emmet-mode
  :bind (
         :map emmet-mode-keymap
              ("C-j" . nil)
         )
  :hook (
         (sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (css-ts-mode . emmet-mode)
         (web-mode . emmet-mode)
         )
  :config
  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  )

;; Fish

(use-package fish-mode
  :defer t)

;; Graphviz

(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

;; Groovy

(use-package groovy-mode
  :defer t)

;; Javascript

;; REMOVED: randomly broke, e.g. code wasn't getting higlighted.
;; (use-package js2-mode
;;   :mode "\\.js\\'"
;;   :bind (
;;          :map js2-mode-map
;;          ("M-," . smart-jump-back)
;;          ("M-." . smart-jump-go)
;;          )
;;   :config
;;   (setq js2-basic-offset 2)
;;   (setq js2-strict-missing-semi-warning nil)
;;   )

;; Formats prettier-compatible source code on save. Automatically finds and uses
;; prettier config.
(use-package prettier
  :hook (
         (json-ts-mode . prettier-mode)
         (typescript-ts-mode . prettier-mode)
         (css-ts-mode . prettier-mode)
         (js-ts-mode . prettier-mode)
         (web-mode . prettier-mode)
         )

  :config
  ;; Should Prettier format the buffer when saving?
  (setq prettier-prettify-on-save-flag t)

  ;; Turn on the minor mode in all major modes supported by your version of
  ;; Prettier.
  ;; (global-prettier-mode)
  )

;; React
(use-package rjsx-mode
  :defer t
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

;; Stylus / .styl
(use-package sws-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . sws-mode))
  )

;; Typescript
(use-package typescript-ts-mode
  :ensure nil
  ;; .ts only — .tsx must go to tsx-ts-mode (JSX grammar), see treesit block.
  :mode "\\.ts\\'"
  :custom
  (typescript-ts-mode-indent-offset 2)
  )
(use-package tide
  :ensure t
  :after (
          flycheck
          ;; typescript-mode
          typescript-ts-mode
          )
  :hook (
         (typescript-mode . tide-setup)
         (typescript-ts-mode . tide-setup)
         ;; REMOVED: Use prettier instead.
         ;; (before-save . tide-format-before-save)
         )
  :config
  (setopt tide-jump-to-definition-reuse-window nil)
  )

;; JSON

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :custom
  (json-ts-mode-indent-offset 4)
  )

;; just

(use-package just-mode
  :defer t
  )

;; Markdown

(use-package markdown-mode
  :mode "\\.md\\'"
  :bind (
         :map markdown-mode-map
         ("M-p" . markdown-previous-visible-heading)
         ("M-n" . markdown-next-visible-heading)
         ("C-c C-c" . markdown-do)
         )
  :config
  ;; Set export command.
  (setq markdown-command "pandoc -f gfm -t html5")
  (setq markdown-xhtml-header-content
        "<style>
body { max-width: 60rem; margin: 2rem auto; padding: 0 1rem;
       font-family: -apple-system, system-ui, sans-serif;
       line-height: 1.6; color: #222; }
h1, h2, h3 { line-height: 1.2; }
table { border-collapse: collapse; width: 100%; margin: 1rem 0; }
th, td { border: 1px solid #ccc; padding: .5rem .75rem;
         text-align: left; vertical-align: top; }
th { background: #f2f2f2; }
tr:nth-child(even) td { background: #fafafa; }
code { background: #f2f2f2; padding: .1rem .3rem; border-radius: 3px; }
pre { background: #1a1a1a; color: #bdbdb3; padding: 1rem;
      overflow-x: auto; border-radius: 6px; }
pre code { background: none; padding: 0; color: inherit; }
pre a { color: inherit; text-decoration: none; }
blockquote { border-left: 3px solid #ccc; margin: 1rem 0;
             padding-left: 1rem; color: #555; }
/* Nimbus syntax theme (https://github.com/mrcnski/nimbus-pygments),
   mapped from Pygments to pandoc/skylighting token classes. */
code span.al { color: #d65946; font-weight: bold; } /* Alert */
code span.an { color: #baba36; } /* Annotation */
code span.at { color: #baba36; } /* Attribute */
code span.bn { color: #df9522; } /* BaseN */
code span.bu { color: #598bc1; } /* BuiltIn */
code span.cf { color: #598bc1; } /* ControlFlow */
code span.ch { color: #6aaf50; } /* Char */
code span.cn { color: #ab75c3; } /* Constant */
code span.co { color: #757575; } /* Comment */
code span.cv { color: #baba36; } /* CommentVar */
code span.do { color: #757575; font-style: italic; } /* Documentation */
code span.dt { color: #d65946; } /* DataType */
code span.dv { color: #df9522; } /* DecVal */
code span.er { color: #9d2512; } /* Error */
code span.fl { color: #df9522; } /* Float */
code span.fu { color: #1d9a79; } /* Function */
code span.im { color: #baba36; } /* Import */
code span.in { color: #baba36; } /* Information */
code span.kw { color: #598bc1; } /* Keyword */
code span.op { color: #bdbdb3; } /* Operator */
code span.ot { color: #ab75c3; } /* Other */
code span.pp { color: #baba36; } /* Preprocessor */
code span.sc { color: #ab75c3; } /* SpecialChar */
code span.ss { color: #d65946; } /* SpecialString (regex) */
code span.st { color: #6aaf50; } /* String */
code span.va { color: #598bc1; } /* Variable */
code span.vs { color: #6aaf50; } /* VerbatimString */
code span.wa { color: #baba36; font-style: italic; } /* Warning */
</style>")

  ;; This mode overrides the return key! Stop!
  (define-key markdown-mode-map (kbd "<return>") nil)
  (define-key markdown-mode-map (kbd "RET") nil)
  )

(use-package markdown-toc
  :after markdown-mode
  :defer t)

;; Nim

(use-package nim-mode
  :defer t
  :bind (:map nim-mode-map ("RET" . newline-and-indent))
  )

;; Python

(use-package python
  :ensure nil
  :bind (
         :map python-ts-mode-map
         ("C-<" . python-indent-shift-left)
         ("C->" . python-indent-shift-right)
         )
  ;; TODO: pylint/flake8 not currently installed; fix paths when installing
  ;; them (Apple Silicon brew uses /opt/homebrew/bin, not /usr/local/bin).
  :hook (python-base-mode . (lambda ()
                              (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
                              (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")))
  )

;; Rust

;; (use-package rust-mode
;;   :bind (:map rust-mode-map ("C-c n" . rust-format-buffer))
;;   :config
;;   (setq
;;    rust-format-on-save nil
;;    rust-rustfmt-switches '("+nightly")
;;    )
;;   )

;; Enhanced Rust mode with automatic LSP support.
(use-package rustic
  :bind (
         :map rustic-mode-map
         ("C-c n" . rustic-format-file)
         ("C-c ," . rustic-cargo-check)
         ("C-c ." . rustic-cargo-test)
         )

  :config

  (setq
   ;; Use an LSP client?
   rustic-lsp-client nil
   rustic-format-on-save nil
   rustic-rustfmt-bin "cargo"
   rustic-rustfmt-args "+nightly fmt --all"
   )

  ;; Compilation settings.
  (setq rustic-compile-command "cargo lcheck --tests")
  (add-to-list 'display-buffer-alist
               '("\\*rustic-compilation\\*"
                 (display-buffer-reuse-window
                  display-buffer-same-window)))
  (advice-add 'rustic-cargo-check :before
            (lambda (&rest _) (save-all)))
  (advice-add 'rustic-recompile :before
            (lambda (&rest _) (save-all)))
  (add-hook 'rustic-mode-hook
            (lambda () (setq-local compilation-read-command nil)))

  (setq rustic-cargo-test-exec-command "ltest")
  (add-to-list 'display-buffer-alist
               '("\\*cargo-test\\*"
                 (display-buffer-reuse-window
                  display-buffer-same-window)))
  (advice-add 'rustic-cargo-test-rerun :before
            (lambda (&rest _) (save-all)))
  (advice-add 'rustic-cargo-test :before
            (lambda (&rest _) (save-all)))
  )

;; Manually set which rust mode I want to use.
(setq auto-mode-alist
      (cl-remove "\\.rs\\'" auto-mode-alist :test 'equal :key 'car))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

;; Web

(use-package web-mode
  ;; .js and .css deliberately not claimed — they go to js-ts-mode /
  ;; css-ts-mode via `major-mode-remap-alist' (see treesit block).
  :mode (
         ("\\.html?\\'" . web-mode)
         )

  :config

  (define-key web-mode-map (kbd "M-;") nil)

  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2

   web-mode-enable-current-element-highlight t
   )
  )

;; YAML

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  )

(provide 'init-packages-languages)
;;; init-packages-languages.el ends here
