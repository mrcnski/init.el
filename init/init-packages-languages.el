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
               (astro . ("https://github.com/virchau13/tree-sitter-astro" "master"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
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
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

;; Astro

(use-package astro-ts-mode
  :after (treesit)

  :config

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

;; CSS / SCSS

(use-package css-mode
  :ensure nil
  :config
  (setopt
   css-indent-offset 2
   )
  )

;; C#

(use-package csharp-mode
  :defer t)

;; Docker

(use-package dockerfile-mode
  :defer t)

;; Emmet

(use-package emmet-mode
  :bind (
         :map emmet-mode-keymap
              ("C-j" . nil)
         )
  :hook (
         (sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)
         )
  :config
  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  )

;; Fish

(use-package fish-mode
  :defer t)

;; Go / Golang

(use-package go-mode
  :defer t
  :bind (:map go-mode-map ("C-c n" . gofmt))
  :config
  (setq
   gofmt-args '("-s")
   gofmt-command "gofmt"

   ;; gofmt-args nil
   ;; gofmt-command "goimports"
   )

  (use-package godoctor)
  (use-package go-errcheck)
  )

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
         (typescript-mode . prettier-mode)
         (typescript-ts-mode . prettier-mode)
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

;; Typescript
(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . eglot-ensure)
  :custom
  (typescript-indent-level 2)
  (typescript-ts-mode-indent-offset 4)
  )
(use-package tide
  :ensure t
  :after (typescript-ts-mode company flycheck)
  :hook (
         (typescript-ts-mode . tide-setup)
         ;; REMOVED: Messes up point position.
         ;; (before-save . tide-format-before-save)
         )
  )

;; JSON

(use-package json-mode
  :defer t
  :config
  (setq
   json-reformat:indent-width 4
   json-ts-mode-indent-offset 4
   )
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  )

;; just

(use-package just-mode
  :defer t
  )

;; Lua

(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 4)
  )

;; Markdown

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
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

;; Prisma

(use-package prisma-ts-mode)

;; Python

(use-package python-mode
  :ensure nil
  :bind (
         :map python-mode-map
         ("C-<" . python-indent-shift-left)
         ("C->" . python-indent-shift-right)
         )
  )

;; TODO: Move this into use-package.
(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
            (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
            ))

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
            #'(lambda (&rest _) (call-interactively 'save-all)))
  (advice-add 'rustic-recompile :before
            #'(lambda (&rest _) (call-interactively 'save-all)))
  (add-hook 'rustic-mode-hook
            #'(lambda (&rest _) (setq-local compilation-read-command nil)))

  (setq rustic-cargo-test-exec-command "ltest")
  (add-to-list 'display-buffer-alist
               '("\\*cargo-test\\*"
                 (display-buffer-reuse-window
                  display-buffer-same-window)))
  (advice-add 'rustic-cargo-test-rerun :before
            #'(lambda (&rest _) (call-interactively 'save-all)))
  (advice-add 'rustic-cargo-test :before
            #'(lambda (&rest _) (call-interactively 'save-all)))
  )

;; Manually set which rust mode I want to use.
(setq auto-mode-alist
      (cl-remove "\\.rs\\'" auto-mode-alist :test 'equal :key 'car))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

;; TOML

(use-package toml-mode
  :mode "\\.toml\\'"
  )

;; Web

(use-package web-mode
  :mode (
         ("\\.js?\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         )

  :config

  (define-key web-mode-map (kbd "M-;") nil)

  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 4
   web-mode-code-indent-offset 2

   web-mode-enable-current-element-highlight t
   )
  )

;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (define-key yaml-mode-map (kbd "DEL") nil)
  )

(provide 'init-packages-languages)
;;; init-packages-languages.el ends here
