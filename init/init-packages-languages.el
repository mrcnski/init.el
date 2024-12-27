;;; init-packages-languages --- Load language packages.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-packages-project)

;; Treesitter

;; ;; Better syntax highlighting.
;; (use-package tree-sitter
;;   :demand t

;;   :config

;;   (use-package tree-sitter-langs
;;     :demand t
;;     )

;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;   (global-tree-sitter-mode)
;;   )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  )

(setq treesit-language-source-alist
      '(
        (astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        ))
;; (mapc #'treesit-install-language-grammar '(astro css typescript tsx))

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
  :bind ("C-M-b" .
         (lambda ()
           (interactive)
           (find-file "~/Dropbox/beancount/main.bean")))
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

;; C#

(use-package csharp-mode
  :defer t)

;; Docker

(use-package dockerfile-mode
  :defer t)

;; Emmet

(use-package emmet-mode
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

;; REMOVED: Complained it couldn't find prettier. Also was trying to run on
;; Emacs Lisp etc.
;; ;; Formats prettier-compatible source code on save. Automatically finds and uses
;; ;; prettier config.
;; (use-package prettier
;;   :bind ("C-c n" . prettier-prettify)
;;   :config
;;   ;; Set this to nil if you don't want Prettier to prettify (format) the buffer
;;   ;; when saving.
;;   (setq prettier-prettify-on-save-flag nil)
;;   ;; Turn on the minor mode in all major modes supported by your version of
;;   ;; Prettier.
;;   (global-prettier-mode)
;;   )

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
  (typescript-indent-level 4)
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
  (setq json-reformat:indent-width 2)
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

(use-package rust-mode
  :bind (:map rust-mode-map ("C-c n" . rust-format-buffer))
  :config
  (setq
   rust-format-on-save nil
   rust-rustfmt-switches '("+nightly")
   )
  )

;; Enhanced Rust mode with automatic LSP support.
(use-package rustic
  :bind (:map rustic-mode-map ("C-c n" . rustic-format-file))

  ;; :init

  ;; (defun format-rust ()
  ;;   (interactive)
  ;;   ;; Save all buffers since `rustic-cargo-fmt' formats all buffers belonging
  ;;   ;; to the workspace.
  ;;   (save-all)
  ;;   (rustic-cargo-fmt)
  ;;   )

  :config

  (setq
   ;; eglot seems to be the best option right now.
   rustic-lsp-client 'eglot
   rustic-format-on-save nil
   rustic-rustfmt-args "+nightly"
   )
  )

;; Manually set which rust mode I want to use.
(setq auto-mode-alist
      (cl-remove "\\.rs\\'" auto-mode-alist :test 'equal :key 'car))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; TOML

(use-package toml-mode
  :mode "\\.toml\\'"
  )

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
