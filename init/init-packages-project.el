;;; init-packages-project --- Load project packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)

;; Company mode for auto-completion.
(use-package company
  :bind (
         ("M-/" . company-complete)
         ("C-M-/" . company-dabbrev-code)

         :map company-active-map

         ("C-h" . nil)
         ("C-s" . company-isearch-forward)
         ("C-r" . company-isearch-backward)

         ;; ;; Prevent SPC from ever triggering a completion.
         ;; ("SPC" . nil)

         ;; Make TAB always complete the current selection.
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Do not complete with RET; should start a new line.
         ("<return>" . nil)
         ("RET" . nil)
         )
  :hook (prog-mode . company-mode)
  :init
  (setq
   ;; Completion delay (nil means no idle completion).
   company-idle-delay nil
   company-minimum-prefix-length 1
   ;; Align tooltips to right border.
   company-tooltip-align-annotations t
   ;; Number the candidates? (Use C-M-1, C-M-2 etc to select completions.)
   company-show-quick-access t

   ;; Fix casing...
   company-dabbrev-downcase nil
   company-dabbrev-code-ignore-case t

   ;; Allow typing normally.
   company-require-match nil
   )

  :config
  (defun company-isearch-backward ()
    "Abort company and search backward."
    (interactive)
    (company-abort)
    (isearch-backward)
    )
  (defun company-isearch-forward ()
    "Abort company and search forward."
    (interactive)
    (company-abort)
    (isearch-forward)
    )

  ;; Rebind the M-digit keys to prevent conflict with winum.
  (dotimes (i 10)
    (define-key company-active-map (kbd (format "M-%d" i)) nil)
    (define-key company-active-map (read-kbd-macro (format "s-%d" i)) 'company-complete-tooltip-row))

  ;; Add commands that should abort completion.
  (add-to-list 'company-continue-commands 'rust-format-buffer t)
  (add-to-list 'company-continue-commands 'indent-buffer t)
  )

;; Show markers in margin indicating changes.
(use-package diff-hl
  :bind (
         ("C-?" . diff-hl-revert-hunk)
         ("M-[" . diff-hl-previous-hunk)
         ("M-]" . diff-hl-next-hunk)
         )
  :hook (
         (prog-mode . enable-diff-hl)
         (text-mode . enable-diff-hl)
         (conf-mode . enable-diff-hl)
         )

  :init

  (defun enable-diff-hl ()
    ;; Make the fringe wide enough to display correctly.
    ;; (setq-local left-fringe-width 16)
    (turn-on-diff-hl-mode))

  :config

  ;; Show diffs in margin, the fringe display was broken.
  (diff-hl-margin-mode)

  ;; Show diffs while buffer is being edited.
  (diff-hl-flydiff-mode)

  ;; Refresh diffs after a Magit commit.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; See diffs in Dired.
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  )

;; EditorConfig helps maintain consistent coding styles for multiple developers
;; working on the same project across various editors and IDEs.
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; TODO: Built-in in 29.1
(use-package eglot
  :defer t
  :config
  (setq
   eglot-ignored-server-capabilities '(
                                        ; Mouse clicks bringing up code actions.
                                       :codeActionProvider
                                        ; Automatic formatting.
                                       :documentFormattingProvider
                                        ; Docs on hover.
                                       :hoverProvider
                                        ; Code signature docs.
                                       :signatureHelpProvider
                                       )
   ;; Prevent automatic syntax checking, which was causing lags and stutters.
   eglot-send-changes-idle-time (* 60 60)
   ;; Disable annoying rust-analyzer progress reports in echo area.
   eglot-report-progress nil
   )

  ;; Disable error diagnostics.
  (with-eval-after-load "eglot"
    (add-to-list 'eglot-stay-out-of 'flymake))

  ;; Disable the annoying doc popups in the minibuffer.
  ;; Show messages in the echo area for errors only.
  ;; From https://github.com/joaotavora/eglot/discussions/898#discussioncomment-2609402
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
              ;; (eldoc-mode -1)

              ;; Disable inlay hints by default.
              (eglot-inlay-hints-mode -1)
              ))
  )

;; On-the-fly syntax checker.
(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (text-mode . flycheck-mode)
         (conf-mode . flycheck-mode)
         )
  :commands flycheck-mode
  :bind (
         ("C-!" . flycheck-list-errors)
         ("C-s-[" . flycheck-previous-error)
         ("C-s-]" . flycheck-next-error)
         )
  :config
  (setq
   flycheck-check-syntax-automatically '(mode-enabled save)
   ;; flycheck-check-syntax-automatically nil

   ;; Set shorter delay for displaying errors at point.
   flycheck-display-errors-delay (* 1 info-delay)
   sentence-end-double-space nil ;; Unnecessary check.
   flycheck-checker-error-threshold 600
   flycheck-emacs-lisp-load-path 'inherit
   )

  ;; Disable checkers.
  (setq-default flycheck-disabled-checkers '(proselint rust rust-cargo rust-clippy))
  ;; (setq-default flycheck-disabled-checkers '(rust)) ;; Doesn't work.

  ;; (flycheck-add-next-checker 'rust-cargo 'rust-clippy)
  )

;; Elisp package lints.
(use-package flycheck-package
  :hook (flycheck-mode . flycheck-package-setup))

;; Project manager.
;; TODO: remove?
(use-package projectile
  :defer t
  :hook (prog-mode . projectile-mode)
  ;; NOTE: project-eshell breaks when there are multiple similarly-named
  ;; projects open...
  :bind (
         ("s-E" . projectile-run-eshell)
         ("s-D" . projectile-dired)
         )
  :config
  (setq projectile-completion-system 'auto)
  ;; Remove from menu bar.
  (define-key projectile-mode-map [menu-bar] nil)

  ;; Integrate projectile with consult.
  (use-package consult-projectile
    :bind ("s-;" . consult-projectile)
    )
  )

;; Jump to definitions using dumb-jump as a fallback.
(use-package smart-jump
  :config
  (smart-jump-setup-default-registers)

  (advice-add 'smart-jump-go :before
              #'(lambda (&rest _) (call-interactively 'save-all)))
  )

;; NOTE: Requires ripgrep with pcre2 support.
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq
   dumb-jump-debug nil
   dumb-jump-force-searcher 'rg
   ;; dumb-jump-prefer-searcher 'rg
   ;; Ignore `target`, `node_modules` directories.
   ;;
   ;; I think dumb-jump's regexes require pcre2.
   dumb-jump-rg-search-args "--pcre2 --glob '!{target,node_modules}'"
   )
  )

(provide 'init-packages-project)
;;; init-packages-project.el ends here
