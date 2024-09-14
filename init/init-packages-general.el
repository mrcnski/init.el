;;; init-packages-general --- Load general packages.
;;
;;; Commentary:
;;
;;; Code:

(require 'ring)
(require 'init-basics)
(require 'init-vertico-et-al)
(require 'init-functions-and-shortcuts)
(require 'init-visual-frame)

;; Display number of matches when searching.
(use-package anzu
  :config
  (setq anzu-cons-mode-line-p t)
  (global-anzu-mode))

;; Avy mode (jump to a char/word using a decision tree).
(use-package avy
  :bind (
         ("C-," . avy-goto-end-of-line)
         ("C-." . avy-goto-char)

         ("s-C-," . avy-save-remote-line-and-yank)
         ("s-C-." . avy-save-remote-symbol-and-yank)
         )

  :config
  (setq
   ;; Use more characters (and better ones) in the decision tree.
   ;; QWERTY keys.
   avy-keys '(
              ?a ?s ?d ?f ?j ?k ?l ?\;
                 ?w ?e ?r       ?o
              )
   ;; Set the background to gray to highlight the decision tree?
   avy-background nil
   ;; Jump automatically when there's one candidate left?
   avy-single-candidate-jump nil
   )

  (defun avy-action-kill-whole-line (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (kill-ring-save-lines)
      )
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-mark-and-excursion
      (end-of-line)
      (yank)
      )
    t)
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-describe (pt)
    (let ((symbol))
      (save-mark-and-excursion
        (goto-char pt)
        (setq symbol (thing-at-point 'symbol t))
        )
      (select-window
       (cdr (ring-ref avy-ring 0)))
      (describe-symbol (intern symbol))
      )
    t)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-mark-and-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-action-flycheck (pt)
    (save-mark-and-excursion
      (goto-char pt)
      (flycheck-display-error-at-point)
      )
    t)

  (defun avy-action-ripgrep-apply (fun &rest args)
    (let ((symbol))
      (save-mark-and-excursion
        (goto-char pt)
        (setq symbol (thing-at-point 'symbol t))
        )
      (select-window
       (cdr (ring-ref avy-ring 0)))
      (funcall fun args symbol)
      )
    t)
  (defun avy-action-ripgrep-exact-project (pt)
    (avy-action-ripgrep-apply #'consult-ripgrep-exact-save nil))
  (defun avy-action-ripgrep-exact-current (pt)
    (avy-action-ripgrep-apply #'consult-ripgrep-exact-save 4))
  (defun avy-action-ripgrep-inexact-project (pt)
    (avy-action-ripgrep-apply #'consult-ripgrep-inexact-save nil))
  (defun avy-action-ripgrep-inexact-current (pt)
    (avy-action-ripgrep-apply #'consult-ripgrep-inexact-save 4))

  ;; TODO: flycheck-display-error-at-point

  (setf
   (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
   (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
   (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
   (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
   (alist-get ?h avy-dispatch-alist) 'avy-action-describe
   (alist-get ?z avy-dispatch-alist) 'avy-action-embark
   (alist-get ?! avy-dispatch-alist) 'avy-action-flycheck

   (alist-get ?u avy-dispatch-alist) 'avy-action-ripgrep-exact-project
   (alist-get ?U avy-dispatch-alist) 'avy-action-ripgrep-exact-current
   (alist-get ?i avy-dispatch-alist) 'avy-action-ripgrep-inexact-project
   (alist-get ?I avy-dispatch-alist) 'avy-action-ripgrep-inexact-current
   )
  )

;; Move buffers around.
(use-package buffer-move
  :bind (
         ("<s-up>"    . buf-move-up)
         ("<s-down>"  . buf-move-down)
         ("<s-left>"  . buf-move-left)
         ("<s-right>" . buf-move-right)
         )
  )

;; First-time setup:
;; M-x copilot-install-server
;; M-x copilot-login
;;
;; Check status:
;; M-x copilot-diagnose
;;
;; See also https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/.
(use-package copilot
  :load-path "~/.emacs.d/packages/copilot.el"
  :hook ((prog-mode) . copilot-mode)
  :bind (
         :map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("M-f" . copilot-accept-completion-by-word)
              ("C-g" . copilot-clear-overlay)
              )
  :custom
  (copilot-indent-offset-warning-disable t)
  )

;; Copy selected region to be pasted into Slack/Github/etc.
(use-package copy-as-format
  :config
  (global-set-key (kbd "s-w")
                  (lambda () (interactive)
                    (select-lines)
                    (copy-as-format-github)
                    (kill-append "\n" t)
                    ))
  )

;; Display available keybindings in Dired mode (? creates popup).
(use-package discover
  :defer 2)

;; Better terminal emulation.
;; NOTE: Don't put this in eshell config or it doesn't work.
(use-package eat
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  ;; Allow all Emacs keybindings.
  (add-hook 'eat-eshell-exec-hook #'eat-eshell-emacs-mode)
  )

;; Show example usage when examining elisp functions in help.
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;; Better comment command.
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Expand-region.
(use-package expand-region
  :bind ("C-;" . er/expand-region)
  :config

  ;; Fix region not highlighting.
  (setq
   shift-select-mode nil
   expand-region-fast-keys-enabled nil
   )
  )

;; Workspaces.
(use-package eyebrowse
  ;; TODO: Needed?
  ;; To prevent mode-line display errors.
  ;; :demand t
  :load-path "~/.emacs.d/packages/eyebrowse"
  :bind (
         ("s-," . eyebrowse-prev-window-config)
         ("s-." . eyebrowse-next-window-config)
         ("s-0" . eyebrowse-switch-to-window-config-0)
         ("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5)
         ("s-6" . eyebrowse-switch-to-window-config-6)
         ("s-7" . eyebrowse-switch-to-window-config-7)
         ("s-8" . eyebrowse-switch-to-window-config-8)
         ("s-9" . eyebrowse-switch-to-window-config-9)
         ("s-/" . eyebrowse-close-window-config)
         ("s--" . eyebrowse-rename-window-config)
         )

  :init

  ;; Free up keybindings unnecessarily stolen by eyebrowse.
  (setq eyebrowse-keymap-prefix (kbd ""))

  :config

  (eyebrowse-mode t)

  (setq
   eyebrowse-wrap-around t
   eyebrowse-switch-back-and-forth nil
   ;; Start out with as empty of a slate as possible (by just displaying a
   ;; single window with the scratch buffer in it)
   eyebrowse-new-workspace t
   eyebrowse-close-window-config-prompt t

   eyebrowse-mode-line-separator " "
   eyebrowse-mode-line-left-delimiter ""
   eyebrowse-mode-line-right-delimiter ""
   eyebrowse-mode-line-current-left-delimiter "["
   eyebrowse-mode-line-current-right-delimiter "]"
   )

  (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :bold t)

  ;;; Show workspaces in title bar.

  ;; Only recalculate the workspaces string when it actually changes.
  (defun frame-title-eyebrowse-update ()
    "Updates eyebrowse indicator in the frame title."
    (let ((indicator (substring-no-properties (eyebrowse-mode-line-indicator))))
      (setq frame-title-eyebrowse
            (when (not (string-empty-p indicator))
              (format "%s%s" frame-title-separator indicator)))))
  (frame-title-eyebrowse-update)

  (add-hook 'eyebrowse-indicator-change-hook 'frame-title-eyebrowse-update)
  )

;; Fix the capitalization commands.
(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)
         )
  )

;; Fontify symbols representing faces with that face.
(use-package fontify-face
  :defer t
  :hook (emacs-lisp-mode . fontify-face-mode)
  )

;; Show unused keys.
(use-package free-keys
  :defer t)

;; Alternative to volatile-highlights.
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Set to nil to disable pulsing.
  (setq-default goggles-pulse nil)
  )

;; Highlight indentation.
;; TODO: Use https://github.com/jdtsmith/indent-bars#installconfig
;; REMOVED: Causing weird issues, and I never really used it.
;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq
;;    highlight-indent-guides-method 'column
;;    ;; Automatically calculate faces?
;;    highlight-indent-guides-auto-enabled nil
;;    ;; Should the current indentation under point be highlighted?
;;    highlight-indent-guides-responsive 'top
;;    highlight-indent-guides-delay 0
;;    )
;;   )

;; Highlight surrounding parentheses.
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("cyan2")
        hl-paren-delay highlight-delay
        )
  )

;; Highlight more elisp syntax.
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Highlight keywords such as TODO, FIXME, NOTE, etc.
;; NOTE: Face values defined in `hl-todo-keyword-faces'.
(use-package hl-todo
  :config
  (add-to-list 'hl-todo-include-modes 'conf-mode)

  (global-hl-todo-mode)

  (add-to-list 'hl-todo-keyword-faces '("REMOVED" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("GIGO" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("WARNING" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("SAFETY" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("RACE" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("DEPRECATED" . "#cc9393"))
  )

;; Highlight symbol under point.
(use-package idle-highlight-mode
  :hook ((prog-mode . idle-highlight-mode)
         (conf-mode . idle-highlight-mode)
         (text-mode . idle-highlight-mode)
         (eshell-mode . idle-highlight-mode)
         )

  :config

  (setq
   idle-highlight-exclude-point t
   idle-highlight-idle-time highlight-delay
   )

  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     ;; Go from least-specific to most specific.
     (when (derived-mode-p 'text-mode)
       (setq-local idle-highlight-exceptions '("-")))
     (when (derived-mode-p 'outline-mode)
       (setq-local idle-highlight-exceptions '("*" "**" "***" "****" "*****")))
     (when (derived-mode-p 'org-mode)
       (setq-local idle-highlight-exceptions '("-" "*" "**" "***" "****" "*****")))
     ))
  )

;; Modify multiple occurrences simultaneously.
(use-package iedit
  :bind ("C-=" . iedit-mode)
  )

;; Frequency statistics of keys.
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(
          self-insert-command
          forward-char
          backward-char
          previous-line
          next-line
          ))
  )

(use-package keys
  :load-path "~/.emacs.d/packages/keys" ; Coming to MELPA soon I hope
  :config

  ;; Customize some settings
  (setq
   keys-keys '("s-w" "M-F" "C-M-y" "C-x 2" "C-M-," "s-D" "M-W")
   keys-display-amount 2 ; How many keys to show at once
   keys-indicator-separator " | " ; Customize the indicator!
   keys-random t ; By default, keys are shown in a random order

                ; Calling associated commands manually is an error!
   keys-force t ; So if you bind `git-link` to C-c g, you get an error when
                ; invoking `M-x git-link` with this configuration set.
   )

  ;; Update the indicator every time it should change.
  ;; You can also just do `(:eval (when global-keys-mode (keys-indicator)))`,
  ;;   but this avoids constantly re-calculating the indicator.
  ;; The same idea applies for the mode-line, header, etc.
  ;; (defvar frame-title-keys)
  ;; (defvar frame-title-separator "  —  ")
  ;; (setq frame-title-format '("Emacs" frame-title-keys))
  (add-hook
   'keys-post-change-hook
   #'(lambda ()
       (let ((indicator (keys-indicator)))
         (setq frame-title-keys
               (when (and global-keys-mode (not (string-empty-p indicator)))
                 (format "%s%s" frame-title-separator indicator))))))

  ;; Ready to turn on keys-mode!
  (global-keys-mode)

  ;; Integrate with midnight-mode.
  (require 'midnight)
  (midnight-delay-set 'midnight-delay "1:00am")
  (add-hook 'midnight-hook 'keys-reset)
  )

;; A package for choosing a color by updating text sample.
;; See https://www.emacswiki.org/emacs/MakeColor.
(use-package make-color
  :defer t)

(use-package mark-yank
  :bind ("C-M-y" . mark-yank)
  :config (mark-yank-mode 1))

;; Multiple cursors.
(use-package multiple-cursors
  :bind (
         ("C-{" . mc/mark-previous-like-this)
         ("C-}" . mc/mark-next-like-this)

         ;; Add cursors with the mouse!
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         )

  :config
  (setq mc/always-run-for-all t)

  ;; Reclaim some keys
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-'") nil)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "C-'") nil)
  (define-key mc/mark-more-like-this-extended-keymap (kbd "<return>") nil)
  )


;; Deal more comfortably with long text.
(use-package olivetti)

;; Highlight color strings with the corresponding color.
(use-package rainbow-mode
  :defer t
  )

;; Open current directory.
(use-package reveal-in-folder
  :bind ("C-c f" . reveal-in-folder)
  )

;; Automatically save place in each file.
(use-package saveplace
  :config
  (save-place-mode t)
  )

;; This package provides functions to accentuate and reset the active window.
;; REMOVED: Breaks fringes.
;; See https://github.com/captainflasmr/selected-window-accent-mode/issues/3.
;; (use-package selected-window-accent-mode
;;   :config
;;   (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-mode-style 'default)
;;   )

;; Commands for converting between programmatic cases.
(use-package string-inflection
  :defer t
  )

;; For inserting, changing, and, deleting surrounding pairs of quotes, braces,
;; etc.
(use-package surround
  :ensure t
  :bind-keymap ("C-'" . surround-keymap)
  )

;; Open current directory in an external terminal emulator.
(use-package terminal-here
  :bind ("C-c t" . terminal-here-launch)
  )

;; Use a sensible mechanism for making buffer names unique.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 2
        uniquify-strip-common-suffix nil
        )
  )

;; Display available keys.
(use-package which-key
  :config
  (which-key-mode)
  (setq
   which-key-sort-order 'which-key-key-order-alpha
   which-key-sort-uppercase-first nil
   )
  )

(use-package whitespace
  ;; :hook (prog-mode . whitespace-mode)

  :init

  (setq whitespace-style
        '(face
          lines-tail
          )
        )

  ;; Highlight the parts of lines that exceed certain column numbers, depending
  ;; on mode.
  (defun c-whitespace-mode ()
    "Set whitespace column and fill column for c-like modes and turn
on `whitespace-mode'."
    (setq whitespace-line-column 80
          fill-column 80)
    (whitespace-mode)
    )
  (add-hook 'c-mode-common-hook 'c-whitespace-mode)
  (add-hook 'nim-mode-hook 'c-whitespace-mode)

  (defun rust-whitespace-mode ()
    "Set whitespace column and fill column for Rust and turn on `whitespace-mode'."
    (setq whitespace-line-column 120
          fill-column 100)
    (whitespace-mode)
    )
  (add-hook 'rust-mode-hook 'rust-whitespace-mode)
  (add-hook 'rustic-mode-hook 'rust-whitespace-mode)

  (defun 100-whitespace-mode ()
    "Set whitespace column and fill column at 100 and turn on `whitespace-mode'."
    (setq whitespace-line-column 100
          fill-column 100)
    (whitespace-mode)
    )
  (add-hook 'python-mode-hook '100-whitespace-mode)
  )

;; Switch windows more easily.
(use-package winum
  :init
  ;; Prevent winum from inserting its own number in the mode-line
  ;; (spaceline already does so).
  ;; (setq winum-auto-setup-mode-line nil)

  ;; This has to be in :init for some reason.
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          (define-key map (kbd "M-9") 'winum-select-window-9)
          (define-key map (kbd "M-0") 'winum-select-window-0)
          map))

  :config

  (setq winum-scope 'frame-local)
  (winum-mode)
  )

;; Automatically clean up extraneous whitespace.
(use-package ws-butler
  :hook (
         (prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)
         (conf-mode . ws-butler-mode)
         )
  )

(use-package yasnippet
  :bind ("C-c y" . yas-insert-snippet)

  :config

  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (setq
   yas-snippet-dirs (list (concat user-emacs-directory "snippets/"))
   )
  (yas-global-mode 1)
  )

(provide 'init-packages-general)
;;; init-packages-general.el ends here
