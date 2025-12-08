;;; init-builtin-settings --- Builtin settings.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)

;; Enable commands that are disabled by default.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Enable show-trailing-whitespace.
(defun enable-trailing-whitespace ()
  "Turn on trailing whitespace."
  (setq show-trailing-whitespace t)
  )
(add-hook 'prog-mode-hook 'enable-trailing-whitespace)
(add-hook 'conf-mode-hook 'enable-trailing-whitespace)
(add-hook 'text-mode-hook 'enable-trailing-whitespace)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 ;; HTML tab width / indent level.
 sgml-basic-offset 2
 js-indent-level 4
 fill-column 80
 ;; Highlight end of buffer?
 indicate-empty-lines t
 )

(defvar apropos-do-all)
(defvar ediff-window-setup-function)
(defvar c-default-style)
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 ;; Enable complete documentation for apropos functions?
 apropos-do-all t
 kill-ring-max 1000
 ;; Add newline at end of buffer with C-n.
 next-line-add-newlines t
 ;; Flash the frame on every error?
 visible-bell nil
 ring-bell-function 'ignore
 ;; Set up ediff windows in the same frame.
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; Display keystrokes immediately.
 echo-keystrokes 0.01
 ;; Disable startup screen.
 inhibit-startup-message t
 ;; Change the initial *scratch* buffer.
 initial-scratch-message ""
 ;; Always confirm before closing Emacs?
 confirm-kill-emacs nil
 ;; Delay for displaying function/variable information.
 eldoc-idle-delay info-delay
 ;; Delay for hiding tooltips in seconds.
 tooltip-hide-delay (* 60 60)
 ;; Delay for showing tooltips, in seconds.
 tooltip-delay 0
 ;; Follow symlinks without asking?
 vc-follow-symlinks t
 ;; Undo limit.
 undo-limit (* 10 1000 1000)
 ;; Replace yes/no prompts with y/n.
 use-short-answers t

 ;; Tab will always just try to indent.
 tab-always-indent 't
 ;; Resize the minibuffer when needed.
 resize-mini-windows t
 ;; Enable recursive editing of minibuffer?
 enable-recursive-minibuffers t
 minibuffer-depth-indicate-mode t

 ;;; Buffer settings

 ;; Tries to preserve last open window point when multiple buffers are open for
 ;; the same file.
 switch-to-buffer-preserve-window-point t

 ;; Where should we open new buffers by default?
 display-buffer-base-action '(display-buffer-same-window)
 ;; Specify custom behavior for misbehaving buffers.
 display-buffer-alist
 '(("\\*Help\\*"
    (display-buffer-same-window
     display-buffer-below-selected))
   ("\\*Ibuffer\\*"
    (display-buffer-same-window))
   )

 ;;; File settings

 ;; Non-nil means suppress warning messages for symlinked files.
 find-file-suppress-same-file-warnings t
 ;; Send deleted files to trash.
 delete-by-moving-to-trash t
 ;; Open files in existing frames.
 pop-up-frames nil
 pop-up-windows t
 ;; Ensure that files end with a newline.
 require-final-newline t

 ;;; Window settings

 ;; Set a larger minimum window width. Smaller than this is hard to read.
 window-min-width 30
 window-min-height 10
 ;; Focus new help windows when opened.
 help-window-select t
 window-combination-resize nil

 ;;; Other

 c-default-style "stroustrup"
 )

;; Scroll settings
(setq
 ;; Move point to beginning or end of buffer when scrolling?
 scroll-error-top-bottom t
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 )
;; How far to scroll windows upward.
(setq-default scroll-up-aggressively nil)

;; Clean up the Mac Menu Bar.
(define-key global-map [menu-bar buffer] nil)
(define-key global-map [menu-bar edit] nil)
(define-key global-map [menu-bar file] nil)
(define-key global-map [menu-bar help-menu] nil)
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar tools] nil)
(define-key minibuffer-mode-map [menu-bar] nil)

;; Set c-style comments to be "//".
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// "
                  comment-end "")))

;; Turn on utf-8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Setup selected file endings to open in certain modes.
(add-to-list 'auto-mode-alist '("\\.prdoc\\'" . yaml-mode))

;;; Backup settings.

;; Note that in dired, backup files are omitted.
(setq
 ;; Where to put backups? (~/.local/emacs/backups)
 backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-config-directory)))
 ;; Inhibit backups?
 backup-inhibited nil
 ;; Make backup files when creating a file?
 make-backup-files t
 ;; Safest but slowest backup method.
 backup-by-copying t
 ;; Silently delete old backup versions.
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 ;; Make numeric backup versions unconditionally.
 version-control t
 ;; Backup files that are in version control?
 vc-make-backup-files t

 ;; Auto save?
 auto-save-default nil

 ;; Create lockfiles (prepended with .#) to avoid editing collisions?
 create-lockfiles nil
 )

;;; Set some built-in modes.

;; Use compressed files like normal files.
(auto-compression-mode t)

;; Display the column number? (Supposed to make editing slower.)
(column-number-mode nil)

;; Replace selected text when typing or pasting.
(delete-selection-mode t)

;; Display line numbers (better than linum).
(defvar display-line-numbers-grow-only)
;; Don't shrink the line numbers.
(setq display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
;; NOTE: Don't add `text-mode-hook', to prevent line numbers in org-mode.
;; Add `yaml-mode-hook' manually (it derives from `text-mode-hook').
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)

;; Auto refresh dired?
;;
;; NOTE: Disabled for now, since dired view could change while
;; viewing/interacting with it. If re-enabling, set `auto-revert-verbose' to
;; `nil'.
(setq global-auto-revert-non-file-buffers nil)
;; Show auto-revert messages?
(setq auto-revert-verbose t)
;; Auto revert files that changed on disk.
(global-auto-revert-mode t)

;; Disable eldoc mode, causes huge slowdown in Rust files and more annoying than
;; useful.
(global-eldoc-mode -1)

;; Highlights URL links and make them clickable.
(global-goto-address-mode t)

;; Keep line highlight across windows?
(setq global-hl-line-sticky-flag nil)
;; Highlight current line.
(global-hl-line-mode)

;; Turn on subword-mode everywhere.
(global-subword-mode t)

;;; Mouse settings

(setq
 ;; Make the mouse wheel not accelerate.
 mouse-wheel-progressive-speed nil
 mouse-yank-at-point t
 )

(provide 'init-builtin-settings)
;;; init-builtin-settings.el ends here
