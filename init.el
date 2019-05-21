;;; init.el --- Emacs configuration file. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019 Marcin Swieczkowski
;;
;;; Commentary:
;;
;; Requires Emacs 26 or higher.
;;
;; Making changes / testing:
;;
;; - Use M-x free-keys to find unused keybindings.
;; - Use M-x bug-hunter-init-file to locate errors.
;; - Use M-x esup to profile startup time,
;;   M-x profiler-start and profiler-report to profile runtime.
;; - Use restart-emacs to restart after making changes.

;;; Code:

;; Show more error info.
;; (setq debug-on-error t)

;; First things first, increase GC threshold to speed up startup.
;; Reset the GC threshold after initialization, and GC whenever we tab out.
(setq gc-cons-threshold (* 64 1000 1000))
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold (* 32 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;;; User-Defined Variables

(defvar user-text-location "~/Dropbox/Text/")
(defvar user-scratchpad-location (concat user-text-location "scratchpad.txt"))

(defvar user-org-directory (concat user-text-location "org/"))
(defvar user-physical-location (concat user-org-directory "physical.org"))
(defvar user-dreams-location (concat user-org-directory "dreams.org"))
(defvar user-ideas-location (concat user-org-directory "ideas.org"))
(defvar user-notes-location (concat user-org-directory "notes.org"))
(defvar user-todo-location (concat user-org-directory "todo.org"))
(defvar user-work-location (concat user-org-directory "work.org"))

(defvar highlight-delay .03)
(defvar info-delay .25)

;; Open .emacs init.
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'open-init-file)

;; Open scratchpad.txt.
(defun open-scratchpad-file ()
  "Open scratchpad file."
  (interactive)
  (find-file user-scratchpad-location))

(global-set-key (kbd "C-c s") 'open-scratchpad-file)

;;; Package settings

(require 'package)
;; Prefer the newest version of a package.
(setq load-prefer-newer t)
;; Only enable packages found in this file (not all installed packages).
(setq package-enable-at-startup nil)
;; Add package sources.
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; Run auto-load functions specified by package authors.
(package-initialize)

;; Require use-package.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Always install missing packages.
(setq use-package-always-ensure t)

;; Keep directories clean.
;; Should be one of the first things loaded.
(use-package no-littering
  :config
  (require 'recentf)

  (defvar recentf-exclude)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;; Inherit environment variables from Shell.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    (exec-path-from-shell-copy-env "HISTSIZE")
    ))

;; ;; Ensure system binaries exist and download them if not
;; (use-package use-package-ensure-system-package)

;; Enable restarting Emacs from within Emacs.
(use-package restart-emacs
  :defer t)

;; Find bugs in Emacs configuration.
(use-package bug-hunter
  :defer t)

;;; Helm

(use-package helm
  :init
  (require 'helm-config)
  :config
  (helm-mode t)

  ;; Rebind tab to run persistent action.
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; Alternate TAB key that works in terminal.
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action) ;; List actions using C-z.
  (define-key helm-map (kbd "M-x") 'helm-select-action)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h C-a") 'helm-apropos)
  (global-set-key (kbd "M-i") 'helm-semantic-or-imenu)

  (setq helm-follow-mode-persistent t)

  (defvar helm-buffers-fuzzy-matching)
  (defvar helm-recentf-fuzzy-match)
  (defvar helm-apropos-fuzzy-match)
  (defvar helm-semantic-fuzzy-match)
  (defvar helm-imenu-fuzzy-match)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)

  (defvar helm-ff-search-library-in-sexp)
  (defvar helm-ff-file-name-history-use-recentf)
  (setq
   ;; Open helm buffer inside current window?
   helm-split-window-inside-p t
   ;; Move to end or beginning of source when reaching top/bottom of source.
   helm-move-to-line-cycle-in-source t
   ;; Search for library in `use-package' and `declare-function' sexp.
   helm-ff-search-library-in-sexp t
   ;; Scroll 8 lines other window using M-<next>/M-<prior>.
   helm-scroll-amount 8
   helm-ff-file-name-history-use-recentf t
   helm-echo-input-in-header-line t
   )

  (defvar helm-buffers-column-separator)
  (setq helm-buffers-column-separator "  ")

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (defvar helm-mini-default-sources)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-files-in-current-dir
                                    ;; helm-source-buffer-not-found
                                    ))

  ;; Better mode help.
  (use-package helm-describe-modes
    :bind ("C-h m" . helm-describe-modes))

  ;; helm-swoop.
  (use-package helm-swoop
    ;; To prevent bug where `helm-swoop-from-isearch' doesn't work the first time.
    :demand t
    :bind (("C-;" . helm-swoop-without-pre-input)
           ("C-:" . helm-multi-swoop-all))
    :config
    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

    ;; When doing isearch, hand the word over to helm-swoop.
    (define-key isearch-mode-map (kbd "C-;") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all.
    (define-key helm-swoop-map (kbd "C-;") 'helm-multi-swoop-all-from-helm-swoop)

    (setq helm-swoop-speed-or-color t) ;; Show syntax highlighting in results.
    )

  ;; ag with helm.
  (use-package helm-ag
    :init
    (setq helm-ag-insert-at-point 'symbol)
    )
  )

;;; Load customizations

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file t)

;;; Quality of life changes

;; Tries to preserve last open window point when multiple buffers are open for
;; the same file.
(setq switch-to-buffer-preserve-window-point t)

;; Replace yes/no prompts with y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Track recently-opened files.
(use-package recentf
  :config
  (setq recentf-max-saved-items 10000)
  (recentf-mode t)
  )

;; Enable functions that are disabled by default.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              ;; Highlight end of buffer?
              indicate-empty-lines t
              ;; Inhibit backups?
              backup-inhibited t
              )

;; Enable show-trailing-whitespace.
(defun enable-trailing-whitespace ()
  "Turn on trailing whitespace."
  (setq show-trailing-whitespace t)
  )
(add-hook 'prog-mode-hook 'enable-trailing-whitespace)
(add-hook 'conf-mode-hook 'enable-trailing-whitespace)
(add-hook 'text-mode-hook 'enable-trailing-whitespace)

(defvar apropos-do-all)
(defvar ediff-window-setup-function)
(defvar c-default-style)
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      ;; TODO: What does this do?
      apropos-do-all t
      kill-ring-max 1000
      ;; Ensure that files end with a newline.
      require-final-newline t
      ;; Add newline at end of buffer with C-n.
      next-line-add-newlines t
      visible-bell t
      ring-bell-function 'ignore
      ;; TODO: What does this do?
      ediff-window-setup-function 'ediff-setup-windows-plain
      window-combination-resize nil
      ;; Display keystrokes immediately.
      echo-keystrokes 0.01
      ;; Disable startup screen.
      inhibit-startup-message t
      ;; Change the initial *scratch* buffer.
      initial-scratch-message ""
      ;; Focus new help windows when opened.
      help-window-select t
      ;; Always confirm before closing Emacs?
      confirm-kill-emacs nil
      ;; Send deleted files to trash.
      delete-by-moving-to-trash t
      ;; Make backup files when creating a file?
      make-backup-files nil
      ;; Silently delete old backup versions.
      delete-old-versions t
      ;; Delay for displaying function/variable information.
      eldoc-idle-delay info-delay

      ;; Open files in existing frames.
      pop-up-frames nil
      pop-up-windows t
      ;; Tab will first try to indent, then complete.
      tab-always-indent 'complete
      ;; Resize the minibuffer when needed.
      resize-mini-windows t
      ;; Enable recursive editing of minibuffer?
      enable-recursive-minibuffers nil
      ;; Move point to beginning or end of buffer when scrolling.
      scroll-error-top-bottom t
      mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))

      ;; Set a larger minimum window width. Smaller than this is hard to read.
      window-min-width 30
      window-min-height 10

      ;; Language-specific settings?
      c-default-style "stroustrup"
      )

;; Change window name to be more descriptive.
(setq frame-title-format '((:eval (when (and (buffer-modified-p) buffer-file-name) "*"))
                           "Emacs - "
                           (buffer-file-name
                            "%f" (dired-directory dired-directory "%b"))
                           ))

;; Set c-style comments to be "//" by default (these are just better, sorry).
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
(add-to-list 'auto-mode-alist '("\\.hdl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.jack\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.over\\'" . json-mode))

;; Set up gpg.

;; Fix EasyPG error.
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)

(setenv "GPG_AGENT_INFO" nil)
(setq epg-gpg-program "/usr/local/bin/gpg2")
(require 'epa-file)

(require 'password-cache)

(setq password-cache-expiry (* 15 60))
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Set some built-in modes.

;; Use compressed files like normal files.
(auto-compression-mode t)

;; Display the column number.
(column-number-mode t)

;; Replace selected text when typing or pasting.
(delete-selection-mode t)

;; Display line numbers (better than linum).
(defvar display-line-numbers-grow-only)
;; Don't shrink the line numbers.
(setq display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; Auto revert files that changed on disk.
(global-auto-revert-mode t)

;; Disable eldoc mode, causes huge slowdown in Rust files.
(global-eldoc-mode -1)

;; Highlight current line.
(defvar global-hl-line-sticky-flag)
;; Keep line highlight across windows?
(setq global-hl-line-sticky-flag t)
(global-hl-line-mode t)

;; Save minibuffer history across Emacs sessions.
(savehist-mode t)

;; Show matching parentheses.
(defvar show-paren-delay)
(setq show-paren-delay highlight-delay)
(show-paren-mode t)

;; Mouse settings

(setq
 ;; Make the mouse wheel not accelerate.
 mouse-wheel-progressive-speed nil
 mouse-yank-at-point t
 )

;;; My Functions and Shortcuts/Keybindings

;; Set up keys using super. s-a, s-s, s-x, s-c, and s-v correspond to
;; select-all, save, cut, copy, and paste, which I've left for
;; consistency/utility on Macs.
(global-set-key (kbd "s-j") 'helm-mini)
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)

(global-set-key (kbd "s-y") 'helm-show-kill-ring)
(global-set-key (kbd "s-h") 'helm-mark-ring)

(global-set-key (kbd "s-i") 'helm-projectile-ag-inexact)
(global-set-key (kbd "s-u") 'helm-projectile-ag-exact)
(global-set-key (kbd "s-o") 'helm-ag-pop-stack)

(global-set-key [f12] 'toggle-frame-fullscreen)

(defun other-window-reverse ()
  "Go to other window in reverse."
  (interactive)
  (other-window -1)
  )

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-reverse)

;; Actions to perform when saving.
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'before-save-hook 'ispell-comments-and-strings)

;; Save the buffer and revert it (reload from disk).
(defun save-revert-buffer ()
  "Save the buffer and then revert it."
  (interactive)
  (save-buffer)
  (revert-buffer))
(global-set-key [f5]  'save-revert-buffer)

(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  ;; Do not prompt for confirmation.
  (save-some-buffers t)
  )
(global-set-key (kbd "C-x s") 'save-all)

;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'focus-out-hook 'save-all)

(defun goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let ((line-numbers display-line-numbers))
    (unwind-protect
        (progn
          (setq-local display-line-numbers t)
          (call-interactively #'goto-line)
          (end-of-line))
      (setq-local display-line-numbers line-numbers))))

(global-set-key (kbd "s-l") 'goto-line-show)

(defun helm-projectile-ag-inexact ()
  "Run helm-projectile-ag case-insensitive and without word boundaries."
  (interactive)
  (save-all)
  (setq helm-ag-base-command "ag --hidden --nocolor --nogroup --ignore-case")
  (setq helm-ag-insert-at-point nil)
  (helm-projectile-ag)
  )
(defun helm-projectile-ag-exact ()
  "Run helm-projectile-ag case-sensitive and with word boundaries."
  (interactive)
  (save-all)
  (setq helm-ag-base-command
        "ag --hidden --nocolor --nogroup --word-regexp --case-sensitive")
  (setq helm-ag-insert-at-point 'symbol)
  (helm-projectile-ag)
  )

;; Commands to split window and move focus to other window.
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows)
  )
(global-set-key (kbd "C-0") 'delete-window-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Code folding.
(require 'hideshow)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(define-key hs-minor-mode-map (kbd "C-z") 'hs-toggle-hiding)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Zoom in/out.
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  )
(global-set-key (kbd "C-c n") 'indent-buffer)
;; (add-hook 'before-save-hook 'indent-buffer)

(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1)
  )
(global-set-key (kbd "C-c h") 'region-history-other)

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        )
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-c r") 'rename-current-buffer-file)

;; Select from point onwards instead of the entire line.
;; + Behaves like C-k.
;; + Can choose whether to keep indentation (run either C-a or M-m beforehand).
;; + Being able to select  from point onwards comes in handy much of the time.
(defun select-line ()
  "Select the rest of the current line."
  (interactive)
  (push-mark (line-end-position) nil t)
  ;; (kill-ring-save nil nil t) ;; Save the current region.
  )

;; Replace default C-l, it's useless.
(global-set-key (kbd "C-l") 'select-line)

;; Select entire line or lines.
;; + Will entirely select any line that's even partially within the region.
;; + Behaves like C-S-k.
(defun select-lines ()
  "Select the entire current line or region.
If called on a region, will entirely select all lines included in
the region."
  (interactive)
  (cond ((region-active-p)
         (select-lines-region (region-beginning) (region-end)))
        (t
         (select-lines-region (point) (point))
         )))
(defun select-lines-region (beg end)
  "Entirely select all lines in the region from BEG to END."
  (goto-char end)
  (end-of-line)
  (push-mark)
  (activate-mark)
  (goto-char beg)
  (beginning-of-line)
  )

(global-set-key (kbd "C-S-l") 'select-lines)

;; Improved kill-whole-line which doesn't change cursor position.
;; Can be called on multiple lines.
;; Will entirely kill any line that's even partially within the region.
(defun annihilate-lines ()
  "Annihilate the current line or region by killing it entirely.
Will delete the resulting empty line and restore cursor position.
If called on a region, will annihilate every line included in the
region."
  (interactive)
  (cond ((region-active-p)
         (annihilate-lines-region (region-beginning) (region-end)))
        (t
         (annihilate-lines-region (point) (point))
         )))
(defun annihilate-lines-region (beg end)
  "Annihilate the region from BEG to END."
  (let ((col (current-column)))
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (kill-region beg end)
    (kill-append "\n" nil)
    ;; Are there more lines after this?
    (if (/= (line-end-position) (point-max))
        (delete-char 1))
    ;; Restore column position
    (move-to-column col)
    ))

(global-set-key (kbd "C-S-k") 'annihilate-lines)

;; Drag up/down single line or lines in region.
(use-package drag-stuff
  :bind (("C-S-n" . drag-stuff-down)
         ("C-S-p" . drag-stuff-up)))

;; Join the following line onto the current line.
;; Use this to quickly consolidate multiple lines into one.
(defun join-next-line ()
  "Join the next line onto the current line, preserving the cursor position.
This command can be used to rapidly consolidate multiple lines
into one."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-j") 'join-next-line)

(defun open-line-below ()
  "Open a new line below while keeping proper indentation."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun open-line-above ()
  "Open a new line above while keeping proper indentation."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<S-return>") 'open-line-above)

(defun open-line-indent ()
  "Like the regular `open-line', but indent the next line."
  (interactive)
  (call-interactively #'open-line)
  (save-excursion
    (forward-line)
    (indent-according-to-mode)
    ))
(global-set-key (kbd "C-o") 'open-line-indent)

(defun clear-line ()
  "Clear the line, but don't delete it."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (indent-according-to-mode)
  )

(defun window-fraction-height (fraction)
  "Get specified FRACTION of the height of the current window."
  (max 1 (/ (1- (window-height (selected-window))) fraction)))

(defun scroll-up-third ()
  "Scrolls up by a third of the current window height."
  (interactive)
  (scroll-up (window-fraction-height 3)))

(defun scroll-down-third ()
  "Scrolls down by a third of the current window height."
  (interactive)
  (scroll-down (window-fraction-height 3)))

(defun scroll-other-window-up-third ()
  "Scrolls other window up by a third of the current window height."
  (interactive)
  (scroll-other-window (window-fraction-height 3)))

(defun scroll-other-window-down-third ()
  "Scrolls other window down by a third of the current window height."
  (interactive)
  (scroll-other-window-down (window-fraction-height 3)))

(global-set-key (kbd "C-v") 'scroll-up-third)
(global-set-key (kbd "M-v") 'scroll-down-third)
(global-set-key (kbd "C-S-v") 'scroll-other-window-up-third)
(global-set-key (kbd "M-V") 'scroll-other-window-down-third)

;; Globally bind these keys so they work in every mode.
(bind-keys*
 )

;; Align region by string.
;; TODO: Enable history in read-string to allow for default values
;;       (i.e. last input).
(defun align-to-string (beg end)
  "Align region from BEG to END along input string."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))
(global-set-key (kbd "M-=") 'align-to-string)

;; Show ASCII table.
;; Obtained from http://www.chrislott.org/geek/emacs/dotemacs.html.
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

;;; Visual settings

;; Set transparency.
(set-frame-parameter (selected-frame) 'alpha '(98))
;; (set-frame-parameter (selected-frame) 'alpha '(100))

;; Turn on blinking/flashing cursor.
(blink-cursor-mode t)
(setq blink-cursor-blinks 0) ;; Blink forever!
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;; Disable scroll bars and the tool bar.
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

;; Allow resizing by pixels.
(setq frame-resize-pixelwise t)

(toggle-frame-maximized) ;; Maximize!
;; (toggle-frame-fullscreen) ;; Maximize MORE

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

;; Load Themes
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; Nimbus is my personal theme, available on Melpa.
(use-package nimbus-theme
  :load-path "~/Dropbox/Code/Elisp/nimbus-theme")

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))
  (declare-function font-exists-p "init.el")

  ;; Set font.
  (cond
   ((font-exists-p "Iosevka")
    (set-face-attribute
     'default nil :font "Iosevka:weight=Regular" :height 120)
    (setq-default line-spacing 0)
    )
   ((font-exists-p "Hack")
    (set-face-attribute
     'default nil :font "Hack:weight=Regular" :height 120)
    (setq-default line-spacing 0)
    )
   )
  )

;;; Built-in mode settings

;; isearch

(setq
 isearch-allow-scroll t ;; Can scroll using C-v and M-v.
 isearch-lazy-highlight t ;; Highlight more matches after a delay.
 lazy-highlight-initial-delay info-delay
 )

;; Display last searched string in minibuffer prompt.
(setq-default isearch-mode-hook nil)
(add-hook 'isearch-mode-hook
          (lambda () (interactive)
            (setq isearch-message
                  (format "%s[%s] "
                          isearch-message
                          (if search-ring
                              (propertize (car search-ring)
                                          'face '(:inherit font-lock-string-face))
                            ""))
                  )
            (isearch-search-and-update)))

;; Dired settings

(defvar dired-mode-map)
(add-hook 'dired-mode-hook
          #'(lambda ()
              ;; Create new file.
              (define-key dired-mode-map "f" 'helm-find-files)
              ))

;; Handle opening and editing zip directories in dired.
(defvar dired-compress-file-suffixes)
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-recursive-copies 'always ;; Always do recursive copies.
              ;; Make sizes human-readable by default and put dotfiles and
              ;; capital-letters first.
              dired-listing-switches "-alhv"
              ;; Try suggesting dired targets.
              dired-dwim-target t
              ;; Update buffer when visiting.
              dired-auto-revert-buffer t
              ;; Don't confirm various actions.
              dired-no-confirm t
              )

;; Expanded dired.
;; Enables jumping to the current directory in dired (default: C-x C-j).
(require 'dired-x)
(global-set-key (kbd "s-d") 'dired-jump)

(add-hook 'dired-mode-hook (lambda ()
                             ;; Prevent certain files from showing up.
                             ;; Use C-x M-o to show omitted files.
                             (dired-omit-mode)
                             ;; Hide details by default.
                             (dired-hide-details-mode)
                             ))
(setq dired-omit-files (concat dired-omit-files "\\|\\.bk$\\|^\\.DS_Store$"))

;; Allow changing file permissions in WDired.
;; NOTE: WDired can be entered with C-x C-q and changes saved with C-c C-c.
(defvar wdired-allow-to-change-permissions)
(setq wdired-allow-to-change-permissions t)

;; ;; Extensions to Dired. Includes more dired colors.
;; (use-package dired+)

;; More dired colors.
(use-package diredfl
  :config (diredfl-global-mode))

;; ibuffer settings

(require 'ibuffer)

;; Unbind ibuffer-visit-buffer-1-window.
(define-key ibuffer-mode-map (kbd "M-o") nil)

;; Don't show filter groups if there are no buffers in that group.
(defvar ibuffer-show-empty-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers.
(defvar ibuffer-expert)
(setq ibuffer-expert t)

;; Use human-readable Size column.
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (let ((bs (buffer-size)))
    (cond ((> bs 1e6) (format "%7.1fm" (/ bs 1e6)))
          ((> bs 1e3) (format "%7.1fk" (/ bs 1e3)))
          (t          (format "%7d" bs)))))

(defvar ibuffer-formats)
(setf ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 24 24 :left :elide)
              " "
              (size-h 8 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 12 16 :left)
              " "
              filename-and-process)))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  )

;; ERC settings

(defvar erc-autojoin-channels-alist)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs")
        ("mozilla.org" "#rust")))
;; (erc :server "irc.mozilla.org" :port 6667 :nick "m-cat")

;; Notify in minibuffer when private messaged.
(defvar erc-echo-notices-in-minibuffer-flag)
(setq erc-echo-notices-in-minibuffer-flag t)

;; Match keywords, highlight pals, ignore fools.
(require 'erc-match)
(setq erc-keywords '("rust"))
(setq erc-pals  '())
(setq erc-fools '())

(use-package erc-scrolltoplace
  :config
  (add-to-list 'erc-modules 'scrolltoplace)
  (erc-update-modules)
  )

;; Eshell settings

(defvar eshell-scroll-show-maximum-output)
(defvar eshell-scroll-to-bottom-on-input)
(defvar eshell-scroll-to-bottom-on-output)
(defvar eshell-hist-ignoredups)
(defvar eshell-history-file-name)
(setq
 ;; Stop output from always going to the bottom.
 eshell-scroll-show-maximum-output nil
 eshell-scroll-to-bottom-on-output nil
 ;; Always insert at the bottom.
 eshell-scroll-to-bottom-on-input t
 eshell-hist-ignoredups t
 ;; Set the history file.
 eshell-history-file-name "~/.bash_history"
 )

(defvar eshell-mode-map)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              ;; Use helm to list eshell history.
              (define-key eshell-mode-map (kbd "M-i") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "M-{") 'eshell-previous-prompt)
              (define-key eshell-mode-map (kbd "M-}") 'eshell-next-prompt)

              ;; Save all buffers before running a command.
              (add-hook 'eshell-pre-command-hook 'save-all)

              ;; Use HISTSIZE as the history size.
              (defvar eshell-history-size)
              (setq eshell-history-size nil)
              ))

;; Open a new eshell buffer.
(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))
(global-set-key [f1] 'projectile-run-eshell)
(global-set-key [f2] 'eshell-new)

;; Add up to eshell.
;; Jump to a directory higher up in the directory hierarchy.
(use-package eshell-up
  :defer 2
  :config (setq eshell-up-print-parent-dir nil))

;; Add z to eshell.
;; Jumps to most recently visited directories.
(use-package eshell-z
  :defer 2)

;;; Load packages

;; Stop execution here for terminal.

;; We typically only want to open `emacs' in the terminal for quick editing.
(when (not (display-graphic-p))
  (with-current-buffer " *load*"
    (goto-char (point-max)))
  )

;; Start loading packages.

;; Display number of matches when searching.
(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode))

;; Avy mode (jump to a char/word using a decision tree).
(use-package avy
  :bind (("C-," . avy-goto-line-end)
         ("C-." . avy-goto-char)
         )
  :init
  ;; Jump to the end of a line using avy's decision tree.
  (defun avy-goto-line-end ()
    "Jump to a line using avy and go to the end of the line."
    (interactive)
    (avy-goto-line)
    (end-of-line)
    )
  :config
  ;; Use more characters (and better ones) in the decision tree.
  ;; QWERTY keys.
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l
                      ?w ?e ?r ?u ?i ?o))

  ;; Set the background to gray to highlight the decision tree?
  (setq avy-background nil)
  )

;; Imagemagick wrapper.
(use-package blimp
  :hook (image-mode . blimp-mode)
  )

;; ;; Move buffers around.
;; (use-package buffer-move
;;   :bind (("<s-up>"    . buf-move-up)
;;          ("<s-down>"  . buf-move-down)
;;          ("<s-left>"  . buf-move-left)
;;          ("<s-right>" . buf-move-right)
;;          ))

;; Copy selected region to be pasted into Slack/Github/etc.
(use-package copy-as-format
  :defer t)

;; ;; Wrap parentheses or quotes around word.
;; REMOVED: Took up too many useful keybindings.
;;          Looking for a more intuitive replacement.
;; (use-package corral
;;   :bind (
;;          ("M-(" . corral-parentheses-backward)
;;          ("M-)" . corral-parentheses-forward)
;;          ("M-[" . corral-brackets-backward)
;;          ("M-]" . corral-brackets-forward)
;;          ;; ("M-{" . corral-braces-backward) ;; Useful keys by default.
;;          ;; ("M-}" . corral-braces-forward)
;;          ("M-`" . corral-backquote-forward)
;;          ("M-~" . corral-backquote-backward)
;;          ;; ("M-'"  . corral-double-quotes-forward)
;;          ;; ("M-\"" . corral-double-quotes-backward)
;;          )
;;   :config (setq corral-preserve-point t))

;; Display available keybindings in Dired mode (? creates popup).
(use-package discover
  :defer 2)

;; ;; Text separated by more than one space doesn't move.
;; (use-package dynamic-spaces
;;   :config
;;   (dynamic-spaces-global-mode))

;; Show example usage when examining elisp functions.
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;; Better comment command.
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Expand-region.
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  ;; Fix region not highlighting.
  ;; See https://github.com/magnars/expand-region.el/issues/229
  (setq shift-select-mode nil)
  (setq expand-region-fast-keys-enabled nil)
  )

;; Workspaces.
(use-package eyebrowse
  :demand t ;; To prevent mode-line display errors.
  :bind (("s-," . eyebrowse-prev-window-config)
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
         ("s-t" . eyebrowse-rename-window-config)
         )
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth nil)
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-close-window-config-prompt t)

  (setq eyebrowse-mode-line-separator ",")
  (setq eyebrowse-mode-line-left-delimiter "[")
  (setq eyebrowse-mode-line-right-delimiter "]")

  (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :bold t)

  ;;; Show workspaces in title bar.

  ;; Only recalculate the workspaces string when it actually changes.
  (defvar eyebrowse-workspaces)
  (defun eyebrowse-current-workspace ()
    "Get the current workspace number."
    (eyebrowse--get 'current-slot))
  (defun eyebrowse-workspaces-string ()
    "Get the current workspaces as a string."
    (let ((workspaces (substring-no-properties (eyebrowse-mode-line-indicator))))
      (setq eyebrowse-workspaces workspaces)))
  (defun eyebrowse-workspaces-string-rename (arg1 arg2)
    "Advice for `eyebrowse-rename-window-config'. Requires two
arguments ARG1 and ARG2 to work..."
    (eyebrowse-workspaces-string))
  (eyebrowse-workspaces-string)
  (add-hook 'eyebrowse-post-window-switch-hook 'eyebrowse-workspaces-string)
  (advice-add 'eyebrowse-close-window-config :after #'eyebrowse-workspaces-string)
  (advice-add 'eyebrowse-rename-window-config :after #'eyebrowse-workspaces-string-rename)

  ;; Append to title list.
  (add-to-list 'frame-title-format
               '(:eval (when (not (string-empty-p eyebrowse-workspaces))
                         (format " - %s - %s" eyebrowse-workspaces (eyebrowse-current-workspace))))
               t
               )
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

;; Highlight indentation.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-enabled nil
        )
  )

;; Highlight numbers in code.
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Highlight operators.
;; Breaks in lisp modes, which is why I enable this on a per-mode basis.
(use-package highlight-operators
  :hook (
         (c-mode-common . highlight-operators-mode)
         ;; REMOVED: Results in higher CPU load and slowdown in large files,
         ;; especially in `rust-mode'.
         ;; (rust-mode . highlight-operators-mode)
         )
  )

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
  (global-hl-todo-mode)

  (add-to-list 'hl-todo-keyword-faces '("REMOVED" . "#cc9393"))
  )

;; A package for choosing a color by updating text sample.
;; See https://www.emacswiki.org/emacs/MakeColor.
(use-package make-color
  :defer t)

;; Multiple cursors.
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-{") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-}") 'mc/mark-next-like-this)
  (define-key mc/keymap (kbd "<return>") nil)
  ;; Add cursors with the mouse!
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

  (setq mc/always-run-for-all t)
  )

;; Synonym lookup.
(use-package powerthesaurus
  :defer t)

;; ;; Highlight delimiters with colors depending on depth.
;; REMOVED: Too slow in large files.
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight color strings with the corresponding color.
(use-package rainbow-mode
  :defer t
  )

;; Open current directory in Finder on Mac.
(use-package reveal-in-osx-finder
  :bind ("C-c f" . reveal-in-osx-finder)
  )

;; Automatically save place in each file.
(use-package saveplace
  :config
  (save-place-mode t)
  )

;; Actually a really nice mode-line package.
;; Requires little configuration.
(use-package spaceline
  :config
  (require 'spaceline-config)

  ;; Don't display minor modes (too messy).
  (defvar spaceline-minor-modes-p)
  (setq spaceline-minor-modes-p nil)
  ;; Don't display eyebrowse workspace numbers (displayed in title bar instead).
  (defvar spaceline-workspace-number-p)
  (setq spaceline-workspace-number-p nil)

  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  )

;; Open current directory in an external terminal emulator.
(use-package terminal-here
  :bind ("C-c t" . terminal-here-launch)
  )

;; A more lightweight alternative to undo-tree.
(use-package undo-propose
  :bind ("C-_" . undo-propose))

;; Use a sensible mechanism for making buffer names unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 1
      uniquify-strip-common-suffix nil
      )

;; Highlight some recent changes such as undos.
(use-package volatile-highlights
  :config (volatile-highlights-mode))

;; Display available keys.
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil))

;; Highlight the parts of lines that exceed certain column numbers.
(use-package whitespace
  :config
  (setq whitespace-style '(face
                           empty lines-tail tabs trailing))

  (defun c-whitespace-mode ()
    "Set whitespace column for c-like modes and turn on `whitespace-mode'."
    (setq whitespace-line-column 80
          fill-column 80)
    (whitespace-mode)
    )
  (add-hook 'c-mode-common-hook 'c-whitespace-mode)
  (add-hook 'nim-mode-hook 'c-whitespace-mode)

  (defun 100-whitespace-mode ()
    "Set whitespace column at 100 and turn on `whitespace-mode'."
    (setq whitespace-line-column 100
          fill-column 100
          )
    (whitespace-mode)
    )
  (add-hook 'rust-mode-hook '100-whitespace-mode)
  (add-hook 'python-mode-hook '100-whitespace-mode)
  )

;; Undo/redo window configurations.
(use-package winner
  :bind (
         ("C-c C-," . winner-undo)
         ("C-c C-." . winner-redo)
         )
  :config (winner-mode t))

;; Switch windows more easily.
(use-package winum
  :init
  ;; Prevent winum from inserting its own number in the mode-line
  ;; (spaceline already does so).
  (setq winum-auto-setup-mode-line nil)

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
  (winum-mode)
  )

;; Wrap regions with pairs.
(use-package wrap-region
  :config
  (wrap-region-add-wrappers
   '(
     ("`" "`")
     ("*" "*")
     ))

  ;; Kepe the region active after adding a pair.
  (setq wrap-region-keep-mark t)

  (wrap-region-global-mode t)
  )

;; Automatically clean up extraneous whitespace.
(use-package ws-butler
  :hook (
         (prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)
         ))

;;; Git packages

;; Interface with GitHub etc. from Magit.
(use-package forge
  :defer 2)

;; Generate links to Github for current code location.
(use-package git-link
  :defer t)

;; Browse historic versions of a file.
(use-package git-timemachine
  :defer t)

(use-package gitignore-mode
  :defer t)

;; Git client in Emacs.
(use-package magit
  :bind (("C-x g" . magit-status)
         ("s-g" . magit-status))

  :init
  (setq
   ;; Show fine differences for all displayed diff hunks.
   magit-diff-refine-hunk `all
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   ;; Don't ask before saving repository buffers.
   magit-save-repository-buffers 'dontask
   )
  )

;;; Project packages

;; ;; Company mode for auto-completion.
;; REMOVED: Heavy-weight. Tab's completion-at-point seems more useful.
;; (use-package company
;;   :bind ("M-/" . company-complete)
;;   :hook (after-init . global-company-mode)
;;   :init
;;   (setq company-idle-delay nil)
;;   (setq company-tooltip-align-annotations t) ;; Align tooltips to right border.
;;   )

;; Show markers in margin indicating changes.
(use-package diff-hl
  :bind (
         ("C-?" . diff-hl-revert-hunk)
         ("M-[" . diff-hl-previous-hunk)
         ("M-]" . diff-hl-next-hunk)
         )
  :hook ((prog-mode . enable-diff-hl)
         (text-mode . enable-diff-hl)
         (conf-mode . enable-diff-hl)
         )
  :init
  (defun enable-diff-hl ()
    ;; Make the fringe wide enough to display correctly.
    (setq-local left-fringe-width 16)
    (turn-on-diff-hl-mode))

  ;; (global-diff-hl-mode)
  :config

  ;; Show diffs in margin when running in terminal.
  (unless (window-system) (diff-hl-margin-mode))
  ;; No need to save before seeing diffs.
  ;; (diff-hl-flydiff-mode)

  ;; Refresh diffs after a Magit commit.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; See diffs in Dired.
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  )

;; On-the-fly syntax checker.
(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (text-mode . flycheck-mode)
         )
  :commands flycheck-mode
  :bind ("C-!" . flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (setq flycheck-check-syntax-automatically nil)

  ;; Set shorter delay for displaying errors at point.
  (setq flycheck-display-errors-delay (* 3 info-delay))
  (setq sentence-end-double-space nil) ;; Stupid check.

  ;; Disable checkers.
  (setq-default flycheck-disabled-checkers '(proselint rust rust-cargo rust-clippy))
  ;; (setq-default flycheck-disabled-checkers '(rust)) ;; Doesn't work.

  ;; (flycheck-add-next-checker 'rust-cargo 'rust-clippy)
  )

;; Elisp package lints.
(use-package flycheck-package
  :hook (flycheck-mode . flycheck-package-setup))

;; Helm interface for projectile.
(use-package helm-projectile
  :bind ("s-;" . helm-projectile)
  :config
  (helm-projectile-on))

;; Project manager.
(use-package projectile
  :defer 1
  :hook (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'helm)
  )

;; Jump to definitions using dumb-jump as a fallback.
(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers))

;; ;; Yasnippet.
;; ;; NOTE: list all snippets for current mode with M-x `yas-describe-tables'.
;; REMOVED: Takes a long time to load, sometimes can't open a buffer because of
;; missing snippet files, sometimes I want to indent instead of expanding a
;; snippet.
;; (use-package yasnippet-snippets)
;; (use-package yasnippet
;;   :requires yasnippet-snippets
;;   :config
;;   (yas-global-mode)

;;   (setq yas-triggers-in-field t) ; Enable nested triggering of snippets.
;;   (setq yas-verbosity 1) ;; No need to be so verbose.
;;   )

;;; Language packages

;; Clojure

(use-package clojure-mode
  :defer t)

(use-package cider
  :after clojure-mode
  :config
  (setq cider-use-overlays nil)
  )

(use-package flycheck-clojure
  :defer t)

;; Javascript

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  ;; TODO: use smart-jump here instead. Do we even need to define a key here?
  (define-key js-mode-map (kbd "M-.") 'dumb-jump-go)
  )

;; JSON

(use-package json-mode
  :defer t)

;; Lua

(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 4)
  )

;; Markdown

;; On-the-fly markdown preview. M-x flymd-flyit
(use-package flymd
  :defer t)

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . markdown-mode-hook-fun)
  :init
  (defun markdown-mode-hook-fun ()
    "Initialize markdown-mode"

    ;; Unbind keys stolen by this idiotic mode.
    (local-unset-key (kbd "M-n"))
    (local-unset-key (kbd "M-p"))
    )
  )

(use-package markdown-toc
  :after markdown-mode
  :defer t)

;; Nim

(use-package nim-mode
  :config
  (define-key nim-mode-map (kbd "RET") 'newline-and-indent)
  )

;; Processing

(use-package processing-mode)

;; Rust

(use-package racer
  :hook ((rust-mode . racer-mode))
  :config
  ;; Don't insert argument placeholders when completing a function.
  (setq racer-complete-insert-argument-placeholders nil)

  (define-key racer-mode-map (kbd "M-,") 'smart-jump-back)
  (define-key racer-mode-map (kbd "M-.") 'smart-jump-go)
  )

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil)

  (define-key rust-mode-map (kbd "C-c n") 'rust-format-buffer)
  )

;; TOML

(use-package toml-mode
  :mode "\\.toml\\'"
  )

;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'")

;;; Org Mode

(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-mode . org-mode-hook-fun)
         (org-agenda-mode . org-agenda-mode-hook-fun))

  :init

  (defun org-mode-hook-fun ()
    "Initialize `org-mode'."

    (visual-line-mode) ;; Word-wrap.
    (toggle-word-wrap t)
    (org-indent-mode) ;; Indented entries.

    ;; Unbind keys stolen by org-mode.
    (local-unset-key (kbd "C-,"))

    ;; Fix tags alignment getting messed up (still not sure of the cause).
    (add-hook 'before-save-hook 'org-align-all-tags nil t)
    )

  (defun org-agenda-mode-hook-fun ()
    "Initialize `org-agenda-mode'."

    (visual-line-mode)
    (toggle-word-wrap t)

    (defvar org-agenda-mode-map)
    (define-key org-agenda-mode-map (kbd "s-\"") 'org-agenda-refile)
    )

  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)
          ))))

  ;; Don't align tags.
  ;; Keep this in :init so that no org-files are opened without these settings.
  ;; Should (hopefully) fix tag alignment getting messed up.
  (setq org-tags-column 0
        org-auto-align-tags nil)

  :config

  ;;; Settings

  ;; Default org directory.
  (setq org-directory user-org-directory)

  ;; The ellipsis to use in the org-mode outline.
  (setq org-ellipsis "...")
  ;; Try to keep cursor before ellipses.
  (setq org-special-ctrl-a/e t)
  ;; Smart editing of invisible region around ellipses.
  (setq org-catch-invisible-edits 'smart)

  ;; All subtasks must be Done before marking a task as Done.
  (setq org-enforce-todo-dependencies t)
  ;; Log time a task was set to Done.
  (setq org-log-done (quote time))
  ;; Don't log the time a task was rescheduled or redeadlined.
  (setq org-log-reschedule nil)
  (setq org-log-redeadline nil)

  ;; Prefer rescheduling to future dates and times.
  (setq org-read-date-prefer-future 'time)

  ;; M-RET should not split the heading if point is not at the end of a line.
  ;; (setq org-M-RET-may-split-line nil)

  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

  ;; Custom to-do states.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELED(x)")))

  ;; Org-agenda settings

  ;; Show scheduled items in order from most to least recent.
  (defvar org-agenda-sorting-strategy)
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up scheduled-down priority-down category-keep)
          (todo   priority-down category-keep)
          (tags   priority-down category-keep)
          (search category-keep)))

  ;; Customize columns (remove filename/category, mostly redundant).
  (defvar org-agenda-prefix-format)
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  ;; Refresh org-agenda after changing an item status.
  ;; (add-hook 'org-trigger-hook 'org-agenda-refresh)
  ;; Refresh org-agenda after rescheduling a task.
  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh))

  ;; Refresh org-agenda after an org-capture.
  (add-hook 'org-capture-after-finalize-hook 'org-agenda-refresh)

  ;; Set location of agenda files.
  (setq org-agenda-files (list user-todo-location
                               user-work-location
                               ))
  ;; Stop org-agenda from messing up my windows!!
  (defvar org-agenda-window-setup)
  (setq org-agenda-window-setup 'current-window)
  ;; Start org-agenda from the current day.
  (defvar org-agenda-start-on-weekday)
  (setq org-agenda-start-on-weekday nil)
  ;; Don't align tags in the org-agenda (sometimes it messes up the display).
  (defvar org-agenda-tags-column)
  (setq org-agenda-tags-column 0)

  ;; org-refile settings

  ;; Refile notes to the top of the list.
  (setq org-reverse-note-order t)
  ;; Use headline paths (level1/level2/...)
  (setq org-refile-use-outline-path t)
  ;; Go down in steps when completing a path.
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 99))
                             (user-notes-location . (:maxlevel . 99))
                             (user-dreams-location . (:maxlevel . 99))
                             (user-work-location . (:maxlevel . 99))
                             (user-ideas-location . (:maxlevel . 99))
                             ))
  ;; Jump to headings with completion.
  (setq org-goto-interface 'outline-path-interface
        org-goto-max-level 99)
  ;; Always show full context, no matter how we get to a certain heading (e.g.
  ;; `isearch', `org-goto', whatever). The default behavior of hiding headings
  ;; is asinine.
  (setq org-show-context-detail '((default . tree)))

  ;; org-capture template.
  (defvar org-capture-templates
    '(("t" "My TODO task format." entry
       (file+headline "todo.org" "General")
       "* %?\nSCHEDULED: %t")
      ("n" "My note format." entry
       (file "notes.org")
       "* %?")))

  ;; Shortcuts/Keybindings

  (defun org-refile-goto ()
    "Use org-refile to conveniently choose and go to a heading."
    (interactive)
    (let ((current-prefix-arg '(4))) (call-interactively 'org-refile))
    )

  ;; org-capture with template as default behavior.
  (defun org-task-capture ()
    "Capture a task with my todo template."
    (interactive)
    (org-capture nil "t"))
  (defun org-note-capture ()
    "Capture a note with my note template."
    (interactive)
    (org-capture nil "n"))

  (defun org-meta-return-end ()
    "Go to end of visual line before calling org-meta-return."
    (interactive)
    (end-of-visual-line)
    (org-meta-return))

  (defun mouse-org-cycle (@click)
    (interactive "e")
    (let ((p1 (posn-point (event-start @click))))
      (goto-char p1)
      (call-interactively 'org-cycle)
      )
    )

  ;; Local keybindings.

  (define-key org-mode-map (kbd "<s-return>") 'org-meta-return-end)
  (define-key org-mode-map (kbd "C-S-n") 'org-metadown)
  (define-key org-mode-map (kbd "C-S-p") 'org-metaup)
  (define-key org-mode-map (kbd "C-<") 'org-shiftmetaleft)
  (define-key org-mode-map (kbd "C->") 'org-shiftmetaright)
  (define-key org-mode-map (kbd "M-m") 'org-beginning-of-line)
  (define-key org-mode-map (kbd "C-^") 'org-up-element)
  (define-key org-mode-map (kbd "s-\"") 'org-refile)
  (define-key org-mode-map (kbd "C-j") 'join-next-line)

  (define-key org-mode-map (kbd "<mouse-3>") 'mouse-org-cycle)

  ;; Global keybindings.

  (global-set-key (kbd "s-'") 'org-refile-goto)

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda-list) ;; Switch to org-agenda.
  (global-set-key (kbd "C-c c") 'org-note-capture) ;; org-capture.
  (global-set-key (kbd "C-c v") 'org-task-capture)

  ;; Jump to last capture.
  (global-set-key (kbd "C-c j") 'org-refile-goto-last-stored)
  )

;; Try to fix the annoying tendency of this function to scroll the point to some
;; random place and mess up my view of the agenda.
(require 'org-agenda)
(defun org-agenda-redo (&optional all)
  "Rebuild possibly ALL agenda view(s) in the current buffer."
  (interactive "P")
  (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
         (cpa (unless (eq all t) current-prefix-arg))
         (org-agenda-doing-sticky-redo org-agenda-sticky)
         (org-agenda-sticky nil)
         (org-agenda-buffer-name (or org-agenda-this-buffer-name
                                     org-agenda-buffer-name))
         (org-agenda-keep-modes t)
         (tag-filter org-agenda-tag-filter)
         (tag-preset (get 'org-agenda-tag-filter :preset-filter))
         (top-hl-filter org-agenda-top-headline-filter)
         (cat-filter org-agenda-category-filter)
         (cat-preset (get 'org-agenda-category-filter :preset-filter))
         (re-filter org-agenda-regexp-filter)
         (re-preset (get 'org-agenda-regexp-filter :preset-filter))
         (effort-filter org-agenda-effort-filter)
         (effort-preset (get 'org-agenda-effort-filter :preset-filter))
         (cols org-agenda-columns-active)
         (line (org-current-line))
         ;; (window-line (- line (org-current-line (window-start))))
         (lprops (get 'org-agenda-redo-command 'org-lprops))
         (redo-cmd (get-text-property p 'org-redo-cmd))
         (last-args (get-text-property p 'org-last-args))
         (org-agenda-overriding-cmd (get-text-property p 'org-series-cmd))
         (org-agenda-overriding-cmd-arguments
          (unless (eq all t)
            (cond ((listp last-args)
                   (cons (or cpa (car last-args)) (cdr last-args)))
                  ((stringp last-args)
                   last-args))))
         (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
    (put 'org-agenda-tag-filter :preset-filter nil)
    (put 'org-agenda-category-filter :preset-filter nil)
    (put 'org-agenda-regexp-filter :preset-filter nil)
    (put 'org-agenda-effort-filter :preset-filter nil)
    (and cols (org-columns-quit))
    (message "Rebuilding agenda buffer...")
    (if series-redo-cmd
        (eval series-redo-cmd)
      (org-let lprops redo-cmd))
    (setq org-agenda-undo-list nil
          org-agenda-pending-undo-list nil
          org-agenda-tag-filter tag-filter
          org-agenda-category-filter cat-filter
          org-agenda-regexp-filter re-filter
          org-agenda-effort-filter effort-filter
          org-agenda-top-headline-filter top-hl-filter)
    (message "Rebuilding agenda buffer...done")
    (put 'org-agenda-tag-filter :preset-filter tag-preset)
    (put 'org-agenda-category-filter :preset-filter cat-preset)
    (put 'org-agenda-regexp-filter :preset-filter re-preset)
    (put 'org-agenda-effort-filter :preset-filter effort-preset)
    (let ((tag (or tag-filter tag-preset))
          (cat (or cat-filter cat-preset))
          (effort (or effort-filter effort-preset))
          (re (or re-filter re-preset)))
      (when tag (org-agenda-filter-apply tag 'tag t))
      (when cat (org-agenda-filter-apply cat 'category))
      (when effort (org-agenda-filter-apply effort 'effort))
      (when re  (org-agenda-filter-apply re 'regexp)))
    (and top-hl-filter (org-agenda-filter-top-headline-apply top-hl-filter))
    (and cols (called-interactively-p 'any) (org-agenda-columns))
    (org-goto-line line)
    ;; Commenting out the following line stops the random scrolling.
    ;; (recenter window-line)
    ))

;; Recurring org-mode tasks.
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (defvar org-recur-mode-map)
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (defvar org-recur-agenda-mode-map)
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t)
  )

;; Display groups in org-agenda to make things a bit more organized.
(use-package org-super-agenda
  :config
  (org-super-agenda-mode)

  (setq org-super-agenda-header-separator "")
  (setq org-super-agenda-unmatched-name "Other")
  (setq org-super-agenda-groups
        '(;; Each group has an implicit OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
                 :time-grid t  ; Items that appear on the time grid.
                 :todo "TODAY"   ; Items that have this todo keyword.
                 )
          (:name "Work"
                 :category "work"
                 )
          (:name "High Priority"
                 :priority "A"
                 :order 1
                 )
          (:name "Physical"
                 :category "physical"
                 :tag "physical"
                 :order 2
                 )
          (:name "Shopping List"
                 :category "shopping"
                 :tag "shopping"
                 :order 3
                 )
          (:name "Cleaning"
                 :category "cleaning"
                 :tag "cleaning"
                 :order 4
                 )
          (:name "Hygiene"
                 :category "hygiene"
                 :tag "hygiene"
                 :order 5
                 )
          (:name "Health"
                 :category "health"
                 :tag "health"
                 :order 6
                 )

          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99

          (:name "Tech"
                 :category "tech"
                 :tag "tech"
                 :order 180
                 )
          (:name "To Read"
                 :category "read"
                 :tag "read"
                 :order 181
                 )
          (:name "To Watch"
                 :category "watch"
                 :tag "watch"
                 :order 182
                 )
          (:todo "WAITING" :order 190)  ; Set order of this section
          ;; (:name "Low priority"
          ;;        :priority "C"
          ;;        :order 200)
          )))

;;; Final

;; Misc

;; Display notes.org and Org Agenda on startup.
(defun emacs-welcome()
  "Display Emacs welcome screen."
  (interactive)
  (find-file user-notes-location)
  (split-window-right-focus)
  (find-file user-todo-location)
  (split-window-right-focus)
  (org-agenda-list)
  )
(emacs-welcome)

(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
