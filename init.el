;;; init.el --- Emacs configuration file.
;;
;; Copyright (C) 2017-2019 Marcin Swieczkowski
;;
;;; Commentary:
;;
;; Requires Emacs 25 or higher.
;;
;; Making changes / testing:
;;
;; - Use M-x free-keys to find unused keybindings.
;; - Use M-x bug-hunter-init-file to locate errors.
;; - Use M-x esup to profile startup time,
;;   M-x profiler-start and profiler-report to profile runtime.
;; - Use restart-emacs to restart after making changes.

;;; Code:

;; First things first, increase GC threshold to speed up startup.
;; Reset the GC threshold after initialization, and GC whenever we tab out.
(setq gc-cons-threshold (* 64 1000 1000))
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold (* 2 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;;; User-Defined Variables

(defvar init-file-location  (concat user-emacs-directory "init.el"))
(defvar scratchpad-location "~/Dropbox/Text/scratchpad.txt")

(defvar user-org-directory     "~/Dropbox/Text/org")
(defvar user-physical-location "~/Dropbox/Text/org/physical.org")
(defvar user-dreams-location   "~/Dropbox/Text/org/dreams.org")
(defvar user-notes-location    "~/Dropbox/Text/org/notes.org")
(defvar user-todo-location     "~/Dropbox/Text/org/todo.org")
(defvar user-work-location     "~/Dropbox/Text/org/work.org")

(defvar highlight-delay .03)
(defvar info-delay      .25)

;; Open .emacs init.
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file init-file-location))
(global-set-key (kbd "C-c i") 'open-init-file)

;; Open scratchpad.txt.
(defun open-scratchpad-file ()
  "Open scratchpad file."
  (interactive)
  (find-file scratchpad-location))
(global-set-key (kbd "C-c s") 'open-scratchpad-file)

;;; Package settings

(require 'package)
;; Explicitly enable packages.
(setq package-enable-at-startup nil)
;; Add package sources.
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Run auto-load functions specified by package authors.
(package-initialize)

;; Require use-package.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Inherit environment variables from Shell.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    ))

;; ;; Ensure system binaries exist and download them if not
;; (use-package use-package-ensure-system-package)

;; Enable restarting Emacs from within Emacs.
(use-package restart-emacs
  :defer t)

;; Find bugs in Emacs configuration.
(use-package bug-hunter
  :defer t)

;; Show more error info.
;; (setq debug-on-error nil)

;; Diminish modeline clutter.
(use-package diminish)
(diminish 'abbrev-mode)

;;; Initialize Helm

(use-package helm
  :init (require 'helm-config)
  :diminish helm-mode
  :config
  (helm-mode 1)
  )

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key   (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Rebind tab to run persistent action.
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; Alternate TAB key that works in terminal.
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action) ;; list actions using C-z
(define-key helm-map (kbd "M-x")   'helm-select-action)

(global-set-key (kbd "M-x")        'helm-M-x)
(global-set-key (kbd "C-x C-f")    'helm-find-files)
(global-set-key (kbd "C-h C-a")    'helm-apropos)
(global-set-key (kbd "C-x C-SPC")  'helm-all-mark-rings)
(global-set-key (kbd "M-i")        'helm-semantic-or-imenu)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t
      helm-follow-mode-persistent t
      )

(setq helm-apropos-fuzzy-match t)

;; Jump to a function definition
(semantic-mode 1)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p t ;; open helm buffer inside current window
      ;; move to end or beginning of source when reaching top/bottom of source.
      helm-move-to-line-cycle-in-source t
      ;; search for library in `use-package' and `declare-function' sexp.
      helm-ff-search-library-in-sexp t
      ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf  t
      helm-echo-input-in-header-line         t
      )

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

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-files-in-current-dir
                                  ;; helm-source-buffer-not-found
                                  ))

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

;; Better mode help
(use-package helm-describe-modes
  :bind ("C-h m" . helm-describe-modes))

;;; Helm-swoop.
(use-package helm-swoop
  :bind (("C-;" . helm-swoop-without-pre-input)
         ("C-:" . helm-multi-swoop-all))
  :config
  ;; Move up and down like isearch
  (define-key helm-swoop-map        (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map        (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map  (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map  (kbd "C-s") 'helm-next-line)

  ;; When doing isearch, hand the word over to helm-swoop.
  (define-key isearch-mode-map (kbd "C-;") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all.
  (define-key helm-swoop-map   (kbd "C-;") 'helm-multi-swoop-all-from-helm-swoop)

  (setq helm-swoop-speed-or-color t) ;; Show syntax highlighting in results.
  )

;; ggtags with helm
(use-package helm-gtags
  :diminish helm-gtags-mode
  :init
  ;; Enable helm-gtags-mode.
  (add-hook 'c-mode-hook   'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  :config
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   )
  (define-key helm-gtags-mode-map (kbd "C-c g a")
    'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "M-.")
    (lambda ()
      (interactive)
      (save-all)
      ;; (helm-gtags-update-tags)
      (let ((current-prefix-arg '(2))) (call-interactively
                                        'helm-gtags-update-tags))
      (helm-gtags-dwim)
      ))

  (define-key helm-gtags-mode-map (kbd "M-,")   'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  )

;; ag with helm.
(use-package helm-ag
  :init
  (setq helm-ag-insert-at-point 'symbol)
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

;; Keep directories clean.
(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;; Track recently-opened files.
(use-package recentf
  :config
  (setq recentf-max-saved-items 10000)
  (recentf-mode t)
  )

;; Turn on blinking/flashing cursor.
(blink-cursor-mode 1)
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;; Disable scroll bars and the tool bar.
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

(toggle-frame-maximized) ;; Maximize!
;; (toggle-frame-fullscreen) ;; Maximize MORE

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(setq x-gtk-use-system-tooltips nil)

;; Enable functions that are disabled by default.
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'scroll-left      'disabled nil)
(put 'scroll-right     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

(setq show-paren-delay highlight-delay)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              indicate-empty-lines nil ;; Highlight end of buffer?
              )

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      kill-ring-max 1000
      require-final-newline t    ;; Ensure that files end with a newline.
      next-line-add-newlines t   ;; Add newline at end of buffer with C-n.
      visible-bell t
      ring-bell-function 'ignore
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      window-combination-resize t
      echo-keystrokes 0.01            ;; Display keystrokes immediately.
      inhibit-startup-message t       ;; Disable startup screen.
      initial-scratch-message ""      ;; Change the initial *scratch* buffer.
      help-window-select t            ;; Focus new help windows when opened.
      confirm-kill-emacs nil          ;; Always confirm before closing Emacs.
      delete-by-moving-to-trash t     ;; Send deleted files to trash.
      ;; backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      version-control t               ;; Always make numeric backup versions.
      vc-make-backup-files t          ;; Make backups of all files.
      delete-old-versions t           ;; Silently delete old backup versions.
      isearch-allow-scroll t
      show-trailing-whitespace 1      ;; Display trailing whitespace.
      isearch-lazy-highlight t
      lazy-highlight-initial-delay info-delay
      ;; Delay for displaying function/variable information.
      eldoc-idle-delay info-delay

      pop-up-frames nil               ;; Open files in existing frames.
      pop-up-windows t
      ;; Tab will first try to indent, then complete.
      tab-always-indent 'complete
      resize-mini-windows t           ;; Resize the minibuffer when needed.
      enable-recursive-minibuffers t  ;; Enable recursive editing of minibuffer.
      ;; (setq max-mini-window-height 0.33)
      ;; Move point to beginning or end of buffer when scrolling.
      scroll-error-top-bottom t
      mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))

      ;; Change window name to be more descriptive
      frame-title-format '((:eval (when (and (buffer-modified-p) buffer-file-name) "*"))
                           "Emacs - "
                           (buffer-file-name
                            "%f" (dired-directory dired-directory "%b")))

      ;; Language-specific settings?
      c-default-style "stroustrup"
      )

;; Set some builtin modes
(setq global-hl-line-sticky-flag t)     ;; Keep line highlight across windows
(global-hl-line-mode t)                 ;; Highlight current line
;; Use compressed files like normal files
(global-hl-line-mode t)                 ;; Highlight current line.
;; Use compressed files like normal files.
(auto-compression-mode 1)
;; (desktop-save-mode 1)                ;; Keep open files open across sessions.
(column-number-mode 1)                  ;; Display the column number.
;; (display-time-mode 1)                ;; Display the current time.
;; (setq display-time-format "%l:%M%p")
;; Replace selected text when typing or pasting.
(delete-selection-mode 1)
;; Auto revert files that changed on disk.
(global-auto-revert-mode 1)

;; Set c-style comments to be "//" by default (these are just better, sorry)
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Preferred comment style
            (setq comment-start "// "
                  comment-end "")))

;; Turn on utf-8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

;; Setup selected file endings to open in certain modes
(add-to-list 'auto-mode-alist '("\\.hdl\\'"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.jack\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.over\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.pdf\\'"  . pdf-view-mode))

;; Mouse settings

(setq mouse-wheel-progressive-speed nil) ;; Make the mouse wheel not accelerate.

;;; My Functions and Shortcuts/Keybindings

(global-set-key (kbd "M-o") 'other-window)

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
  (save-some-buffers t) ;; Do not prompt for confirmation.
  )
(global-set-key (kbd "C-x s") 'save-all)

;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'focus-out-hook 'save-all)

(defun goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively #'goto-line))
    (linum-mode -1)))
(global-set-key (kbd "s-l") 'goto-line-show)

(defun helm-projectile-ag-inexact ()
  "Run helm-projectile-ag case-insensitive and without word boundaries."
  (interactive)
  (setq helm-ag-base-command "ag --hidden --nocolor --nogroup --ignore-case")
  (setq helm-ag-insert-at-point nil)
  (helm-projectile-ag)
  )
(defun helm-projectile-ag-exact ()
  "Run helm-projectile-ag case-sensitive and with word boundaries."
  (interactive)
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
  (balance-windows))

;; Remap the default window-splitting commands to the ones above.
(global-set-key (kbd "C-x 2") 'split-window-below-focus)
(global-set-key (kbd "C-x 3") 'split-window-right-focus)

(global-set-key (kbd "C-0") 'delete-window-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)

(global-set-key (kbd "C-j") 'indent-new-comment-line)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(define-key key-translation-map (kbd "<C-tab>") (kbd "TAB"))

;; Narrow/widen easily.
(defun narrow-dwim ()
  "Widen if currently narrowed, else narrow to function."
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen))
   (t (narrow-to-defun))))
(global-set-key (kbd "C-(") 'narrow-dwim)

;; Code folding.
(require 'hideshow)
(diminish 'hs-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(define-key hs-minor-mode-map (kbd "C-)") 'hs-toggle-hiding)

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

;; Delete current buffer file.
(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-current-buffer-file)

;; Rename current buffer file.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
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

;; Select the current line.
(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))
;; Replace default C-l, it's useless
(global-set-key (kbd "C-l") 'select-current-line)

;; Improved kill-whole-line which doesn't change cursor position
;; Can be called on multiple lines.
;; Will entirely kill any line that's even partially within the region
(defun annihilate-line-dwim ()
  "Annihilate the current line or region by killing it, deleting the empty \
line, and restoring cursor position. If called on a region, will annihilate \
every line included in the region."
  (interactive)
  (cond
   ;; No region selected
   ((not (region-active-p))
    (annihilate-line))
   ;; There is an active region
   (t
    (annihilate-line-region (region-beginning) (region-end)))))

(defun annihilate-line ()
  "Annihilate the current line."
  (interactive)
  (let ((col (current-column)))
    (kill-region (line-beginning-position) (line-end-position))
    (kill-append "\n" t)
    (if (/= (line-end-position) (point-max)) ;; Are there more lines after this?
        (delete-char 1))
    (move-to-column col))
  )

(defun annihilate-line-region (beg end)
  "Annihilate the region from BEG to END."
  (interactive "r")
  (let ((col (current-column)))
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (kill-region beg end)
    (kill-append "\n" t)
    (if (/= (line-end-position) (point-max)) ;; Are there more lines after this?
        (delete-char 1))
    (move-to-column col))) ;; Restore column position

(global-set-key (kbd "C-S-k") 'annihilate-line-dwim)

;; Move current line up or down.
(defun move-line-down ()
  "Move the current line down, preserving the cursor position."
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))
(defun move-line-up ()
  "Move the current line up, preserving the cursor position."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(global-set-key (kbd "C-S-n") 'move-line-down)
(global-set-key (kbd "C-S-p") 'move-line-up)

;; Join the following line onto the current line.
;; Use this to quickly consolidate multiple lines into one.
(defun join-next-line ()
  "Join the following line onto the current line, preserving the cursor \
position. This command can be used to rapidly consolidate multiple lines into \
one."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))

(global-set-key (kbd "M-j") 'join-next-line)

(defun open-line-below ()
  "Open a new line below, even if the point is midsentence, keeping proper \
indentation."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun open-line-above ()
  "Open a new line above, even if the point is midsentence, keeping proper \
indentation."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<S-return>") 'open-line-above)

(defun clear-line ()
  "Clear the line, but don't delete it."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (indent-according-to-mode)
  )

(defvar scroll-amount 8)

;; Scroll down/up by a smaller amount, doesn't change cursor position.
(defun scroll-up-line-quick ()
  "Scroll up by a smaller amount without changing the cursor position."
  (interactive)
  (scroll-up-line scroll-amount))
(defun scroll-down-line-quick ()
  "Scroll down by a smaller amount without changing the cursor position."
  (interactive)
  (scroll-down-line scroll-amount))

;; Scroll other window down/up.
(defun scroll-other-window-up-quick ()
  "Scroll the other window up by a smaller amount."
  (interactive)
  (scroll-other-window scroll-amount))
(defun scroll-other-window-down-quick ()
  "Scroll the other window down by a smaller amount."
  (interactive)
  (scroll-other-window-down scroll-amount))

;; Globally bind these keys so they work in every mode.
(bind-keys*
 ("M-n" . scroll-up-line-quick)
 ("M-p" . scroll-down-line-quick)
 ("M-N" . scroll-other-window-up-quick)
 ("M-P" . scroll-other-window-down-quick)

 ("<mouse-3>" . previous-buffer)
 ("<mouse-8>" . previous-buffer)
 ("<mouse-4>" . next-buffer)
 ("<mouse-9>" . next-buffer)
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
;; Obtained from http://www.chrislott.org/geek/emacs/dotemacs.html
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

;; Load Themes
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; Nimbus is my personal theme, available on Melpa.
(use-package nimbus-theme)
;; (use-package zerodark-theme)
;; (use-package zeno-theme)

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))

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

;;; Dired settings

(add-hook 'dired-mode-hook
          #'(lambda ()
              ;; Make C-l go up a directory in dired
              (define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
              (define-key dired-mode-map (kbd "s-l") 'dired-up-directory)
              (define-key dired-mode-map "f"         'helm-find-files)
              ))

;; Handle opening and editing zip directories in dired.
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-recursive-copies 'always ;; Always do recursive copies.
              ;; Make sizes human-readable by default and put dotfiles and
              ;; capital-letters first.
              dired-listing-switches "-alhv"
              dired-dwim-target t            ;; Try suggesting dired targets.
              dired-auto-revert-buffer t     ;; Update buffer when visiting.
              )

;; ;; Extensions to Dired. Includes more dired colors.
;; (use-package dired+)

;; Colorify files in dired based on type.
;; Use `diredful-add' and `diredful-edit' to make changes.
(use-package diredful
  :config (diredful-mode))

;; Expanded dired
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
(setq wdired-allow-to-change-permissions t)

;;; ERC settings


(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs")
        ("mozilla.org" "#rust")))
;; (erc :server "irc.mozilla.org" :port 6667 :nick "m-cat")

;; Notify in minibuffer when private messaged.
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

;;; Eshell settings

;; Supposed to stop auto-scrolling of eshell output.
;; TODO: Not even sure if this works.
(setq eshell-scroll-show-maximum-output nil
      eshell-scroll-to-bottom-on-input  nil
      eshell-scroll-to-bottom-on-output nil
      )

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-,")
                'eshell-previous-matching-input-from-input)
              (define-key eshell-mode-map (kbd "M-.")
                'eshell-next-matching-input-from-input)

              ;; Use helm to list eshell history.
              (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "M-{") 'eshell-previous-prompt)
              (define-key eshell-mode-map (kbd "M-}") 'eshell-next-prompt)
              ))

;; Open a new eshell buffer.
(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))
(global-set-key [f1] 'projectile-run-eshell)
(global-set-key [f2] 'eshell-new)

;; Add z to eshell
;; Jumps to most recently visited directories
(use-package eshell-z)
;; Add z to eshell.
;; Jumps to most recently visited directories.
(use-package eshell-z
  :defer 2)

;; Add up to eshell.
;; Jump to a directory higher up in the directory hierarchy.
(use-package eshell-up
  :defer 2
  :config (setq eshell-up-print-parent-dir nil))

;; Open certain programs from eshell in a term buffer
(add-hook 'eshell-load-hook #'(add-to-list 'eshell-visual-commands "ghci"))


;;; Load packages

;; Stop execution here for terminal.
;; We typically only want to open `emacs' in the terminal for quick editing.
(when (not (display-graphic-p))
  (with-current-buffer " *load*"
    (goto-char (point-max)))
  )

;; View PDF files in Emacs.
(use-package pdf-tools
  :config
  (pdf-loader-install))

;; Text separated by more than one space doesn't move.
(use-package dynamic-spaces
  :config
  (dynamic-spaces-global-mode))

;; A package for choosing a color by updating text sample.
;; See https://www.emacswiki.org/emacs/MakeColor.
(use-package make-color
  :defer t)

;; ;; Go to last change without undoing it
;; (use-package goto-last-change
;;   :bind ("C-?" . goto-last-change)
;;   )

;; Workspaces
(use-package eyebrowse
  :bind (("s-," . eyebrowse-prev-window-config)
         ("s-." . eyebrowse-next-window-config)
         ;; ("C-\"" . eyebrowse-create-window-config)
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
  ;; :init
  ;; (setq eyebrowse-keymap-prefix nil) ; Broken!
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-switch-back-and-forth nil)
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-close-window-config-prompt t)

  (setq eyebrowse-mode-line-separator " ")
  (setq eyebrowse-mode-line-left-delimiter "[ ")
  (setq eyebrowse-mode-line-right-delimiter " ]")

  (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :bold nil)
  )

;; Fix the capitalization commands.
(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)
         )
  )

;; Show unused keys.
(use-package free-keys
  :defer t)

;; Mode for writing.
(use-package olivetti
  :bind ("s-m" . olivetti-mode)
  :init
  (setq olivetti-hide-mode-line t
        olivetti-body-width     0.9
        )
  )

;; ;; Add indicators for position in buffer and end of buffer.
;; ;; Only load this for graphical displays (i.e. not the terminal).
;; ;; Buggy.
;; (when (display-graphic-p)
;;   (use-package indicators
;;     :diminish indicators-mode
;;     :hook ((prog-mode . new-indicators)
;;            (conf-mode . new-indicators)
;;            (text-mode . new-indicators)
;;            )
;;     :config
;;     (defun new-indicators ()
;;       "Create new indicators in the current buffer."
;;       (interactive)

;;       ;; ;; show an arrow at the end of buffer using the default fringe face
;;       ;; (ind-create-indicator 'point-max
;;       ;;                       :managed t
;;       ;;                       :relative nil
;;       ;;                       :fringe 'left-fringe
;;       ;;                       :bitmap 'right-arrow
;;       ;;                       :face 'fringe)

;;       ;; show relative position in the file (a.k.a. scroll bar)
;;       (ind-create-indicator 'point :managed t)
;;       )
;;     )
;;   )

;; Copy selected region to be pasted into Slack/Github/etc.
(use-package copy-as-format
  :defer t)

;; Jump to definitions using ag.
(use-package dumb-jump
  :bind (
         ;; ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g q" . dumb-jump-quick-look)
         )
  :config
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-prefer-searcher 'ag)
  )

;; ;; Line numbers.
;; (use-package nlinum
;;   :hook ((prog-mode . nlinum-mode)
;;          (conf-mode . nlinum-mode)
;;          (text-mode . nlinum-mode)
;;          )
;;   :config
;;   (setq linum-format "%3d") ;; Set linum format, minimum 3 lines at all times
;;   (setq nlinum-highlight-current-line t)
;;   )
;; ;; Fix line numbers occasionally not appearing.
;; (use-package nlinum-hl)

;; ;; Display current function in mode line. Sometimes doesn't work.
;; (use-package which-func
;;   :config
;;   (which-function-mode 1))

;; Highlight indentation.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-enabled nil
        )
  )

;; Automatically clean up extraneous whitespace.
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (
         (prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)
         ))

;; Save open files across Emacs sessions.
;; I use this instead of Desktop.el, which saves the entire session, as often
;; I want to start Emacs with fresh settings.
;; (use-package save-visited-files
;;   :config (turn-on-save-visited-files-mode))

;; Avy mode (jump to a char/word using a decision tree).
(use-package avy
  :bind (("C-," . avy-goto-line-end)
         ;; ("C-<" . avy-goto-char-in-line)
         ("C-." . avy-goto-char)
         ;; ("C->" . avy-goto-word-1)
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

  ;; QWERTY keys
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l
                      ?w ?e ?r ?u ?i ?o))
  ;; DVORAK keys
  ;; (setq avy-keys '(?p ?g ?c ?r
  ;;                     ?a ?o ?e ?u ?h ?t ?n ?s))

  ;; Set the background to gray to highlight the decision tree?
  (setq avy-background nil)
  )

;; Use a sensible mechanism for making buffer names unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 1)

;; Automatically save place in each file.
(use-package saveplace
  :config
  (save-place-mode 1)
  )

;; ;; Toggle the mode-line to save space.
;; (use-package hide-mode-line
;;   :config
;;   (defun hide-mode-line-toggle ()
;;     (interactive)
;;     (hide-mode-line-mode (if hide-mode-line-mode -1 +1))
;;     (unless hide-mode-line-mode
;;       (redraw-display)))
;;   (global-set-key (kbd "s-m") 'hide-mode-line-toggle)
;;   )

;; ;; Midnight mode - clean up buffers older than 3 days.
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; Highlight the parts of lines that exceed column 80.
(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))

  (defun c-whitespace-mode ()
    "Set whitespace column for c-like modes and turn on `whitespace-mode'."
    (setq whitespace-line-column 80
          fill-column 80
          )
    (whitespace-mode)
    )
  (add-hook 'c-mode-hook 'c-whitespace-mode)
  (add-hook 'c++-mode-hook 'c-whitespace-mode)
  (add-hook 'emacs-lisp-mode-hook 'c-whitespace-mode)
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

;; Display number of matches when searching.
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; Enable more powerful replace, plus Python regexps.
;; Notes: Enable expressions with C-c C-c
;;        You can use variables (like i, the match counter) in expressions
(use-package visual-regexp-steroids
  :bind (("C-`"   . vr/query-replace)
         ;; Hold down Alt for regexp-powered search
         ;; Keep the default search regexp-free for case insensitivity
         ("C-M-s" . vr/isearch-forward)
         ("C-M-r" . vr/isearch-backward)
         ("C-s"   . isearch-forward)
         ("C-r"   . isearch-backward)))

;; Fontify symbols representing faces with that face.
(use-package fontify-face
  :diminish fontify-face-mode
  :defer t
  )

;; Highlight color strings with the corresponding color.
(use-package rainbow-mode
  :diminish rainbow-mode
  :defer t
  )

;; ;; Highlight blocks based on depth. Buggy.
;; (use-package rainbow-blocks
;;   :bind ("s-r" . rainbow-blocks-mode)
;;   )

;; Highlight delimiters with colors depending on depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight numbers in code.
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Highlight more elisp syntax.
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Highlight operators.
(use-package highlight-operators
  :hook ((c-mode-common . highlight-operators-mode)
         (rust-mode     . highlight-operators-mode)
         )
  )

;; Highlight some recent changes such as undos.
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode))

;; Highlight surrounding parentheses.
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("cyan3")
        hl-paren-delay highlight-delay
        )
  )

;; Open current directory in Finder on Mac.
(use-package reveal-in-osx-finder
  :bind ("C-c f" . reveal-in-osx-finder)
  )

;; Display available keybindings in Dired mode (? creates popup).
(use-package discover)

;; ;; Temporarily "hide" other windows.
;; (use-package zygospore
;;   :bind ("M-o" . zygospore-toggle-delete-other-windows)
;;   )

;; Undo/redo window configurations.
(use-package winner
  :bind (
         ("C-c C-," . winner-undo)
         ("C-c C-." . winner-redo)
         )
  :config (winner-mode 1))

;; ;; Enable undo tree mode (C-x u)
;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))

;; Display available keys.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil))

;; Switch windows more easily.
(use-package winum
  :init
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
  (winum-mode)
  )

;; ;; Move buffers around.
;; (use-package buffer-move
;;   :bind (("<s-up>"    . buf-move-up)
;;          ("<s-down>"  . buf-move-down)
;;          ("<s-left>"  . buf-move-left)
;;          ("<s-right>" . buf-move-right)
;;          ))

;; ;; Make currently focused window larger.
;; ;; This package is not actively maintained.
;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :config
;;   (golden-ratio-mode)
;;   (setq golden-ratio-auto-scale nil)
;;   (setq golden-ratio-adjust-factor 0.80)
;;   (defadvice select-window-by-number
;;       (after golden-ratio-resize-window activate)
;;     (golden-ratio) nil)
;;   )

;; Expand-region.
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  ;; Fix region not highlighting.
  ;; See https://github.com/magnars/expand-region.el/issues/229
  (setq shift-select-mode nil)
  )

;; Wrap parentheses or quotes around word.
(use-package corral
  :bind (("M-(" . corral-parentheses-backward)
         ("M-)" . corral-parentheses-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward)
         ("M-{" . corral-braces-backward)
         ("M-}" . corral-braces-forward)
         ("M-`" . corral-backquote-forward)
         ("M-~" . corral-backquote-backward)
         ("M-'"  . corral-double-quotes-forward)
         ("M-\"" . corral-double-quotes-backward)
         )
  :config (setq corral-preserve-point t))

;; Better comment command.
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Synonym lookup.
(use-package powerthesaurus
  :defer t)

;; Imagemagick wrapper.
(use-package blimp
  :hook (image-mode . blimp-mode)
  )

;;; Git packages.

;; Git client in Emacs.
(use-package magit
  :diminish auto-revert-mode
  :bind ("C-x g" . magit-status)
  :bind (("C-x g" . magit-status)
         ("s-g"   . magit-status))
  :init
  (setq magit-diff-refine-hunk `all)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  )

;; Browse historic versions of a file.
(use-package git-timemachine
  :defer t)

;; Generate links to Github for current code location.
(use-package git-link
  :defer t)

;;; Project packages.

;; Project manager.
(use-package projectile
  :defer 1
  :hook (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'helm)
  )

(use-package helm-projectile
  :bind ("s-'" . helm-projectile)
  :config
  (helm-projectile-on))

;; Show markers in margin indicating changes.
(use-package diff-hl
  :bind (("C-c d" . diff-hl-revert-hunk)
         ("C-<"   . diff-hl-previous-hunk)
         ("C->"   . diff-hl-next-hunk)
         )
  ;; :hook (prog-mode . turn-on-diff-hl-mode)
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-margin-mode) ;; Show diffs in margin.
  ;; (diff-hl-flydiff-mode) ;; No need to save before seeing diffs.
  (diff-hl-dired-mode)   ;; See diffs in Dired.

  ;; Refresh diffs after a Magit commit.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

;; On-the-fly syntax checker.
(use-package flycheck
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (text-mode . flycheck-mode)
         )
  :commands flycheck-mode
  :bind ("C-!" . flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled save))
  ;; (setq flycheck-check-syntax-automatically nil)

  ;; Set shorter delay for displaying errors at point.
  (setq flycheck-display-errors-delay (* 3 info-delay))
  (setq sentence-end-double-space nil) ;; Stupid check.

  ;; Disable checkers.
  (setq-default flycheck-disabled-checkers '(rust rust-cargo rust-clippy))
  ;; (setq-default flycheck-disabled-checkers '(rust))

  (flycheck-add-mode 'proselint 'text-mode)
  (flycheck-add-mode 'proselint 'org-mode)
  (flycheck-add-next-checker 'markdown-mdl 'proselint)
  )

;; Elisp package lints.
(use-package flycheck-package
  :hook (flycheck-mode . flycheck-package-setup))

;; Company mode for auto-completion.
(use-package company
  :diminish company-mode
  :bind ("M-/" . company-complete)
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay nil)
  (setq company-tooltip-align-annotations t) ;; Align tooltips to right border.
  )

;; Yasnippet.
;; NOTE: list all snippets for current mode with M-x `yas-describe-tables'.
(use-package yasnippet-snippets)
(use-package yasnippet
  :requires yasnippet-snippets
  :diminish yas-minor-mode
  :config
  (yas-global-mode)

  (setq yas-triggers-in-field t) ; Enable nested triggering of snippets
  (setq yas-verbosity 1) ;; No need to be so verbose
  )

;;; Language packages

;; Clojure

(use-package clojure-mode
  :defer t)

(use-package flycheck-clojure
  :defer t)

(use-package cider
  :after clojure-mode
  :config
  (setq cider-use-overlays nil)
  )

;; Git

(use-package gitignore-mode
  :defer t)

;; Haskell

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (define-key haskell-mode-map (kbd "C-#") 'haskell-process-load-or-reload)
  ;; Use hoogle in Haskell buffers.
  ;; Supply prefix arg for full info (C-u C-c h)
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)

  (setq haskell-indentation-layout-offset 4
        haskell-indentation-left-offset   4
        haskell-indentation-ifte-offset   4
        haskell-hoogle-command "hoogle")
  )

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))

;; ;; Completions for Haskell
;; ;; FIXME: Doesn't get loaded...
;; (use-package company-ghc
;;   :after company
;;   :init
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   :config
;;   (add-to-list 'company-backends 'company-ghc))

;; ;; Haskell snippets
;; (use-package haskell-snippets
;;   :after yasnippet)

;; Javascript

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (define-key js-mode-map (kbd "M-.") 'dumb-jump-go)
  )

;; Javascript REPL
(use-package nodejs-repl
  :defer t
  :config
  (define-key js-mode-map (kbd "C-M-x")   'nodejs-repl-send-buffer)
  (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
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

(use-package markdown-mode
  :mode "\\.md\\'"
  )
(use-package markdown-toc
  :defer t)

;; On-the-fly markdown preview. M-x flymd-flyit
(use-package flymd
  :defer t)

;; Mode for JIRA-markup formatted text.
(use-package jira-markup-mode
  :defer t)

;; Nim

(use-package nim-mode
  :hook (nim-mode . nimsuggest-mode)
  :init
  (setq nim-nimsuggest-path "~/.nim/bin/nimsuggest")
  ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode...
  :config
  (define-key nim-mode-map (kbd "RET") #'newline-and-indent)
  )

;; Rust

(use-package rust-mode
  :mode "\\.rs\\'"
  :diminish eldoc-mode
  :init
  (setq rust-format-on-save nil)
  :config
  (define-key rust-mode-map (kbd "C-c n") #'rust-format-buffer)
  )

(use-package racer
  :diminish racer-mode
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)
         )
  :init
  :config
  (define-key racer-mode-map (kbd "<mouse-2>") 'mouse-find-definition)
  (define-key racer-mode-map (kbd "<mouse-3>") 'pop-tag-mark)

  (defun mouse-find-definition (@click)
    (interactive "e")
    (let ((p1 (posn-point (event-start @click))))
      (goto-char p1)
      (call-interactively 'racer-find-definition)
      )
    )
  )

;; ;; Run cargo commands in rust buffers, e.g. C-c C-c C-r for cargo-run
;; (use-package cargo
;;   :diminish cargo-minor-mode
;;   :hook ((rust-mode . cargo-minor-mode)
;;          (toml-mode . cargo-minor-mode)
;;          )
;;   )

;; TOML

(use-package toml-mode
  :mode "\\.toml\\'"
  )

;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'")

;;; Org Mode

(defun org-mode-hook-fun ()
  "Initialize `org-mode'."
  (visual-line-mode) ;; Word-wrap
  (toggle-word-wrap t)
  (org-indent-mode) ;; Indented entries
  (local-unset-key (kbd "C-,")) ;; Unbind keys stolen by org-mode
  ;; Add a buffer-local hook.
  ;; (add-hook 'after-save-hook 'org-agenda-refresh nil 'make-it-local)
  )

(use-package org
  :hook (org-mode . org-mode-hook-fun)
  :diminish visual-line-mode
  :diminish org-indent-mode

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

  ;; Tags
  (setq org-tags-column 0)

  ;; All subtasks must be DONE before marking a task as DONE.
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time)) ;; Log time a task was set to DONE.
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)

  ;; M-RET should not split the heading if point is not at the end of a line.
  ;; (setq org-M-RET-may-split-line nil)

  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

  ;; Custom to-do states.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELED(x)")))

  ;; Refresh org-agenda after changing an item status.
  ;; (add-hook 'org-trigger-hook 'org-agenda-refresh)
  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh))

  ;; Set location of agenda files.
  (setq org-agenda-files (list user-todo-location
                               ;; user-work-location
                               ))
  ;; Stop org-agenda from messing up my windows!!
  (setq org-agenda-window-setup 'current-window)
  ;; Start org-agenda from the current day.
  (setq org-agenda-start-on-weekday nil)

  ;; Org-refile settings

  ;; `org-refile' notes to the top of the list.
  (setq org-reverse-note-order t)
  ;; Use headline paths (level1/level2/...)
  (setq org-refile-use-outline-path t)
  ;; Go down in steps when completing a path.
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                             (user-notes-location . (:maxlevel . 9))
                             (user-dreams-location . (:maxlevel . 9))
                             ))
  ;; Jump to headings with completion.
  (setq org-goto-interface 'outline-path-interface
        org-goto-max-level 10)

  ;; org-capture template.
  (defvar org-capture-templates
    '(("t" "My TODO task format." entry
       (file+headline "todo.org" "General")
       "* %?\nSCHEDULED: %t")
      ("n" "My note format." entry
       (file "notes.org")
       "* %?")))

  ;; Shortcuts/Keybindings

  (defun org-done ()
    "Change task status to DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree)
    )
  (defun org-agenda-done ()
    "In org-agenda, change task status to DONE and archive it."
    (interactive)
    (let ((heading (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (cond ((string-match "\\[\\(.*\\)\\]" heading)
             (org-agenda-schedule nil (match-string 1 heading)))
            (t
             (org-agenda-todo 'done)
             (org-agenda-archive)
             )))
    )

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

  (define-key org-mode-map (kbd "<M-return>") 'org-meta-return-end)
  (define-key org-mode-map (kbd "C-S-n") 'org-metadown)
  (define-key org-mode-map (kbd "C-S-p") 'org-metaup)
  (define-key org-mode-map (kbd "C-<")   'org-shiftmetaleft)
  (define-key org-mode-map (kbd "C->")   'org-shiftmetaright)
  (define-key org-mode-map (kbd "M-m")   'org-beginning-of-line)

  (define-key org-mode-map (kbd "<mouse-3>") 'mouse-org-cycle)

  (define-key org-mode-map (kbd "C-c t") 'org-done)
  (define-key org-mode-map (kbd "s-;")   'org-refile-goto)
  (define-key org-mode-map (kbd "s-:")   'org-refile)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map (kbd "C-c t") 'org-agenda-done)
              (define-key org-agenda-mode-map (kbd "d")     'org-agenda-done)
              (visual-line-mode)
              (toggle-word-wrap t)
              ))

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda-list)  ;; Switch to org-agenda.

  (global-set-key (kbd "C-c c") 'org-note-capture) ;; org-capture.
  (global-set-key (kbd "C-c v") 'org-task-capture)

  ;; Jump to last capture.
  (global-set-key (kbd "C-c j") 'org-refile-goto-last-stored)
  )

;; Open .org files in org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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
         (org-agenda-tag-filter-while-redo (or tag-filter tag-preset))
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

(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)
        ))))

;; Export org to Reveal.js.
(use-package ox-reveal
  :config
  (setq org-reveal-hlevel 1)
  (setq org-reveal-title-slide "<h1>%t<h2><br>%a<br><br>%d")
  )

;;; Final

;; Set mode-line format.
;; This should run after (winum-mode).

;; Count total lines in buffer.
;; From https://stackoverflow.com/a/8191130.
(defvar mode-line-buffer-line-count nil)
(make-variable-buffer-local 'mode-line-buffer-line-count)

(defun mode-line-count-lines ()
  "Count the total number of lines in the current buffer."
  (setq mode-line-buffer-line-count
        (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'mode-line-count-lines)
(add-hook 'after-save-hook 'mode-line-count-lines)
(add-hook 'after-revert-hook 'mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'mode-line-count-lines)
(add-hook 'after-change-major-mode-hook 'mode-line-count-lines)

;; Active window detection.
;; From https://emacs.stackexchange.com/a/26345.
(defvar mode-line-selected-window nil)

(defun mode-line-record-selected-window ()
  "Record the current window as selected."
  (setq mode-line-selected-window (selected-window)))
(add-hook 'post-command-hook 'mode-line-record-selected-window)

(defun mode-line-update-all ()
  "Update all mode lines."
  (force-mode-line-update t))
(add-hook 'buffer-list-update-hook 'mode-line-update-all)

;; For right-aligning.
;; From https://stackoverflow.com/a/22971471.
(defun mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize
   " "
   'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
   ))

;; Set the mode-line.
(setq-default
 mode-line-format
 '((:eval
    (list
     " ["
     '(:eval (winum-get-number-string))
     "] "
     'mode-line-modified
     " "
     '(:eval (propertize "%b"
                         'face '(:weight bold)
                         'help-echo (buffer-file-name)))
     '(:eval (when line-number-mode ":%l:%C"))
     " "
     '(:eval (when (and line-number-mode mode-line-buffer-line-count buffer-file-name)
               (let ((str "["))
                 (when (buffer-modified-p)
                   (setq str (concat str "*")))
                 (setq str (concat str mode-line-buffer-line-count "]"))
                 str)))
     '(:eval (when buffer-file-name (let ((str "["))
                                      (when (buffer-modified-p)
                                        (setq str (concat str "*")))
                                      (setq str (concat str "%I]"))
                                      str)))
     '(:eval (when (not (string-equal major-mode 'org-agenda-mode))
               (propertize "[%m] "
                           ;; 'face '(:weight bold)
                           'help-echo (format "%s" major-mode)
                           )))
     '(:eval (when buffer-read-only
               (propertize "RO "
                           'face 'font-lock-preprocessor-face
                           'help-echo "Buffer is read-only")))
     '(:eval
       (when mark-active
         (concat "{" (number-to-string (abs (- (point) (mark)))) "}")))
     " "

     '(:eval (when (fboundp 'eyebrowse-mode-line-indicator)
               (concat
                (mode-line-fill (+ 1 (length (substring-no-properties
                                              (eyebrowse-mode-line-indicator)))))
                (eyebrowse-mode-line-indicator))))
     ;; " "
     ;; '(:eval (propertize (format-time-string "%H:%M")))
     ))))

;; Misc

;; Display notes.org and Org Agenda on startup.
(defun emacs-welcome()
  "Display Emacs welcome screen."
  (interactive)
  (find-file user-notes-location)
  (other-window 1)
  (split-window-right-focus)
  (org-agenda-list)
  )
(emacs-welcome)

(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
