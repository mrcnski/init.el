;;; .emacs --- Emacs configuration file

;; Copyright (C) 2017 Marcin Swieczkowski

;;; Commentary:

;; Making changes:
;; Use bug-hunter-init-file to locate errors
;; Use esup to profile startup time
;; Use C-c r to reload or restart-emacs to restart after making changes

;; I prefer to explicitly define functions when I could use lambdas instead.
;; Defining functions makes them more discoverable in many situations

;;; TODO:

;; * Some code snippets are in the wrong section

;;; Code:

;;; Initialize initialization

;; First things first, increase GC threshold to speed up startup.
;; Reset the GC threshold after initialization, and GC whenever we tab out
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'focus-out-hook 'garbage-collect)

;; First things first, define the init file location and make it easy to reload
(defvar init-file-location (concat user-emacs-directory "init.el"))

;; Reload init file
(defun reload-init-file ()
  "Reload the init file."
  (interactive)
  (load-file init-file-location))
(global-set-key (kbd "C-c r") 'reload-init-file)

;; Open .emacs init
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file init-file-location))
(global-set-key (kbd "C-c i") 'open-init-file)

;;; Package settings

(require 'package)
;; Explicitly enable packages
(setq package-enable-at-startup nil)
;; Add package sources (use package-refresh-contents)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
;;(package-refresh-contents)

;; Run auto-load functions specified by package authors
(package-initialize)

;; Require use-package
(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Enable restarting Emacs from within Emacs
(use-package restart-emacs)

;; Diminish modeline clutter
(use-package diminish)

;;; Initialize Helm

(require 'helm-config)
(use-package helm
  :diminish helm-mode
  :config (helm-mode 1))

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-h C-a") 'helm-apropos)
(setq helm-apropos-fuzzy-match t)

(global-set-key (kbd "C-x C-SPC") 'helm-all-mark-rings)

;; Jump to a function definition
(semantic-mode 1)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
;;(global-set-key (kbd "M-i") #'imenu-anywhere)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `use-package' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

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
                                  helm-source-files-in-all-dired
                                  helm-source-buffer-not-found))

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

;; Better mode help
(use-package helm-describe-modes
  :bind ("C-h m" . helm-describe-modes))

;;; Helm-swoop
(use-package helm-swoop)
(global-set-key (kbd "C-;") 'helm-swoop-without-pre-input)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-:") 'helm-multi-swoop-all)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "C-;") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "C-;") 'helm-multi-swoop-all-from-helm-swoop)

(setq helm-swoop-speed-or-color t) ;; Show syntax highlighting in results

;; Silver Searcher interface with helm
(use-package helm-ag)

;;; Load customizations

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file t)

;;; Quality of life changes

;; Enable ido mode if helm's not there
(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

;; Turn off blinking cursor
(blink-cursor-mode 0)
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point
(setq x-stretch-cursor 1)

;; Disable scroll bars and the tool bar
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
;; Enable tooltips in the echo area
(tooltip-mode nil)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Use a sensible mechanism for making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Automatically save place in each file
(use-package saveplace)
(setq-default save-place t)

;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(setq isearch-allow-scroll t)
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

(fset 'yes-or-no-p 'y-or-n-p)        ;; Replace yes/no prompts with y/n
(put 'downcase-region 'disabled nil) ;; Enable downcase-region
(put 'upcase-region 'disabled nil)   ;; Enable upcase-region
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil
              tab-width 4)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline nil
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      window-combination-resize t
      save-place-file (concat user-emacs-directory "places")
      echo-keystrokes 0.01  ;; Display keystrokes immediately
      inhibit-startup-message t ;; Disable startup screen
      initial-scratch-message "Welcome to Emacs!\n" ;; Change the initial *scratch* buffer
      help-window-select t ;; Focus new help windows when opened
      confirm-kill-emacs 'yes-or-no-p ;; Always confirm before closing Emacs
      delete-by-moving-to-trash t ;; Send deleted files to trash
      backup-directory-alist `(("." .
                                ,(concat user-emacs-directory "backups")))
      version-control t      ;; Always make numeric backup versions
      vc-make-backup-files t ;; Make backups of all files
      delete-old-versions t) ;; Silently delete old backup versions)

(global-subword-mode t) ;; Recognize camelCase
(diminish 'subword-mode)
(global-hl-line-mode t) ;; Highlight current line
(setq global-hl-line-sticky-flag t) ;; Keep highlight across windows
(auto-compression-mode 1) ;; Use compressed files like normal files

;; Turn on utf-8 by default
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; User-Defined Variables

(defvar user-todo-location "~/Text/org/todo.org")
(defvar user-notes-location "~/Text/org/notes.org")

;;; Load visual settings

;; Load Theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
;; (load-theme 'paganini t)
;; (use-package afternoon-theme)
;; (load-theme 'afternoon t)
(use-package ample-theme)
(load-theme 'ample)
;; (use-package cyberpunk-theme)
;; (load-theme 'cyberpunk)

;; Set font
(defun font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font)) nil t))

(cond
 ;; ((font-exists-p "Iosevka Slab")
 ;;  (set-face-attribute 'default nil :font "Iosevka Slab"))
 ((font-exists-p "Iosevka Medium")
  (set-face-attribute 'default nil :font "Iosevka Medium"))
 ((font-exists-p "Hack")
  (set-face-attribute 'default nil :font "Hack"))
 ((font-exists-p "DejaVu Sans Mono")
  (set-face-attribute 'default nil :font "DejaVu Sans Mono")))

;;; Dired settings

;; Handle opening and editing zip directories in dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-listing-switches "-alhv" ;; Make sizes human-readable by default and put dotfiles and capital-letters first.
              dired-recursive-copies 'always ;; Always do recursive copies
              dired-dwim-target t            ;; Try suggesting dired targets
              dired-auto-revert-buffer t     ;; Update buffer when revisiting
              )

;; Hide file details in Dired
(use-package dired-details+
  :config (setq dired-details-propagate-flag t))

;; Colorify files in dired based on type
(use-package diredful
  :config (diredful-mode))

;; Expanded dired
;; Enables C-x C-j to jump to the current directory in dired
(require 'dired-x)

;; Make C-l go up a directory in dired
(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)

;; Allow changing file permissions in WDired
;; Notes: WDired can be enabled with C-x C-q and compiled with C-c C-c
(setq wdired-allow-to-change-permissions t)

;;; Eshell options

;; Open a new eshell buffer
(defun eshell-new ()
  "Open a new eshell buffer."
  (interactive)
  (eshell t))
(global-set-key [f1] 'eshell)
(global-set-key [f2] 'eshell-new)

;; Add z to eshell
;; Jumps to most recently visited directories
(use-package eshell-z)

;; Add up to eshell
;; Jump to a directory higher up in the directory hierarchy
(use-package eshell-up
  :config (setq eshell-up-print-parent-dir t))

;; Open certain programs from eshell in a term buffer
(add-hook 'eshell-load-hook #'(add-to-list 'eshell-visual-commands "ghci"))

;;; Load packages

;; setup selected file endings to open in certain modes
(add-to-list 'auto-mode-alist '("\\.hdl\\'" . c-mode))

;; Improved mode line
(use-package powerline
  ;; :disabled
  :config (powerline-default-theme))
(use-package spaceline-config
  :disabled
  :config (spaceline-emacs-theme))

;; Midnight mode - clean up buffers older than 3 days
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; Open files in existing frames
(setq pop-up-frames nil
      pop-up-windows t)

;; Persistent scratch
(use-package persistent-scratch
  :disabled
  :config (persistent-scratch-setup-default))

;; Neotree directory tree
(use-package neotree
  ;; :disabled
  :bind ("C-x C-d" . neotree-toggle)
  :config (setq neo-smart-open t))

;; Display number of matches when searching
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; Make marks visible
(use-package visible-mark
  :config (global-visible-mark-mode t))

;; ;; Make it easier to find the cursor
;; (use-package beacon
;;   :diminish beacon-mode)
;; (setq beacon-size 20
;;       beacon-color "#006666"
;;       beacon-blink-delay .15
;;       beacon-blink-duration .3)
;; (beacon-mode t)

;; Nyan mode
;; (use-package nyan-mode)
;; (nyan-mode)

;; Highlight color strings with the corresponding color
(use-package rainbow-mode
  :diminish rainbow-mode
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

;; Highlight delimiters with colors depending on depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ;; Always keep the cursor centered
;; (use-package centered-cursor-mode)
;; (global-centered-cursor-mode)

;; Track recently-opened files
(use-package recentf
  :config
  (setq recentf-max-saved-items 10000) ;; Go ahead and save everything
  (recentf-mode t))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

;; Isearch convenience, space matches anything (non-greedy)
;; (setq search-whitespace-regexp ".*?")

;; ;; Keep open files open across sessions
;; (desktop-save-mode 1)

;; Display the column number
(column-number-mode 1)

;; ;; Display the current time
;; (display-time-mode 1)
;; (setq display-time-format "%l:%M%p")

;; Replace any selected text with what you are typing or pasting
(delete-selection-mode 1)

;; Turn on line numbers only in programming modes
;; (global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

;; Resize the minibuffer when needed. Enable recursive editing of minibuffer
(setq resize-mini-windows t
      enable-recursive-minibuffers t)
;; (setq max-mini-window-height 0.33)

;; Change window name to be more descriptive
(setq frame-title-format
      '("Emacs - " (buffer-file-name "%f"
                                     (dired-directory dired-directory "%b"))))

;; Undo/redo window configurations
;; Default keys are C-c left and C-c right
(use-package winner
  :defer 1
  :config (winner-mode 1))

;; Smooth scrolling (always a line at a time)
(setq scroll-step 1)
;;scroll-conservatively  10000)

;; Start scrolling near the edge of the screen
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 10))

;; Enable undo tree mode (C-x u)
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Display available keys
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil))

;; Highlight some recent changes such as undos
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode))

;; Highlight matching parentheses around point
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; Highlight numbers in code
(use-package highlight-numbers
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; ;; C-n at the end of the buffer inserts newlines
;; (setq next-line-add-newlines t)

;; Clean up whitespace when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Tab will first try to indent, then always complete
(setq tab-always-indent 'complete)

;; Automatically save on loss of focus
(defun save-all ()
  "Automatically save all file-visiting buffers when Emacs loses focus."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; Auto-focus help buffers and allow exiting with C-g
(use-package popwin
  :config (popwin-mode 1))

;; ;; Aggressive indent mode
;; (use-package aggressive-indent
;;   :init (add-hook 'prog-mode-hook #'aggressive-indent-mode))

;;; Misc Bindings

;; Select the current line
(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))
;; Replace default C-l, it's useless
(global-set-key (kbd "C-l") 'select-current-line)

;; Improved kill-whole-line which doesn't change cursor position
;; TODO: this currently saves the lines in the kill ring in reverse order
(defun annihilate-line ()
  "Annihilate the current line by killing it, deleting the empty line, and restoring cursor position. If called on a region, will annihilate every line included in the region."
  (interactive)
  (let ((col (current-column)))
    (cond
     ;; No region selected
     ((not (region-active-p))
      (kill-whole-line))

     ;; There is an active region
     (t
      (let ((beg (region-beginning))
            (end (region-end))
            (done nil))
        (goto-char end)
        (while (not done)
          (kill-whole-line)
          (forward-line -1)
          (if (< (line-end-position) beg)
              (setq done t)))
        (forward-line))))

    ;; Restore column position
    (move-to-column col)))

(global-set-key (kbd "C-S-k") 'annihilate-line)

;; Reload the current buffer from disk
(global-set-key [f5] 'revert-buffer)

;; Previous/next buffers
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-c f") 'next-buffer)

;; Increase/decrease text size
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

;; Go to line
(global-set-key (kbd "M-g") 'goto-line)

;; Rebind query-replace
(global-set-key (kbd "C-`") 'query-replace)

;; Make switching windows better
(use-package switch-window
  :bind ("M-o" . switch-window))
(use-package window-numbering
  :config (window-numbering-mode))

;; Expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Easily open info-display-manual
(global-set-key (kbd "C-h I") 'info-display-manual)

;; On-the-fly syntax checker
(use-package flycheck
  :defer t
  :init
  ;; (global-flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (setq sentence-end-double-space nil)
  :commands flycheck-mode
  :bind ("C-!" . flycheck-list-errors)
  :config (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; Company mode for auto-completion
(use-package company
  :diminish company-mode
  :bind ("M-/" . company-complete)
  :init (add-hook 'after-init-hook #'global-company-mode)
  :config (setq company-idle-delay nil))

;; ;; Display tooltips for company completion candidates
;; (use-package company-quickhelp
;;   :bind ("C-c h" . company-quickhelp-manual-begin)
;;   :config (company-quickhelp-mode 1))

;; Haskell mode
(use-package haskell-mode
  :defer t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              ;; (ghc-init)
              (define-key haskell-mode-map (kbd "C-#")   'haskell-process-load-or-reload)
              ;; Use hoogle in Haskell buffers. Supply prefix arg for full info (C-u C-c h)
              (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
              ))
  :config
  (setq haskell-indentation-layout-offset 4
        haskell-indentation-left-offset   4
        haskell-indentation-ifte-offset   4
        haskell-hoogle-command "hoogle"))

;; ;; Completions for Haskell
;; ;; TODO: doesn't get loaded...
;; (use-package company-ghc
;;   :after company
;;   :init
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   :config
;;   (add-to-list 'company-backends 'company-ghc))

;; Yasnippet
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

;; Haskell snippets
(use-package haskell-snippets
  :after yasnippet)

;; Git client in Emacs
(use-package magit
  :bind ("C-x g" . magit-status))

;; (use-package paredit)
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)

;; (use-package parinfer
;;   :ensure t
;;   :bind ("C-'" . parinfer-toggle-mode)
;;   :init
;;   (progn
;;     ;; (setq parinfer-extensions
;;     ;;       '( defaults       ; should be included.
;;     ;;          ;; pretty-parens  ; different paren styles for different modes.
;;     ;;          ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;     ;;          ;; paredit        ; Introduce some paredit commands.
;;     ;;          ;; smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;     ;;          ;; smart-yank   ; Yank behavior depend on mode.
;;     ;;          ))
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; (use-package lispy
;;  :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; TODO: This seems to be buggy in org-mode
(defun cleanup-empty-lines ()
  "Remove all empty lines from the buffer."
  (interactive)
  (save-excursion
    (flush-lines "^$" (point-min) (point-max))))
(global-set-key (kbd "C-c m") 'cleanup-empty-lines)

;; Delete current buffer file
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
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Rename current buffer file
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

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Move current line up or down
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
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

;; Join the following line onto the current line
;; Use this to quickly consolidate multiple lines into one
(defun join-next-line ()
  "Join the following line onto the current line, preserving the cursor position.  This command can be used to rapidly consolidate multiple lines into one."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))
(global-set-key (kbd "M-j") 'join-next-line)

;; Scroll down/up by a smaller amount, doesn't change cursor position
(defun scroll-up-line-quick ()
  "Scroll up by a smaller amount without changing the cursor position."
  (interactive)
  (scroll-up-line 5))
(defun scroll-down-line-quick ()
  "Scroll down by a smaller amount without changing the cursor position."
  (interactive)
  (scroll-down-line 5))
(global-set-key (kbd "M-n") 'scroll-up-line-quick)
(global-set-key (kbd "M-p") 'scroll-down-line-quick)

;; Scroll other window down/up
(defun scroll-other-window-up-quick ()
  "Scroll the other window up by a smaller amount."
  (interactive)
  (scroll-other-window 5))
(defun scroll-other-window-down-quick ()
  "Scroll the other window down by a smaller amount."
  (interactive)
  (scroll-other-window-down 5))
(global-set-key (kbd "M-N") 'scroll-other-window-up-quick)
(global-set-key (kbd "M-P") 'scroll-other-window-down-quick)

;; Open a new line below or above, even if the point is midsentence
(defun open-line-below ()
  "Open a new line below, even if the point is midsentence."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun open-line-above ()
  "Open a new line above, even if the point is midsentence."
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Wrap parentheses or quotes around word
(use-package corral
  :bind (("M-9" . corral-parentheses-backward)
         ("M-0" . corral-parentheses-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward)
         ("M-{" . corral-braces-backward)
         ("M-}" . corral-braces-forward)
         ("M-\"" . corral-double-quotes-backward))
  :config (setq corral-preserve-point t))

;; More powerful (way better) comment command
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Align region by character.
;; TODO: Enable history in read-string to allow for default values (i.e. last input)
(defun align-to-string (beg end)
  "Align region along character CHAR from BEG to END."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))
(global-set-key (kbd "M-=") 'align-to-string)

;; Avy mode (jump to a char/word using a decision tree)
(defun avy-goto-line-end ()
  "Jump to a line using avy and go to the end of the line."
  (interactive)
  (avy-goto-line)
  (end-of-line))

(use-package avy
  :bind (("C-," . avy-goto-line-end)
         ("C-<" . avy-goto-char-in-line)
         ("C-." . avy-goto-char)
         ("C->" . avy-goto-word-1))
  :config
  ;; Use more characters (and better ones) in the decision tree
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l
                      ?w ?e ?r ?u ?i ?o))
  (setq avy-background t) ;; Set the background to gray to highlight the decision tree
  )

;; Commands to split window and move focus to other window
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (other-window 1))
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (other-window 1))

;; Remap the default window-splitting commands to the ones above
(global-set-key (kbd "C-x 3") 'split-window-right-focus)
(global-set-key (kbd "C-x 2") 'split-window-below-focus)

;; Show ascii table
;; Obtained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun ascii-table ()
  "Print the ascii table.  Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;;; Org Mode

(use-package org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ; Open .org files in org-mode

;; The ellipsis to use in the org-mode outline
(setq org-ellipsis " (...)")

(setq org-enforce-todo-dependencies t) ; All subtasks must be DONE before marking a task as DONE
(setq org-log-done (quote time)) ; Log the time a task was set to DONE
(setq org-log-redeadline (quote time)) ; Log the time a task's deadline changed
(setq org-log-reschedule (quote time)) ; Log the time a task was rescheduled

(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Org-mode word-wrap
(add-hook 'org-mode-hook #'
          (lambda ()
            (visual-line-mode)
            (org-indent-mode)
            (toggle-word-wrap)))

;; Unbind keys stolen by org-mode
(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-,"))))

;; Custom to-do states
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(x)")))

;; Set location of agenda files
(setq org-agenda-files '("~/Text/org/todo.org"))

;; Org-refile settings
(setq org-reverse-note-order t) ; org-refile notes to the top of the list
(setq org-refile-use-outline-path t) ; Use headline paths (level1/level2/level3...)
(setq org-outline-path-complete-in-steps nil) ; Go down in steps when completing a path

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))
                           ("~/Text/org/notes.org" . (:maxlevel . 9))))

;; org-capture template
(defvar org-capture-templates
  '(("t" "My TODO task format." entry
     (file+headline "todo.org" "Todo List")
     "* TODO %?\nSCHEDULED: %t")
    ("n" "My note format." entry
     (file "notes.org")
     "* %?")))

;;; Shortcuts

(global-set-key (kbd "C-c <up>") 'outline-up-heading)
(global-set-key (kbd "C-c <left>") 'outline-previous-visible-heading)
(global-set-key (kbd "C-c <right>") 'outline-next-visible-heading)

;; (global-set-key (kbd "C-c t") 'org-show-todo-tree) ; Show all todo tasks

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file user-notes-location)))
(global-set-key (kbd "C-c p")
                (lambda () (interactive) (find-file user-todo-location)))

(global-set-key (kbd "C-c a") 'org-agenda-list) ; Switch to org-agenda

;; org-capture with template as default behavior
(defun org-task-capture ()
  "Capture a task with my todo template."
  (interactive)
  (org-capture nil "t"))

(defun org-note-capture ()
  "Capture a note with my note template."
  (interactive)
  (org-capture nil "n"))

(global-set-key (kbd "C-c c") 'org-task-capture) ; org-capture
(global-set-key (kbd "C-c v") 'org-note-capture)

(global-set-key (kbd "C-c j") 'org-refile-goto-last-stored) ; jump to last capture

;;; Final
(setq default-directory "~/") ; Default directory
(setq org-directory "~/Text/org") ; Default org directory

;;(find-file user-todo-list-location) ; Start with notes.org
;;(org-agenda nil "a") ; Open org-agenda

(provide 'init)
;;; init.el ends here
