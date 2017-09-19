;;; .emacs --- Emacs configuration file
;;
;; Copyright (C) 2017 Marcin Swieczkowski
;;
;;; Commentary:
;;
;; Requires Emacs 25.
;;
;; Making changes / testing:
;;
;; Use M-x free-keys to find unused keybindings.
;; Use M-x bug-hunter-init-file to locate errors.
;; Use M-x esup to profile startup time,
;; M-x profiler-start and profiler-report to profile runtime.
;; Use restart-emacs to restart after making changes.
;; To stop execution of this file at some point, put in (error "Done").
;;
;; I prefer to explicitly define functions instead of using lambdas.
;; Defining named functions makes them more discoverable in many situations.

;;; Code:

;;; Initialize initialization

;; First things first, increase GC threshold to speed up startup.
;; Reset the GC threshold after initialization, and GC whenever we tab out
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 20000000)))
(add-hook 'focus-out-hook 'garbage-collect)

;; First things first, define the init file location and make it easy to reload
(defvar init-file-location   (concat user-emacs-directory "init.el"))
(defvar packages-location    (concat user-emacs-directory "packages/nimbus"))
(defvar scratchpad-location  "~/Text/scratchpad.txt")

;; ;; Reload init file
;; (defun reload-init-file ()
;;   "Reload the init file."
;;   (interactive)
;;   (load-file init-file-location))
;; (global-set-key (kbd "C-c r") 'reload-init-file)

;; Open .emacs init
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file init-file-location))
(global-set-key (kbd "C-c i") 'open-init-file)

;; Open scratchpad.txt
(defun open-scratchpad-file ()
  "Open scratchpad file."
  (interactive)
  (find-file scratchpad-location))
(global-set-key (kbd "C-c s") 'open-scratchpad-file)

;;; Package settings

;; Add "packages" folder to load-path
(add-to-list 'load-path packages-location)

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
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Enable restarting Emacs from within Emacs
(use-package restart-emacs)

;; Diminish modeline clutter
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
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action) ;; list actions using C-z
(define-key helm-map (kbd "M-x") 'helm-select-action)

(global-set-key (kbd "M-x")       'helm-M-x)
(global-set-key (kbd "C-x C-m")   'execute-extended-command)
(global-set-key (kbd "C-x b")     'helm-mini)
(global-set-key (kbd "C-x C-f")   'helm-find-files)
(global-set-key (kbd "C-h C-a")   'helm-apropos)
(global-set-key (kbd "C-x C-SPC") 'helm-all-mark-rings)
(global-set-key (kbd "M-i")       'helm-semantic-or-imenu)

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
                                  helm-source-buffer-not-found))

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 40)
(helm-autoresize-mode 1)

;; Better mode help
(use-package helm-describe-modes
  :bind ("C-h m" . helm-describe-modes))

;;; Helm-swoop
(use-package helm-swoop
  :bind (("C-;" . helm-swoop-without-pre-input)
         ("C-:" . helm-multi-swoop-all))
  :config
  ;; Move up and down like isearch
  (define-key helm-swoop-map        (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map        (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map  (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map  (kbd "C-s") 'helm-next-line)

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "C-;") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "C-;") 'helm-multi-swoop-all-from-helm-swoop)

  (setq helm-swoop-speed-or-color t) ;; Show syntax highlighting in results
  )

;; ggtags with helm
(use-package helm-gtags
  :diminish helm-gtags-mode
  :init
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook  'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook      'helm-gtags-mode)
  (add-hook 'c++-mode-hook    'helm-gtags-mode)
  (add-hook 'asm-mode-hook    'helm-gtags-mode)
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
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  )

;; ag with helm
(use-package helm-ag
  :init
  (setq helm-ag-insert-at-point 'symbol)
  )

;;; Load customizations

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file t)

;;; Quality of life changes

;; Keep directories clean
(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

;; Track recently-opened files
(use-package recentf
  :config
  (setq recentf-max-saved-items 10000) ;; Go ahead and save everything
  (recentf-mode t)
  )

;; Enable ido mode if helm's not there
(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

;; Turn on blinking/flashing cursor
(blink-cursor-mode 1)
(when (display-graphic-p)
  (setq-default cursor-type 'bar))
;; Stretch cursor to be as wide as the character at point
(setq x-stretch-cursor 1)

;; Disable scroll bars and the tool bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode 0))

(toggle-frame-maximized) ;; Maximize!
;; (toggle-frame-fullscreen) ;; Maximize MORE

;; Enable popup tooltips, use emacs tooltip implementation
(tooltip-mode nil)
(setq x-gtk-use-system-tooltips nil)

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
      kill-ring-max 1000
      require-final-newline t    ;; Ensure that files end with a newline
      next-line-add-newlines t   ;; add newline at end of buffer with C-n
      visible-bell t
      ring-bell-function 'ignore
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      window-combination-resize t
      echo-keystrokes 0.01            ;; Display keystrokes immediately
      inhibit-startup-message t       ;; Disable startup screen
      initial-scratch-message ""      ;; Change the initial *scratch* buffer
      help-window-select t            ;; Focus new help windows when opened
      confirm-kill-emacs nil          ;; Always confirm before closing Emacs
      delete-by-moving-to-trash t     ;; Send deleted files to trash
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      version-control t               ;; Always make numeric backup versions
      vc-make-backup-files t          ;; Make backups of all files
      delete-old-versions t           ;; Silently delete old backup versions
      isearch-allow-scroll t
      ;; Isearch convenience, space matches anything
      ;; search-whitespace-regexp ".*?"
      ;; Display trailing whitespace
      show-trailing-whitespace 1

      pop-up-frames nil               ;; Open files in existing frames
      pop-up-windows t
      ;; Tab will first try to indent, then complete
      tab-always-indent 'complete
      resize-mini-windows t           ;; Resize the minibuffer when needed.
      enable-recursive-minibuffers t  ;; Enable recursive editing of minibuffer
      ;; (setq max-mini-window-height 0.33)
      ;; Move point to beginning or end of buffer when scrolling
      scroll-error-top-bottom t

      ;; Change window name to be more descriptive
      frame-title-format
      '("Emacs - " (buffer-file-name "%f"
                                     (dired-directory dired-directory "%b")))

      ;; Language-specific settings?
      ;; c-default-style "gnu"
      )

;; Set some builtin modes
(setq global-hl-line-sticky-flag t)     ;; Keep highlight across windows
(global-hl-line-mode t)                 ;; Highlight current line
;; Use compressed files like normal files
(auto-compression-mode 1)
;; (desktop-save-mode 1)                ;; Keep open files open across sessions
(column-number-mode 1)                  ;; Display the column number
;; (display-time-mode 1)                ;; Display the current time
;; (setq display-time-format "%l:%M%p")
;; Replace selected text when typing or pasting
(delete-selection-mode 1)
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
(prefer-coding-system 'utf-8)

;; Setup selected file endings to open in certain modes
(add-to-list 'auto-mode-alist '("\\.hdl\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.jack\\'" . java-mode))

;; Actions to perform when saving
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'before-save-hook 'ispell-comments-and-strings)

;; Automatically save on loss of focus
(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))

;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'focus-out-hook 'save-all)
;; (add-hook 'focus-out-hook 'balance-windows)
(global-set-key (kbd "C-x s") 'save-all)

;;; User-Defined Variables

(defvar user-todo-location "~/Text/org/todo.org")
(defvar user-notes-location "~/Text/org/notes.org")

;;; My Functions and Shortcuts/Keybindings

;; Set up keys using super. s-a, s-s, s-x, s-c, and s-v correspond to
;; select-all, save, cut, copy, and paste, which I've left for
;; consistency/utility on Macs. s-l is goto-line which is already useful.
(global-set-key (kbd "s-j") 'helm-mini)
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-y") 'helm-show-kill-ring)
(global-set-key (kbd "s-h") 'helm-mark-ring)
(global-set-key (kbd "s-g") 'magit-status)

(global-set-key (kbd "s-i") 'helm-projectile-ag-inexact)
(global-set-key (kbd "s-u") 'helm-projectile-ag-exact)
(global-set-key (kbd "s-o") 'helm-ag-pop-stack)

(defun helm-projectile-ag-inexact ()
  "Run helm-projectile-ag case-insensitive and without word boundaries."
  (interactive)
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-insert-at-point nil)
  (helm-projectile-ag)
  )
(defun helm-projectile-ag-exact ()
  "Run helm-projectile-ag case-sensitive and with word boundaries."
  (interactive)
  (setq helm-ag-base-command
        "ag --nocolor --nogroup --word-regexp --case-sensitive")
  (setq helm-ag-insert-at-point 'symbol)
  (helm-projectile-ag)
  )

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)
(global-set-key (kbd "s-C-,") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-C-.") (lambda () (interactive) (other-window 1)))

(global-set-key (kbd "C-j") 'indent-new-comment-line)
;; (global-set-key (kbd "M-$") 'ispell-buffer)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(define-key key-translation-map (kbd "<C-tab>") (kbd "TAB"))

;; narrow/widen easily
(defun narrow-dwim ()
  "Widen if currently narrowed, else narrow to function."
  (interactive)
  (cond
   ((buffer-narrowed-p) (widen))
   (t (narrow-to-defun))))
(global-set-key (kbd "C-(") 'narrow-dwim)

;; code folding
(require 'hideshow)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(define-key hs-minor-mode-map (kbd "C-)") 'hs-toggle-hiding)
(diminish 'hs-minor-mode)

;; artist mode
(global-set-key (kbd "C-$") 'artist-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Select the current line
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
  "Annihilate the current line or region by killing it, deleting the empty line, and restoring cursor position. If called on a region, will annihilate every line included in the region."
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

;; Reload the current buffer from disk
(global-set-key [f5] 'revert-buffer)

;; Zoom in/out
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

;; Easily open info-display-manual
(global-set-key (kbd "C-h I") 'info-display-manual)

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  ;; (balance-windows) ;; might as well stick this in here
  )

(global-set-key (kbd "C-c n") 'indent-buffer)
;; (add-hook 'before-save-hook 'indent-buffer)

(defun region-history-other (begin end)
  "Display the source controlled region history in another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1)
  )
(global-set-key (kbd "C-c h") 'region-history-other)

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

(global-set-key (kbd "C-c C-k") 'delete-current-buffer-file)

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

(global-set-key (kbd "C-c C-r") 'rename-current-buffer-file)

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

;; Join the following line onto the current line
;; Use this to quickly consolidate multiple lines into one
(defun join-next-line ()
  "Join the following line onto the current line, preserving the cursor position. This command can be used to rapidly consolidate multiple lines into one."
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
  (indent-according-to-mode))
(defun clear-line ()
  "Clear the line, but don't delete it."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (indent-according-to-mode)
  )

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'clear-line)

;; Align region by character.
;; TODO: Enable history in read-string to allow for default values
;; (i.e. last input)
(defun align-to-string (beg end)
  "Align region along character CHAR from BEG to END."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))

(global-set-key (kbd "M-=") 'align-to-string)

;; Commands to split window and move focus to other window
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  ;; (balance-windows)
  (other-window 1))
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  ;; (balance-windows)
  (other-window 1))

;; Remap the default window-splitting commands to the ones above
(global-set-key (kbd "C-x 3") 'split-window-right-focus)
(global-set-key (kbd "C-x 2") 'split-window-below-focus)

;; Show ASCII table
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

;; set transparency (cool but distracting)
;; (set-frame-parameter (selected-frame) 'alpha '(99))

;; Load Themes
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
;; Nimbus is my personal theme, now available on Melpa
(use-package nimbus-theme)
(load-theme 'nimbus)

;; Function for checking font existence
(defun font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font)) nil t))

;; Set font
(cond
 ((font-exists-p "Iosevka Light")
  (set-face-attribute 'default nil :font "Iosevka Light")
  (setq-default line-spacing 0)
  )
 ((font-exists-p "Fira Mono")
  (set-face-attribute 'default nil :font "Fira Mono")
  (setq-default line-spacing 1)
  )
 ((font-exists-p "Hack")
  (set-face-attribute 'default nil :font "Hack")
  (setq-default line-spacing 1)
  )
 )

(set-face-attribute 'default nil :height 120)

;;; Dired settings

(with-eval-after-load 'dired
;; Make C-l go up a directory in dired
  (define-key dired-mode-map (kbd "C-l") 'dired-up-directory)
  (define-key dired-mode-map (kbd "s-l") 'dired-up-directory)
  (define-key dired-mode-map "f"         'helm-find-files)
  (define-key dired-mode-map (kbd "M-p") 'scroll-down-line-quick)
  )

;; Handle opening and editing zip directories in dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq-default dired-recursive-copies 'always ;; Always do recursive copies
              ;; Make sizes human-readable by default and put dotfiles and
              ;; capital-letters first.
              dired-listing-switches "-alhv"
              dired-dwim-target t            ;; Try suggesting dired targets
              dired-auto-revert-buffer t     ;; Update buffer when visiting
              indicate-empty-lines nil       ;; highlight end of buffer?
              )

;; Extensions to Dired
(use-package dired+)

;; Hide file details in Dired
(use-package dired-details+
  :config (setq dired-details-propagate-flag t))

;; Colorify files in dired based on type
(use-package diredful
  :config (diredful-mode))

;; Expanded dired
;; Enables C-x C-j to jump to the current directory in dired
(use-package dired-x
  :ensure nil
  :bind ("s-d" . dired-jump)
  :config
  ;; Prevent certain files from showing up
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files
        (concat dired-omit-files "\\|\\.bk$\\|^\\.DS_Store$"))
  )

;; Allow changing file permissions in WDired
;; Notes: WDired can be enabled with C-x C-q and compiled with C-c C-c
(setq wdired-allow-to-change-permissions t)

;;; ERC settings

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs")
        ("mozilla.org" "#rust")))
;; (erc :server "irc.mozilla.org" :port 6667 :nick "m-cat")

;; Notify in minibuffer when private messaged
(setq erc-echo-notices-in-minibuffer-flag t)

;; Match keywords, highlight pals, ignore fools
(require 'erc-match)
(setq erc-keywords '("rust"))
(setq erc-pals  '())
(setq erc-fools '())

;;; Eshell settings

;; Supposed to stop auto-scrolling of eshell output.
;; TODO: Not even sure if this works.
(setq eshell-scroll-show-maximum-output nil
      eshell-scroll-to-bottom-on-input  nil
      eshell-scroll-to-bottom-on-output nil
      )

;; Use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "M-{") 'eshell-previous-prompt)
              (define-key eshell-mode-map (kbd "M-}") 'eshell-next-prompt)
              ))

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
  :config (setq eshell-up-print-parent-dir nil))

;; Open certain programs from eshell in a term buffer
(add-hook 'eshell-load-hook #'(add-to-list 'eshell-visual-commands "ghci"))

;; Mouse settings

(setq mouse-wheel-progressive-speed nil) ;; Make the mouse wheel not accelerate

(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

;;; Load packages

;; Improved package management
;; (use-package paradox)

;; ;; Key chords
;; (use-package key-chord
;;   :init
;;   (key-chord-mode 1)
;;   (setq key-chord-one-key-delay .25)
;;   (setq key-chord-two-keys-delay .15)

;;   (key-chord-define-global "jx" 'helm-mini)
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
         )
  ;; :init
  ;; (setq eyebrowse-keymap-prefix nil) ; Broken!
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-new-workspace t)
  )

;; Go to last change without undoing it
(use-package goto-last-change
  :bind ("C-?" . goto-last-change)
  )

;; Fix the capitalization commands
(use-package fix-word
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)
         )
  )

;; Show unused keys
(use-package free-keys)

;; Dim surrounding paragraphs
(use-package focus
  :bind ("s-q" . focus-mode)
  )

;; Mode for writing
(use-package olivetti
  :bind ("s-w" . olivetti-mode)
  :init
  (setq olivetti-hide-mode-line t
        olivetti-body-width     .9
        )
  )

;; Add indicators for position in buffer and end of buffer
(use-package indicators
  :diminish indicators-mode
  :init (add-hook 'prog-mode-hook 'new-indicators)
  )
(defun new-indicators ()
  "Create new indicators in the current buffer."
  (interactive)

  ;; ;; show a little arrow at the end of buffer using the default fringe face
  ;; (ind-create-indicator 'point-max
  ;;                       :managed t
  ;;                       :relative nil
  ;;                       :fringe 'left-fringe
  ;;                       :bitmap 'right-arrow
  ;;                       :face 'fringe)

  ;; show relative position in the file (a.k.a. scroll bar)
  (ind-create-indicator 'point :managed t)
  )

;; Copy selected region to be pasted into Slack/Github/etc.
(use-package copy-as-format)

;; Jump to tag definitions using ripgrep
;; (use-package dumb-jump
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g q" . dumb-jump-quick-look)
;;          ;; ("M-g x" . dumb-jump-go-prefer-external)
;;          ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
;;          )
;;   :config
;;   (setq dumb-jump-selector 'helm)
;;   (setq dumb-jump-prefer-searcher 'rg)
;;   :ensure
;;   )

;; Line numbers
(use-package nlinum
  :init (add-hook 'prog-mode-hook 'nlinum-mode)
  :config
  (setq linum-format "%3d") ;; Set linum format, minimum 3 lines at all times
  )

;; Show info about the current region
;; (use-package region-state
;;   :config (region-state-mode))

;; Display current function in mode line
(use-package which-func
  :config
  (which-function-mode 1))

;; Highlight indentation using periods
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\.)
  )

;; Automatically clean up extraneous whitespace
(use-package ws-butler
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

;; ;; Save open files across Emacs sessions.
;; ;; I use this instead of Desktop.el which saves the entire session.
;; (use-package save-visited-files
;;   :config (turn-on-save-visited-files-mode))

;; Jump to the end of a line using avy's decision tree
(defun avy-goto-line-end ()
  "Jump to a line using avy and go to the end of the line."
  (interactive)
  (avy-goto-line)
  (end-of-line)
  )

;; Avy mode (jump to a char/word using a decision tree)
(use-package avy
  :bind (("C-," . avy-goto-line-end)
         ;; ("C-<" . avy-goto-char-in-line)
         ("C-." . avy-goto-char)
         ;; ("C->" . avy-goto-word-1)
         )
  :config
  ;; Use more characters (and better ones) in the decision tree
  ;; QWERTY keys
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l
                      ?w ?e ?r ?u ?i ?o))
  ;; DVORAK keys
  ;; (setq avy-keys '(?p ?g ?c ?r
  ;;                     ?a ?o ?e ?u ?h ?t ?n ?s))
  ;; Set the background to gray to highlight the decision tree
  (setq avy-background t)
  )

;; Use a sensible mechanism for making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Automatically save place in each file
(use-package saveplace
  :config
  (save-place-mode 1)
  )

;; Nicer-looking modeline
;; Looks nice, but causes lag.
;; (use-package powerline
;;   :config (powerline-default-theme))

;; Modeline improvement
;; (use-package smart-mode-line
;;   :config
;;   (setq sml/theme 'respectful)
;;   (sml/setup)
;;   )

;; Midnight mode - clean up buffers older than 3 days
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; highlight the parts of lines that exceed column 80
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(add-hook 'c-mode-hook 'c-whitespace-mode)
(add-hook 'c++-mode-hook 'c-whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'c-whitespace-mode)
(add-hook 'rust-mode-hook 'rust-whitespace-mode)
(defun c-whitespace-mode ()
  "Set whitespace column for c-like modes and turn on `whitespace-mode'."
  (setq-local whitespace-line-column 80)
  (whitespace-mode)
  )
(defun rust-whitespace-mode ()
  "Set whitespace column for rust-mode and turn on `whitespace-mode'."
  (setq-local whitespace-line-column 100)
  (whitespace-mode)
  )
(diminish 'whitespace-mode)

;; Multiple cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-{") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-}") 'mc/mark-next-like-this)
  (define-key mc/keymap (kbd "<return>") nil)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

  (setq mc/always-run-for-all t)
  )

;; Display number of matches when searching
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; Enable more powerful replace, plus Python regexps
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

;; Make marks visible
;; (use-package visible-mark
;;   :config (global-visible-mark-mode t))

;; Highlight color strings with the corresponding color
(use-package rainbow-mode
  :diminish rainbow-mode
  )

;; Highlight delimiters with colors depending on depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight numbers in code
(use-package highlight-numbers
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlight more elisp syntax
(use-package highlight-quoted
  :init (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; Highlight operators
(use-package highlight-operators
  :init
  (add-hook 'c-mode-common-hook 'highlight-operators-mode)
  (add-hook 'rust-mode-hook     'highlight-operators-mode)
  )

;; Highlight some recent changes such as undos
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode))

;; Highlight surrounding parentheses
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("white")
        hl-paren-delay .05
        )
  )

;; Inherit environment variables from Shell
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")
    ))

;; Open files in Finder on Mac
(use-package reveal-in-osx-finder
  :bind ("C-c f" . reveal-in-osx-finder)
  )

;; Display available keybindings in Dired mode (? creates popup)
(use-package discover)

;; Maximize/unmaximize current window.
(use-package zygospore
  :bind (
         ;; ("C-x 1" . zygospore-toggle-delete-other-windows)
         ("M-o"   . zygospore-toggle-delete-other-windows)
         ))

;; Undo/redo window configurations
(use-package winner
  :defer 1
  :bind (("C-c C-," . winner-undo)
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

;; Display available keys
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil))

;; Auto-focus help buffers and allow exiting with C-g
(use-package popwin
  :config (popwin-mode 1))

;; Switch windows more easily
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

;; Move buffers around
(use-package buffer-move
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)
         ))

;; Make currently focused window larger.
;; This package is not actively maintained.
(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  ;; (golden-ratio-mode)
  (setq golden-ratio-auto-scale nil)
  (setq golden-ratio-adjust-factor 0.8)
  (defadvice select-window-by-number
      (after golden-ratio-resize-window activate)
    (golden-ratio) nil)
  )

;; Expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  )

;; Wrap parentheses or quotes around word
(use-package corral
  :bind (("M-(" . corral-parentheses-backward)
         ("M-)" . corral-parentheses-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward)
         ("M-{" . corral-braces-backward)
         ("M-}" . corral-braces-forward)
         ("M-`" . corral-backquote-forward)
         ("M-~" . corral-backquote-backward)
         ("M-'"  . corral-double-quotes-backward)
         ("M-\"" . corral-double-quotes-forward)
         )
  :config (setq corral-preserve-point t))

;; More powerful (way better) comment command
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;;; Git packages

;; Git client in Emacs
(use-package magit
  :diminish auto-revert-mode
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-refine-hunk `all)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  )

;; Browse historic versions of a file
(use-package git-timemachine)

;; Generate links to Github for current code location
(use-package git-link)

;;; Project packages

;; Project manager
(use-package projectile
  :bind ("<f6>" . projectile-compile-project)
  :init
  (add-hook 'prog-mode-hook #'projectile-mode)
  :config
  (setq projectile-completion-system 'helm)
  ;; Change mode line indicator
  (setq projectile-mode-line '(:eval
                               (if
                                   (file-remote-p default-directory)
                                   " Projectile"
                                 (format " [%s]"
                                         (projectile-project-name)))))
  )

(use-package helm-projectile
  :bind (("C-\"" . helm-projectile-recentf)
         ("C-'"  . helm-projectile)
         )
  :config
  (helm-projectile-on))

;; Show markers in margin indicating changes
(use-package diff-hl
  :bind (("C-c d" . diff-hl-revert-hunk)
         ("C-<"   . diff-hl-previous-hunk)
         ("C->"   . diff-hl-next-hunk)
         )
  :init (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  :config
  ;; (diff-hl-margin-mode)
  )

;;; Language packages

;; On-the-fly syntax checker
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'text-mode-hook #'flycheck-mode)
  :commands flycheck-mode
  :bind ("C-!" . flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (setq flycheck-check-syntax-automatically nil)

  (setq sentence-end-double-space nil) ;; Stupid check
  ;; Disable checkers that don't work correctly
  (setq-default flycheck-disabled-checkers '(rust rust-cargo))
  ;; (setq-default flycheck-disabled-checkers '(rust))

  ;; Proselint
  ;; Not available on MELPA yet.
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode))
  (add-to-list 'flycheck-checkers 'proselint)
  )

(use-package flycheck-vale
  :config
  (flycheck-vale-setup)
  )

;; Add linting for elisp packages
(use-package flycheck-package
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-package-setup))

;; Company mode for auto-completion
(use-package company
  :diminish company-mode
  :bind ("M-/" . company-complete)
  :config
  (global-company-mode)
  (setq company-idle-delay nil)
  (setq company-tooltip-align-annotations t) ;; align tooltips to right border
  (add-to-list 'company-backends 'company-racer)
  )

;; Yasnippet
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

;; Markdown mode
(use-package markdown-mode
  :mode "\\.md\\'")

;; YAML mode
(use-package yaml-mode
  :mode "\\.yml\\'")

;; TOML mode
(use-package toml-mode
  :mode "\\.toml\\'"
  )

;; Javascript mode
(use-package js2-mode
  :mode "\\.js\\'")

;; Javascript REPL
(use-package nodejs-repl
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (define-key js-mode-map (kbd "C-M-x")   'nodejs-repl-send-buffer)
              (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
              (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
              (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
              )))

;; Haskell mode
(use-package haskell-mode
  :defer t
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              ;; (ghc-init)
              (define-key haskell-mode-map (kbd "C-#")
                'haskell-process-load-or-reload)
              ;; Use hoogle in Haskell buffers.
              ;; Supply prefix arg for full info (C-u C-c h)
              (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
              ))
  :config
  (setq haskell-indentation-layout-offset 4
        haskell-indentation-left-offset   4
        haskell-indentation-ifte-offset   4
        haskell-hoogle-command "hoogle")
  )

(use-package flycheck-haskell
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package rust-mode
  :mode "\\.rs\\'"
  :diminish eldoc-mode
  :init
  (setq rust-format-on-save nil)
  )

(use-package racer
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  ;; (setq racer-rust-src-path "/usr/local/src/rust/src")

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

;; Run cargo commands in rust buffers, e.g. C-c C-c C-r for cargo-run
(use-package cargo
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode)
  )

;; Doesn't work, json-read-error
;; (use-package flycheck-rust
;;   :init
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package nim-mode
  :init
  (setq nim-nimsuggest-path "~/.nim/bin/nimsuggest")
  ;; Currently nimsuggest doesn't support nimscript files, so only nim-mode...
  (add-hook 'nim-mode-hook 'nimsuggest-mode)
  :config
  (define-key nim-mode-map (kbd "RET") #'newline-and-indent)
  )

;; ;; Completions for Haskell
;; ;; TODO: doesn't get loaded...
;; (use-package company-ghc
;;   :after company
;;   :init
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)
;;   :config
;;   (add-to-list 'company-backends 'company-ghc))

;; Haskell snippets
(use-package haskell-snippets
  :after yasnippet)

;;; Org Mode

;; TODO: organize this section

(use-package org
  :diminish visual-line-mode
  :diminish org-indent-mode
  )
;; Open .org files in org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Enables exporting to markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; The ellipsis to use in the org-mode outline
;; (setq org-ellipsis "...")
;; Try to keep cursor before ellipses
(setq org-special-ctrl-a/e t)
;; Smart editing of invisible region around ellipses
(setq org-catch-invisible-edits 'smart)

;; All subtasks must be DONE before marking a task as DONE
(setq org-enforce-todo-dependencies t)
(setq org-log-done (quote time))       ;; Log the time a task was set to DONE
(setq org-log-redeadline (quote time)) ;; Log the time a task's deadline changed
(setq org-log-reschedule (quote time)) ;; Log the time a task was rescheduled

(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Org-mode word-wrap and indented entries
(add-hook 'org-mode-hook #'
          (lambda ()
            (visual-line-mode)
            (org-indent-mode)
            (toggle-word-wrap)
            ))

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
;; `org-refile' notes to the top of the list
(setq org-reverse-note-order t)
;; Use headline paths (level1/level2/...)
(setq org-refile-use-outline-path t)
;; Go down in steps when completing a path
(setq org-outline-path-complete-in-steps nil)

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

;; Shortcuts/Keybindings

;; This binding conflicts with projectile so get rid of it
(define-key org-mode-map (kbd "C-'")   nil)
(define-key org-mode-map (kbd "C-S-n") 'org-move-subtree-down)
(define-key org-mode-map (kbd "C-S-p") 'org-move-subtree-up)

(define-key org-mode-map (kbd "C-<")   'org-do-promote)
(define-key org-mode-map (kbd "C->")   'org-do-demote)

(defun org-refile-goto ()
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively 'org-refile))
  )
(define-key org-mode-map (kbd "s-;")   'org-refile-goto)
(define-key org-mode-map (kbd "s-'")   'org-refile)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda-list) ;; Switch to org-agenda

;; org-capture with template as default behavior
(defun org-task-capture ()
  "Capture a task with my todo template."
  (interactive)
  (org-capture nil "t"))

(defun org-note-capture ()
  "Capture a note with my note template."
  (interactive)
  (org-capture nil "n"))

(global-set-key (kbd "C-c c") 'org-note-capture) ;; org-capture
(global-set-key (kbd "C-c v") 'org-task-capture)

;; Jump to last capture
(global-set-key (kbd "C-c j") 'org-refile-goto-last-stored)

;;; Final

;; Set mode-line format
(setq-default
 mode-line-format
 (list
  " "
  '(:eval (winum-get-number-string))
  " "
  'mode-line-modified
  " "
  ;; '(:eval (when buffer-file-name
  ;;           (concat (file-name-nondirectory
  ;;                    (directory-file-name default-directory))
  ;;                   " | "
  ;;                   )))
  '(:eval (propertize "%b"
                      'face '(:weight bold)
                      'help-echo (buffer-file-name)))
  " - "
  "%02l:%02c"
  " - "
  '(:eval (propertize "[%m]"
                      ;; 'face '(:weight bold)
                      'help-echo buffer-file-coding-system))

  ;; is this buffer read-only?
  '(:eval (when buffer-read-only
            (concat " - "  (propertize "RO"
                                       'face 'font-lock-preprocessor-face
                                       'help-echo "Buffer is read-only"))))
  " - "
  'mode-line-misc-info
  ;; " "
  ;; '(:eval (propertize (format-time-string "%H:%M")))
  'mode-line-end-spaces
  ))

;; Set final variables
(setq org-directory "~/Text/org")     ;; Default org directory

;; Starts with todo.org and notes.org
(find-file user-todo-location)
(split-window-right-focus)
(find-file user-notes-location)
;;(org-agenda nil "a")                ;; Open org-agenda

(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
