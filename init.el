;;; init.el --- Emacs configuration file. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2023 Marcin Swieczkowski
;;
;;; Commentary:
;;
;; Requires:
;;
;; - Emacs 28 or higher.
;;
;; Currently building Emacs with:
;;
;; $ brew tap d12frosted/emacs-plus
;; $ brew install emacs-plus --with-poll --with-native-comp
;;
;; Making changes / testing:
;;
;; - Use M-x free-keys to find unused keybindings.
;; - Use M-x bug-hunter-init-file to locate errors.
;; - Use M-x profiler-start and profiler-report to profile runtime.
;; - Use restart-emacs to restart after making changes.
;;
;; TODO:
;;
;; - [x] Fix awful performance in Emacs 29.1 [done by switching to emacs-plus]
;; - [ ] With Emacs 29+:
;;   - [x] set native-comp load-path (in early-init.el)
;;   - [ ] switch to built-in tree-sitter (once it's easier to setup...)
;;   - [ ] use new indent package (once stipples are available)
;;
;;; Code:

;; Show more error info?
(setq debug-on-error nil)

(defun load-init-file (file)
  "Loads FILE inside init/ folder."
  (load-file (concat user-emacs-directory "init/" file)))

;;; User-Defined Variables

(defvar user-code-directory "~/Repos/")
(defvar user-text-directory "~/Text/")

(defvar user-scratchpad-path (concat user-text-directory "scratchpad.txt"))
(defvar user-data-path (concat user-text-directory "data.txt"))
(defvar user-org-directory (concat user-text-directory "org/"))

(defvar user-ideas-org (concat user-org-directory "ideas.org"))
(defvar user-notes-org (concat user-org-directory "notes.org"))
(defvar user-projects-org (concat user-org-directory "notes.org"))
(defvar user-todo-org (concat user-org-directory "todo.org"))
(defvar user-work-org (concat user-org-directory "work.org"))

(defvar user-emacs-config-directory "~/.local/emacs/")
(defvar user-emacs-var-directory (concat user-emacs-config-directory "var/"))
(defvar user-emacs-etc-directory (concat user-emacs-config-directory "etc/"))
(defvar user-emacs-elpa-directory (concat user-emacs-config-directory "elpa/"))

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
  (find-file user-scratchpad-path))
(global-set-key (kbd "C-c s") 'open-scratchpad-file)

;; Open data.txt.
(defun open-data-file ()
  "Open data file."
  (interactive)
  (find-file user-data-path))
(global-set-key (kbd "C-c d") 'open-data-file)

;;; Package settings

(require 'package)
(setq
 ;; Set the elpa directory.
 package-user-dir user-emacs-elpa-directory
 ;; Prefer the newest version of a package.
 load-prefer-newer t
 ;; TODO: is this correct?
 ;; Only enable packages found in this file (not all installed packages).
 package-enable-at-startup nil
 )
;; Add package sources.
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Run auto-load functions specified by package authors.
(package-initialize)

;; Require use-package.
(when (not (file-directory-p user-emacs-elpa-directory))
  (package-refresh-contents)
  ; TODO: Built-in in 29.1.
  (package-install 'use-package)
  )
(require 'use-package)
;; Always install missing packages.
(setq use-package-always-ensure t)
;; (setq package-check-signature nil)
;; ;; Install elpa .gnupg folder.
;; (use-package gnu-elpa-keyring-update
;;   :config
;;   (gnu-elpa-keyring-update)
;;   )

;;; Utilities

;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with
;; user activity.
(use-package gcmh
  :config
  (gcmh-mode 1)
  )

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Keep directories clean.
;; Should be one of the first things loaded.
(use-package no-littering
  :init

  (setq
   ;; Keep these in the user home directory to prevent constant sync conflicts.
   no-littering-var-directory user-emacs-var-directory
   no-littering-etc-directory user-emacs-etc-directory
   )

  :config

  ;; Exclude from recentf.
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
    ;; Set the shell (default is /bin/zsh on MacOS).
    (setq shell-file-name "/bin/bash")
    (exec-path-from-shell-initialize)
    ))

;; Enable restarting Emacs from within Emacs.
;; NOTE: built in functionality as of Emacs 29.
(use-package restart-emacs
  :defer t)

;; Find bugs in Emacs configuration.
(use-package bug-hunter
  :defer t)

;;; vertico + consult + orderless + marginalia

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  (setq
   ;; Scroll margin.
   vertico-scroll-margin 3

   ;; Show more candidates
   ;; vertico-count 20

   ;; Grow and shrink the Vertico minibuffer
   ;; vertico-resize t

   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
   vertico-cycle nil
   )

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config

  ;; Enable mouse support for vertico.
  (use-package vertico-mouse
    :ensure nil
    :config
    (vertico-mouse-mode)
    )

  ;; Quickly select a line.
  (use-package vertico-quick
    :ensure nil
    :bind (
           :map vertico-map
           ("C-," . vertico-quick-exit)
           )
    )

  ;; Repeat previous vertico inputs even if they were aborted.
  (use-package vertico-repeat
    :ensure nil
    :bind (
           :map vertico-map
           ("M-P" . vertico-repeat-previous)
           ("M-N" . vertico-repeat-next)
           )
    :init
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    )
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
   completion-styles '(orderless partial-completion)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))
   )
  )

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (
         ("M-i" . consult-imenu)
         ("s-j" . consult-buffer) ;; orig. switch-to-buffer
         ("s-m" . consult-mark)
         ("s-i" . consult-ripgrep-inexact-save)
         ("s-u" . consult-ripgrep-exact-save)
         ("s-l" . consult-goto-line) ;; orig. goto-line
         ("s-y" . consult-yank-pop) ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos) ;; REMOVED: use C-h o instead.
         ("<help> m" . consult-minor-mode-menu)

         ;; Consult bindings from readme
         ;; ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ;; ("C-c k" . consult-kmacro)
         ([remap Info-search] . consult-info)
         ;; M-s bindings (search-map)
         ("M-s l" . consult-line)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  (defun consult-ripgrep-exact-save (&optional arg)
    "Save before calling `consult-ripgrep', matching exactly."
    (interactive "P")
    (save-all)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / \
               --case-sensitive --no-heading --with-filename --line-number \
               --word-regexp --hidden --glob !{.git,target,node_modules}"
           ))
      (cond ((and arg
                  (= 4 (prefix-numeric-value arg)))
             (consult-ripgrep default-directory))
            (t (consult-ripgrep))))
    )

  (defun consult-ripgrep-inexact-save (&optional arg)
    "Save before calling `consult-ripgrep', matching inexactly."
    (interactive "P")
    (save-all)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / \
               --ignore-case --no-heading --with-filename --line-number \
               --hidden --glob !{.git,target,node_modules}"
           ))
      (cond ((and arg
                  (= 4 (prefix-numeric-value arg)))
             (consult-ripgrep default-directory))
            (t (consult-ripgrep))))
    )

  (setq consult-async-min-input 2)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-bookmark
   consult-theme
   consult-recent-file
   consult-ripgrep
   consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '(:debounce 0.2 any)
   ;; :preview-key "M-."
   )
  ;; Run some more hooks when previewing files.
  (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode-check-buffers)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

;; Enable richer annotations using Marginalia.
(use-package marginalia
  :bind (
         ;; Either bind `marginalia-cycle` globally or only in the minibuffer
         ("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)
         )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (
   ("C-z" . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'

    :map embark-general-map
    ("K" . my/embark-search)
   )

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (setq embark-quit-after-action nil)

  ;; Show embark help popup?
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  (delete 'embark-mixed-indicator embark-indicators)

  (defun my/embark-search (term)
    (interactive "sSearch Term: ")
    (browse-url
     (format "https://kagi.com/search?q=%s" term)))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

;;; Load customizations

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file t)

;;; Quality of life changes

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
 js-indent-level 2
 fill-column 80
 ;; Highlight end of buffer?
 indicate-empty-lines t
 )

(defvar apropos-do-all)
(defvar ediff-window-setup-function)
(defvar c-default-style)
(setq
 ;; Tries to preserve last open window point when multiple buffers are open for
 ;; the same file.
 switch-to-buffer-preserve-window-point t
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 ;; Enable complete documentation for apropos functions?
 apropos-do-all t
 kill-ring-max 1000
 ;; Ensure that files end with a newline.
 require-final-newline t
 ;; Add newline at end of buffer with C-n.
 next-line-add-newlines t
 ;; Flash the frame on every error?
 visible-bell nil
 ring-bell-function 'ignore
 ;; Set up ediff windows in the same frame.
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
 ;; Delay for displaying function/variable information.
 eldoc-idle-delay info-delay
 ;; Delay for hiding tooltips in seconds.
 tooltip-hide-delay (* 60 60)
 ;; Delay for showing tooltips, in seconds.
 tooltip-delay 0
 ;; Fix flickering in Emacs 26 on OSX.
 recenter-redisplay nil
 ;; Follow symlinks without asking?
 vc-follow-symlinks t
 ;; Undo limit.
 undo-limit (* 10 1000 1000)
 ;; Replace yes/no prompts with y/n.
 use-short-answers t

 ;; Inhibit backups?
 backup-inhibited t
 ;; Make backup files when creating a file?
 make-backup-files nil
 ;; Silently delete old backup versions.
 delete-old-versions t
 ;; Auto save?
 auto-save-default nil
 ;; Create interlock files?
 create-lockfiles nil
 ;; Send deleted files to trash.
 delete-by-moving-to-trash t

 ;; Where should we open new buffers by default?
 display-buffer-base-action '(display-buffer-below-selected)
 ;; Specify custom behavior for misbehaving buffers.
 display-buffer-alist
 '(("\\*Help\\*"
    (display-buffer-reuse-window
     display-buffer-below-selected))
   ("\\*Ibuffer\\*"
    (display-buffer-same-window))
   )
 ;; Open files in existing frames.
 pop-up-frames nil
 pop-up-windows t
 ;; Tab will always just try to indent.
 tab-always-indent 't
 ;; Resize the minibuffer when needed.
 resize-mini-windows t
 ;; Enable recursive editing of minibuffer?
 enable-recursive-minibuffers t
 minibuffer-depth-indicate-mode t
 ;; Move point to beginning or end of buffer when scrolling.
 scroll-error-top-bottom t
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))

 ;; Set a larger minimum window width. Smaller than this is hard to read.
 window-min-width 30
 window-min-height 10

 ;; Language-specific settings?
 c-default-style "stroustrup"
 )

;; Change window name to be more descriptive.
(setq frame-title-format
      '((:eval (when (and (buffer-modified-p) buffer-file-name) "*"))
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
(add-to-list 'auto-mode-alist '("\\.prdoc\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.zndsl\\'" . yaml-mode))

;; Set some built-in modes.

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

;; Auto refresh dired.
(setq global-auto-revert-non-file-buffers t)

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

;; Set up gpg.
;; For full instructions, see https://emacs.stackexchange.com/a/12213.

;; Don't bring up key recipient dialogue.
(require 'epa-file)
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("scatman@bu.edu"))

;; Fix EasyPG error.
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)

;; Kill GPG buffers when idle.
(defun kill-gpg-buffers ()
  "Kill GPG buffers."
  (interactive)
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match ".*\.gpg$" (buffer-name buffer))
          (message "Auto killing .gpg buffer '%s'" (buffer-name buffer))
          (when (buffer-modified-p buffer)
            (save-buffer))
          (kill-buffer buffer)
          (setq buffers-killed (+ buffers-killed 1)))))
    (unless (zerop buffers-killed)
      ;; Kill gpg-agent.
      (shell-command "gpgconf --kill gpg-agent")
      (message "%s .gpg buffers have been autosaved and killed" buffers-killed))))

(run-with-idle-timer 120 t 'kill-gpg-buffers)

;; Mouse settings

(setq
 ;; Make the mouse wheel not accelerate.
 mouse-wheel-progressive-speed nil
 mouse-yank-at-point t
 )

;;; My Functions and Shortcuts/Keybindings

;; For consistency with every other application.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Set s-s to save all buffers (default is only current buffer).
(global-set-key (kbd "s-s") 'save-all)

;; Set up keys using super. s-a, s-x, s-c, and s-v correspond to
;; select-all, save, cut, copy, and paste, which I've left for
;; consistency/utility on Macs.
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)

;; Enable OSX full screen shortcut.
(global-set-key (kbd "C-s-f") 'toggle-frame-maximized)

(defun isearch-backward-symbol-at-point (&optional arg)
  "The backwards version of `isearch-forward-symbol-at-point'. ARG!"
  (interactive "P")
  (isearch-mode nil nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds))
       (count (and arg (prefix-numeric-value arg))))
   (cond
    (bounds
     (when (< (car bounds) (point))
	    (goto-char (car bounds)))
     (isearch-yank-string
      (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (isearch-repeat-backward)
     (when count
       (isearch-repeat-backward count)))
    (t
     (setq isearch-error "No symbol at point")
     (isearch-push-state)
     (isearch-update)))))
(global-set-key (kbd "M-s ,") 'isearch-backward-symbol-at-point)

;; Enable OSX CMD+backspace.
(defun kill-line-backwards ()
  "Kill the line backwards."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "s-<backspace>") 'kill-line-backwards)

;; Disable annoying popup on OSX.
(global-set-key (kbd "s-t") 'make-frame)
;; I don't want to accidentally press this.
(global-set-key (kbd "s-q") nil)

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
  (revert-buffer-quick))
(global-set-key (kbd "s-r") 'save-revert-buffer)

(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  ;; Do not prompt for confirmation.
  (save-some-buffers t)
  )
(global-set-key (kbd "C-x s") 'save-all)

;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'focus-out-hook 'save-all)
;; Run `save-all' when idle for a while.
;; Shouldn't run too quickly as it is a bit distracting.
(run-with-idle-timer 60 t 'save-all)

;; Commands to split window and move focus to other window.
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  (other-window 1))
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  (other-window 1))
(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  )
(global-set-key (kbd "C-0") 'delete-window-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Code folding.
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (
         :map hs-minor-mode-map
         ("s-[" . hs-hide-level)
         ("s-]" . hs-show-all)
         ("s-\\" . hs-toggle-hiding)
         )
  )

;; Zapping.
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Easier repeat.
;; (global-set-key (kbd "C-z") 'repeat)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Zoom in/out.
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

(defun highlight-line ()
  "Toggle highlighting the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (min (point-max) (+ 1 (line-end-position))))
         (overlays (overlays-in beg end)))
    (if (-any? #'(lambda (ov) (equal (overlay-get ov 'face) 'bookmark-face))
               overlays)
        (remove-overlays beg end 'face 'bookmark-face)
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'bookmark-face)))))
(global-set-key (kbd "C-c l") 'highlight-line)

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  )
(global-set-key (kbd "C-c n") 'indent-buffer)

(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1)
  )
(global-set-key (kbd "C-c h") 'region-history-other)

(defun project-notes ()
  "Open a notes.org file in the root of the project."
  (interactive)
  (find-file (concat (vc-root-dir) "notes.org"))
  )
(global-set-key (kbd "s-o") 'project-notes)

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

;; Line operations.

;; Select from point onwards instead of the entire line.
;; + Behaves like C-k.
;; + Can choose whether to keep indentation (run either C-a or M-m beforehand).
;; + Being able to select from point onwards comes in handy much of the time.
(defun select-line ()
  "Select the rest of the current line."
  (interactive)
  (push-mark (line-end-position) nil t)
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
    (kill-append "\n" t)
    ;; Are there more lines after this?
    (if (/= (line-end-position) (point-max))
        (delete-char 1))
    ;; Restore column position
    (move-to-column col)
    ))
(global-set-key (kbd "C-S-k") 'annihilate-lines)

;; Drag up/down single line or lines in region.
(use-package drag-stuff
  :defer t
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

(defun goto-line-below (arg)
  "Open and goto a new line below while keeping proper indentation."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode)
  )
(defun goto-line-above (arg)
  "Open and goto a new line above while keeping proper indentation."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode)
  )
(global-set-key (kbd "<C-return>") 'goto-line-below)
(global-set-key (kbd "<S-return>") 'goto-line-above)

;; Indentation functions.

(defvar indent-amount 4)
;; Set per-mode overrides.
(add-hook 'text-mode-hook #'(lambda () (setq-local indent-amount 2)))
(add-hook 'yaml-mode-hook #'(lambda () (setq-local indent-amount 2)))

(defun indent-left ()
  "Indent left by the amount used in the mode, or the default amount."
  (interactive)
  (cond ((region-active-p)
         (indent-region-relative (region-beginning) (region-end) (- indent-amount)))
        (t
         (indent-region-relative (point) (point) (- indent-amount))
         ))
  )
(defun indent-right ()
  "Indent right by the amount used in the mode, or the default amount."
  (interactive)
  (cond ((region-active-p)
         (indent-region-relative (region-beginning) (region-end) indent-amount))
        (t
         (indent-region-relative (point) (point) indent-amount)
         ))
  )

(defun indent-region-relative (beg end amount)
  "Indent from BEG to END by the specified AMOUNT."
  (save-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (indent-rigidly beg end amount)
    )
  )

(global-set-key (kbd "C-<") 'indent-left)
(global-set-key (kbd "C->") 'indent-right)

;; Better scrolling functions.

(defun window-fraction-height (fraction)
  "Get specified FRACTION of the height of the current window."
  (max 1 (/ (1- (window-height (selected-window))) fraction)))
(defvar scroll-fraction 4)

(defun scroll-up-fraction ()
  "Scrolls up by a fraction of the current window height."
  (interactive)
  (scroll-up (window-fraction-height scroll-fraction)))
(defun scroll-down-fraction ()
  "Scrolls down by a fraction of the current window height."
  (interactive)
  (scroll-down (window-fraction-height scroll-fraction)))
(defun scroll-other-window-up-fraction ()
  "Scrolls other window up by a fraction of the current window height."
  (interactive)
  (scroll-other-window (window-fraction-height scroll-fraction)))
(defun scroll-other-window-down-fraction ()
  "Scrolls other window down by a fraction of the current window height."
  (interactive)
  (scroll-other-window-down (window-fraction-height scroll-fraction)))

;; Enable these commands in isearch.
(put 'scroll-up-fraction 'isearch-scroll t)
(put 'scroll-down-fraction 'isearch-scroll t)
(put 'scroll-other-window-up-fraction 'isearch-scroll t)
(put 'scroll-other-window-down-fraction 'isearch-scroll t)

(global-set-key (kbd "C-v") 'scroll-up-fraction)
(global-set-key (kbd "M-v") 'scroll-down-fraction)
(global-set-key (kbd "C-S-v") 'scroll-other-window-up-fraction)
(global-set-key (kbd "M-V") 'scroll-other-window-down-fraction)

;; Better beginning-of-line function.
;; From https://www.reddit.com/r/emacs/comments/15xeb1s/electric_mm/.

(let ((c-like '("//+!?" "/?\\*+"))
      (lisp '(";+"))
      (org-header-regexp (rx bol (+ "*") (+ space)
                             ;; I know this is a filthy way of doing the keywords but I just can't
                             ;; be bothered to do it properly.
                             (? (or "TODO"
                                    "NOW"
                                    "DONE"
                                    "DEAD")
                                (+ space))
                             (* (seq (or (seq "["
                                              (or (seq (* digit) "/" (* digit))
                                                  (seq (* digit) "%")
                                                  (seq "#" (or (any "A-Z") (+ digit))))
                                              "]"))
                                     (* space)))))
      (org-list-item-regexp (rx (or "-" "+"
                                    (seq (+ digit) (or "." ")"))
                                    (seq (any "a-z" "A-Z") (or "." ")")))
                                (+ space)
                                (? (seq "[" (or " " "-" "X") "]" (* space)))
                                (* (seq "["
                                        (or (seq (* digit) "/" (* digit))
                                            (seq (* digit) "%"))
                                        "]")
                                   (* space)))))
  (setq skip-prefixes-alist
        `(
          (text-mode . (,org-list-item-regexp))
          (org-mode . (,org-header-regexp ,org-list-item-regexp "#" "|"))
          (lisp-mode . ,lisp)
          (emacs-lisp-mode . ,lisp)
          (c-mode . ,c-like)
          (c++-mode . ,c-like)
          (eshell-mode . ("$+"))
          (rust-mode . ,(cons "//!" c-like))
          (zig-mode . ,c-like)
          (csharp-mode . ,c-like)
          (sh-mode . ("#+"))
          (python-mode . ("#+"))
          (red-mode . ("comment"))
          (markdown-mode . ("^#+" "^-"))
          )))

(defun skip-prefixes ()
  "Calls `back-to-indentation', then skips the first matching
regexp associated with the first mode equal to or derived from
the current major mode in `skip-prefixes-alist' (plus any
whitespace following it). If no regexps match, just skips over
`comment-start-skip'."
  (interactive)
  (beginning-of-visual-line)
  (back-to-indentation)
  (let ((eol (save-excursion (move-end-of-line 1) (point))))
    (unless (catch 'loop
              (dolist (prefix (cdr (assoc major-mode skip-prefixes-alist #'provided-mode-derived-p)))
                (when (looking-at-p prefix)
                  (search-forward-regexp prefix eol)
                  (search-forward-regexp "[[:space:]]*" eol)
                  (throw 'loop t))))
      ;; Fall back to just skipping the comment delimiter for the mode.
      (when (and comment-start-skip (looking-at-p comment-start-skip))
        (search-forward-regexp comment-start-skip eol)))))

(global-set-key (kbd "M-M") 'skip-prefixes)

;; Other

;; Align region by string.
;; TODO: Enable history in read-string to allow for default values
;;       (i.e. last input).
(defun align-to-string (beg end)
  "Align region from BEG to END along input string."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))

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

;; Helps with "too many files" error.
;; From https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html.
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;;; Visual settings

;; Set transparency.
(set-frame-parameter (selected-frame) 'alpha '(100))
;; (set-frame-parameter (selected-frame) 'alpha '(98))

;; Turn on blinking/flashing cursor? (-1 to disable)
(blink-cursor-mode -1)
;; Blink forever!
(setq blink-cursor-blinks 0)
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;; Allow resizing by pixels.
(setq frame-resize-pixelwise t)

(toggle-frame-maximized) ;; Maximize!

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

;; Make mark visible.
(load-init-file "mmv.el")

;; Load Theme

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; A good light theme for when I'm outside.
(use-package leuven-theme
  :config
  ;; Disable scaling.
  (setq
   leuven-scale-outline-headlines nil
   leuven-scale-org-agenda-structure nil
   leuven-scale-org-document-title nil
   leuven-scale-volatile-highlight nil
   )
  )

;; Nimbus is my personal theme, available on Melpa.
(use-package nimbus-theme
  :load-path "~/repos/github.com/mrcnski/nimbus-theme"
  :config
  (nimbus-theme)
  )

;; Set font.

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))
  (declare-function font-exists-p "init.el")

  (cond
   ((font-exists-p "Iosevka Comfy Fixed")
    (set-face-attribute
     'default nil :font "Iosevka Comfy Fixed:weight=Regular" :height 110)
    (setq-default line-spacing 0)
    )
   ((font-exists-p "Iosevka")
    (set-face-attribute
     'default nil :font "Iosevka:weight=Regular" :height 120)
    ;; 'default nil :font "Iosevka:weight=Light" :height 120)
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

(use-package isearch
  :ensure nil
  :config
  (setq
   ;; Can scroll in isearch?
   isearch-allow-scroll t

   ;; Highlight more matches after a delay.
   isearch-lazy-highlight t
   isearch-lazy-count t
   lazy-highlight-initial-delay info-delay
   )

  ;; Fix broken C-g.
  (define-key isearch-mode-map (kbd "C-g")
              (lambda () (interactive)
                (isearch-abort)
                (isearch-abort)
              ))

  ;; Display last searched string in minibuffer prompt.
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
  )

(use-package diff-mode
  :ensure nil
  :bind (
         :map diff-mode-map
         ("M-o" . nil)
         )
  )

(use-package dired
  :ensure nil
  :demand t
  :bind (
         :map dired-mode-map
         ("f" . find-file)
         ("M-n" . dired-next-subdir)
         ("M-p" . dired-prev-subdir)
         )
  :hook (dired-mode . dired-hide-details-mode)

  :config

  ;; Settings

  (setq-default
   ;; Always do recursive copies.
   dired-recursive-copies 'always
   ;; Make sizes human-readable by default and put dotfiles and capital-letters
   ;; first.
   dired-listing-switches "-alhv"
   ;; Try suggesting dired targets.
   dired-dwim-target t
   ;; Update buffer when visiting.
   dired-auto-revert-buffer t
   ;; Don't confirm various actions.
   dired-no-confirm t
   )

  ;; Set up packages.

  ;; Expanded dired.
  ;; Enables jumping to the current directory in dired (default: C-x C-j).
  (use-package dired-x
    :ensure nil
    :demand t
    ;; Prevent certain files from showing up.
    ;; NOTE: Use C-x M-o to show omitted files.
    :hook (dired-mode . dired-omit-mode)
    :bind ("s-d" . dired-jump)
    :config
    (setq dired-omit-files
          (concat dired-omit-files "\\|\\.bk$\\|^\\.DS_Store$"))
    )

  ;; More dired colors.
  (use-package diredfl
    :config (diredfl-global-mode))

  ;; Allow changing file permissions in WDired.
  ;; NOTE: WDired can be entered with C-x C-q and changes saved with C-c C-c.
  ;; (defvar wdired-allow-to-change-permissions)
  (use-package wdired
    :ensure nil
    :config (setq wdired-allow-to-change-permissions t))
  )

;; ibuffer settings

(use-package ibuffer
  :ensure nil
  :bind (
         :map ibuffer-mode-map
         ;; Unbind ibuffer-visit-buffer-1-window.
         ("M-o" . nil)
         )
  :config

  ;; (define-key ibuffer-mode-map (kbd "M-o") nil)

  (setq
   ;; Don't show filter groups if there are no buffers in that group.
   ibuffer-show-empty-filter-groups nil
   ;; Don't ask for confirmation to delete marked buffers.
   ibuffer-expert t
   )

  ;; Use human-readable Size column.
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (let ((bs (buffer-size)))
      (cond ((> bs 1e6) (format "%7.1fm" (/ bs 1e6)))
            ((> bs 1e3) (format "%7.1fk" (/ bs 1e3)))
            (t          (format "%7d" bs)))))

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
  )

;; Track recently-opened files.
(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 5000)
  (recentf-mode t)
  )

;; Save minibuffer history across Emacs sessions.
(use-package savehist
  :ensure nil
  :config
  (savehist-mode t)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  )

;; Ediff settings

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  )

;; ERC settings

(use-package erc
  :ensure nil
  :hook (erc-mode . erc-settings)
  :config

  ;; Set up packages.

  (use-package erc-join
    :ensure nil
    :config
    (setq erc-autojoin-channels-alist
          '(
            ("freenode.net"
             "#emacs"
             "#org-mode"
             "#emacsconf"
             "#bash"
             )
            ))
    )

  (use-package erc-notify
    :ensure nil
    :config
    ;; Notify in minibuffer when private messaged.
    (setq erc-echo-notices-in-minibuffer-flag t)
    )

  ;; Match keywords, highlight pals, ignore fools.
  (use-package erc-match
    :ensure nil
    :config
    (setq erc-keywords '()
          erc-pals  '()
          erc-fools '()
          )
    )

  ;; Settings

  (defun erc-settings ()
    "Set erc settings."
    ;; Move prompt one line at a time when point goes off the screen
    ;; (was centering the point before).
    (setq-local scroll-conservatively 999)
    )

  (setq
   erc-nick "bytedude"
   ;; How to open new channel buffers?
   erc-join-buffer 'window-noselect
   )

  ;; Show ERC activity in mode-line.
  (erc-track-mode)
  )

;; Eshell settings

(use-package eshell
  :ensure nil
  :bind (
         ("s-e" . eshell-new)
         )
  ;; Save all buffers before running a command.
  :hook (eshell-pre-command . save-all)

  :config

  (setq
   ;; Stop cursor from always going to the bottom after command finishes.
   eshell-scroll-show-maximum-output nil
   eshell-scroll-to-bottom-on-output nil
   ;; Always insert at the bottom.
   eshell-scroll-to-bottom-on-input t
   ;; Remove unnecessary extra newline.
   eshell-banner-message "Welcome to the Emacs shell!\n"
   ;; File locations.
   eshell-rc-script (concat user-emacs-directory ".eshell-rc")
   eshell-login-script (concat user-emacs-directory ".eshell-login")
   )

  ;; Set keys up in this hook. This doesn't work in :bind.
  (add-hook 'eshell-first-time-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
                (define-key eshell-mode-map (kbd "C-a") 'beginning-of-line)
                (define-key eshell-mode-map (kbd "M-{") 'eshell-previous-prompt)
                (define-key eshell-mode-map (kbd "M-}") 'eshell-next-prompt)

                ;; Better history command!
                (define-key eshell-mode-map (kbd "M-r") 'consult-history)
                (define-key eshell-hist-mode-map (kbd "M-r") nil)

                ;; Allow M-s .
                (define-key eshell-mode-map (kbd "M-s") nil)
                (define-key eshell-hist-mode-map (kbd "M-s") nil)
                ))

  ;; Fix eshell overwriting history.
  ;; From https://emacs.stackexchange.com/a/18569/15023.
  (setq eshell-save-history-on-exit nil)
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))
  (add-hook 'eshell-pre-command-hook #'eshell-append-history)
  (add-hook 'eshell-mode-hook #'(lambda () (setq eshell-exit-hook nil)))

  (use-package em-hist
    :ensure nil
    :config
    (setq
     eshell-hist-ignoredups t
     ;; Set the history file.
     eshell-history-file-name "~/.bash_history"
     ;; If nil, use HISTSIZE as the history size.
     eshell-history-size 10000
     )
    )

  ;; Open a new eshell buffer.
  (defun eshell-new ()
    "Open a new eshell buffer."
    (interactive)
    (eshell t))

  ;; Set up a custom prompt.

  ;; Bits taken from https://emacs.stackexchange.com/a/33408/15023.
  (defun custom-eshell-prompt ()
    "My custom eshell prompt."
    (let* (
           ;; Get the git branch.
           (git-branch-unparsed
            (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
           (git-branch
            (if (string= git-branch-unparsed "")
                ""
              ;; Remove the trailing newline.
              (substring git-branch-unparsed 0 -1)))
           )
      (mapconcat
       (lambda (list)
         ;; Make all prompt components read-only, so that the prompt cannot be
         ;; deleted as regular text. Allow text inserted before the prompt to
         ;; inherit this property, as per eshell defaults.
         (propertize (car list)
                     'read-only      t
                     'face           (cdr list)
                     'front-sticky   '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only)))

       `(
         ;; Line to distinguish end of previous output.
         ("==="
          'font-lock-comment-face)
         ("\n")
         ;; Timestamp.
         (,(format-time-string "[%a, %b %d | %H:%M:%S]\n" (current-time))
          'font-lock-keyword-face)
         ;; Directory.
         ;;
         ;; Try to abbreviate-file-name of current directory as per `eshell'
         ;; defaults, e.g. display `~' instead of `/path/to/user/home'.
         (,(format "[%s]" (abbreviate-file-name (eshell/pwd)))
          'font-lock-constant-face)
         ;; Git branch.
         (,(if (string= git-branch "") "" (format " %s" git-branch))
          'font-lock-preprocessor-face)
         ;; The last exit code.
         (,(if-let ((status eshell-last-command-status))
               (if (= status 0) "" (format " [%s]" status)))
          'error)
         ("\n")
         ;; NOTE: Choose between prompts # and $ depending on user privileges,
         ;; as per Bourne and eshell defaults.
         (,(if (zerop (user-uid)) " # " " $ ")
          'minibuffer-prompt)
         )
       ""))
    )
  (setq eshell-prompt-function 'custom-eshell-prompt)
  ;; Should the prompt be highlighted?
  (setq eshell-highlight-prompt nil)

  ;; Load external eshell packages.

  (use-package eshell-syntax-highlighting
    :config
    ;; Enable in all Eshell buffers.
    (eshell-syntax-highlighting-global-mode 1))

  ;; Add z to eshell.
  ;; Jumps to most recently visited directories.
  (use-package eshell-z)
  )

(use-package outline
  :ensure nil
  :demand t
  :bind (
         :map outline-mode-map
         ("M-n" . outline-next-heading)
         ("M-p" . outline-previous-heading)
         )
  )

;; Show matching parentheses.
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (show-paren-mode t)
  )

;; Undo/redo window configurations.
(use-package winner
  :ensure nil
  :bind (
         ("C-c C-," . winner-undo)
         ("C-c C-." . winner-redo)
         )
  :config (winner-mode t))

;; REMOVED: Performance hit and I wasn't using it much.
;; (use-package which-func
;;   :ensure nil
;;   )

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
  (setq anzu-cons-mode-line-p t)
  (global-anzu-mode))

;; Avy mode (jump to a char/word using a decision tree).
(use-package avy
  :bind (
         ("C-," . avy-goto-line-end)
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
                      ?w ?e ?r ?u    ?o))

  ;; Set the background to gray to highlight the decision tree?
  (setq avy-background nil)
  )

;; Move buffers around.
(use-package buffer-move
  :bind (
         ("<s-up>"    . buf-move-up)
         ("<s-down>"  . buf-move-down)
         ("<s-left>"  . buf-move-left)
         ("<s-right>" . buf-move-right)
         ))

;; Copy selected region to be pasted into Slack/Github/etc.
(use-package copy-as-format
  :defer t)

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

;; Show example usage when examining elisp functions.
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

  (defun mark-inside-backticks ()
    "Mark up to enclosing `backticks`, not including the backticks."
    (interactive)
    (search-forward "`")
    (backward-char)
    (set-mark (point))
    (search-backward "`")
    (forward-char)
    )
  (defun mark-outside-backticks ()
    "Mark the enclosing `backticks`, including the backticks."
    (interactive)
    (search-forward "`")
    (set-mark (point))
    (search-backward "`" nil nil 2)
    )

  (setq er/try-expand-list
        (append er/try-expand-list
                '(mark-inside-backticks mark-outside-backticks)))

  ;; Fix region not highlighting.
  (setq
   shift-select-mode nil
   expand-region-fast-keys-enabled nil
   )
  )

;; Workspaces.
(use-package eyebrowse
  ;; To prevent mode-line display errors.
  :demand t

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
   eyebrowse-mode-line-left-delimiter "["
   eyebrowse-mode-line-right-delimiter "]"
   )

  (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :bold t)

  ;;; Show workspaces in title bar.

  ;; Only recalculate the workspaces string when it actually changes.
  (defvar eyebrowse-workspaces)
  (defun eyebrowse-current-workspace ()
    "Get the current workspace number."
    (eyebrowse--get 'current-slot))
  (defun eyebrowse-workspaces-update ()
    "Updates eyebrowse workspaces."
    ;; Update any visible org-agenda buffers.
    ;; (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
    ;; Update the workspaces string.
    (let ((workspaces (substring-no-properties (eyebrowse-mode-line-indicator))))
      (setq eyebrowse-workspaces workspaces)))
  (defun eyebrowse-workspaces-update-one-arg (_arg1)
    "Advice for `eyebrowse-workspaces-update' with one arg."
    (eyebrowse-workspaces-update))
  (defun eyebrowse-workspaces-update-two-args (_arg1 _arg2)
    "Advice for `eyebrowse-workspaces-update' with two args."
    (eyebrowse-workspaces-update))
  (eyebrowse-workspaces-update)

  (add-hook 'eyebrowse-post-window-switch-hook 'eyebrowse-workspaces-update)
  (advice-add 'eyebrowse-close-window-config :after #'eyebrowse-workspaces-update)
  (advice-add 'eyebrowse-rename-window-config :after #'eyebrowse-workspaces-update-two-args)
  (advice-add 'other-frame :after #'eyebrowse-workspaces-update-one-arg)
  (advice-add 'make-frame :after #'eyebrowse-workspaces-update)
  (advice-add 'delete-frame :after #'eyebrowse-workspaces-update)

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

;; Alternative to volatile-highlights.
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  ;; Set to nil to disable pulsing.
  (setq-default goggles-pulse nil)
  )

;; Highlight indentation.
;; TODO: Use https://github.com/jdtsmith/indent-bars#installconfig
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq
   highlight-indent-guides-method 'column
   ;; Automatically calculate faces?
   highlight-indent-guides-auto-enabled nil
   ;; Should the current indentation under point be highlighted?
   highlight-indent-guides-responsive 'top
   highlight-indent-guides-delay 0
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
  :hook ((prog-mode conf-mode text-mode eshell-mode) . idle-highlight-mode)
  :config

  (setq
   idle-highlight-exclude-point t
   idle-highlight-idle-time highlight-delay
   )

  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     (when (derived-mode-p 'org-mode)
       (setq-local idle-highlight-exceptions '("-" "*" "**" "***" "****" "*****")))
     (when (derived-mode-p 'outline-mode)
       (setq-local idle-highlight-exceptions '("*" "**" "***" "****" "*****")))
     (when (derived-mode-p 'markdown-mode)
       (setq-local idle-highlight-exceptions '("-")))
     ))
  )

;; Modify multiple occurrences simultaneously.
(use-package iedit
  :bind ("C-=" . iedit-mode)
  )

;; A package for choosing a color by updating text sample.
;; See https://www.emacswiki.org/emacs/MakeColor.
(use-package make-color
  :defer t)

;; Multiple cursors.
(use-package multiple-cursors
  :bind (
         ("C-{" . mc/mark-previous-like-this)
         ("C-}" . mc/mark-next-like-this)

         ;; Add cursors with the mouse!
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)

         :map mc/keymap

         ;; Reclaim some keys...
         ("<return>" . nil)
         ("C-'" . nil)
         )
  :config
  (setq mc/always-run-for-all t)
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
        uniquify-min-dir-content 1
        uniquify-strip-common-suffix nil
        )
  )

;; Display available keys.
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-sort-uppercase-first nil))

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

;;; Git packages

;; Generate links to Github for current code location.
(use-package git-link
  :defer t
  :config
  ;; With a double prefix argument invert the value of `git-link-use-commit'.
  (setq git-link-use-commit t)
  )

;; .gitignore etc.
(use-package git-modes)

;; Browse historic versions of a file.
(use-package git-timemachine
  :defer t)

;; Git client in Emacs.
(use-package magit
  :bind (("C-x g" . magit-status)
         ("s-g" . magit-status))

  :init
  (setq
   ;; Show fine differences for all displayed diff hunks.
   magit-diff-refine-hunk `all
   ;; How to display new magit buffers?
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   ;; Don't ask before saving repository buffers.
   magit-save-repository-buffers 'dontask
   ;; Stop magit from stupidly messing up my window configuration when quitting
   ;; buffers.
   magit-bury-buffer-function 'quit-window
   ;; Show diffs in the commit flow?
   magit-commit-show-diff nil
   ;; How many recent commits to show in certain log sections.
   magit-log-section-commit-count 50
   )

  :config
  (magit-auto-revert-mode t)
  )

;; Quick and easy organization of repos and jumping to them.
(use-package my-repo-pins
  :bind (("s-h" . my-repo-pins))
  :config
  (setq
   my-repo-pins-code-root user-code-directory
   my-repo-pins-max-depth 2
   my-repo-pins-open-function #'magit-status
   )
  )

;;; Project packages

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
    (define-key company-active-map (read-kbd-macro (format "s-%d" i)) 'company-complete-number))

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
    (setq-local left-fringe-width 16)
    (turn-on-diff-hl-mode))

  :config

  ;; Show diffs in margin when running in terminal.
  (unless (window-system) (diff-hl-margin-mode))

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

; TODO: Built-in in 29.1
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
  :bind ("C-!" . flycheck-list-errors)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ;; (setq flycheck-check-syntax-automatically nil)

  ;; Set shorter delay for displaying errors at point.
  (setq flycheck-display-errors-delay (* 1 info-delay))
  (setq sentence-end-double-space nil) ;; Stupid check.
  (setq flycheck-checker-error-threshold 600)

  ;; Disable checkers.
  (setq-default flycheck-disabled-checkers '(proselint rust rust-cargo rust-clippy))
  ;; (setq-default flycheck-disabled-checkers '(rust)) ;; Doesn't work.

  ;; (flycheck-add-next-checker 'rust-cargo 'rust-clippy)
  )

;; Elisp package lints.
(use-package flycheck-package
  :hook (flycheck-mode . flycheck-package-setup))

;; Project manager.
(use-package projectile
  :defer t
  :hook (prog-mode . projectile-mode)
  :bind ("s-w" . projectile-run-eshell)
  :config
  (setq projectile-completion-system 'auto)

  ;; Integrate projectile with consult.
  (use-package consult-projectile
    :bind ("s-;" . consult-projectile)
    )
  )

;; Jump to definitions using dumb-jump as a fallback.
(use-package smart-jump
  :config
  (smart-jump-setup-default-registers)
  )

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq
   dumb-jump-force-searcher 'rg
   ;; Ignore `target`, `node_modules` directories.
   ;;
   ;; I think dumb-jump's regexes require pcre2.
   dumb-jump-rg-search-args "--pcre2 --glob '!{target,node_modules}'"
   )
  )

;; Better syntax highlighting.
(use-package tree-sitter
  :demand t

  :config

  (use-package tree-sitter-langs
    :demand t
    )

  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode)
  )

;;; Languages / Language packages

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

;; REMOVED: Performance issues I think? Try again?
;; Typescript
;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook (
;;          (typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          ;; REMOVED: Messes up point position.
;;          ;; (before-save . tide-format-before-save)
;;          )
;;   :config
;;   (setq
;;    typescript-indent-level 2
;;    )
;;   )
(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . eglot-ensure)
  :custom
  (typescript-indent-level 2)
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

;; REMOVED: rust-analyzer takes too many resources and has too many annoying
;;          features.
;; ;; Enhanced Rust mode with automatic LSP support.
;; (use-package rustic
;;   :bind (:map rustic-mode-map ("C-c n" . rustic-format-file))

;;   ;; :init

;;   ;; (defun format-rust ()
;;   ;;   (interactive)
;;   ;;   ;; Save all buffers since `rustic-cargo-fmt' formats all buffers belonging
;;   ;;   ;; to the workspace.
;;   ;;   (save-all)
;;   ;;   (rustic-cargo-fmt)
;;   ;;   )

;;   :config

;;   (setq
;;    ;; eglot seems to be the best option right now.
;;    rustic-lsp-client 'eglot
;;    rustic-format-on-save nil
;;    rustic-rustfmt-args "+nightly"
;;    )
;;   )

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

;;; Org Mode

(use-package org
  :ensure nil

  :bind (
         ;; Insert link with C-c C-l.
         ;; ("C-c C-l" . org-store-link)
         ("C-c c" . org-capture)

         ;; Jump to last refile or capture.
         ("C-c j" . org-refile-goto-last-stored)

         :map org-mode-map

         ;; ("<s-return>" . org-meta-return-end)
         ("C-S-n" . org-metadown)
         ("C-S-p" . org-metaup)
         ("C-<" . org-shiftmetaleft)
         ("C->" . org-shiftmetaright)
         ("M-p" . org-previous-visible-heading)
         ("M-n" . org-next-visible-heading)
         ("C-^" . org-up-element)
         ("C-j" . join-next-line)
         ("C-c SPC" . org-table-blank-field)
         ("C-c C-j" . consult-org-heading) ; orig. org-goto

         ("<mouse-3>" . mouse-org-cycle)
         )

  :mode ("\\.org$" . org-mode)

  :hook (
         (org-mode . org-mode-hook-fun)
         (org-mode . company-mode)
         )

  :init

  (defun org-align-tags-all ()
    (interactive)
    (org-align-tags t))

  (defun org-update-cookies-after-save ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (org-update-statistics-cookies "ALL")))

  (defun org-mode-hook-fun ()
    "Initialize `org-mode'."

    ;; Unbind key stolen by org-mode ('org-cycle-agenda-files').
    (local-unset-key (kbd "C-,"))
    (local-unset-key (kbd "C-'"))

    ;; Align all tags.
    (add-hook 'before-save-hook 'org-align-tags-all nil 'make-it-local)

    ;; Update checkboxes after saving.
    (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)
    )

  :config

  ;;; Settings

  (setq
   ;; Default org directory.
   org-directory user-org-directory

   ;; Initial visibility.
   org-startup-folded t

   ;; Inline images.
   org-startup-with-inline-images t
   org-image-actual-width '(0.5)

   ;; Hide leading stars?
   org-hide-leading-stars t
   org-adapt-indentation nil
   org-startup-indented nil
   org-odd-levels-only nil
   ;; Use different styling for nested bullets.
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))

   org-ellipsis " …"

   ;; Non-nil means unchecked boxes will prevent switching the parent to DONE.
   org-enforce-todo-checkbox-dependencies nil
   ;; All subtasks must be DONE before marking a task as DONE.
   org-enforce-todo-dependencies t

   ;; Try to keep cursor before ellipses.
   org-special-ctrl-a/e t
   ;; Smart editing of invisible region around ellipses.
   org-catch-invisible-edits 'smart

   ;; Prefer rescheduling to future dates and times.
   org-read-date-prefer-future 'time

   ;; M-RET should not split the heading if point is not at the end of a line.
   ;; (setq org-M-RET-may-split-line nil)

   ;; Should ‘org-insert-heading’ leave a blank line before new heading/item?
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))

   ;; Custom to-do states.
   org-todo-keywords
   '((sequence "TODO(t)" "NOW(n)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)")
     (sequence "|" "CANCELED(x)"))

   ;; tag settings

   ;; Don't align tags.
   ;; org-tags-column 0
   ;; org-auto-align-tags nil

   ;; Should the ORDERED property also be shown as a tag?
   org-track-ordered-property-with-tag t

   ;; logging settings

   ;; Log into LOGBOOK drawer.
   org-log-into-drawer t
   ;; Log time a task was set to Done.
   org-log-done 'time
   ;; Don't log the time a task was rescheduled or redeadlined.
   org-log-reschedule nil
   org-log-redeadline nil

   ;; org-refile settings

   ;; Refile notes to the top of the list?
   org-reverse-note-order nil
   ;; Use headline paths (level1/level2/...)
   org-refile-use-outline-path t
   ;; Go down in steps when completing a path.
   org-outline-path-complete-in-steps nil
   org-refile-targets
   '(
     ;; The current buffer.
     (nil . (:maxlevel . 99))
     ;; My custom files.
     (user-todo-org . (:maxlevel . 99))
     (user-notes-org . (:maxlevel . 99))
     (user-work-org . (:maxlevel . 99))
     (user-ideas-org . (:maxlevel . 99))
     (user-projects-org . (:maxlevel . 99))
     )
   ;; Jump to headings with completion.
   org-goto-interface 'outline-path-interface
   org-goto-max-level 99
   ;; Always show full context, no matter how we get to a certain heading (e.g.
   ;; `isearch', `org-goto', whatever).
   org-fold-show-context-detail '((default . tree))
   )

  ;; org-capture settings

  ;; org-capture template.
  (setq org-capture-templates
        '(
          (
           "o" "One-off task." entry
           (file+headline "todo.org" "General")
           "* %?\nSCHEDULED: %t"
           :unnarrowed t
           :empty-lines-before 1
           )
          (
           "r" "Recurring task." entry
           (file+olp "todo.org" "Recurring" "General")
           "* |%^{Recurrence}| %?\nSCHEDULED: %t"
           :unnarrowed t
           :empty-lines-before 1
           )
          (
           "w" "Work task." entry
           (file+headline "work.org" "Todo")
           "* TODO %?"
           :unnarrowed t
           :empty-lines-before 1
           :prepend 1
           )
          ))

  ;; Shortcuts/Keybindings

  ;; REMOVED: Not using it for now.
  ;; ;; org-capture with template as default behavior.
  ;; (defun org-task-capture ()
  ;;   "Capture a task with my todo template."
  ;;   (interactive)
  ;;   (org-capture nil "t"))
  ;; (defun org-note-capture ()
  ;;   "Capture a note with my note template."
  ;;   (interactive)
  ;;   (org-capture nil "n"))

  (defun mouse-org-cycle (@click)
    (interactive "e")
    (let ((p1 (posn-point (event-start @click))))
      (goto-char p1)
      (call-interactively 'org-cycle)
      )
    )

  (defun org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries
     (lambda ()
       (org-with-wide-buffer
        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
        ;; newlines before the current heading, so we do this part widened.
        (while (not (looking-back "\n\n" nil))
          ;; Insert blank lines before heading.
          (insert "\n")))
       (let ((end (org-entry-end-position)))
         ;; Insert blank lines before entry content
         (forward-line)
         (while (and (org-at-planning-p)
                     (< (point) (point-max)))
           ;; Skip planning lines
           (forward-line))
         (while (re-search-forward org-drawer-regexp end t)
           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
           ;; for some reason it doesn't work correctly when operating on hidden text.
           ;; This works, taken from `org-agenda-get-some-entry-text'.
           (re-search-forward "^[ \t]*:END:.*\n?" end t)
           (goto-char (match-end 0)))
         (unless (or (= (point) (point-max))
                     (org-at-heading-p)
                     (looking-at-p "\n"))
           (insert "\n"))))
     t (if prefix
           nil
         'tree)))

  ;;; org packages

  ;; Markdown export.
  (require 'ox-md)

  (use-package org-agenda
    :ensure nil
    :hook (org-agenda-mode . visual-line-mode)
    :bind (
           ("C-c a" . org-agenda-personal)
           ("C-c w" . org-agenda-work)

           :map org-agenda-mode-map

           ("s" . org-agenda-schedule)
           ("M" . org-agenda-bulk-mark-all)
           ("M-n" . org-agenda-next-date-line)
           ("M-p" . org-agenda-previous-date-line)
           )

    :init

    (defun org-agenda-personal ()
      (interactive)
      ;; Set location of agenda files.
      (setq org-agenda-files (list
                              user-todo-org
                              ))
      (org-agenda-list)
      )
    (defun org-agenda-work ()
      (interactive)
      ;; Set location of agenda files.
      (setq org-agenda-files (list
                              user-work-org
                              ))
      (org-todo-list)
      )

    :config

    ;; Set default span of agenda view.
    (setq org-agenda-span 'week)

    ;; Show scheduled items in order from most to least recent.
    (setq org-agenda-sorting-strategy
          '((agenda habit-down time-up scheduled-down priority-down category-keep)
            (todo   priority-down category-keep)
            (tags   priority-down category-keep)
            (search category-keep)))

    ;; Customize columns (remove filename/category, mostly redundant).
    (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                     (todo . " %i %-12:c")
                                     (tags . " %i %-12:c")
                                     (search . " %i %-12:c")))

    (setq
     ;; Stop org-agenda from messing up my windows!!
     org-agenda-window-setup 'current-window
     ;; Start org-agenda from the current day.
     org-agenda-start-on-weekday nil
     ;; Don't align tags in the org-agenda (sometimes it messes up the display).
     ;; org-agenda-tags-column 0
     )

    (defun org-agenda-refresh ()
      "Refresh all `org-agenda' buffers."
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-agenda-mode)
            (org-agenda-maybe-redo)
            ))))

    ;; Refresh org-agenda after changing an item status.
    ;; (add-hook 'org-trigger-hook 'org-agenda-refresh)
    ;; Refresh org-agenda after rescheduling a task.
    (defadvice org-schedule (after refresh-agenda activate)
      "Refresh `org-agenda'."
      (org-agenda-refresh))

    ;; Refresh org-agenda after an org-capture.
    (add-hook 'org-capture-after-finalize-hook 'org-agenda-refresh)
    )

  ;; Recurring org-mode tasks.
  (use-package org-recur
    ;; :load-path "~/projects/org-recur/"
    :after org
    :bind (
           :map org-recur-mode-map

           ;; ("C-c d" . org-recur-finish)
           ("C-c 0" . org-recur-schedule-today)
           ("C-c 1" . org-recur-schedule-1)
           ("C-c 2" . org-recur-schedule-2)

           :map org-recur-agenda-mode-map

           ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
           ("d" . org-recur-finish)
           ("0" . org-recur-schedule-today)
           ("1" . org-recur-schedule-1)
           ("2" . org-recur-schedule-2)
           ;; ("C-c d" . org-recur-finish)
           ("C-c 0" . org-recur-schedule-today)
           ("C-c 1" . org-recur-schedule-1)
           ("C-c 2" . org-recur-schedule-2)
           )
    :hook ((org-mode . org-recur-mode)
           (org-agenda-mode . org-recur-agenda-mode))
    :demand t
    :config
    (defun org-recur-schedule-1 ()
      (interactive)
      (org-recur-schedule-date "|+1|"))
    (defun org-recur-schedule-2 ()
      (interactive)
      (org-recur-schedule-date "|+2|"))

    (setq org-recur-finish-done t
          ;; `org-log-done' should not be 'note if this is t.
          org-recur-finish-archive t)
    )

  ;; Display groups in org-agenda to make things a bit more organized.
  (use-package org-super-agenda
    :after org-agenda
    :config
    (org-super-agenda-mode)

    (setq
     org-super-agenda-header-separator ""
     org-super-agenda-unmatched-name "Other"
     org-super-agenda-groups
     '(
       ;; Each group has an implicit OR operator between its selectors.
       (:name "Today"  ; Optionally specify section name
              :time-grid t  ; Items that appear on the time grid.
              :todo "TODAY"   ; Items that have this todo keyword.
              )
       (:name "Work"
              :category "work"
              :tag "work"
              )
       (:name "High Priority"
              :priority "A"
              :order 1
              )
       (:name "Deep Work"
              :category "deep"
              :tag "deep"
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
       (:name "Financial"
              :category "financial"
              :tag "financial"
              :order 7
              )
       (:name "Self-improvement"
              :category "self"
              :tag "self"
              :order 8
              )
       (:name "Physical"
              :category "physical"
              :tag "physical"
              :order 10
              )

       (:name "Move"
              :category "move"
              :tag "move"
              :order 20
              )

       (:name "Travel"
              :category "travel"
              :tag "travel"
              :order 30
              )

       ;; Here, the agenda will display items that didn't match any of these
       ;; groups, with the default order position of 99.

       (:name "Tech"
              :category "tech"
              :tag "tech"
              :order 200
              )
       (:name "Blog"
              :category "blog"
              :tag "blog"
              :order 210
              )
       (:name "To Read"
              :category "read"
              :tag "read"
              :order 220
              )
       (:name "To Watch"
              :category "watch"
              :tag "watch"
              :order 230
              )
       (:name "Waiting"
              :todo "WAITING"
              :order 240
              )
       (:name "Reminders"
              :category "reminder"
              :tag "reminder"
              :order 250
              )
       (:name "Evening"
              :category "evening"
              :tag "evening"
              :order 260
              )
       (:name "Low priority"
              :priority "C"
              :order 500
              )
       )))
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

;; REMOVED: What was this for?
;; (defun mode-line-update-all ()
;;   "Update all mode lines."
;;   (force-mode-line-update t))
;; (add-hook 'buffer-list-update-hook 'mode-line-update-all)

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
     ;; Winum string.
     " ["
     '(:eval (winum-get-number-string))
     "] "
     ;; Modified indicator.
     'mode-line-modified
     " "
     ;; Buffer name.
     '(:eval (propertize "%b"
                         'face '(:weight bold)
                         'help-echo (buffer-file-name)))
     " |"
     ;; The current line/column.
     '(:eval (when line-number-mode " %l"))
     '(:eval (when column-number-mode ":%C"))
     " "
     ;; The total number of lines. Only recount after certain events, like
     ;; saving.
     '(:eval
       (when (and line-number-mode
                  mode-line-buffer-line-count
                  buffer-file-name)
         (let ((modified (if (buffer-modified-p) "*" "")))
           (format "[%s%s]" mode-line-buffer-line-count modified)
           )))
     ;; The buffer/filesize.
     '(:eval "[%I] ")
     ;; Major mode.
     '(:eval (propertize (format-mode-line mode-name)
                         'face '(:weight bold)
                         'help-echo (format "%s" major-mode)
                         ))
     " "
     ;; Limited set of useful minor modes.
     `(:eval (when iedit-mode "[=iedit=] "))

     ;; Read-only.
     '(:eval (when buffer-read-only
               (propertize "RO "
                           'face 'font-lock-preprocessor-face
                           'help-echo "Buffer is read-only")))
     ;; Number of characters in the region.
     '(:eval
       (when mark-active
         (let ((region-count (abs (- (point) (mark)))))
           (when (> region-count 0)
             (format "{%s} " (number-to-string region-count)))
           )))
     ;; Latest eshell command status.
     '(:eval
       (when (string-equal major-mode 'eshell-mode)
         (let ((status
                (if eshell-current-command
                    "..."
                  eshell-last-command-status
                  )))
           (format "[status: %s] " status)
           )))
     ;; Recursive editing level.
     "%[%] "

     ;; REMOVED: Performance hit and I wasn't using it much.
     ;; '(:eval
     ;;   (when (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
     ;;     (let ((f (which-function)))
     ;;       (when f
     ;;         (concat "[" f "] ")
     ;;         ))))

     ;; " "
     ;; '(:eval (propertize (format-time-string "%H:%M")))
     ))))

;; Start server.

(server-start)

;; Misc

;; Open stuff on startup.
(defun emacs-welcome()
  "Display Emacs welcome screen."
  (interactive)

  ;; Set up org files and agenda.

  (find-file user-notes-org)
  (split-window-right-focus)
  (find-file user-todo-org)

  ;; Name eyebrowse slots.

  (eyebrowse-rename-window-config 1 "org")
  )

(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
