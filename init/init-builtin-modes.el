;;; init-builtin-modes --- Built-in mode settings
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)

(use-package isearch
  :ensure nil
  :bind (
         :map isearch-mode-map
         ("C-g" . isearch-abort-fixed)
         )
  :init
  (defun isearch-abort-fixed ()
    "Fix broken C-g."
    (interactive)
    (isearch-abort)
    (isearch-abort)
    )

  :config

  (setq
   ;; Can scroll in isearch?
   isearch-allow-scroll t

   ;; Highlight more matches after a delay.
   isearch-lazy-highlight t
   isearch-lazy-count t
   lazy-highlight-initial-delay info-delay
   )

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
    (setq
     dired-omit-files (concat dired-omit-files "\\|\\.bk$\\|^\\.DS_Store$")
     dired-omit-verbose nil
     )
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
   )

  ;; Add consult-outline support.
  (add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))

  ;; Set keys up in this hook. This doesn't work in :bind.
  (add-hook 'eshell-first-time-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
                (define-key eshell-mode-map (kbd "M-m") 'beginning-of-line)
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
  ;; TODO: why is this needed?
  (add-hook 'eshell-mode-hook #'(lambda () (setq eshell-exit-hook nil)))

  ;; Show the directory contents after changing directories.
  (add-hook 'eshell-directory-change-hook
            (lambda ()
              (eshell/ls "-a")))

  (use-package em-hist
    :ensure nil
    :config
    (setq
     eshell-hist-ignoredups t
     ;; Set the history file.
     eshell-history-file-name "~/.zsh_history"
     ;; If nil, use HISTSIZE as the history size.
     eshell-history-size 10000
     )
    )

  (use-package em-script
    :ensure nil
    :config
    (setq
     ;; File locations.
     eshell-rc-script (concat user-emacs-directory ".eshell-rc")
     eshell-login-script (concat user-emacs-directory ".eshell-login")
     )
    )

  ;; Open a new eshell buffer.
  (defun eshell-new ()
    "Open a new eshell buffer."
    (interactive)
    (eshell t))

  ;;; Set up a custom prompt.

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
                     'face           (cdr list)
                     'front-sticky   '(font-lock-face read-only)
                     'rear-nonsticky '(font-lock-face read-only)))

       `(
         ;; Line to distinguish end of previous output.
         ("==="
          font-lock-comment-face)
         ("\n")
         ;; Timestamp.
         (,(format-time-string "[%a, %b %d | %H:%M:%S]\n" (current-time))
          font-lock-keyword-face)
         ;; Directory.
         ;;
         ;; Try to abbreviate-file-name of current directory as per `eshell'
         ;; defaults, e.g. display `~' instead of `/path/to/user/home'.
         (,(format "[%s]" (abbreviate-file-name (eshell/pwd)))
          font-lock-constant-face)
         ;; Git branch.
         (,(if (string= git-branch "") "" (format " %s" git-branch))
          font-lock-preprocessor-face)
         ;; The last exit code.
         (,(if-let ((status eshell-last-command-status))
               (if (= status 0) "" (format " [%s]" status)))
          error)
         ("\n")
         ;; NOTE: Choose between prompts # and $ depending on user privileges,
         ;; as per Bourne and eshell defaults.
         (,(if (zerop (user-uid)) " # " " $ ")
          minibuffer-prompt)
         )
       ""))
    )
  (setq eshell-prompt-function 'custom-eshell-prompt)
  ;; Should the prompt be highlighted?
  (setq eshell-highlight-prompt nil)

  ;;; Load external eshell packages.

  (use-package compnav-eshell
    :load-path "~/.emacs.d/packages/compnav-eshell"
    )

  (use-package eshell-syntax-highlighting
    :config
    ;; Enable in all Eshell buffers.
    (eshell-syntax-highlighting-global-mode 1))
  )

(use-package outline
  :ensure nil
  :bind (
         :map outline-minor-mode-map

         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-heading)
         ("C-<return>" . outline-insert-heading)
         ("C-<" . outline-promote)
         ("C->" . outline-demote)
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

(provide 'init-builtin-modes)
;;; init-builtin-modes.el ends here
