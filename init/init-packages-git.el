;;; init-packages-git --- Load git packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)
(require 'desktop)  ; for the magit desktop integration below

;; Generate links to Github for current code location.
(use-package git-link
  :defer t
  :config
  ;; With a double prefix argument invert the value of `git-link-use-commit'.
  (setq git-link-use-commit t)
  )

;; .gitignore etc.
(use-package git-modes
  :mode (("\\.dockerignore\\'" . gitignore-mode))
  )

;; Browse historic versions of a file.
(use-package git-timemachine
  :defer t)

;; Git client in Emacs.
(use-package magit
  :bind (("C-x g" . magit-status)
         ("s-g" . magit-status))

  :init
  (setopt
   ;; Show fine (word-level) differences only for the hunk at point.
   ;; `all' refines every visible hunk on each render, which is one of the
   ;; biggest fontification costs on large diffs.
   magit-diff-refine-hunk t
   ;; How to display new magit buffers?
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   ;; Don't ask before saving repository buffers.
   magit-save-repository-buffers 'dontask
   ;; Stop magit from messing up my window configuration when quitting buffers.
   magit-bury-buffer-function 'quit-window
   ;; Show diffs in the commit flow?
   magit-commit-show-diff nil
   ;; How many recent commits to show in certain log sections.
   magit-log-section-commit-count 16
   ;; Homebrew git is ~2x faster to start than /usr/bin/git.
   ;; Magit spawns many git subprocesses per refresh, so this compounds.
   magit-git-executable "/opt/homebrew/bin/git"
   )

  ;; Desktop integration: recreate status buffers on desktop restore, so
  ;; eyebrowse workspaces showing magit don't collapse to scratch
  ;; (desktop only persists file-visiting buffers).  Status buffers
  ;; only; each restore runs a full refresh.  Registered in :init since
  ;; magit is deferred but desktop-read runs at startup.
  (defun magit-save-desktop-buffer (_desktop-dirname)
    "Return the repository directory to persist in the desktop file."
    default-directory)

  (defun magit-restore-desktop-buffer (_file-name _buffer-name repo)
    "Recreate a magit status buffer for REPO on desktop restore."
    (when (file-directory-p repo)
      (require 'magit)
      ;; Leave the window layout to eyebrowse.
      (save-window-excursion
        (magit-status-setup-buffer repo))))

  (add-hook 'magit-status-mode-hook
            (lambda ()
              (setq-local desktop-save-buffer #'magit-save-desktop-buffer)))
  (add-to-list 'desktop-buffer-mode-handlers
               '(magit-status-mode . magit-restore-desktop-buffer))

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

(provide 'init-packages-git)
;;; init-packages-git.el ends here
