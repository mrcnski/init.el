;;; init-packages-git --- Load git packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)
(require 'desktop)  ; for the magit desktop integration below

;; Jump to changed/tracked files and stashes via consult.
(use-package consult-ls-git
  :bind (
         ;; lists just the changed files
         ("s-:" . consult-ls-git-ls-status)
         ;; shows all sources (narrow with s/z/f).
         ("C-c g" . consult-ls-git)
         )
  )

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

  ;; Clean buffer-naming scheme: "magit: NAME".
  (declare-function magit-generate-buffer-name-default-function "magit-mode")
  (defun magit-generate-short-buffer-name (mode &optional value)
    "Return \"magit: NAME\" (repo basename).
`magit-uniquify-buffer-names' controls two things in magit:

- whether generated names use the repo's directory name (\"magit:
dotfiles\") instead of its full path (\"*magit: ~/Sync/Home/dotfiles*\")

- whether new buffers are registered with uniquify.

We want the short names without the registration, so bind the variable
non-nil while the stock generator formats the name. The
registration step in `magit-generate-new-buffer' runs after this binding
has exited and sees the real value."
    (let ((magit-uniquify-buffer-names t))
      (magit-generate-buffer-name-default-function mode value)))

  (setopt
   ;; Short names via the generator above; see its commentary.
   magit-uniquify-buffer-names nil
   magit-generate-buffer-name-function #'magit-generate-short-buffer-name
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
   magit-commit-show-diff t
   ;; How many recent commits to show in certain log sections.
   magit-log-section-commit-count 50
   ;; Homebrew git is ~2x faster to start than /usr/bin/git.
   ;; Magit spawns many git subprocesses per refresh, so this compounds.
   magit-git-executable "/opt/homebrew/bin/git"
   )

  ;; Desktop integration: recreate status buffers on desktop restore, so
  ;; eyebrowse workspaces showing magit don't collapse to scratch.
  ;;
  ;; The restored buffer keeps the exact saved name, so eyebrowse's window
  ;; configs always find it, including the "<2>" suffix for same-named repos.
  ;;
  ;; Registered in :init since magit is deferred but desktop-read runs at
  ;; startup.
  (defun magit-save-desktop-buffer (_desktop-dirname)
    "Return the repository directory to persist in the desktop file."
    default-directory)

  (defun magit-restore-desktop-buffer (_file-name buffer-name repo)
    "Recreate a magit status buffer named BUFFER-NAME for REPO."
    (when (file-directory-p repo)
      (require 'magit)
      ;; Leave the window layout to eyebrowse.
      (save-window-excursion
        (let ((buffer (magit-status-setup-buffer repo)))
          (with-current-buffer buffer
            (rename-buffer buffer-name t))
          buffer))))

  (add-hook 'magit-status-mode-hook
            (lambda ()
              (setq-local desktop-save-buffer #'magit-save-desktop-buffer)))
  (add-to-list 'desktop-buffer-mode-handlers
               '(magit-status-mode . magit-restore-desktop-buffer))

  :config
  (magit-auto-revert-mode t)

  ;; Reveal the position magit jumps to in a folded org buffer.
  (declare-function org-reveal "org-fold")
  (defun magit-diff-visit-reveal-org-context ()
    "Unfold around point after magit jumps into an org buffer."
    (when (derived-mode-p 'org-mode)
      (org-reveal)
      ))
  (add-hook 'magit-diff-visit-file-hook #'magit-diff-visit-reveal-org-context)
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
