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

  ;;; Desktop integration

  ;; Re-uniquify magit buffer names after a desktop restore.
  ;;
  ;; magit names buffers via `uniquify' (see `magit-uniquify-buffer-names'),
  ;; so a status buffer for a worktree ends up as e.g.
  ;; "magit: desmos-classroom-2<github.com/amplify-education>" once its sibling
  ;; file/dired buffers in the same directory are present.  `desktop' saves and
  ;; restores buffers under their uniquify *base* name ("magit: desmos-classroom-2")
  ;; and renames each restored buffer back to that base -- but eyebrowse persists
  ;; the *uniquified* names in its window configs.  After a restore the two
  ;; disagree, so `window-state-put' can't find the buffers eyebrowse references
  ;; and those windows collapse to *scratch*.  Which names happen to match is
  ;; order-dependent, so only some workspaces come back.
  ;;
  ;; Re-run magit's own uniquification (mirroring `magit-toggle-buffer-lock')
  ;; over every restored magit buffer on `desktop-after-read-hook'.  That runs at
  ;; the end of `desktop-read' (on `after-init-hook') -- after every buffer,
  ;; including the sibling file/dired buffers uniquify keys off, has been
  ;; restored, and before eyebrowse restores window configs (on
  ;; `emacs-startup-hook').  With the same buffer set present, the result matches
  ;; the live names eyebrowse recorded.
  (defun magit-reuniquify-desktop-buffer-names ()
    "Re-apply magit's buffer-name uniquification after a desktop restore."
    (when magit-uniquify-buffer-names
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'magit-mode)
            (let* ((mode major-mode)
                   (name (if magit-buffer-locked-p
                             (funcall magit-generate-buffer-name-function
                                      mode (magit-buffer-value))
                           (funcall magit-generate-buffer-name-function mode))))
              (rename-buffer (generate-new-buffer-name name))
              (with-temp-buffer
                (magit--maybe-uniquify-buffer-names buffer name mode))))))))
  (add-hook 'desktop-after-read-hook #'magit-reuniquify-desktop-buffer-names)

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
