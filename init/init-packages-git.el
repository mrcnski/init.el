;;; init-packages-git --- Load git packages.
;;
;;; Commentary:
;;
;;; Code:

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
  (setopt
   ;; Show fine differences for all displayed diff hunks.
   magit-diff-refine-hunk `all
   ;; How to display new magit buffers?
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   ;; Don't ask before saving repository buffers.
   magit-save-repository-buffers 'dontask
   ;; Stop magit from messing up my window configuration when quitting buffers.
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

(provide 'init-packages-git)
;;; init-packages-git.el ends here
