;;; init-early-utils --- Utilities that are useful to have ASAP (in case loading crashes).
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)

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
  :ensure t
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
    (setq
     exec-path-from-shell-shell-name "/bin/bash"
     ;; Don't start interactively (should be faster). Env vars should be defined
     ;; in .profile.
     exec-path-from-shell-arguments '("-l")
     )
    ;; Set the desired env vars.
    (dolist (var '(
                   "ANTHROPIC_AUTH_TOKEN"
                   "ANTHROPIC_API_KEY"
                   "ANTHROPIC_BASE_URL"
                   "COMPNAV_DIR"
                   "COMPNAV_H_REPOS_DIR"
                   "HISTSIZE"
                   ))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)

    ;; Check that each variable has been set.
    (dolist (var exec-path-from-shell-variables)
      (unless (getenv var)
        (message "Warning: env var %s is not set" var)))
    ))

;; Enable restarting Emacs from within Emacs.
;; NOTE: built in functionality as of Emacs 29.
(use-package restart-emacs
  :defer t)

;; Find bugs in Emacs configuration.
(use-package bug-hunter
  :defer t)

(provide 'init-early-utils)
;;; init-early-utils.el ends here
