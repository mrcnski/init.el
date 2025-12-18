;;; init-package --- Package settings. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)

(require 'package)
(setq
 ;; Set the elpa directory.
 package-user-dir user-emacs-elpa-directory
 ;; Prefer the newest version of a package.
 load-prefer-newer t
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

(provide 'init-package)
;;; init-package.el ends here
