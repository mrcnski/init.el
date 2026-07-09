;;; init-package --- Package settings. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-basics)
(require 'package)
;; Built-in since 29.1.
(require 'use-package)

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

;; On a fresh machine, fetch the package archives up front so the
;; `:ensure' installs below start from current contents.
(when (not (file-directory-p user-emacs-elpa-directory))
  (package-refresh-contents))

;; Always install missing packages.
(setq use-package-always-ensure t)

(provide 'init-package)
;;; init-package.el ends here
