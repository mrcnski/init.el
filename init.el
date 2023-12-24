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

(add-to-list 'load-path "/Users/marcin/.emacs.d/init")

(require 'init-basics)
(require 'init-package) ;; Can use Melpa packages after this.
(require 'init-early-utils) ;; Things good to have early.
(require 'init-vertico-et-al) ;; Also good to have early.
(require 'init-builtin-settings)
(require 'init-builtin-modes)
(require 'init-visual)
(require 'init-functions-and-shortcuts)

;; Stop execution here for terminal (used only for quick editing).
(when (not (display-graphic-p))
  (with-current-buffer " *load*"
    (goto-char (point-max)))
  )

(require 'init-packages-general)
(require 'init-packages-git)
(require 'init-packages-project)
(require 'init-packages-languages)
(require 'init-org)
(require 'init-mode-line)

;;; Load customizations
(setq custom-file (locate-user-emacs-file "customize.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

(server-start)
(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
