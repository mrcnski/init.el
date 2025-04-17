;;; init.el --- Emacs configuration file. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2024 Marcin Swieczkowski
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
;; - For traces:
;;     M-: (trace-function #'some-function nil (lambda () (backtrace-to-string (backtrace-get-frames 'backtrace))))
;;
;; TODO:
;;
;; - [ ] With Emacs 29+:
;;   - [ ] switch to built-in tree-sitter (once it's easier to setup...)
;;   - [ ] (requires tree-sitter) use new indent package (once stipples are available)
;;
;;; Code:

;; Show more error info?
(setq debug-on-error nil)

(add-to-list 'load-path "~/.emacs.d/init")

(require 'init-basics)
(require 'init-package) ;; Can use Melpa packages after this.
(require 'init-early-utils) ;; Things important to have early.
(require 'init-functions-and-shortcuts)
(require 'init-vertico-et-al) ;; Also good to have early.
(require 'init-builtin-settings)
(require 'init-builtin-modes)
(require 'init-visual)

;; Stop execution here for terminal (used only for quick editing).
(when (not (display-graphic-p))
  (with-current-buffer " *load*"
    (goto-char (point-max))))

(require 'init-packages-general)
;(require 'init-packages-ai)
(require 'init-packages-git)
(require 'init-packages-project)
(require 'init-packages-languages)
(require 'init-org)
;; Put it last to override any changes from packages.
(require 'init-mode-line)

;;; Load customizations
(setq custom-file (locate-user-emacs-file "customize.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

;;; Finish up
(server-start)
(message "init.el finished loading successfully!")

(provide 'init)
;;; init.el ends here
