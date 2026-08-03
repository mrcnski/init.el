;;; init-test-helper.el --- Shared setup for the tests in this directory. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file contains helpers and is not a test file itself.  Loading an
;; `init/*.el' module under `emacs -Q' takes a little setup -- there is no
;; package.el, so `use-package' is unbound, and neither `init/' nor elpa is on
;; the `load-path'.  Requiring this file puts all three in place.
;;
;; A test file starts with:
;;
;;   (require 'ert)
;;   (add-to-list 'load-path
;;                (file-name-directory (or load-file-name default-directory)))
;;   (require 'init-test-helper)
;;
;; and then loads what it needs with `init-test-load-module' or, for a
;; module reached through `require', straight off the `load-path' this
;; file extends.
;;
;;; Code:

(defvar init-test-root
  (locate-dominating-file (or load-file-name default-directory) "init.el")
  "Repository root, used to locate the init files under test.")

;; Stub `use-package' so an init module loads under -Q, where package.el
;; was never set up.  Note that this discards the body of every
;; `use-package' form, so a test for code living inside one needs its own
;; approach rather than this file.
(defmacro use-package (&rest _)
  "Stub for batch testing."
  nil)

(add-to-list 'load-path (expand-file-name "init" init-test-root))

(defun init-test-load-module (name)
  "Load the init module NAME, e.g. \"init-functions-for-comments\".
Loads the file by path rather than by feature, so a module gets read
again on a second call and its load-time side effects -- advice,
hooks -- are re-applied."
  (load (expand-file-name (concat "init/" name ".el") init-test-root)
        nil t))

(defun init-test-add-elpa-package (name)
  "Put the newest installed elpa package matching NAME on the `load-path'.
NAME is the package name without a version, e.g. \"markdown-mode\".
Signals an error when nothing matches: a test that needs a package is
better off failing loudly than skipping in silence.  Returns the
directory added."
  ;; init-basics is what defines where elpa lives.
  (require 'init-basics)
  (let ((dir (car (last (file-expand-wildcards
                         (expand-file-name (concat name "-*")
                                           user-emacs-elpa-directory))))))
    (unless dir
      (error "No %s installed under %s" name user-emacs-elpa-directory))
    (add-to-list 'load-path dir)
    dir))

(provide 'init-test-helper)
;;; init-test-helper.el ends here
