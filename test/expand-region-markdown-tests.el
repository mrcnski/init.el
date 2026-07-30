;;; expand-region-markdown-tests.el --- Tests for the Markdown expand-region expansions. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/expand-region-markdown-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Covers `er/mark-markdown-inline-code' and
;; `er/mark-markdown-outside-inline-code'.  The `er/try-expand-list' wiring in
;; the same module isn't exercised: these are plain marking commands.
;;
;;; Code:

(require 'ert)

;; Stub use-package so init-packages-general.el loads under -Q (no package.el
;; setup in batch).
(defmacro use-package (&rest _)
  "Stub for batch testing."
  nil)

(defvar er-md-tests--root
  (locate-dominating-file (or load-file-name default-directory) "init.el")
  "Repository root, used to locate the init files under test.")

(add-to-list 'load-path (expand-file-name "init" er-md-tests--root))

;; The functions guard on `markdown-inline-code-at-point-p', and markdown-mode
;; isn't on the load-path under -Q.  init-basics defines where elpa lives.
(require 'init-basics)

(let ((dir (car (last (file-expand-wildcards
                       (expand-file-name "markdown-mode-*"
                                         user-emacs-elpa-directory))))))
  (unless dir
    (error "No markdown-mode installed under %s" user-emacs-elpa-directory))
  (add-to-list 'load-path dir))
(require 'markdown-mode)

(require 'init-packages-general)

(defun er-md-tests--mark (fn text)
  "Call FN in a `markdown-mode' buffer holding TEXT; return the marked string.
Point starts where the first `|' in TEXT is, and that `|' is removed before
FN runs.  Return nil when FN marked nothing."
  (with-temp-buffer
    (markdown-mode)
    (insert text)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    ;; markdown-mode's code-block detection reads syntax properties, which are
    ;; applied lazily -- there's no redisplay in batch to trigger them.
    (syntax-propertize (point-max))
    (funcall fn)
    (when (and (mark t) (/= (mark t) (point)))
      (buffer-substring-no-properties (min (point) (mark t))
                                      (max (point) (mark t))))))

(ert-deftest er-md-marks-span-contents ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-inline-code
                                    "Use the `fo|o` helper.")
                 "foo")))

(ert-deftest er-md-marks-span-with-backticks ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "`fo|o` starts the line.")
                 "`foo`")))

(ert-deftest er-md-prose-between-spans-marks-nothing ()
  ;; The guard's whole purpose.  Without it the nearest backtick in each
  ;; direction would be a *neighbouring* span's delimiter, and this would
  ;; wrongly mark "  in between  ".
  (should-not (er-md-tests--mark #'er/mark-markdown-inline-code
                                 "`a` in be|tween `b`.")))

(ert-deftest er-md-handles-doubled-delimiters ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-inline-code
                                    "Use ``fo|o`` here.")
                 "foo"))
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "Use ``fo|o`` here.")
                 "``foo``")))

(ert-deftest er-md-span-at-buffer-start ()
  ;; Exercises skip-chars-backward running into point-min.
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "``fo|o``")
                 "``foo``")))

(ert-deftest er-md-inside-fenced-block-marks-nothing ()
  ;; ASSUMPTION: markdown-mode reports no inline code inside a fenced block, so
  ;; `C-;' there falls through to the generic expansions.  If this test fails,
  ;; suspect the assumption rather than the marking functions.
  (should-not (er-md-tests--mark #'er/mark-markdown-inline-code
                                 "```\nsome `co|de` line\n```\n")))

(ert-deftest er-md-backtick-inside-contents ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-inline-code
                                    "``a| ` b``")
                 "a ` b"))
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "``a| ` b``")
                 "``a ` b``")))

(ert-deftest er-md-point-on-opening-delimiter ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "Match |`*.test.react.*` here.")
                 "`*.test.react.*`"))
  (should (equal (er-md-tests--mark #'er/mark-markdown-inline-code
                                    "Match |`*.test.react.*` here.")
                 "*.test.react.*")))

(ert-deftest er-md-point-on-closing-delimiter ()
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "Match `*.test.react.*|` here.")
                 "`*.test.react.*`"))
  (should (equal (er-md-tests--mark #'er/mark-markdown-inline-code
                                    "Match `*.test.react.*|` here.")
                 "*.test.react.*")))

(ert-deftest er-md-point-on-delimiter-at-buffer-start ()
  ;; The regex has a leading group for the char before the span, which isn't
  ;; there at `point-min'.
  (should (equal (er-md-tests--mark #'er/mark-markdown-outside-inline-code
                                    "|`*.test.react.*`")
                 "`*.test.react.*`")))

(provide 'expand-region-markdown-tests)
;;; expand-region-markdown-tests.el ends here
