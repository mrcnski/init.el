;;; fill-comment-list-item-tests.el --- Tests for fill-comment-list-item. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/fill-comment-list-item-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)

;; Stub use-package so the init module loads under -Q (no package.el
;; setup in batch).  Loading the file also installs the `:around' advice
;; on `fill-paragraph' that these tests exercise.
(defmacro use-package (&rest _)
  "Stub for batch testing."
  nil)

(load (expand-file-name
       "init/init-functions-for-comments.el"
       (locate-dominating-file
        (or load-file-name default-directory) "init.el"))
      nil t)

;; Loading cc-mode fires the `with-eval-after-load' that advises
;; `c-fill-paragraph', so the CC Mode tests below exercise it.
(require 'cc-mode)

(defun fill-item-test--run (input line &optional mode fill-col fill-fn)
  "Fill the paragraph at LINE of INPUT and return the resulting buffer.
INPUT is inserted into a MODE buffer (default `emacs-lisp-mode') with
`fill-column' FILL-COL (default 40).  Point is placed at the end of the
1-based LINE, then FILL-FN (default `fill-paragraph') runs.  CC Mode
tests pass `c-fill-paragraph', the command its M-q is bound to."
  (with-temp-buffer
    (funcall (or mode #'emacs-lisp-mode))
    (setq fill-column (or fill-col 40))
    (insert input)
    (goto-char (point-min))
    (forward-line (1- line))
    (end-of-line)
    (funcall (or fill-fn #'fill-paragraph))
    (buffer-string)))

;; The current bullet is filled with a hanging indent; later bullets are
;; left untouched instead of being swallowed into one paragraph.
(ert-deftest fill-item-current-bullet-only ()
  (should (equal (fill-item-test--run
                  (concat ";; - alpha beta gamma delta epsilon zeta eta theta\n"
                          ";; - second bullet stays untouched on its long line\n")
                  1)
                 (concat ";; - alpha beta gamma delta epsilon zeta\n"
                         ";;   eta theta\n"
                         ";; - second bullet stays untouched on its long line\n"))))

;; A manually wrapped continuation line collapses back into the item and
;; is refilled under the item text.
(ert-deftest fill-item-collapse-manual-continuation ()
  (should (equal (fill-item-test--run
                  (concat ";; - alpha beta gamma delta\n"
                          ";;   epsilon zeta eta theta iota kappa\n"
                          ";; - next bullet untouched here on its own line yes\n")
                  1)
                 (concat ";; - alpha beta gamma delta epsilon zeta\n"
                         ";;   eta theta iota kappa\n"
                         ";; - next bullet untouched here on its own line yes\n"))))

;; Filling from a continuation line resolves to the enclosing bullet.
(ert-deftest fill-item-point-on-continuation ()
  (should (equal (fill-item-test--run
                  (concat ";; - alpha beta gamma delta epsilon\n"
                          ";;   zeta eta theta iota kappa lambda\n"
                          ";; - next bullet untouched\n")
                  2)
                 (concat ";; - alpha beta gamma delta epsilon zeta\n"
                         ";;   eta theta iota kappa lambda\n"
                         ";; - next bullet untouched\n"))))

;; A plain (non-list) comment paragraph fills normally, with no marker
;; padding on the continuation lines.
(ert-deftest fill-item-prose-unaffected ()
  (should (equal (fill-item-test--run
                  ";; plain prose comment paragraph long enough to wrap across lines here yes indeed\n"
                  1)
                 (concat ";; plain prose comment paragraph long\n"
                         ";; enough to wrap across lines here yes\n"
                         ";; indeed\n"))))

;; Numbered markers count as list items and get a wider hanging indent.
(ert-deftest fill-item-numbered-marker ()
  (should (equal (fill-item-test--run
                  (concat ";; 1. alpha beta gamma delta epsilon zeta eta theta\n"
                          ";; 2. second stays untouched here long line\n")
                  1)
                 (concat ";; 1. alpha beta gamma delta epsilon\n"
                         ";;    zeta eta theta\n"
                         ";; 2. second stays untouched here long line\n"))))

;; Works with indentation and a single-character comment starter.
(ert-deftest fill-item-indented-single-char-comment ()
  (should (equal (fill-item-test--run
                  (concat "    # - alpha beta gamma delta epsilon zeta eta theta iota\n"
                          "    # - second untouched\n")
                  1 #'python-mode)
                 (concat "    # - alpha beta gamma delta epsilon\n"
                         "    #   zeta eta theta iota\n"
                         "    # - second untouched\n"))))

;; A blank comment line ends the item: neither it nor the prose after it
;; is drawn into the fill.
(ert-deftest fill-item-blank-line-boundary ()
  (should (equal (fill-item-test--run
                  (concat ";; - alpha beta gamma delta epsilon zeta eta theta\n"
                          ";;\n"
                          ";; trailing prose stays put on its long single line here\n")
                  1)
                 (concat ";; - alpha beta gamma delta epsilon zeta\n"
                         ";;   eta theta\n"
                         ";;\n"
                         ";; trailing prose stays put on its long single line here\n"))))

;; The list-item matcher is shared with `skip-prefixes'.
(ert-deftest fill-item-shared-regexp-bound ()
  (should (stringp skip-prefixes-list-item-regexp))
  (should (string-match-p (concat "\\`" skip-prefixes-list-item-regexp) "- x"))
  (should (string-match-p (concat "\\`" skip-prefixes-list-item-regexp) "1. x")))

;; CC Mode binds M-q to `c-fill-paragraph', which the advice also wraps.
(ert-deftest fill-item-cc-line-comment ()
  (should (equal (fill-item-test--run
                  (concat "// - alpha beta gamma delta epsilon zeta eta theta\n"
                          "// - second untouched long line here yes\n")
                  1 #'c-mode nil #'c-fill-paragraph)
                 (concat "// - alpha beta gamma delta epsilon zeta\n"
                         "//   eta theta\n"
                         "// - second untouched long line here yes\n"))))

;; Star-adorned block comments (Javadoc/Doxygen) fill per item too.
(ert-deftest fill-item-cc-block-comment ()
  (should (equal (fill-item-test--run
                  (concat "/*\n"
                          " * - alpha beta gamma delta epsilon zeta eta theta\n"
                          " * - second untouched long line\n"
                          " */\n")
                  2 #'c-mode nil #'c-fill-paragraph)
                 (concat "/*\n"
                         " * - alpha beta gamma delta epsilon zeta\n"
                         " *   eta theta\n"
                         " * - second untouched long line\n"
                         " */\n"))))

(ert-deftest fill-item-java-javadoc ()
  (should (equal (fill-item-test--run
                  (concat "/**\n"
                          " * - alpha beta gamma delta epsilon zeta eta theta iota\n"
                          " * - second untouched long line here\n"
                          " */\n")
                  2 #'java-mode nil #'c-fill-paragraph)
                 (concat "/**\n"
                         " * - alpha beta gamma delta epsilon zeta\n"
                         " *   eta theta iota\n"
                         " * - second untouched long line here\n"
                         " */\n"))))

(provide 'fill-comment-list-item-tests)
;;; fill-comment-list-item-tests.el ends here
