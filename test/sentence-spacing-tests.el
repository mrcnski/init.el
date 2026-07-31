;;; sentence-spacing-tests.el --- Tests for sentence spacing on fill. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Covers the two halves of how init-functions-and-shortcuts.el treats
;; sentence spacing when filling: `repunctuate-paragraph', which widens
;; single spaces to double in prose, and
;; `fill-ignore-single-space-nobreak', which stops a single-spaced
;; sentence end from blocking a line break.
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/sentence-spacing-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The typescript-ts-mode test is skipped when the tree-sitter grammar is
;; not installed; the CC Mode tests cover the same code path without one.
;;
;;; Code:

(require 'ert)
(require 'cc-mode)

;; Stub use-package so init-functions-and-shortcuts.el loads under -Q
;; (no package.el setup in batch).
(defmacro use-package (&rest _)
  "Stub for batch testing."
  nil)

(load (expand-file-name
       "init/init-functions-and-shortcuts.el"
       (locate-dominating-file
        (or load-file-name default-directory) "init.el"))
      nil t)

(defun sentence-spacing-test--run (input &optional mode)
  "Run `repunctuate-paragraph' on INPUT with point at start, return result.
Buffer uses MODE (default `text-mode')."
  (with-temp-buffer
    (funcall (or mode #'text-mode))
    (insert input)
    (goto-char (point-min))
    (repunctuate-paragraph)
    (buffer-string)))

(ert-deftest repunctuate-basic-sentences ()
  (should (equal (sentence-spacing-test--run "One two. Three four! Five? Six.")
                 "One two.  Three four!  Five?  Six.")))

(ert-deftest repunctuate-existing-double-space-unchanged ()
  (should (equal (sentence-spacing-test--run "One two.  Three four.")
                 "One two.  Three four.")))

(ert-deftest repunctuate-normalizes-extra-spaces ()
  (should (equal (sentence-spacing-test--run "One two.   Three four.")
                 "One two.  Three four.")))

(ert-deftest repunctuate-closing-quote ()
  (should (equal (sentence-spacing-test--run "He said \"stop.\" Then he left.")
                 "He said \"stop.\"  Then he left.")))

(ert-deftest repunctuate-abbreviations-kept-single ()
  (should (equal (sentence-spacing-test--run "Fill tools, e.g. this one, help.")
                 "Fill tools, e.g. this one, help."))
  (should (equal (sentence-spacing-test--run "Ask Mr. Smith about it.")
                 "Ask Mr. Smith about it."))
  (should (equal (sentence-spacing-test--run "Compare A vs. B before deciding.")
                 "Compare A vs. B before deciding.")))

(ert-deftest repunctuate-ellipsis-kept-single ()
  (should (equal (sentence-spacing-test--run "He waited... and waited. The end.")
                 "He waited... and waited.  The end.")))

(ert-deftest repunctuate-numbered-list-marker-kept-single ()
  (should (equal (sentence-spacing-test--run "1. Point one. More words.")
                 "1. Point one.  More words."))
  ;; Multiple items in one paragraph, including indented ones.
  (should (equal (sentence-spacing-test--run "1. One two. Three.\n  10. Four five. Six.")
                 "1. One two.  Three.\n  10. Four five.  Six.")))

(ert-deftest repunctuate-number-at-sentence-end-still-doubled ()
  ;; Only line-start list markers are exempt; a number that ends a
  ;; sentence mid-line still gets a double space.
  (should (equal (sentence-spacing-test--run "Born in 1990. Moved away later.")
                 "Born in 1990.  Moved away later.")))

(ert-deftest repunctuate-abbreviation-at-sentence-end-limitation ()
  ;; Known limitation: an abbreviation that truly ends a sentence still
  ;; gets a single space.
  (should (equal (sentence-spacing-test--run "She is a Dr. They trust her.")
                 "She is a Dr. They trust her.")))

(ert-deftest repunctuate-only-current-paragraph ()
  (should (equal (sentence-spacing-test--run "First para. Changed.\n\nSecond para. Unchanged.")
                 "First para.  Changed.\n\nSecond para. Unchanged.")))

(ert-deftest repunctuate-requires-text-mode ()
  (should (equal (sentence-spacing-test--run "One two. Three four." #'fundamental-mode)
                 "One two. Three four.")))

(ert-deftest repunctuate-respects-sentence-end-double-space ()
  (let ((sentence-end-double-space nil))
    (should (equal (sentence-spacing-test--run "One two. Three four.")
                   "One two. Three four."))))

(ert-deftest repunctuate-triggered-by-fill-paragraph ()
  ;; The advice should repunctuate when filling.
  (with-temp-buffer
    (text-mode)
    (setq fill-column 70)
    (insert "One two. Three four.")
    (goto-char (point-min))
    (fill-paragraph)
    (should (equal (buffer-string) "One two.  Three four."))))

(ert-deftest repunctuate-org-fill-paragraph ()
  ;; org remaps M-q to org-fill-paragraph; the extra advice should fire.
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "One two. Three four.")
    (goto-char (point-min))
    (org-fill-paragraph)
    (should (equal (buffer-string) "One two.  Three four."))))

(ert-deftest repunctuate-org-list-item ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "- One two. Three four.")
    (goto-char (point-min))
    (org-fill-paragraph)
    (should (equal (buffer-string) "- One two.  Three four."))))

(ert-deftest repunctuate-org-numbered-list-item ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "1. One two. Three four.")
    (goto-char (point-min))
    (org-fill-paragraph)
    (should (equal (buffer-string) "1. One two.  Three four."))))

(ert-deftest repunctuate-org-skips-src-block ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src elisp\n;; A comment. Not prose.\n#+end_src\n")
    (goto-char (point-min))
    (forward-line 1)
    (org-fill-paragraph)
    (should (string-search ". Not prose." (buffer-string)))))

(ert-deftest repunctuate-org-skips-headline ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "* A headline. With punctuation.")
    (goto-char (point-min))
    (org-fill-paragraph)
    (should (equal (buffer-string) "* A headline. With punctuation."))))

(ert-deftest repunctuate-text-mode-region-fill ()
  ;; With an active region, fill-paragraph fills the whole region; all
  ;; paragraphs in it should get repunctuated.
  (with-temp-buffer
    (text-mode)
    (transient-mark-mode 1)
    (insert "One two. Three.\n\nFour five. Six seven.")
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (fill-paragraph nil t)
    (should (equal (buffer-string)
                   "One two.  Three.\n\nFour five.  Six seven."))))

(ert-deftest repunctuate-org-region-fill ()
  ;; Region spanning prose and a src block: prose repunctuated, code not.
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (transient-mark-mode 1)
    (insert "Para one. More text.\n\n"
            "#+begin_src elisp\n;; Code. Not prose.\n#+end_src\n\n"
            "Para two. End text.\n")
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (org-fill-paragraph nil t)
    (should (string-search "Para one.  More text." (buffer-string)))
    (should (string-search "Para two.  End text." (buffer-string)))
    (should (string-search ";; Code. Not prose." (buffer-string)))))


;;; `fill-ignore-single-space-nobreak'
;;
;; Prose that is not repunctuated -- code comments, which the advice above
;; deliberately leaves alone -- keeps its single-spaced sentence ends.  Those
;; used to block `fill-nobreak-p' from breaking the line there.

;; Grammars live outside the repo, in no-littering's var directory (see
;; init-packages-languages.el), so -Q does not know where to look for them.
(when (require 'treesit nil t)
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "~/.local/emacs/var/treesit/")))

(defun sentence-spacing-test--fill (input line &optional mode fill-col fill-fn)
  "Fill the paragraph at LINE of INPUT and return the resulting buffer.
INPUT is inserted into a MODE buffer (default `emacs-lisp-mode') with
`fill-column' FILL-COL (default 90).  Point is placed at the end of the
1-based LINE, then FILL-FN (default `fill-paragraph') runs.  CC Mode
tests pass `c-fill-paragraph', the command its M-q is bound to."
  (with-temp-buffer
    (funcall (or mode #'emacs-lisp-mode))
    ;; Mirror the settings from init-builtin-settings.el that decide how
    ;; filling treats sentence ends.
    (setq-local sentence-end-double-space t
                sentence-end (let ((sentence-end-double-space nil))
                               (sentence-end))
                fill-column (or fill-col 90))
    (insert input)
    (goto-char (point-min))
    (forward-line (1- line))
    (end-of-line)
    (funcall (or fill-fn #'fill-paragraph))
    (buffer-string)))

;; The reported case: a JSDoc comment where the only word that can move up
;; ("below.") sits right after a single-spaced sentence end.  Without the
;; advice `fill-nobreak-p' vetoes that break and M-q does nothing at all.
(defconst sentence-spacing-test--jsdoc
  (concat "class Foo {\n"
          "  bar() {\n"
          "    /**\n"
          "     * The known skips already return a clean reason and don't reach the catch\n"
          "     * below. Anything that does is a genuine failure, rethrown as a HandledError tagged\n"
          "     */\n"
          "  }\n"
          "}\n"))

(defconst sentence-spacing-test--jsdoc-filled
  (concat "class Foo {\n"
          "  bar() {\n"
          "    /**\n"
          "     * The known skips already return a clean reason and don't reach the catch below.\n"
          "     * Anything that does is a genuine failure, rethrown as a HandledError tagged\n"
          "     */\n"
          "  }\n"
          "}\n"))

(ert-deftest fill-nobreak-typescript-jsdoc ()
  (skip-unless (and (treesit-available-p)
                    (treesit-language-available-p 'typescript)))
  (should (equal (sentence-spacing-test--fill sentence-spacing-test--jsdoc 4
                                         #'typescript-ts-mode)
                 sentence-spacing-test--jsdoc-filled)))

;; Same paragraph through CC Mode's own fill command, which reaches
;; `fill-nobreak-p' by a different route.
(ert-deftest fill-nobreak-java-javadoc ()
  (should (equal (sentence-spacing-test--fill sentence-spacing-test--jsdoc 4
                                         #'java-mode nil #'c-fill-paragraph)
                 sentence-spacing-test--jsdoc-filled)))

(ert-deftest fill-nobreak-c-line-comment ()
  (should (equal (sentence-spacing-test--fill
                  (concat "// alpha beta gamma delta epsilon zeta. Eta theta iota\n"
                          "// kappa lambda mu nu.\n")
                  1 #'c-mode 40 #'c-fill-paragraph)
                 (concat "// alpha beta gamma delta epsilon zeta.\n"
                         "// Eta theta iota kappa lambda mu nu.\n"))))

;; Elisp gets the same treatment -- the advice is not mode-specific.
(ert-deftest fill-nobreak-elisp-line-comment ()
  (should (equal (sentence-spacing-test--fill
                  (concat ";; alpha beta gamma delta epsilon zeta. Eta theta iota\n"
                          ";; kappa lambda mu nu.\n")
                  1 nil 40)
                 (concat ";; alpha beta gamma delta epsilon zeta.\n"
                         ";; Eta theta iota kappa lambda mu nu.\n"))))

;; The advice is scoped to `fill-nobreak-p', so `sentence-end-double-space'
;; still reaches `canonically-space-region': existing double spaces are
;; capped at two rather than squeezed to one by a refill.
(ert-deftest fill-nobreak-keeps-double-spaces-elisp ()
  (should (equal (sentence-spacing-test--fill
                  ";; alpha beta gamma delta epsilon zeta.  Second sentence here with more words.  Third one too.\n"
                  1 nil 60)
                 (concat ";; alpha beta gamma delta epsilon zeta.  Second sentence\n"
                         ";; here with more words.  Third one too.\n"))))

(ert-deftest fill-nobreak-keeps-double-spaces-text-mode ()
  (should (equal (sentence-spacing-test--fill
                  "alpha beta gamma delta epsilon.  Second sentence here with more words.  Third one too.\n"
                  1 #'text-mode 60)
                 (concat "alpha beta gamma delta epsilon.  Second sentence here with\n"
                         "more words.  Third one too.\n"))))

;; Only the one clause is neutralized: `fill-nobreak-p' still refuses to
;; break where the rest of the line would read as a new paragraph.
(ert-deftest fill-nobreak-other-checks-still-apply ()
  (should (equal (sentence-spacing-test--fill
                  ";; alpha beta gamma delta epsilon zeta eta\n"
                  1 nil 20)
                 (concat ";; alpha beta gamma\n"
                         ";; delta epsilon\n"
                         ";; zeta eta\n")))
  (with-temp-buffer
    (text-mode)
    (insert "alpha beta gamma\n")
    (goto-char (point-min))
    (search-forward "beta ")
    ;; Point sits before "gamma"; breaking here would leave "gamma" at
    ;; the start of what `paragraph-start' calls a new paragraph.
    (let ((paragraph-start "gamma\\|[ \t]*$"))
      (should (fill-nobreak-p)))
    (let ((paragraph-start "[ \t]*$"))
      (should-not (fill-nobreak-p)))))

(provide 'sentence-spacing-tests)
;;; sentence-spacing-tests.el ends here
