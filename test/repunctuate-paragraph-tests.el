;;; repunctuate-paragraph-tests.el --- Tests for repunctuate-paragraph. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/repunctuate-paragraph-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)

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

(defun repunctuate-test--run (input &optional mode)
  "Run `repunctuate-paragraph' on INPUT with point at start, return result.
Buffer uses MODE (default `text-mode')."
  (with-temp-buffer
    (funcall (or mode #'text-mode))
    (insert input)
    (goto-char (point-min))
    (repunctuate-paragraph)
    (buffer-string)))

(ert-deftest repunctuate-basic-sentences ()
  (should (equal (repunctuate-test--run "One two. Three four! Five? Six.")
                 "One two.  Three four!  Five?  Six.")))

(ert-deftest repunctuate-existing-double-space-unchanged ()
  (should (equal (repunctuate-test--run "One two.  Three four.")
                 "One two.  Three four.")))

(ert-deftest repunctuate-normalizes-extra-spaces ()
  (should (equal (repunctuate-test--run "One two.   Three four.")
                 "One two.  Three four.")))

(ert-deftest repunctuate-closing-quote ()
  (should (equal (repunctuate-test--run "He said \"stop.\" Then he left.")
                 "He said \"stop.\"  Then he left.")))

(ert-deftest repunctuate-abbreviations-kept-single ()
  (should (equal (repunctuate-test--run "Fill tools, e.g. this one, help.")
                 "Fill tools, e.g. this one, help."))
  (should (equal (repunctuate-test--run "Ask Mr. Smith about it.")
                 "Ask Mr. Smith about it."))
  (should (equal (repunctuate-test--run "Spaces, tabs, etc. are whitespace.")
                 "Spaces, tabs, etc. are whitespace.")))

(ert-deftest repunctuate-ellipsis-kept-single ()
  (should (equal (repunctuate-test--run "He waited... and waited. The end.")
                 "He waited... and waited.  The end.")))

(ert-deftest repunctuate-abbreviation-at-sentence-end-limitation ()
  ;; Known limitation: an abbreviation that truly ends a sentence still
  ;; gets a single space.
  (should (equal (repunctuate-test--run "I like fruit, nuts, etc. They like meat.")
                 "I like fruit, nuts, etc. They like meat.")))

(ert-deftest repunctuate-only-current-paragraph ()
  (should (equal (repunctuate-test--run "First para. Changed.\n\nSecond para. Unchanged.")
                 "First para.  Changed.\n\nSecond para. Unchanged.")))

(ert-deftest repunctuate-requires-text-mode ()
  (should (equal (repunctuate-test--run "One two. Three four." #'fundamental-mode)
                 "One two. Three four.")))

(ert-deftest repunctuate-respects-sentence-end-double-space ()
  (let ((sentence-end-double-space nil))
    (should (equal (repunctuate-test--run "One two. Three four.")
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

(provide 'repunctuate-paragraph-tests)
;;; repunctuate-paragraph-tests.el ends here
