;;; init-functions-for-comments --- Functions for comments. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Two comment-aware editing helpers that share a list-item matcher:
;;
;; - `skip-prefixes' (M-M): a smarter `back-to-indentation'.
;; - `fill-comment-list-item': `:around' advice on the fill commands so M-q
;;   inside a comment fills only the current bulleted/numbered item.
;;
;;; Code:

;; Better beginning-of-line function.
;;
;; Originally from https://www.reddit.com/r/emacs/comments/15xeb1s/electric_mm/.

(defconst skip-prefixes-list-item-regexp
  (rx (or "-" "+"
          (seq (+ digit) (or "." ")"))
          (seq (any "a-z" "A-Z") (or "." ")")))
      (+ space)
      (? (seq "[" (or " " "-" "X") "]" (* space)))
      (* (seq "["
              (or (seq (* digit) "/" (* digit))
                  (seq (* digit) "%"))
              "]")
         (* space)))
  "Regexp matching a list-item marker at the start of the text.
Handles bullets (\"-\"/\"+\"), numbered or lettered markers (\"1.\",
\"a)\"), checkboxes, and progress cookies, plus the trailing whitespace.
Shared by `skip-prefixes' and `fill-comment-list-item'.")

(defvar skip-prefixes-alist nil
  "Maps major modes to prefix regexps `skip-prefixes' steps over.
Keyed with `provided-mode-derived-p', so a derived mode inherits its
parent's entry.")

(let* ((c-like '("//+!?" "/?\\*+"))
       (lisp '(";+"))
       (org-header-regexp (rx bol (+ "*") (+ space)
                             ;; I know this is a filthy way of doing the keywords but I just can't
                             ;; be bothered to do it properly.
                             (? (or "TODO"
                                    "NOW"
                                    "DONE"
                                    "DEAD")
                                (+ space))
                             (* (seq (or (seq "["
                                              (or (seq (* digit) "/" (* digit))
                                                  (seq (* digit) "%")
                                                  (seq "#" (or (any "A-Z") (+ digit))))
                                              "]"))
                                     (* space)))))
       (org-list-item-regexp skip-prefixes-list-item-regexp)
       (js-like (list (concat "//+!?[[:space:]]+" org-list-item-regexp)
                      (concat "/?\\*+[[:space:]]+" org-list-item-regexp)
                      (car c-like)
                      (cadr c-like))))
  (setq skip-prefixes-alist
        `(
          (text-mode . (,org-list-item-regexp))
          (org-mode . (,org-header-regexp ,org-list-item-regexp "#" "|"))
          (beancount-mode . (,org-header-regexp ,org-list-item-regexp "#" "|"))
          (lisp-mode . ,lisp)
          (emacs-lisp-mode . ,lisp)
          (c-mode . ,c-like)
          (c++-mode . ,c-like)
          (eshell-mode . ("$+"))
          (rust-mode . ,(cons "//!" c-like))
          (zig-mode . ,c-like)
          (markdown-mode . (,org-list-item-regexp "^#+"))
          (python-mode . ("#+"))
          (red-mode . ("comment"))
          (sh-mode . ("#+"))
          (js-ts-mode . ,js-like)
          (typescript-ts-mode . ,js-like)
          )))

(defun skip-prefixes ()
  "Like `back-to-indentation', but also skips prefixes in comments.
Calls `back-to-indentation', then skips the first matching regexp
associated with the first mode equal to or derived from the current
major mode in `skip-prefixes-alist' (plus any whitespace following
it).  If no regexps match, just skips over `comment-start-skip'."
  (interactive)
  (beginning-of-visual-line)
  (back-to-indentation)
  (let ((eol (save-mark-and-excursion (move-end-of-line 1) (point)))
        (prefixes (cdr (assoc major-mode skip-prefixes-alist #'provided-mode-derived-p))))
    (unless (catch 'loop
              (dolist (prefix prefixes)
                (when (looking-at-p prefix)
                  (search-forward-regexp prefix eol)
                  (search-forward-regexp "[[:space:]]*" eol)
                  (throw 'loop t))))
      ;; Fall back to just skipping the comment delimiter for the mode.
      (when (and comment-start-skip (looking-at-p comment-start-skip))
        (search-forward-regexp comment-start-skip eol)))))

(global-set-key (kbd "M-M") 'skip-prefixes)

;; Fill only the current list item when filling inside a comment.
;;
;; Stock `fill-paragraph' lumps every consecutive comment line into a single
;; paragraph.  `fill-comment-paragraph' does obey `paragraph-start' and
;; `paragraph-separate' within the comment, so it suffices to teach
;; `paragraph-start' about comment-embedded list markers and hand-fill an
;; explicit hanging-indent `fill-prefix'.
;;
;; Language coverage:
;; - most modes route M-q through `fill-paragraph' (directly, via
;;   `fill-paragraph-function', or via `prog-fill-reindent-defun'), so advising
;;   it covers line comments everywhere: `;;', `//', `#', etc.
;; - CC Mode (c/c++/java/...) instead binds M-q straight to `c-fill-paragraph',
;;   so that gets the same advice below.
;; - Block comments whose continuation lines are adorned with `*'
;;   (Javadoc/Doxygen) are handled too: their prefix is the `*' run rather than
;;   a `comment-start-skip' match.
(progn
  (defconst fill-comment-list-item-block-prefix-regexp "[ \t]*\\*+[ \t]*"
    "Regexp for a block-comment continuation prefix: leading whitespace,
a run of `*' adornment (as in Javadoc/Doxygen), and trailing whitespace.
Used when `comment-start-skip' does not match but point is inside a
comment.")

  (defun fill-comment-list-item-marker-info ()
    "For the current line, if it is a comment line return a plist:
:prefix     the comment prefix (indentation and comment starter or `*'),
:content    the buffer position where the comment text begins,
:marker-end the position after a leading list marker, or nil.
Return nil when the current line is not a comment line.  The prefix is
either a `comment-start-skip' match or, on a `*'-adorned block-comment
continuation line, a `fill-comment-list-item-block-prefix-regexp' match.
Point is left unchanged."
    (save-excursion
      (back-to-indentation)
      (let ((content
             (cond
              ((and comment-start-skip (looking-at comment-start-skip))
               (match-end 0))
              ;; Block-comment continuation line (e.g. Javadoc " * ..."):
              ;; the comment starter is on an earlier line, so match the
              ;; `*' adornment instead, but only when really in a comment.
              ((and (nth 4 (syntax-ppss))
                    (looking-at fill-comment-list-item-block-prefix-regexp))
               (match-end 0)))))
        (when content
          (goto-char content)
          (list :prefix (buffer-substring-no-properties
                         (line-beginning-position) content)
                :content content
                :marker-end (and (looking-at skip-prefixes-list-item-regexp)
                                 (match-end 0)))))))

  (defun fill-comment-list-item-prefix ()
    "Return the `fill-prefix' for a comment list item at point, or nil.
When point is inside a bulleted or numbered list item within a comment
-- on the marker line or on any of its continuation lines -- return the
comment prefix followed by spaces the width of the list marker, so that
wrapped lines hang under the item text.  Return nil otherwise.  Walks
upward across continuation lines to find the marker line; a blank comment
line or a non-comment line ends the search."
    (save-excursion
      (beginning-of-line)
      (catch 'done
        (while t
          (let ((info (fill-comment-list-item-marker-info)))
            (cond
             ;; Not a comment line, or a blank comment line: no item here.
             ((or (null info)
                  (save-excursion
                    (goto-char (plist-get info :content))
                    (looking-at-p "[[:space:]]*$")))
              (throw 'done nil))
             ;; Marker line: build the hanging-indent prefix.
             ((plist-get info :marker-end)
              (throw 'done
                     (concat (plist-get info :prefix)
                             (make-string (- (plist-get info :marker-end)
                                             (plist-get info :content))
                                          ?\s))))
             ;; Continuation line: keep looking upward for the marker.
             ((bobp) (throw 'done nil))
             (t (forward-line -1))))))))

  (defun fill-comment-list-item (orig &rest args)
    "Restrict a fill command to the current comment list item.
When point is inside a bulleted or numbered item within a comment, teach
`paragraph-start' about comment-embedded markers so only the current
item is filled, and set a hanging-indent `fill-prefix' so wrapped lines
align under the item text.  Elsewhere, fill normally."
    (let ((prefix (and (not (use-region-p))
                       (fill-comment-list-item-prefix))))
      (if (not prefix)
          (apply orig args)
        (let ((paragraph-start
               (concat paragraph-start "\\|[ \t]*\\(?:"
                       (or comment-start-skip "")
                       "\\|" fill-comment-list-item-block-prefix-regexp
                       "\\)" skip-prefixes-list-item-regexp))
              (fill-prefix prefix))
          (apply orig args)))))
  (advice-add 'fill-paragraph :around #'fill-comment-list-item)
  ;; CC Mode binds M-q directly to `c-fill-paragraph', bypassing the
  ;; advice on `fill-paragraph', so advise it too.
  (with-eval-after-load 'cc-mode
    (advice-add 'c-fill-paragraph :around #'fill-comment-list-item)))

(provide 'init-functions-for-comments)
;;; init-functions-for-comments.el ends here
