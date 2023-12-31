;;; init-functions-and-shortcuts --- My Functions and Shortcuts/Keybindings.
;;
;;; Commentary:
;;
;;; Code:

;; For consistency with every other application.
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Set s-s to save all buffers (default is only current buffer).
(global-set-key (kbd "s-s") 'save-all)

;; Set up keys using super. s-a, s-x, s-c, and s-v correspond to
;; select-all, save, cut, copy, and paste, which I've left for
;; consistency/utility on Macs.
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)

;; Enable OSX full screen shortcut.
(global-set-key (kbd "C-s-f") 'toggle-frame-maximized)

(defun backward-symbol ()
  "The backwards version of `forward-symbol'."
  (interactive)
  (forward-symbol -1)
  )
(defun kill-symbol ()
  "The symbol version of `kill-word'."
  (interactive)
  (save-mark-and-excursion
    (push-mark)
    (activate-mark)
    (forward-symbol 1)
    (call-interactively 'kill-region)
    )
  )
(defun backward-kill-symbol ()
  "The backwards, symbol version of `kill-word'."
  (interactive)
  (save-mark-and-excursion
    (push-mark)
    (activate-mark)
    (forward-symbol -1)
    (call-interactively 'kill-region)
    )
  )
(defun transpose-symbols (arg)
  "The symbol version of `transpose-words'. ARG!"
  (interactive "*p")
  (transpose-subr 'forward-symbol arg)
  )
(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "M-B") 'backward-symbol)
(global-set-key (kbd "M-D") 'kill-symbol)
(global-set-key (kbd "M-S-<backspace>") 'backward-kill-symbol)
(global-set-key (kbd "M-T") 'transpose-symbols)

(defun isearch-backward-symbol-at-point (&optional arg)
  "The backwards version of `isearch-forward-symbol-at-point'. ARG!"
  (interactive "P")
  (isearch-mode nil nil nil nil 'isearch-symbol-regexp)
  (let ((bounds (find-tag-default-bounds))
       (count (and arg (prefix-numeric-value arg))))
   (cond
    (bounds
     (when (< (car bounds) (point))
	    (goto-char (car bounds)))
     (isearch-yank-string
      (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (isearch-repeat-backward)
     (when count
       (isearch-repeat-backward count)))
    (t
     (setq isearch-error "No symbol at point")
     (isearch-push-state)
     (isearch-update)))))
(global-set-key (kbd "M-s ,") 'isearch-backward-symbol-at-point)

;; Enable OSX CMD+backspace.
(defun kill-line-backwards ()
  "Kill the line backwards."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "s-<backspace>") 'kill-line-backwards)

;; It is the opposite of fill-paragraph.
;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
 (global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Disable annoying popup on OSX.
(global-set-key (kbd "s-t") 'make-frame)
;; I don't want to accidentally press this.
(global-set-key (kbd "s-q") nil)

;; Save the buffer and revert it (reload from disk).
(defun save-revert-buffer ()
  "Save the buffer and then revert it."
  (interactive)
  (save-buffer)
  (delete-all-overlays)
  (revert-buffer-quick))
(global-set-key (kbd "s-r") 'save-revert-buffer)

(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  ;; Do not prompt for confirmation.
  (save-some-buffers t)
  )
(global-set-key (kbd "C-x s") 'save-all)

;; Automatically save all file-visiting buffers when Emacs loses focus.
(add-hook 'after-focus-change-hook 'save-all)
;; Run `save-all' when idle for a while.
;; Shouldn't run too quickly as it is a bit distracting.
(run-with-idle-timer 60 t 'save-all)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Code folding.
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (
         :map hs-minor-mode-map
         ("s-[" . hs-hide-level)
         ("s-]" . hs-show-all)
         ("s-\\" . hs-toggle-hiding)
         )
  )

;; Zapping.
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Easier repeat.
;; (global-set-key (kbd "C-z") 'repeat)

(global-set-key (kbd "C-M-,") 'xref-go-back)
(global-set-key (kbd "C-M-.") 'xref-go-forward)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Zoom in/out.
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-=") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

(defun highlight-line ()
  "Toggle highlighting the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (min (point-max) (+ 1 (line-end-position))))
         (overlays (overlays-in beg end)))
    (if (-any? #'(lambda (ov) (equal (overlay-get ov 'face) 'bookmark-face))
               overlays)
        (remove-overlays beg end 'face 'bookmark-face)
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'bookmark-face)))))
(global-set-key (kbd "C-c l") 'highlight-line)

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  )
(global-set-key (kbd "C-c n") 'indent-buffer)

(defun region-history-other (begin end)
  "Display the source controlled history of region from BEGIN to END in \
another window."
  (interactive "r")
  (vc-region-history begin end)
  (other-window 1)
  )
(global-set-key (kbd "C-c h") 'region-history-other)

(defun project-notes ()
  "Open a notes.org file in the root of the project."
  (interactive)
  (find-file (concat (vc-root-dir) "notes.org"))
  )
(global-set-key (kbd "s-o") 'project-notes)

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        )
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-c k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-c r") 'rename-current-buffer-file)

;; Window operations.

(defun other-window-reverse ()
  "Go to other window in reverse."
  (interactive)
  (other-window -1)
  )
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-reverse)

;; Commands to split window and move focus to other window.
(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  (other-window 1))
(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  (other-window 1))
(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows)
  ;; Update any visible org-agenda buffers.
  (when (fboundp 'org-agenda-refresh) (org-agenda-refresh))
  )
(global-set-key (kbd "C-0") 'delete-window-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)

;; Line operations.

;; Select from point onwards instead of the entire line.
;; + Behaves like C-k.
;; + Can choose whether to keep indentation (run either C-a or M-m beforehand).
;; + Being able to select from point onwards comes in handy much of the time.
(defun select-line ()
  "Select the rest of the current line."
  (interactive)
  (push-mark (line-end-position) nil t)
  )

;; Replace default C-l, it's useless.
(global-set-key (kbd "C-l") 'select-line)

;; Select entire line or lines.
;; + Will entirely select any line that's even partially within the region.
;; + Behaves like C-S-k.
(defun select-lines ()
  "Select the entire current line or region.
If called on a region, will entirely select all lines included in
the region."
  (interactive)
  (cond ((region-active-p)
         (select-lines-region (region-beginning) (region-end)))
        (t
         (select-lines-region (point) (point))
         )))
(defun select-lines-region (beg end)
  "Entirely select all lines in the region from BEG to END."
  (goto-char end)
  (end-of-line)
  (push-mark)
  (activate-mark)
  (goto-char beg)
  (beginning-of-line)
  )
(global-set-key (kbd "C-S-l") 'select-lines)

;; Improved kill-whole-line which doesn't change cursor position.
;; Can be called on multiple lines.
;; Will entirely kill any line that's even partially within the region.
(defun annihilate-lines ()
  "Annihilate the current line or region by killing it entirely.
Will delete the resulting empty line and restore cursor position.
If called on a region, will annihilate every line included in the
region."
  (interactive)
  (cond ((region-active-p)
         (annihilate-lines-region (region-beginning) (region-end)))
        (t
         (annihilate-lines-region (point) (point))
         )))
(defun annihilate-lines-region (beg end)
  "Annihilate the region from BEG to END."
  (let ((col (current-column)))
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (kill-region beg end)
    (kill-append "\n" t)
    ;; Are there more lines after this?
    (if (/= (line-end-position) (point-max))
        (delete-char 1))
    ;; Restore column position
    (move-to-column col)
    ))
(global-set-key (kbd "C-S-k") 'annihilate-lines)

(defun kill-ring-save-lines ()
  "Save the current line, or all lines included in the region."
  (interactive)
  (let (
        ;; If last command was a kill, copy-region-as-kill would append.
        (last-command nil)
        )
    (save-mark-and-excursion
      (select-lines)
      (call-interactively 'copy-region-as-kill)
      (kill-append "\n" t)
      )
    ))
(global-set-key (kbd "M-W") 'kill-ring-save-lines)

;; Drag up/down single line or lines in region.
(use-package drag-stuff
  :defer t
  :bind (("C-S-n" . drag-stuff-down)
         ("C-S-p" . drag-stuff-up)))

;; Join the following line onto the current line.
;; Use this to quickly consolidate multiple lines into one.
(defun join-next-line ()
  "Join the next line onto the current line, preserving the cursor position.
This command can be used to rapidly consolidate multiple lines
into one."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))
(global-set-key (kbd "C-j") 'join-next-line)

(defun goto-line-below (arg)
  "Open and goto a new line below while keeping proper indentation. ARG!"
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line arg)
  (indent-according-to-mode)
  )
(defun goto-line-above (arg)
  "Open and goto a new line above while keeping proper indentation. ARG!"
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode)
  )
(global-set-key (kbd "s-C-n") 'goto-line-below)
(global-set-key (kbd "s-C-p") 'goto-line-above)

(defun duplicate-line-below ()
  "Duplicate the current line below while keeping point location."
  (interactive)
  (let ((point-at-beginning (eq (point) (line-beginning-position)))
        ;; If last command was a kill, copy-region-as-kill would append.
        (last-command nil)
        )
    (save-mark-and-excursion
      (beginning-of-line)
      (select-line)
      (call-interactively 'copy-region-as-kill)
      (open-line 1)
      (yank)
      )
    (when point-at-beginning
      (forward-line))
    ))
(defun duplicate-line-above ()
  "Duplicate the current line above while keeping point location."
  (interactive)
  (let (
        ;; If last command was a kill, copy-region-as-kill would append.
        (last-command nil)
        )
    (save-mark-and-excursion
      (beginning-of-line)
      (select-line)
      (call-interactively 'copy-region-as-kill)
      (forward-line 1)
      (open-line 1)
      (yank)
      )
    ))
(global-set-key (kbd "M-N") 'duplicate-line-below)
(global-set-key (kbd "M-P") 'duplicate-line-above)

;; Indentation functions.

(defvar indent-amount 4)
;; Set per-mode overrides.
(add-hook 'text-mode-hook #'(lambda () (setq-local indent-amount 2)))
(add-hook 'yaml-mode-hook #'(lambda () (setq-local indent-amount 2)))

(defun indent-left ()
  "Indent left by the amount used in the mode, or the default amount."
  (interactive)
  (cond ((region-active-p)
         (indent-region-relative (region-beginning) (region-end) (- indent-amount)))
        (t
         (indent-region-relative (point) (point) (- indent-amount))
         ))
  )
(defun indent-right ()
  "Indent right by the amount used in the mode, or the default amount."
  (interactive)
  (cond ((region-active-p)
         (indent-region-relative (region-beginning) (region-end) indent-amount))
        (t
         (indent-region-relative (point) (point) indent-amount)
         ))
  )

(defun indent-region-relative (beg end amount)
  "Indent from BEG to END by the specified AMOUNT."
  (save-mark-and-excursion
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (indent-rigidly beg end amount)
    )
  )

(global-set-key (kbd "C-<") 'indent-left)
(global-set-key (kbd "C->") 'indent-right)

;; Better scrolling functions.

(defun window-fraction-height (fraction)
  "Get specified FRACTION of the height of the current window."
  (max 1 (/ (1- (window-height (selected-window))) fraction)))
(defvar scroll-fraction 4)

(defun scroll-up-fraction ()
  "Scrolls up by a fraction of the current window height."
  (interactive)
  (scroll-up (window-fraction-height scroll-fraction)))
(defun scroll-down-fraction ()
  "Scrolls down by a fraction of the current window height."
  (interactive)
  (scroll-down (window-fraction-height scroll-fraction)))
(defun scroll-other-window-up-fraction ()
  "Scrolls other window up by a fraction of the current window height."
  (interactive)
  (scroll-other-window (window-fraction-height scroll-fraction)))
(defun scroll-other-window-down-fraction ()
  "Scrolls other window down by a fraction of the current window height."
  (interactive)
  (scroll-other-window-down (window-fraction-height scroll-fraction)))

;; Enable these commands in isearch.
(put 'scroll-up-fraction 'isearch-scroll t)
(put 'scroll-down-fraction 'isearch-scroll t)
(put 'scroll-other-window-up-fraction 'isearch-scroll t)
(put 'scroll-other-window-down-fraction 'isearch-scroll t)

(global-set-key (kbd "C-v") 'scroll-up-fraction)
(global-set-key (kbd "M-v") 'scroll-down-fraction)
(global-set-key (kbd "C-S-v") 'scroll-up)
(global-set-key (kbd "M-V") 'scroll-down)
(global-set-key (kbd "C-M-v") 'scroll-other-window-up-fraction)
(global-set-key (kbd "C-M-V") 'scroll-other-window-down-fraction)

;; Better beginning-of-line function.
;; From https://www.reddit.com/r/emacs/comments/15xeb1s/electric_mm/.

(let ((c-like '("//+!?" "/?\\*+"))
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
      (org-list-item-regexp (rx (or "-" "+"
                                    (seq (+ digit) (or "." ")"))
                                    (seq (any "a-z" "A-Z") (or "." ")")))
                                (+ space)
                                (? (seq "[" (or " " "-" "X") "]" (* space)))
                                (* (seq "["
                                        (or (seq (* digit) "/" (* digit))
                                            (seq (* digit) "%"))
                                        "]")
                                   (* space)))))
  (setq skip-prefixes-alist
        `(
          (text-mode . (,org-list-item-regexp))
          (org-mode . (,org-header-regexp ,org-list-item-regexp "#" "|"))
          (lisp-mode . ,lisp)
          (emacs-lisp-mode . ,lisp)
          (c-mode . ,c-like)
          (c++-mode . ,c-like)
          (eshell-mode . ("$+"))
          (rust-mode . ,(cons "//!" c-like))
          (zig-mode . ,c-like)
          (csharp-mode . ,c-like)
          (sh-mode . ("#+"))
          (python-mode . ("#+"))
          (red-mode . ("comment"))
          (markdown-mode . ("^#+" "^-"))
          )))

(defun skip-prefixes ()
  "Calls `back-to-indentation', then skips the first matching
regexp associated with the first mode equal to or derived from
the current major mode in `skip-prefixes-alist' (plus any
whitespace following it). If no regexps match, just skips over
`comment-start-skip'."
  (interactive)
  (beginning-of-visual-line)
  (back-to-indentation)
  (let ((eol (save-mark-and-excursion (move-end-of-line 1) (point))))
    (unless (catch 'loop
              (dolist (prefix (cdr (assoc major-mode skip-prefixes-alist #'provided-mode-derived-p)))
                (when (looking-at-p prefix)
                  (search-forward-regexp prefix eol)
                  (search-forward-regexp "[[:space:]]*" eol)
                  (throw 'loop t))))
      ;; Fall back to just skipping the comment delimiter for the mode.
      (when (and comment-start-skip (looking-at-p comment-start-skip))
        (search-forward-regexp comment-start-skip eol)))))

(global-set-key (kbd "M-M") 'skip-prefixes)

;; Print debugging utility.
;;
;; TODO: separate package?
(defvar iter-print-increment 0)
(defun iter-print ()
  "Insert incrementing print statements."
  (interactive)
  (insert (format "\nprintln!(\"%d\");" iter-print-increment))
  (setq iter-print-increment (1+ iter-print-increment))
  )

;; Other

;; Align region by string.
;; TODO: Enable history in read-string to allow for default values
;;       (i.e. last input).
(defun align-to-string (beg end)
  "Align region from BEG to END along input string."
  (interactive "r")
  (let ((char (read-string "string: ")))
    (align-regexp beg end (concat "\\(\\s-*\\)" char))))

;; Show ASCII table.
;; Obtained from http://www.chrislott.org/geek/emacs/dotemacs.html.
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (goto-char (point-min)))

;; Helps with "too many files" error.
;; From https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html.
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(provide 'init-functions-and-shortcuts)
;;; init-functions-and-shortcuts.el ends here
