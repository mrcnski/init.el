;;; init-mode-line --- Set mode-line format. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; Count total lines in buffer.
;; From https://stackoverflow.com/a/8191130.
(defvar mode-line-buffer-line-count nil)
(make-variable-buffer-local 'mode-line-buffer-line-count)

(defun mode-line-count-lines ()
  "Count the total number of lines in the current buffer."
  (setq mode-line-buffer-line-count
        (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'mode-line-count-lines)
(add-hook 'after-save-hook 'mode-line-count-lines)
(add-hook 'after-revert-hook 'mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'mode-line-count-lines)
(add-hook 'after-change-major-mode-hook 'mode-line-count-lines)

;; For right-aligning.
;; From https://stackoverflow.com/a/22971471.
(defun mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize
   " "
   'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
   ))

(defun mode-line-render-truncated (format)
  "Render mode-line construct FORMAT, truncated to the window width.
On overflow show a truncation indicator.  Hovering the indicator shows
the full text in a tooltip."
  (let* ((full (format-mode-line format))
         (width (window-total-width))
         (indicator " ..."))
    ;; The returned string is reinterpreted as a mode-line construct, so
    ;; literal % has to be escaped.
    (if (<= (string-width full) width)
        (replace-regexp-in-string "%" "%%" full t t)
      (concat
       (replace-regexp-in-string
        "%" "%%"
        (truncate-string-to-width
         full (max 0 (- width (string-width indicator))))
        t t)
       (propertize indicator 'help-echo (substring-no-properties full))))))

;; Gray out the tab-line in non-selected windows, like the mode-line.
(defface tab-line-inactive
  '((t (:inherit mode-line-inactive)))
  "Face for the tab line of non-selected windows.")

;; Show the buffer name in the tab-line instead of the mode-line.  The
;; tab-line rather than the header-line, because various modes take over
;; the header-line (tabulated-list, Info, ...), hiding the buffer name.
(setq-default
 tab-line-format
 '((:eval
    (let ((line
           (mode-line-render-truncated
            (list
             ;; Winum string.
             " ["
             '(:eval (winum-get-number-string))
             "] "
             ;; Buffer name.
             '(:eval (propertize "%b"
                                 ;; 'face '(:weight bold)
                                 'help-echo (buffer-name)))
             ;; Ghostel: the terminal title (running command or Claude Code's
             ;; session summary).
             '(:eval (when (and (boundp 'ghostel-title)
                                (derived-mode-p 'ghostel-mode)
                                ghostel-title
                                (not (string= ghostel-title "")))
                       (propertize
                        (format " (%s)" (truncate-string-to-width
                                         ghostel-title 60 nil nil t))
                        'face 'font-lock-keyword-face
                        'help-echo ghostel-title)))
             ))))
      (if (mode-line-window-selected-p)
          line
        ;; Pad to the window edge so the inactive background covers the
        ;; whole line, then paint over any segment faces.  Plain `right'
        ;; stops at the text area, leaving fringe-width artifacts.
        (let ((line (concat line
                            (propertize
                             " " 'display
                             '(space :align-to
                                     (+ right right-fringe right-margin))))))
          (add-face-text-property 0 (length line) 'tab-line-inactive
                                  nil line)
          line))))))

;; The default tab-line would otherwise show up in the minibuffer too.
(add-hook 'minibuffer-setup-hook
          (lambda () (setq tab-line-format nil)))

;; Set the mode-line.
(setq-default
 mode-line-format
 '((:eval
    (mode-line-render-truncated
     (list
      ;; Winum string.
      " ["
      '(:eval (winum-get-number-string))
      "] "
      ;; Modified indicator.
      'mode-line-modified
      " "
      ;; File name.
      '(:eval (when-let* ((file-name (buffer-file-name))
                          (base-file-name (file-name-nondirectory file-name)))
                (concat (propertize base-file-name
                            'face '(:weight bold)
                            'help-echo base-file-name)
                        " ")))
      "|"
      ;; The current line/column.
      '(:eval (when line-number-mode " %l"))
      '(:eval (when column-number-mode ":%C"))
      " "
      ;; The total number of lines. Only recount after certain events, like
      ;; saving.
      '(:eval
        (when (and line-number-mode
                   mode-line-buffer-line-count
                   buffer-file-name)
          (let ((modified (if (buffer-modified-p) "*" "")))
            (format "[%s%s]" mode-line-buffer-line-count modified)
            )))
      ;; The buffer/filesize.
      '(:eval "[%I] ")
      ;; Major mode.
      '(:eval (propertize (format-mode-line mode-name)
                          'face '(:weight bold)
                          'help-echo (format "%s" major-mode)
                          ))
      " "
      ;; Limited set of useful minor mode indicators.
      `(:eval (when (and (boundp 'iedit-mode) iedit-mode) "=iedit= "))
      `(:eval (when (and (boundp 'olivetti-mode) olivetti-mode) "=olivetti= "))

      ;; Read-only.
      '(:eval (when buffer-read-only
                (propertize "RO "
                            'face 'font-lock-preprocessor-face
                            'help-echo "Buffer is read-only")))
      ;; Number of characters in the region.
      '(:eval
        (when mark-active
          (let ((region-count (abs (- (point) (mark)))))
            (when (> region-count 0)
              (format "{%s} " (number-to-string region-count)))
            )))
      ;; Latest eshell command status.
      '(:eval
        (when (string-equal major-mode 'eshell-mode)
          (let ((status
                 (if eshell-current-command
                     "..."
                   eshell-last-command-status
                   )))
            (format "[status: %s] " status)
            )))
      ;; which-function-mode
      '(:eval
        ;; Hide the segment when point is not in a defun, where stock alone
        ;; would show `which-func-unknown' ("n/a").
        (when (and (bound-and-true-p which-function-mode)
                   which-func-mode
                   (gethash (selected-window) which-func-table))
          (list "" which-func-format " ")))

      ;; Recursive editing level.
      "%[%] "

      ;; " "
      ;; '(:eval (propertize (format-time-string "%H:%M")))
      )))))

(provide 'init-mode-line)
;;; init-mode-line.el ends here
