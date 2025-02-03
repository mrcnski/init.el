;;; init-mode-line --- Set mode-line format.
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

;; Active window detection.
;; From https://emacs.stackexchange.com/a/26345.
(defvar mode-line-selected-window nil)

(defun mode-line-record-selected-window ()
  "Record the current window as selected."
  (setq mode-line-selected-window (selected-window)))
(add-hook 'post-command-hook 'mode-line-record-selected-window)

;; REMOVED: What was this for?
;; (defun mode-line-update-all ()
;;   "Update all mode lines."
;;   (force-mode-line-update t))
;; (add-hook 'buffer-list-update-hook 'mode-line-update-all)

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

;; Set the mode-line.
(setq-default
 mode-line-format
 '((:eval
    (list
     ;; Winum string.
     " ["
     '(:eval (winum-get-number-string))
     "] "
     ;; Modified indicator.
     'mode-line-modified
     " "
     ;; Buffer name.
     '(:eval (propertize "%b"
                         'face '(:weight bold)
                         'help-echo (buffer-file-name)))
     " |"
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
     ;; Limited set of useful minor modes.
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
       (when (and (boundp 'which-func-mode) which-func-mode)
         (let ((f (which-function)))
           (when f
             (concat "[" f "] ")
             ))))

     ;; Recursive editing level.
     "%[%] "

     ;; " "
     ;; '(:eval (propertize (format-time-string "%H:%M")))
     ))))

(provide 'init-mode-line)
;;; init-mode-line.el ends here
