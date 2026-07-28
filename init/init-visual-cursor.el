;;; init-visual-cursor --- Cursor Settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;; Basic settings.

;; Turn on blinking/flashing cursor? (-1 to disable)
(blink-cursor-mode 1)
(setq
 blink-cursor-delay 0.5
 ;; 0 to blink forever.
 blink-cursor-blinks 10
 )

(when (display-graphic-p)
  (setq-default cursor-type 'box))

;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;;; Some fancy settings.

(defvar my-cursor-original-color
  "yellow"
  ;; (face-background 'cursor)
  )
(defvar my-cursor-current-color ""
  "Cursor color last applied to `my-cursor-current-frame'.")
(defvar my-cursor-read-only-color "#db931f") ; orange
(defvar my-cursor-current-frame nil
  "Frame that `my-cursor-current-color' was last applied to.")
(defvar my-cursor-active-timer nil)
(defvar my-cursor-editing-timer nil)
(defvar my-cursor-editing-delay 5)

(defun my-cursor-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; `set-cursor-color' is somewhat costly, so we only call it when needed:
  ;; when the color actually changes, or when we have moved to a different
  ;; frame, since the cursor color is a per-frame parameter.
  (let ((color (cond (buffer-read-only my-cursor-read-only-color)
                     (overwrite-mode "red")
                     (t my-cursor-original-color))))
    (unless (and (string= color my-cursor-current-color)
                 (eq (selected-frame) my-cursor-current-frame))
      (set-cursor-color color)
      (setq my-cursor-current-color color
            my-cursor-current-frame (selected-frame))
      (when buffer-read-only
        (my-cursor-set-not-editing)))))

(defun my-cursor-set-editing ()
  "Use a bar cursor while typing, reverting to a box after an idle delay."
  (unless (eq (default-value 'cursor-type) 'bar)
    (setq-default cursor-type 'bar)
    (setq my-cursor-editing-timer
          (run-with-idle-timer my-cursor-editing-delay nil
                               #'my-cursor-set-not-editing))))
(defun my-cursor-set-not-editing ()
  (when my-cursor-editing-timer
    (cancel-timer my-cursor-editing-timer))
  (setq-default cursor-type 'box)
  )

(add-hook 'post-command-hook 'my-cursor-according-to-mode)
(add-hook 'post-self-insert-hook 'my-cursor-set-editing)

(provide 'init-visual-cursor)
;;; init-visual-cursor.el ends here
