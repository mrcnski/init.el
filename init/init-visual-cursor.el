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
(defvar my-cursor-current-color "")
(defvar my-cursor-read-only-color "#db931f") ; orange
(defvar my-cursor-buffer "")
(defvar my-cursor-active-timer nil)
(defvar my-cursor-editing-timer nil)
(defvar my-cursor-editing-delay 5)

(defun my-cursor-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; `set-cursor-color' is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only
             my-cursor-read-only-color
           (if overwrite-mode "red"
             my-cursor-original-color
             ))))
    (unless (and
             (string= color my-cursor-current-color)
             (string= (buffer-name) my-cursor-buffer))
      (set-cursor-color (setq my-cursor-current-color color))
      (setq my-cursor-buffer (buffer-name))
      (when buffer-read-only
        (my-cursor-set-not-editing))
      )))

(defun my-cursor-set-editing ()
  (setq-default cursor-type 'bar)
  (when my-cursor-editing-timer
    (cancel-timer my-cursor-editing-timer))
  (setq my-cursor-editing-timer (run-with-idle-timer my-cursor-editing-delay nil 'my-cursor-set-not-editing))
  )
(defun my-cursor-set-not-editing ()
  (when my-cursor-editing-timer
    (cancel-timer my-cursor-editing-timer))
  (setq-default cursor-type 'box)
  )

(add-hook 'post-command-hook 'my-cursor-according-to-mode)
(add-hook 'post-self-insert-hook 'my-cursor-set-editing)

(provide 'init-visual-cursor)
;;; init-visual-cursor.el ends here
