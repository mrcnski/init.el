;;; init-visual-cursor --- Cursor Settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Cursor appearance:
;; - The cursor color encodes buffer state:
;;   - yellow for anormal editable buffer
;;   - orange for a read-only one
;;   - red for `overwrite-mode'
;; - The cursor turns into a bar while actively typing, reverting to a box after
;;   `my-cursor-editing-delay' seconds of idle time.
;;
;; Both of these can be turned off independently.
;;
;; Both hooks here run after every command or every self-insert, so they are
;; written to bail out as early as they can.  In particular `set-cursor-color'
;; is costly and sets a *frame* parameter, so its result is cached per frame.
;;
;;; Code:

;;; Basic settings.

(defvar my-cursor-blink nil
  "Whether the cursor blinks.
Read at load time only; to toggle in a running session use
\\[blink-cursor-mode].")

(blink-cursor-mode (if my-cursor-blink 1 -1))
(setq
 blink-cursor-delay 0.5
 ;; 0 to blink forever.
 blink-cursor-blinks 10
 )

(setq-default cursor-type 'box)

;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor t)

;;; Some fancy settings.

(defvar my-cursor-original-color "yellow"
  "Cursor color for normal, editable buffers.
Deliberately a fixed color rather than the `cursor' face: the point is
to stand out from the theme, not to match it.")
(defvar my-cursor-read-only-color "#db931f" ; orange
  "Cursor color for read-only buffers.")
(defvar my-cursor-overwrite-color "red"
  "Cursor color when `overwrite-mode' is enabled.")

(defvar my-cursor-current-color ""
  "Cursor color last applied to `my-cursor-current-frame'.")
(defvar my-cursor-current-frame nil
  "Frame that `my-cursor-current-color' was last applied to.")
(defvar my-cursor-last-buffer nil
  "Buffer that the cursor shape was last updated for.")

(defvar my-cursor-editing-timer nil
  "Idle timer that reverts the cursor shape once typing stops.")
(defvar my-cursor-editing-delay 5
  "Seconds of idle time before the typing cursor reverts to a box.")
(defvar my-cursor-bar-while-typing t
  "Whether the cursor turns into a bar while typing.
Set to nil to keep the box cursor at all times.  Checked on every
self-insert, so it takes effect as soon as it is set.")

(defun my-cursor-set-not-editing ()
  "Revert to a box cursor, cancelling any pending idle timer."
  (when my-cursor-editing-timer
    (cancel-timer my-cursor-editing-timer)
    (setq my-cursor-editing-timer nil))
  (setq-default cursor-type 'box))

(defun my-cursor-set-editing ()
  "Use a bar cursor while typing, reverting to a box after an idle delay."
  (when (and my-cursor-bar-while-typing (display-graphic-p))
    ;; Claim the buffer so `my-cursor-according-to-mode', which runs right
    ;; after us, doesn't immediately undo this on the first character typed.
    (setq my-cursor-last-buffer (current-buffer))
    ;; The idle timer already fires `my-cursor-editing-delay' seconds after
    ;; the last input event, so it only needs arming on the box-to-bar edge.
    (unless (eq (default-value 'cursor-type) 'bar)
      (setq-default cursor-type 'bar)
      (setq my-cursor-editing-timer
            (run-with-idle-timer my-cursor-editing-delay nil
                                 #'my-cursor-set-not-editing)))))

(defun my-cursor-according-to-mode ()
  "Change cursor color according to some minor modes."
  (when (display-graphic-p)
    ;; `set-cursor-color' is somewhat costly, so we only call it when needed:
    ;; when the color actually changes, or when we have moved to a different
    ;; frame, since the cursor color is a per-frame parameter.
    (let ((color (cond (buffer-read-only my-cursor-read-only-color)
                       (overwrite-mode my-cursor-overwrite-color)
                       (t my-cursor-original-color))))
      (unless (and (string= color my-cursor-current-color)
                   (eq (selected-frame) my-cursor-current-frame))
        (set-cursor-color color)
        (setq my-cursor-current-color color
              my-cursor-current-frame (selected-frame))))
    ;; Don't carry a mid-typing bar cursor into a buffer we aren't typing in.
    (unless (eq (current-buffer) my-cursor-last-buffer)
      (setq my-cursor-last-buffer (current-buffer))
      (my-cursor-set-not-editing))))

(add-hook 'post-command-hook #'my-cursor-according-to-mode)
(add-hook 'post-self-insert-hook #'my-cursor-set-editing)

(provide 'init-visual-cursor)
;;; init-visual-cursor.el ends here
