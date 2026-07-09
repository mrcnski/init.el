;;; init-visual-frame --- Frame Settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;; Frame settings.

;; Set window name/title.
(defvar frame-title-eyebrowse)
(defvar frame-title-keys)
(defvar frame-title-separator "  —  ")
(setq frame-title-format '("Emacs" frame-title-eyebrowse frame-title-keys))
(defun frame-title-update ()
  "Update the frame title."
  (set-frame-parameter nil 'title (format-mode-line frame-title-format)))
(run-with-idle-timer 1 t 'frame-title-update)
;; Keep the frame title correct when resizing the frame.
(add-hook 'window-size-change-functions
          (lambda (_frame)
            (frame-title-update)
            ;; Defer an update to handle the final mouse-release.
            (run-with-timer 0.1 nil #'frame-title-update)))

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Assuming you are using a dark theme.
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; Disable icon for a cleaner title.
(setq ns-use-proxy-icon nil)

;; Set transparency?
(set-frame-parameter (selected-frame) 'alpha '(100))
;; (set-frame-parameter (selected-frame) 'alpha '(98))

;; Allow resizing by pixels.
(setq frame-resize-pixelwise t)

(toggle-frame-maximized) ;; Maximize!

(provide 'init-visual-frame)
;;; init-visual-frame.el ends here
