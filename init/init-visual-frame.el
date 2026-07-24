;;; init-visual-frame --- Frame Settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;; Frame settings.

;; Set window name/title.
;;
;; `keycoach-indicator-string' is maintained by keycoach itself, and is void
;; until it loads (a void symbol in a mode line construct is simply skipped).
(defvar frame-title-eyebrowse)
(defvar frame-title-separator "  —  ")
;; Maintained event-driven by init-org's org-capture hooks; empty unless a
;; capture buffer is open.  A bare string element (not `:eval') so the title is
;; recomputed only when the string actually changes, matching keycoach.
(defvar frame-title-capture-string "")
(setq frame-title-format
      '("Emacs" frame-title-eyebrowse keycoach-indicator-string
        frame-title-capture-string))
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
