;;; init-visual-frame --- Frame Settings
;;
;;; Commentary:
;;
;;; Code:

;;; Frame settings.

;; Set window name/title.
(defvar frame-title-eyebrowse)
(setq frame-title-format '("Emacs" frame-title-eyebrowse))

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
