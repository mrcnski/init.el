;;; init-visual --- Visual Settings
;;
;;; Commentary:
;;
;;; Code:

;; Set transparency.
(set-frame-parameter (selected-frame) 'alpha '(100))
;; (set-frame-parameter (selected-frame) 'alpha '(98))

;; Turn on blinking/flashing cursor? (-1 to disable)
(blink-cursor-mode -1)
;; Blink forever!
(setq blink-cursor-blinks 0)
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;; Allow resizing by pixels.
(setq frame-resize-pixelwise t)

(toggle-frame-maximized) ;; Maximize!

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

;; Make mark visible.
(require 'init-visual-mmv)

;; Load Theme

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

;; A good light theme for when I'm outside.
(use-package leuven-theme
  :config
  ;; Disable scaling.
  (setq
   leuven-scale-outline-headlines nil
   leuven-scale-org-agenda-structure nil
   leuven-scale-org-document-title nil
   leuven-scale-volatile-highlight nil
   )
  )

;; Nimbus is my personal theme, available on Melpa.
(use-package nimbus-theme
  :load-path "~/.emacs.d/nimbus-theme"
  :config
  (nimbus-theme)
  )

;; Set font.

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))
  (declare-function font-exists-p "init.el")

  (cond
   ((font-exists-p "Iosevka Comfy Fixed")
    (set-face-attribute
     'default nil :font "Iosevka Comfy Fixed:weight=Regular" :height 110)
    (setq-default line-spacing 0)
    )
   ((font-exists-p "Iosevka")
    (set-face-attribute
     'default nil :font "Iosevka:weight=Regular" :height 120)
    ;; 'default nil :font "Iosevka:weight=Light" :height 120)
    (setq-default line-spacing 0)
    )
   ((font-exists-p "Hack")
    (set-face-attribute
     'default nil :font "Hack:weight=Regular" :height 120)
    (setq-default line-spacing 0)
    )
   )
  )

(provide 'init-visual)
;;; init-visual.el ends here
