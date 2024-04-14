;;; init-visual --- Visual Settings
;;
;;; Commentary:
;;
;;; Code:

(require 'init-visual-frame)
;; Make mark visible.
(require 'init-visual-mmv)

;;; Cursor settings.

;; Turn on blinking/flashing cursor? (-1 to disable)
(blink-cursor-mode -1)
;; Blink forever!
(setq blink-cursor-blinks 0)
(when (display-graphic-p)
  (setq-default cursor-type 'box))
;; Stretch cursor to be as wide as the character at point.
(setq x-stretch-cursor 1)

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

;;; Load Theme

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
  :load-path "~/.emacs.d/packages/nimbus-theme"
  :config
  (nimbus-theme)
  )

;; The best light theme ever?
(use-package catppuccin-theme
  :load-path "~/.emacs.d/packages/catppuccin-theme"
  :config
  (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha
  (catppuccin-reload))

;;; Set font.

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
     'default nil :font "Iosevka:weight=Regular" :height 110)
    ;; 'default nil :font "Iosevka:weight=Light" :height 110)
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
