;;; init-visual --- Visual Settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'init-visual-cursor)
(require 'init-visual-frame)

;; Make mark visible.
(use-package visible-mark
  :init
  ;; Define before the package loads so its gray-background default loses.
  (defface visible-mark-active
    '((t (:underline t)))
    "Face for the active mark.")
  :config
  ;; Faces for inactive marks (default nil would make them invisible).
  (defface visible-mark-inactive
    '((t (:underline t)))
    "Face for inactive marks, matching the old mmv underline look.")
  (setq visible-mark-faces '(visible-mark-inactive))

  ;; A mark at end of line/buffer is drawn as an injected one-space
  ;; before-string, and overlay strings don't inherit other overlays' faces --
  ;; on the current line that punches a hole in the hl-line highlight. Merge
  ;; hl-line into the space's face when it sits on point's line.
  (define-advice visible-mark--move-overlay
      (:after (overlay _mark face) hl-line-compat)
    (when (and global-hl-line-mode
               (overlay-start overlay)
               (overlay-get overlay 'before-string)
               (= (line-beginning-position)
                  (save-excursion (goto-char (overlay-start overlay))
                                  (line-beginning-position))))
      (overlay-put overlay 'before-string
                   (propertize " " 'face (list face 'hl-line)))))

  (global-visible-mark-mode 1)
  )

;; Enable popup tooltips, use emacs tooltip implementation.
(tooltip-mode nil)
(defvar x-gtk-use-system-tooltips)
(setq x-gtk-use-system-tooltips nil)

;;; Load Theme

(defun load-theme--clear-previous (&rest _)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--clear-previous)

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

;; The best light theme ever?
(use-package catppuccin-theme
  :load-path "~/.emacs.d/packages/catppuccin-theme"
  :config
  (setq catppuccin-flavor 'latte) ;; or 'latte, 'macchiato, or 'mocha
  (catppuccin-reload))

;; Nimbus is my personal theme, available on Melpa.
(use-package nimbus-theme
  :load-path "~/.emacs.d/packages/nimbus-theme"
  :config
  (nimbus-theme)
  )

;;; Set font.

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))

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

  (set-face-attribute 'variable-pitch nil
                      :font "Iosevka Etoile"
                      :height 110
                      :weight 'light)

  (use-package mixed-pitch
    :hook
    ((org-mode markdown-mode) . mixed-pitch-mode)
    (text-mode . mixed-pitch-maybe-plain-text)
    :init
    (defun mixed-pitch-maybe-plain-text ()
      "Enable `mixed-pitch-mode' in .txt buffers.
Hooking `text-mode' directly would also catch derived modes that
should stay monospace (e.g. `yaml-ts-mode', commit messages), so
match the file extension instead."
      (when (and buffer-file-name
                 (string-match-p "\\.txt\\'" buffer-file-name))
        (mixed-pitch-mode 1)))
    :config
    (setq mixed-pitch-variable-pitch-cursor nil)
    )
  )

(provide 'init-visual)
;;; init-visual.el ends here
