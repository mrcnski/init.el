;;; init-basics --- Basics for the rest of the config.
;;
;;; Commentary:
;;
;;; Code:

(defvar user-code-directory "~/Repos/")
(defvar user-text-directory "~/Text/")

(defvar user-scratchpad-path (concat user-text-directory "scratchpad.txt"))
(defvar user-data-path (concat user-text-directory "data.txt"))
(defvar user-org-directory (concat user-text-directory "org/"))

(defvar user-ideas-org (concat user-org-directory "ideas.org"))
(defvar user-notes-org (concat user-org-directory "notes.org"))
(defvar user-projects-org (concat user-org-directory "notes.org"))
(defvar user-todo-org (concat user-org-directory "todo.org"))
(defvar user-work-org (concat user-org-directory "work.org"))

(defvar user-emacs-config-directory "~/.local/emacs/")
(defvar user-emacs-var-directory (concat user-emacs-config-directory "var/"))
(defvar user-emacs-etc-directory (concat user-emacs-config-directory "etc/"))
(defvar user-emacs-elpa-directory (concat user-emacs-config-directory "elpa/"))

;; Should be based on keyboard rate, to not trigger when cursor is moving.
(defvar highlight-delay .04)

(defvar info-delay .25)

;; Open .emacs init.
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

;; Open scratchpad.txt.
(defun open-scratchpad-file ()
  "Open scratchpad file."
  (interactive)
  (find-file user-scratchpad-path))
(global-set-key (kbd "C-c s") 'open-scratchpad-file)

;; Open data.txt.
(defun open-data-file ()
  "Open data file."
  (interactive)
  (find-file user-data-path))
(global-set-key (kbd "C-c d") 'open-data-file)

(provide 'init-basics)
;;; init-basics.el ends here
