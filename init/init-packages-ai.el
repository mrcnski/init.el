;;; init-packages-ai --- Load AI packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package agent-shell
  :ensure t
  ;; Loaded eagerly rather than on first command: `agent-shell-desktop' below
  ;; requires agent-shell at load time, and has to enable its mode before
  ;; `desktop-read' runs on `after-init-hook'.
  :demand t
  :ensure-system-package
  (
   (claude . "npm install -g @anthropic-ai/claude-code")
   (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp")
   )

  :preface

  (defun my-agent-shell-dnd-send-files (event)
    "Send files dropped with EVENT into an `agent-shell' buffer as context."
    (interactive "e")
    (let* ((arg (nth 2 event))
           (buffer (window-buffer (posn-window (nth 1 event))))
           (files (when (eq (car-safe arg) 'file)
                    (seq-filter #'file-exists-p (cddr arg)))))
      (if (and files
               (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                        'agent-shell-mode))
          (with-current-buffer buffer
            (agent-shell-insert
             :text (agent-shell--get-files-context :files files)))
        ;; Drops not handled here (text, or files that no longer exist) fall
        ;; through to `ns-drag-n-drop'.
        (ns-drag-n-drop event))))

  ;; See https://github.com/xenodium/shell-maker/pull/44.
  (defun my-shell-maker-search-history ()
    "Search input history (M-r), most recent input first.
Like `shell-maker-search-history', but hands `completing-read' a
table whose metadata preserves the input ring's newest-first
order, which vertico would otherwise re-sort by length and
alphabetically."
    (interactive)
    (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
      (user-error "Not in a shell"))
    (let* ((items (delete-dups
                   (seq-filter
                    (lambda (item)
                      (not (string-empty-p item)))
                    (ring-elements comint-input-ring))))
           (candidate (completing-read
                       "History: "
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             '(metadata (display-sort-function . identity)
                                        (cycle-sort-function . identity))
                           (complete-with-action action items string pred)))
                       nil t)))
      (delete-region (comint-line-beginning-position) (point-max))
      (insert candidate)))

  :bind (
         ("s-A" . agent-shell)

         :map agent-shell-mode-map
         ("M-<return>" . newline)
         ("M-p" . agent-shell-previous-item)
         ("M-n" . agent-shell-next-item)
         ("<drag-n-drop>" . my-agent-shell-dnd-send-files)
         )

  :config

  (setq
   agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
   agent-shell-header-style 'text
   ;; Fix a bug. See https://github.com/xenodium/agent-shell/issues/793.
   agent-shell-chat-mode-enabled nil
   ;; Don't auto-send point-derived context (current line, error at point)
   ;; when opening a shell. Keep only the explicit sources.
   agent-shell-context-sources '(files region)
   )

  (advice-add 'shell-maker-search-history
              :override #'my-shell-maker-search-history)

  ;; Persist agent-shell sessions across restarts, alongside
  ;; `desktop-save-mode'.  Not on MELPA; `:vc' installs from git and also
  ;; suppresses `use-package-always-ensure'.
  ;;
  ;; Reaches into agent-shell internals, and carries a local fix.
  (use-package agent-shell-desktop
    :vc (:url "https://github.com/timfel/agent-shell-desktop.el")
    :demand t
    :preface
    ;; Drop this once upstream takes the fix. See
    ;; https://github.com/timfel/agent-shell-desktop.el/issues/1.
    (defun my-agent-shell-desktop--config (config-id)
      "Return the agent config whose `:identifier' is CONFIG-ID."
      (seq-find (lambda (candidate)
                  (eq (map-elt candidate :identifier) config-id))
                (agent-shell--resolved-agent-configs)))
    :config
    (advice-add 'agent-shell-desktop--config
                :override #'my-agent-shell-desktop--config)
    (agent-shell-desktop-mode 1)
    )
  )

;; REMOVED: Doesn't work.
;; First-time setup:
;; M-x copilot-install-server
;; M-x copilot-login
;;
;; Check status:
;; M-x copilot-diagnose
;;
;; See also https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/.
;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")
;;  :hook ((prog-mode conf-mode) . copilot-mode)
;;  :bind (
;;         ("s-/" . copilot-complete)

;;         :map copilot-completion-map
;;         ("<tab>" . copilot-accept-completion)
;;         ("TAB"   . copilot-accept-completion)
;;         ;; ("C-f" . copilot-accept-completion)
;;         ("M-f" . copilot-accept-completion-by-word)
;;         ("C-e" . copilot-accept-completion-by-line)
;;         ("s-/" . copilot-accept-completion)
;;         ("C-g" . copilot-clear-overlay)
;;         ("M-n" . copilot-next-completion)
;;         ("M-p" . copilot-previous-completion)
;;         )
;;  :config
;;  (setq
;;   ;; Disable the idle delay?
;;   copilot-idle-delay nil
;;   copilot-indent-offset-warning-disable t
;;   ;; Try to reduce the warning rate.
;;   ;;
;;   ;; NOTE: Try setting `copilot-max-char-warning-disable' if this continues to
;;   ;; be a problem.
;;   copilot-max-char (* 1000 1000)
;;   )
;;  )

(use-package dream-search
  :load-path "~/.emacs.d/packages/dream-search"
  ;; Needed for load-path packages.
  :commands (dream-search-similar dream-search-theme dream-search-reindex)
  :config
  (setq dream-search-file "~/Sync/Text/org/therapy/dreams.org"
        dream-search-dir "~/Sync/Text/org/therapy/dreams/")
  )

(use-package promptu
  :load-path "~/.emacs.d/packages/promptu.el"
  :bind ("s-\"" . promptu)
  :config
  (setq
   promptu-history-file (no-littering-expand-var-file-name "promptu-history.el")
   ;; Shared with the promptu menubar app.
   promptu-blocks (promptu-blocks-from-json "~/.config/promptu/blocks.json")
   )
  )

(use-package surveyor
  :load-path "~/.emacs.d/packages/surveyor.el" ; Local repo; not yet a submodule.
  ;; Needed for load-path packages.
  :commands (surveyor surveyor-defun surveyor-file)
  :config
  (setq gptel-model 'claude-opus-4-8
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key #'gptel-api-key-from-auth-source))
  )

(provide 'init-packages-ai)
;;; init-packages-ai.el ends here
