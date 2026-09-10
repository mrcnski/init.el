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
  ;; Copies from agent-shell buffers yield the agent's original markdown rather
  ;; than the rendered text.
  ;;
  ;; `filter-buffer-substring-function' covers everything built on the kill
  ;; machinery (M-w, C-l, M-W); `copy-as-format' (s-w) extracts text with
  ;; `buffer-substring-no-properties', so it needs the advice below.
  (defun my-agent-shell-filter-buffer-substring (beg end &optional delete)
    "Reconstruct the original markdown between BEG and END when copying.

Exception: a selection lying entirely within a single inline construct
copies as the visible plain text (grabbing a filename or symbol to
navigate somewhere shouldn't drag its markers along).

Delegate to the default filter when DELETE is non-nil."
    (if delete
        (buffer-substring--filter beg end delete)
      (let ((source (get-text-property beg 'agent-shell-markdown-source)))
        (if (and source
                 (not (equal source ""))
                 (not (string-search "\n" source))
                 (>= (next-single-property-change
                      beg 'agent-shell-markdown-source nil (point-max))
                     end))
            (buffer-substring-no-properties beg end)
          (agent-shell-markdown-reconstruct beg end)))))

  (defun my-agent-shell-setup-markdown-copy ()
    "Make copy commands in this buffer yield the original markdown."
    (setq-local filter-buffer-substring-function
                #'my-agent-shell-filter-buffer-substring))

  (defun my-copy-as-format-agent-shell-markdown (orig-fun)
    "Give `copy-as-format' the reconstructed markdown in agent-shell buffers."
    (if (and (bound-and-true-p agent-shell-ui-mode) (use-region-p))
        (my-agent-shell-filter-buffer-substring (region-beginning) (region-end))
      (funcall orig-fun)))

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
alphabetically.

Moves to the prompt first, so it works from anywhere in the buffer."
    (interactive)
    (unless (eq major-mode (shell-maker-major-mode shell-maker--config))
      (user-error "Not in a shell"))
    (goto-char (point-max))
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

  ;; Typing, yanking, or recalling history over read-only text goes to the
  ;; prompt first, like eshell.
  ;;
  ;; `comint-scroll-to-bottom-on-input' doesn't work: agent-shell's headings and
  ;; buttons carry keymaps that remap `self-insert-command' to `ignore'.  And
  ;; comint's history commands refuse to run away from the prompt.
  (defun my-agent-shell-preinput-goto-prompt ()
    "Move to the prompt before a key that would insert or yank there."
    (when-let* ((process (get-buffer-process (current-buffer)))
                ((< (point) (process-mark process)))
                (command (key-binding (this-command-keys-vector) t t (point-max)))
                ((memq command '(self-insert-command yank
                                 comint-previous-input comint-next-input))))
      (setq this-command (or (command-remapping command (point-max)) command))
      (goto-char (point-max))))

  (defun my-agent-shell-setup-preinput-goto-prompt ()
    "Install `my-agent-shell-preinput-goto-prompt' in this buffer."
    (add-hook 'pre-command-hook #'my-agent-shell-preinput-goto-prompt nil t))

  (defun my-agent-shell-context-indicator-append-cost (indicator)
    "Append the session's cumulative cost to the header context INDICATOR."
    (if-let* ((indicator)
              (usage (map-elt (agent-shell--state) :usage))
              (amount (map-elt usage :cost-amount))
              ((> amount 0)))
        (concat indicator
                (propertize (format " · $%.2f" amount)
                            'face 'agent-shell-secondary))
      indicator))

  :bind (
         ("s-A" . agent-shell)

         :map agent-shell-mode-map
         ("M-{" . comint-previous-prompt)
         ("M-}" . comint-next-prompt)

         :map agent-shell-ui-mode-map
         ("M-<return>" . newline)
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
  ;; See the markdown-copy functions in :preface.
  (add-hook 'agent-shell-ui-mode-hook #'my-agent-shell-setup-markdown-copy)
  (advice-add 'copy-as-format--extract-text
              :around #'my-copy-as-format-agent-shell-markdown)

  ;; Show the session cost in the header.
  (advice-add 'agent-shell--context-usage-indicator
              :filter-return #'my-agent-shell-context-indicator-append-cost)

  ;; See `my-agent-shell-preinput-goto-prompt' in :preface.
  (add-hook 'agent-shell-mode-hook #'my-agent-shell-setup-preinput-goto-prompt)

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
