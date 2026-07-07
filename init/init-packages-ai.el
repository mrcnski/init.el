;;; init-packages-ai --- Load AI packages. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package agent-shell
  :ensure t
  :ensure-system-package
  (
   (claude . "brew install claude-code")
   (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")
   )
  :bind (
         ("C-q" . agent-shell)

         :map agent-shell-mode-map
         ("M-p" . agent-shell-previous-item)
         ("M-n" . agent-shell-next-item)
         )
  :config
  (setq
   agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
   )
  )

;; First-time setup:
;; M-x copilot-install-server
;; M-x copilot-login
;;
;; Check status:
;; M-x copilot-diagnose
;;
;; See also https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/.
(use-package copilot
 :hook ((prog-mode conf-mode) . copilot-mode)
 :bind (
        ("s-/" . copilot-complete)

        :map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB"   . copilot-accept-completion)
        ;; ("C-f" . copilot-accept-completion)
        ("M-f" . copilot-accept-completion-by-word)
        ("C-e" . copilot-accept-completion-by-line)
        ("s-/" . copilot-accept-completion)
        ("C-g" . copilot-clear-overlay)
        ("M-n" . copilot-next-completion)
        ("M-p" . copilot-previous-completion)
        )
 :config
 (setq
  ;; Disable the idle delay?
  copilot-idle-delay nil
  copilot-indent-offset-warning-disable t
  ;; Try to reduce the warning rate.
  ;;
  ;; NOTE: Try setting `copilot-max-char-warning-disable' if this continues to
  ;; be a problem.
  copilot-max-char (* 1000 1000)
  )
 )

(use-package promptu
  :load-path "~/.emacs.d/packages/promptu"
  :bind ("s-\"" . promptu)
  :config
  (setq
   promptu-history-file (no-littering-expand-var-file-name "promptu-history.el")
   promptu-blocks (append
                   promptu-default-blocks
                   '(
                     (:key "e" :desc "explain" :text "explain")
                     (:key "l" :desc "leave a comment" :text "leave a concise review comment")
                     (:key "g" :desc "go ahead" :text "go ahead")
                     (:key "m" :desc "move on" :text "move on")
                     ))
   )
  )
  )

(provide 'init-packages-ai)
;;; init-packages-ai.el ends here
