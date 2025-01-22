;;; init-packages-general --- Load general packages.
;;
;;; Commentary:
;;
;;; Code:

(use-package aider
  :load-path "~/.emacs.d/packages/aider"
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "gpt-4o-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-`") 'aider-transient-menu)
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
         :map copilot-completion-map
              ;; ("C-<tab>" . copilot-accept-completion)
              ;; ("C-TAB"   . copilot-accept-completion)
              ("C-g" . copilot-clear-overlay)
              ("C-f" . copilot-accept-completion)
              ("M-f" . copilot-accept-completion-by-word)
              ("C-e" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              )
  :config
  (setq
   copilot-indent-offset-warning-disable t
   ;; Try to reduce the warning rate.
   ;;
   ;; NOTE: Try setting `copilot-max-char-warning-disable' if this continues to
   ;; be a problem.
   copilot-max-char (* 1000 1000)
   )
  )

(provide 'init-packages-ai)
;;; init-packages-ai.el ends here
