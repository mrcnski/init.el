;;; init-packages-general --- Load general packages.
;;
;;; Commentary:
;;
;;; Code:

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

;; REMOVED: Not allowed at Amplify.
;; (use-package gptel
;;   :bind (
;;          ("C-`" . gptel)

;;          :map gptel-mode-map
;;          ("<return>" . gptel-send)
;;          )
;;   :config
;;   (setq
;;    gptel-model 'claude-3-5-sonnet-20241022
;;    gptel-backend (gptel-make-anthropic
;;                      "Claude"
;;                    :stream t
;;                    :key (getenv "ANTHROPIC_API_KEY")
;;                    )
;;    )
;;   (add-to-list 'display-buffer-alist
;;                '("\\*Claude\\*"
;;                  (display-buffer-reuse-window
;;                   display-buffer-same-window)))

;;   ;; Always moves point to the end of the current input, first.
;;   (advice-add 'gptel-send :before
;;               #'(lambda (&rest _) (call-interactively 'end-of-buffer)))
;;   )

(provide 'init-packages-ai)
;;; init-packages-ai.el ends here
