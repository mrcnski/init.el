;;; init-vertico-et-al --- vertico + consult + orderless + marginalia.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-functions-and-shortcuts)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  (setq
   ;; Scroll margin.
   vertico-scroll-margin 3

   ;; Show more candidates
   ;; vertico-count 20

   ;; Grow and shrink the Vertico minibuffer
   ;; vertico-resize t

   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
   vertico-cycle nil
   )

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config

  ;; Enable mouse support for vertico.
  (use-package vertico-mouse
    :ensure nil
    :config
    (vertico-mouse-mode)
    )

  ;; Quickly select a line.
  (use-package vertico-quick
    :ensure nil
    :bind (
           :map vertico-map
           ("C-," . vertico-quick-exit)
           )
    )

  ;; Repeat previous vertico inputs even if they were aborted.
  (use-package vertico-repeat
    :ensure nil
    :bind (
           :map vertico-map
           ("M-P" . vertico-repeat-previous)
           ("M-N" . vertico-repeat-next)
           )
    :init
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    )
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
   completion-styles '(orderless partial-completion)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))
   )
  )

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (
         ("M-i" . consult-imenu)
         ("s-j" . consult-buffer) ;; orig. switch-to-buffer
         ("s-m" . consult-mark)
         ("s-i" . consult-ripgrep-inexact-save)
         ("s-u" . consult-ripgrep-exact-save)
         ("s-l" . consult-goto-line) ;; orig. goto-line
         ("s-y" . consult-yank-pop) ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos) ;; REMOVED: use C-h o instead.
         ("<help> m" . consult-minor-mode-menu)

         ;; Consult bindings from readme
         ;; ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ;; ("C-c k" . consult-kmacro)
         ([remap Info-search] . consult-info)
         ;; M-s bindings (search-map)
         ("M-s l" . consult-line)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  (defun consult-ripgrep-exact-save (&optional prefix initial)
    "Save before calling `consult-ripgrep', matching exactly."
    (interactive "P")
    (save-all)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / \
               --case-sensitive --no-heading --with-filename --line-number \
               --word-regexp --hidden --glob !{.git,target,node_modules}"
           )
          (dir
             (cond ((and prefix
                         (= 4 (prefix-numeric-value prefix)))
                    default-directory)
                   (t nil))))
        (consult-ripgrep dir initial))
    )

  (defun consult-ripgrep-inexact-save (&optional prefix initial)
    "Save before calling `consult-ripgrep', matching inexactly."
    (interactive "P")
    (save-all)
    (defvar consult-ripgrep-args)
    (let ((consult-ripgrep-args
           "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / \
               --ignore-case --no-heading --with-filename --line-number \
               --hidden --glob !{.git,target,node_modules}"
           )
          (dir
             (cond ((and prefix
                         (= 4 (prefix-numeric-value prefix)))
                    default-directory)
                   (t nil))))
        (consult-ripgrep dir initial))
    )

  (setq consult-async-min-input 2)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-bookmark
   consult-theme
   consult-recent-file
   consult-ripgrep
   consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '(:debounce 0.2 any)
   ;; :preview-key "M-."
   )
  ;; Run some more hooks when previewing files.
  (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode-check-buffers)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  (use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
  )

;; Enable richer annotations using Marginalia.
(use-package marginalia
  :bind (
         ;; Either bind `marginalia-cycle` globally or only in the minibuffer
         ("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)
         )

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (
   ("C-z" . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'

   :map embark-general-map
   ("K" . my/embark-search)
   :map embark-function-map
   ("K" . my/embark-search)
   )

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Use C-u before embark-act to reverse the behavior.
  (setq embark-quit-after-action
        '(
          ;; (eshell . t)
          ;; (embark-copy-as-kill . nil)
          ;; (embark-insert . t)
          (t . t)
          ))

  ;; Show embark help popup?
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  (delete 'embark-mixed-indicator embark-indicators)

  (defun my/embark-search (term)
    (interactive "sSearch Term: ")
    (browse-url
     (format "https://kagi.com/search?q=%s" term)))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(provide 'init-vertico-et-al)
;;; init-vertico-et-al.el ends here
