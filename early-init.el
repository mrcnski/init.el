;;; early-init.el --- Early init file.

;;; Commentary:

;; Most of this file lifted from
;; https://github.com/jimeh/.emacs.d/blob/master/early-init.el.

;;; Code:

;; Native-Comp
(setq native-comp-speed 2
      comp-speed 2)
(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t
      comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

;; Native-Comp
(when (or (boundp 'comp-eln-load-path) (boundp 'native-comp-eln-load-path))
  (let ((eln-cache-dir "~/.eln-cache/")
        (find-exec (executable-find "find")))

    ;; Change the eln-cache location so it doesn't get picked up by cloud sync.
    (if (boundp 'native-comp-eln-load-path)
        (setcar native-comp-eln-load-path eln-cache-dir)
      (setcar comp-eln-load-path eln-cache-dir))

    ;; Keep the default in-config location as a symlink to the real cache.
    ;;
    ;; Child Emacs processes (e.g. flycheck byte-compiling an init file) never
    ;; load this file, fall back to the default location, and would otherwise
    ;; litter it with .eln files.
    (make-directory eln-cache-dir t)
    (let ((default-cache (directory-file-name (locate-user-emacs-file "eln-cache"))))
      (unless (file-symlink-p default-cache)
         ;; A real directory here is stale cache from before this symlink
         ;; existed (or from a crashed child) and is safe to delete.
        (when (file-directory-p default-cache)
          (delete-directory default-cache t))
        (make-symbolic-link (directory-file-name (expand-file-name eln-cache-dir))
                            default-cache t)))

    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete"))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
