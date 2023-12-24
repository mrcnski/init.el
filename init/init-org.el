;;; init-org --- Org mode settings and packages.
;;
;;; Commentary:
;;
;;; Code:

(use-package org
  :ensure nil

  :bind (
         ;; Insert link with C-c C-l.
         ;; ("C-c C-l" . org-store-link)
         ("C-c c" . org-capture)

         ;; Jump to last refile or capture.
         ("C-c j" . org-refile-goto-last-stored)

         :map org-mode-map

         ;; ("<s-return>" . org-meta-return-end)
         ("C-S-n" . org-metadown)
         ("C-S-p" . org-metaup)
         ("C-<" . org-shiftmetaleft)
         ("C->" . org-shiftmetaright)
         ("M-p" . org-previous-visible-heading)
         ("M-n" . org-next-visible-heading)
         ("C-^" . org-up-element)
         ("C-j" . join-next-line)
         ("C-c SPC" . org-table-blank-field)
         ("C-c C-j" . consult-org-heading) ; orig. org-goto

         ("<mouse-3>" . mouse-org-cycle)
         )

  :mode ("\\.org$" . org-mode)

  :hook (
         (org-mode . org-mode-hook-fun)
         (org-mode . company-mode)
         )

  :init

  (defun org-align-tags-all ()
    (interactive)
    (org-align-tags t))

  (defun org-update-cookies-after-save ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (org-update-statistics-cookies "ALL")))

  (defun org-mode-hook-fun ()
    "Initialize `org-mode'."

    ;; Unbind key stolen by org-mode ('org-cycle-agenda-files').
    (local-unset-key (kbd "C-,"))
    (local-unset-key (kbd "C-'"))

    ;; Align all tags.
    (add-hook 'before-save-hook 'org-align-tags-all nil 'make-it-local)

    ;; Update checkboxes after saving.
    (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)
    )

  :config

  ;;; Settings

  (setq
   ;; Default org directory.
   org-directory user-org-directory

   ;; Initial visibility.
   org-startup-folded t

   ;; Inline images.
   org-startup-with-inline-images t
   org-image-actual-width '(0.5)

   ;; Hide leading stars?
   org-hide-leading-stars t
   org-adapt-indentation nil
   org-startup-indented nil
   org-odd-levels-only nil
   ;; Use different styling for nested bullets.
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))

   org-ellipsis " …"

   ;; Non-nil means unchecked boxes will prevent switching the parent to DONE.
   org-enforce-todo-checkbox-dependencies nil
   ;; All subtasks must be DONE before marking a task as DONE.
   org-enforce-todo-dependencies t

   ;; Try to keep cursor before ellipses.
   org-special-ctrl-a/e t
   ;; Smart editing of invisible region around ellipses.
   org-catch-invisible-edits 'smart

   ;; Prefer rescheduling to future dates and times.
   org-read-date-prefer-future 'time

   ;; M-RET should not split the heading if point is not at the end of a line.
   ;; (setq org-M-RET-may-split-line nil)

   ;; Should ‘org-insert-heading’ leave a blank line before new heading/item?
   org-blank-before-new-entry '((heading . t) (plain-list-item . nil))

   ;; Custom to-do states.
   org-todo-keywords
   '((sequence "TODO(t)" "NOW(n)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)")
     (sequence "|" "CANCELED(x)"))

   ;; tag settings

   ;; Don't align tags.
   ;; org-tags-column 0
   ;; org-auto-align-tags nil

   ;; Should the ORDERED property also be shown as a tag?
   org-track-ordered-property-with-tag t

   ;; logging settings

   ;; Log into LOGBOOK drawer.
   org-log-into-drawer t
   ;; Log time a task was set to Done.
   org-log-done 'time
   ;; Don't log the time a task was rescheduled or redeadlined.
   org-log-reschedule nil
   org-log-redeadline nil

   ;; org-refile settings

   ;; Refile notes to the top of the list?
   org-reverse-note-order nil
   ;; Use headline paths (level1/level2/...)
   org-refile-use-outline-path t
   ;; Go down in steps when completing a path.
   org-outline-path-complete-in-steps nil
   org-refile-targets
   '(
     ;; The current buffer.
     (nil . (:maxlevel . 99))
     ;; My custom files.
     (user-todo-org . (:maxlevel . 99))
     (user-notes-org . (:maxlevel . 99))
     (user-work-org . (:maxlevel . 99))
     (user-ideas-org . (:maxlevel . 99))
     (user-projects-org . (:maxlevel . 99))
     )
   ;; Jump to headings with completion.
   org-goto-interface 'outline-path-interface
   org-goto-max-level 99
   ;; Always show full context, no matter how we get to a certain heading (e.g.
   ;; `isearch', `org-goto', whatever).
   org-fold-show-context-detail '((default . tree))
   )

  ;; org-capture settings

  ;; org-capture template.
  (setq org-capture-templates
        '(
          (
           "o" "One-off task." entry
           (file+headline "todo.org" "General")
           "* %?\nSCHEDULED: %t"
           :unnarrowed t
           :empty-lines-before 1
           )
          (
           "r" "Recurring task." entry
           (file+olp "todo.org" "Recurring" "General")
           "* |%^{Recurrence}| %?\nSCHEDULED: %t"
           :unnarrowed t
           :empty-lines-before 1
           )
          (
           "w" "Work task." entry
           (file+headline "work.org" "Todo")
           "* TODO %?"
           :unnarrowed t
           :empty-lines-before 1
           :prepend 1
           )
          ))

  ;; Shortcuts/Keybindings

  ;; REMOVED: Not using it for now.
  ;; ;; org-capture with template as default behavior.
  ;; (defun org-task-capture ()
  ;;   "Capture a task with my todo template."
  ;;   (interactive)
  ;;   (org-capture nil "t"))
  ;; (defun org-note-capture ()
  ;;   "Capture a note with my note template."
  ;;   (interactive)
  ;;   (org-capture nil "n"))

  (defun mouse-org-cycle (@click)
    (interactive "e")
    (let ((p1 (posn-point (event-start @click))))
      (goto-char p1)
      (call-interactively 'org-cycle)
      )
    )

  (defun org-fix-blank-lines (&optional prefix)
    "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
    (interactive "P")
    (org-map-entries
     (lambda ()
       (org-with-wide-buffer
        ;; `org-map-entries' narrows the buffer, which prevents us from seeing
        ;; newlines before the current heading, so we do this part widened.
        (while (not (looking-back "\n\n" nil))
          ;; Insert blank lines before heading.
          (insert "\n")))
       (let ((end (org-entry-end-position)))
         ;; Insert blank lines before entry content
         (forward-line)
         (while (and (org-at-planning-p)
                     (< (point) (point-max)))
           ;; Skip planning lines
           (forward-line))
         (while (re-search-forward org-drawer-regexp end t)
           ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
           ;; for some reason it doesn't work correctly when operating on hidden text.
           ;; This works, taken from `org-agenda-get-some-entry-text'.
           (re-search-forward "^[ \t]*:END:.*\n?" end t)
           (goto-char (match-end 0)))
         (unless (or (= (point) (point-max))
                     (org-at-heading-p)
                     (looking-at-p "\n"))
           (insert "\n"))))
     t (if prefix
           nil
         'tree)))

  ;;; org packages

  ;; Markdown export.
  (require 'ox-md)

  (use-package org-agenda
    :ensure nil
    :hook (org-agenda-mode . visual-line-mode)
    :bind (
           ("C-c a" . org-agenda-personal)
           ("C-c w" . org-agenda-work)

           :map org-agenda-mode-map

           ("s" . org-agenda-schedule)
           ("M" . org-agenda-bulk-mark-all)
           ("M-n" . org-agenda-next-date-line)
           ("M-p" . org-agenda-previous-date-line)
           )

    :init

    (defun org-agenda-personal ()
      (interactive)
      ;; Set location of agenda files.
      (setq org-agenda-files (list
                              user-todo-org
                              ))
      (org-agenda-list)
      )
    (defun org-agenda-work ()
      (interactive)
      ;; Set location of agenda files.
      (setq org-agenda-files (list
                              user-work-org
                              ))
      (org-todo-list)
      )

    :config

    ;; Set default span of agenda view.
    (setq org-agenda-span 'week)

    ;; Show scheduled items in order from most to least recent.
    (setq org-agenda-sorting-strategy
          '((agenda habit-down time-up scheduled-down priority-down category-keep)
            (todo   priority-down category-keep)
            (tags   priority-down category-keep)
            (search category-keep)))

    ;; Customize columns (remove filename/category, mostly redundant).
    (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                     (todo . " %i %-12:c")
                                     (tags . " %i %-12:c")
                                     (search . " %i %-12:c")))

    (setq
     ;; Stop org-agenda from messing up my windows!!
     org-agenda-window-setup 'current-window
     ;; Start org-agenda from the current day.
     org-agenda-start-on-weekday nil
     ;; Don't align tags in the org-agenda (sometimes it messes up the display).
     ;; org-agenda-tags-column 0
     )

    (defun org-agenda-refresh ()
      "Refresh all `org-agenda' buffers."
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-agenda-mode)
            (org-agenda-maybe-redo)
            ))))

    ;; Refresh org-agenda after changing an item status.
    ;; (add-hook 'org-trigger-hook 'org-agenda-refresh)
    ;; Refresh org-agenda after rescheduling a task.
    (defadvice org-schedule (after refresh-agenda activate)
      "Refresh `org-agenda'."
      (org-agenda-refresh))

    ;; Refresh org-agenda after an org-capture.
    (add-hook 'org-capture-after-finalize-hook 'org-agenda-refresh)
    )

  ;; Recurring org-mode tasks.
  (use-package org-recur
    ;; :load-path "~/projects/org-recur/"
    :after org
    :bind (
           :map org-recur-mode-map

           ;; ("C-c d" . org-recur-finish)
           ("C-c 0" . org-recur-schedule-today)
           ("C-c 1" . org-recur-schedule-1)
           ("C-c 2" . org-recur-schedule-2)

           :map org-recur-agenda-mode-map

           ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
           ("d" . org-recur-finish)
           ("0" . org-recur-schedule-today)
           ("1" . org-recur-schedule-1)
           ("2" . org-recur-schedule-2)
           ;; ("C-c d" . org-recur-finish)
           ("C-c 0" . org-recur-schedule-today)
           ("C-c 1" . org-recur-schedule-1)
           ("C-c 2" . org-recur-schedule-2)
           )
    :hook ((org-mode . org-recur-mode)
           (org-agenda-mode . org-recur-agenda-mode))
    :demand t
    :config
    (defun org-recur-schedule-1 ()
      (interactive)
      (org-recur-schedule-date "|+1|"))
    (defun org-recur-schedule-2 ()
      (interactive)
      (org-recur-schedule-date "|+2|"))

    (setq org-recur-finish-done t
          ;; `org-log-done' should not be 'note if this is t.
          org-recur-finish-archive t)
    )

  ;; Display groups in org-agenda to make things a bit more organized.
  (use-package org-super-agenda
    :after org-agenda
    :config
    (org-super-agenda-mode)

    (setq
     org-super-agenda-header-separator ""
     org-super-agenda-unmatched-name "Other"
     org-super-agenda-groups
     '(
       ;; Each group has an implicit OR operator between its selectors.
       (:name "Today"  ; Optionally specify section name
              :time-grid t  ; Items that appear on the time grid.
              :todo "TODAY"   ; Items that have this todo keyword.
              )
       (:name "Work"
              :category "work"
              :tag "work"
              )
       (:name "High Priority"
              :priority "A"
              :order 1
              )
       (:name "Deep Work"
              :category "deep"
              :tag "deep"
              :order 2
              )
       (:name "Shopping List"
              :category "shopping"
              :tag "shopping"
              :order 3
              )
       (:name "Cleaning"
              :category "cleaning"
              :tag "cleaning"
              :order 4
              )
       (:name "Hygiene"
              :category "hygiene"
              :tag "hygiene"
              :order 5
              )
       (:name "Health"
              :category "health"
              :tag "health"
              :order 6
              )
       (:name "Financial"
              :category "financial"
              :tag "financial"
              :order 7
              )
       (:name "Self-improvement"
              :category "self"
              :tag "self"
              :order 8
              )
       (:name "Physical"
              :category "physical"
              :tag "physical"
              :order 10
              )

       (:name "Move"
              :category "move"
              :tag "move"
              :order 20
              )

       (:name "Travel"
              :category "travel"
              :tag "travel"
              :order 30
              )

       ;; Here, the agenda will display items that didn't match any of these
       ;; groups, with the default order position of 99.

       (:name "Tech"
              :category "tech"
              :tag "tech"
              :order 200
              )
       (:name "Blog"
              :category "blog"
              :tag "blog"
              :order 210
              )
       (:name "To Read"
              :category "read"
              :tag "read"
              :order 220
              )
       (:name "To Watch"
              :category "watch"
              :tag "watch"
              :order 230
              )
       (:name "Waiting"
              :todo "WAITING"
              :order 240
              )
       (:name "Reminders"
              :category "reminder"
              :tag "reminder"
              :order 250
              )
       (:name "Evening"
              :category "evening"
              :tag "evening"
              :order 260
              )
       (:name "Low priority"
              :priority "C"
              :order 500
              )
       )))
  )

(provide 'init-org)
;;; init-org.el ends here
