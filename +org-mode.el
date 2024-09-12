;;; +org-mode.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Org-mode configuration
;; - configuration being tested, may need further adjusting for Doom


(after! org
  (setq
   ;; Default location of org files
   org-directory "~/org/personal"
   ;; Define the location of the file to hold tasks
   org-default-notes-file "~/org/personal/inbox.org"

   ;; Define stages for todo tasks
   org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "WAIT(h)" "|" "DONE(d)" "CNCL(c)"))
   ;;org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))
   ;;
   org-todo-keyword-faces '(("PROJ" . +org-todo-active)
                            ("WAIT" . +org-todo-onhold)
                            ("CNCL" . +org-todo-cancel))

   ;; trigger task states
   org-todo-state-tags-triggers
   (quote (("CNCL" ("CNCL" . t))
           ("WAIT" ("WAIT" . t))
           (done ("WAIT"))
           ("TODO" ("WAIT") ("CNCL"))
           ("PROJ" ("WAIT") ("CNCL"))
           ("DONE" ("WAIT") ("CNCL"))))

   ;; When item enters DONE, add a CLOSED: property with current date-time stamp
   org-log-done 'time

   ;; Make TODO states easier to distinguish by using different colours
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   hl-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("PROJ" . "DarkOrchid")
     ("WAIT" . "Firebrick")
     ("DONE" . "ForestGreen")
     ("CNCL" .  "SlateBlue"))

   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("PROJ" . "DarkOrchid")
     ("WAIT" . "Firebrick")
     ("DONE" . "ForestGreen")
     ("CNCL" .  "SlateBlue"))

   ;; org-apperancce
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))

   ;; exclude PROJECT tag from being inherited
   ;;org-tags-exclude-from-inheritance '("project")
   ;; show inherited tags in agenda view
   org-agenda-show-inherited-tags t
   ;; Removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   ;;org-clock-in-switch-to-state "STRT"
   org-clock-continuously t ;; Will fill in gaps between the last and current clocked-in task.
   ;;
   )

  (when (require 'org-superstar nil 'noerror)
    (setq org-superstar-headline-bullets-list '("◉")
          org-superstar-item-bullet-alist nil))

  (when (require 'org-fancy-priorities nil 'noerror)
    (setq org-fancy-priorities-list '("⚑" "❗" "⬆")))

  (setq org-agenda-custom-commands
        '(
          ("i" "Inbox" tags-todo "+TODO=\"TODO\""
           ((org-agenda-files (file-expand-wildcards "~/org/personal/inbox.org"))))
          ("n" "Next actions" tags-todo "+TODO=\"TODO\"")
          ("p" "Projects" tags-todo "+TODO=\"PROJ\"")
          ("w" "Waiting" tags-todo "+TODO=\"WAIT\"")
          ("s" "Someday" tags-todo "+TODO=\"TODO\"|TODO=\"PROJ\""
           ((org-agenda-files (file-expand-wildcards "~/org/personal/someday.org"))))
          ("o" "Actions and Projects" tags-todo "+TODO=\"TODO\"|TODO=\"PROJ\"")
          ))

  ;; (setq org-agenda-prefix-format '((agenda . "  %-25:c%?-12t% s")
  ;;       			   (timeline . "  % s")
  ;;       			   (todo . "  %-12:c")
  ;;       			   (tags . "  %-25:c")
  ;;       			   (search . "  %-12:c")))

  (setq org-agenda-tags-column -120)
  (setq org-tags-column -80)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down todo-state-up category-keep)
          (tags priority-down todo-state-up category-keep)
          (search category-keep)))


  ;; M-x org-agenda # to show the stuck projects
  (setq org-stuck-projects
        '("+TODO=\"PROJ\"" ("TODO") nil "") )

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps 'nil)
  (setq refile-targets (file-expand-wildcards "~/org/personal/*.org"))
  (setq org-refile-targets '(( refile-targets :todo . "PROJ" )))

  (setq org-capture-templates
        '(
          ("i" "Inbox" entry
           (file "~/org/personal/inbox.org")
           "* TODO %^{Brief Description}\nCREATED: %U\n%?" :empty-lines 1 :prepend t)

          ("n" "Next action" entry
           (file "~/org/personal/main.org")
           "** TODO %^{Brief Description}\nCREATED: %U\n%?" :empty-lines 1 :prepend t)

          ("w" "Waiting" entry
           (file "~/org/personal/main.org")
           "** WAIT %^{Brief Description}\nCREATED: %U\n%?" :empty-lines 1 :prepend t)

          ("p" "Project" entry
           (file "~/org/personal/main.org")
           "* PROJ %^{Brief Description}\n:PROPERTIES:\n:CATEGORY: %^{Id}\n:END:\nCREATED: %U\n%?" :empty-lines 1 :prepend t)

          ("s" "Someday" entry
           (file "~/org/personal/someday.org")
           "* TODO %^{Brief Description}\nCREATED: %U\n%?" :empty-lines 1 :prepend t)
          ))

  ;; To show the agenda in a more compact manner and skip a time line when something is scheduled:

  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "......"
          "----------------"))

  ;; GTD setup
  ;; (setq org-capture-templates
  ;;       `(("t" "Tâche" entry (file "inbox.org")
  ;;          ,(string-join '("* TODO %?"
  ;;                          ":PROPERTIES:"
  ;;                          ":CREATED: %U"
  ;;                          ":END:")
  ;;                        "\n"))
  ;;         ("n" "Note" entry (file "inbox.org")
  ;;          ,(string-join '("* %?"
  ;;                          ":PROPERTIES:"
  ;;                          ":CREATED: %U"
  ;;                          ":END:")
  ;;                        "\n"))
  ;;         ("m" "Réunion" entry (file "inbox.org")
  ;;          ,(string-join '("* %? :meeting:"
  ;;                          "<%<%Y-%m-%d %a %H:00>>"
  ;;                          ""
  ;;                          "/Met with: /")
  ;;                        "\n"))
  ;;         ("a" "Rendez-vous" entry (file "inbox.org")
  ;;          ,(string-join '("* %? :appointment:"
  ;;                          ":PROPERTIES:"
  ;;                          ":CREATED: %U"
  ;;                          ":END:")
  ;;                        "\n"))
  ;;         ))



  ;;(define-key global-map "\C-c n c" 'org-capture)
  ;; (setq org-agenda-custom-commands
  ;;       '(("g" "Faire avancer les choses (GTD)"
  ;;          ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
  ;;          ((tags "inbox"
  ;;                 ((org-agenda-prefix-format "  %?-12t% s")
  ;;                  ;; The list of items is already filtered by this tag, no point in showing that it exists
  ;;                  (org-agenda-hide-tags-regexp "inbox")
  ;;                  ;; The header of this section should be "Inbox: clarify and organize"
  ;;                  (org-agenda-overriding-header "\nInbox: clarifier et organiser\n")))))))
  ;; I will be using add-to-list to shadow the previous values in org-capture-templates. The new versions of a task
  ;; and note will have an extra line linking to the context they were taken in (placeholder %a)
  ;; (add-to-list 'org-capture-templates
  ;;              `("t" "Tâche" entry (file "inbox.org")
  ;;                ,(string-join '("* TODO %?"
  ;;                                ":PROPERTIES:"
  ;;                                ":CREATED: %U"
  ;;                                ":END:"
  ;;                                "/Context:/ %a")
  ;;                              "\n"
  ;;                              )))
  ;; (add-to-list 'org-capture-templates
  ;;              `("n" "Note" entry (file "inbox.org")
  ;;                ,(string-join '("* %?"
  ;;                                ":PROPERTIES:"
  ;;                                ":CREATED: %U"
  ;;                                ":END:"
  ;;                                "/Context:/ %a")
  ;;                              "\n")))

  ;; (require 'dash)

  ;; (defvar org-refile-contexts "Contexts for `org-capture'.

  ;; Takes the same values as `org-capture-templates-contexts' except
  ;; that the first value of each entry should be a valid setting for
  ;; `org-refile-targets'.")

  ;; (defun org-refile--get-context-targets ()
  ;;   "Get the refile targets for the current headline.

  ;; Returns the first set of targets in `org-refile-contexts' that
  ;; the current headline satisfies, or `org-refile-targets' if there
  ;; are no such."
  ;;   (or (car (-first (lambda (x)
  ;;                      (org-contextualize-validate-key
  ;;                       (car x)
  ;;                       org-refile-contexts))
  ;;                    org-refile-contexts
  ;;                    ))
  ;;       org-refile-targets)
  ;;   )

  ;; (defun org-refile-with-context (&optional arg default-buffer rfloc msg)
  ;;   "Refile the headline to a location based on `org-refile-targets'.

  ;; Changes the set of available refile targets based on `org-refile-contexts', but is otherwise identical to `org-refile'"
  ;;   (interactive "P")
  ;;   (let ((org-refile-targets (org-refile--get-context-targets)))
  ;;     (org-refile arg default-buffer rfloc msg)
  ;;     ))

  ;; (setq org-refile-contexts
  ;;       '((((("inbox.org") . (:regexp . "Projects"))) ;; example
  ;;          ((lambda () (string= (org-find-top-headline) "Inbox")))
  ;;          )
  ;;         ;; 6: Notes without a project go to notes.org
  ;;         (((("inbox.org") . (:regexp . "Notes")))
  ;;          ;;((lambda () (string= (org-element-property :my_type (org-element-at-point)) "NOTE")))
  ;;          ((lambda () ('regexp ":my_type:")))
  ;;          )
  ;;         ))

  ;; (setq org-agenda-files (list "inbox.org" "agenda.org"
  ;;                              "notes.org" "projects.org"))
  (setq org-agenda-custom-commands
        '(("g" "Faire avancer les choses (GTD)"
           ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
           ((tags "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   ;; The header of this section should be "Inbox: clarify and organize"
                   (org-agenda-overriding-header "\nInbox: clarifier et organiser\n")))
            ;; Show tasks that can be started and their estimates, do not show inbox
            (todo "TODO"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "main.org" "someday.org" "inbox.org"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-max-entries 5)
                   (org-agenda-overriding-header "\nTâches: Peut être fait\n")))
            ;; Show agenda around today
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))
            ;; Show tasks on hold
            (todo "WAIT"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: en attente\n")))
            ;; Show tasks that are in progress
            (todo "PROJ"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: en cours\n")))

            ;; Show tasks that I completed today
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nTerminé aujourd'hui\n"))))
           (
            ;; The list of items is already filtered by this tag, no point in showing that it exists
            (org-agenda-hide-tags-regexp "inbox")))
          ("G" "Toutes les tâches réalisables"
           ((todo "TODO"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "inbox.org" "someday.org" "main.org")) (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: réalisables\n")))
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))))


  ;; taken from stackexchange
  ;; https://emacs.stackexchange.com/questions/59357/custom-agenda-view-based-on-effort-estimates
  (defun fs/org-get-effort-estimate ()
    "Return effort estimate when point is at a given org headline.
If no effort estimate is specified, return nil."
    (let ((limits (org-get-property-block)))
      (save-excursion
        (when (and limits                            ; when non-nil
                   (re-search-forward ":Effort:[ ]*" ; has effort estimate
                                      (cdr limits)
                                      t))
          (buffer-substring-no-properties (point)
                                          (re-search-forward "[0-9:]*"
                                                             (cdr limits)))))))
  (defun fs/org-search-for-quickpicks ()
    "Display entries that have effort estimates inferior to 15.
ARG is taken as a number."
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 1 15 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))
  (defun vt/org-search-for-long-tasks ()
    "Display entries that have effort estimates longer than 1h "
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 120 600 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))

  (add-to-list 'org-agenda-custom-commands
               '("E" "Efforts view"
                 ((alltodo ""
                           ((org-agenda-skip-function 'fs/org-search-for-quickpicks)
                            (org-agenda-overriding-header "tâches rapides")))
                  (alltodo ""
                           ((org-agenda-skip-function 'vt/org-search-for-long-tasks)
                            ;; For longer tasks - show how long they are
                            (org-agenda-prefix-format "[%e] ")
                            (org-agenda-overriding-header "tâches longues"))))))


  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook! org-mode
    (lambda ()
      "Beautify Org Checkbox Symbol"
      (push '("[ ]" . "☐") prettify-symbols-alist)
      (push '("[X]" . "☑" ) prettify-symbols-alist)
      (push '("[-]" . "❍" ) prettify-symbols-alist)
      (prettify-symbols-mode)))

  )

(provide '+org-mode)
;; End of Org-mode Configuration
;; ---------------------------------------
