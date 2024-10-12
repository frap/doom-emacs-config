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
   org-agenda-files (file-expand-wildcards "~/org/personal/*.org")

   ;; Define stages for todo tasks
   org-todo-keywords
   '((sequence "TODO(t)" "COUR(c)" "PROJ(p)" "WAIT(h)" "|" "DONE(d)" "CNCL(a)"))
   ;;org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))
   ;;
   org-todo-keyword-faces '(("PROJ" . +org-todo-active)
                            ("COUR" . +org-todo-active)
                            ("WAIT" . +org-todo-onhold)
                            ("CNCL" . +org-todo-cancel))

   ;; trigger task states
   ;; org-todo-state-tags-triggers
   ;; (quote (("CNCL" ("CNCL" . t))
   ;;         ("WAIT" ("WAIT" . t))
   ;;         (done ("WAIT"))
   ;;         ("COUR" ("WAIT") ("CNCL"))
   ;;         ("TODO" ("COUR") ("WAIT") ("CNCL"))
   ;;         ("PROJ" ("WAIT") ("CNCL"))
   ;;         ("DONE" ("WAIT") ("CNCL"))))

   ;; When item enters DONE, add a CLOSED: property with current date-time stamp
   org-log-done 'time

   ;; Make TODO states easier to distinguish by using different colours
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   hl-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("COUR" . "LightBlue")
     ("PROJ" . "DarkOrchid")
     ("WAIT" . "Firebrick")
     ("DONE" . "ForestGreen")
     ("CNCL" .  "SlateBlue"))

   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("COUR" . "LightBlue")
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

  (setq org-agenda-prefix-format
        '((agenda   . "  %-12:c%?-12t% s")
          ;;         (timeline . "  % s")
          (todo     . " ")
          (tags     . "  %-12:c")
          (search   . "  %-12:c")))


  ;; M-x org-agenda # to show the stuck projects
  ;; (setq org-stuck-projects
  ;;       '("+TODO=\"PROJ\"" ("TODO") nil "") )

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps 'nil)
  (setq refile-targets (file-expand-wildcards "~/org/personal/*.org"))
  (setq org-refile-targets '(( refile-targets :todo . "PROJ" )))


  ;; To show the agenda in a more compact manner and skip a time line when something is scheduled:

  ;; (setq org-agenda-time-grid
  ;;       '((daily today require-timed remove-match)
  ;;         (800 1000 1200 1400 1600 1800 2000)
  ;;         "......"
  ;;         "----------------"))

  ;; GTD setup
  (setq org-capture-templates
        `(("t" "Brève description de la tâche non urgente" entry (file+headline "inbox.org" "Tâches" )
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("p" "Brève description de la Projet" entry (file+headline "inbox.org" "Projets")
           ,(string-join '("* PROJ %?"
                           ":PROPERTIES:"
                           ":CATEGORY: %^{Projet}"
                           ":CREATED: %U"
                           ":END:"
                           "/Contexte:/ %a")
                         "\n"))
          ("u" "Brève description de la tâche urgente" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@urgente:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("i" "Brève description de la tâche importante" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@importante:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("n" "Prochaine action" entry (file "inbox.org")
           ,(string-join '("** TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("m" "Réunion" entry (file+headline "agenda.org" "Avenir")
           ,(string-join '("* %? :@meeting:"
                           "<%<%Y-%m-%d %a %H:00-%H:30>>"
                           "\n"
                           "/Rencontré:/ %a"
                           "\n")))
          ("a" "Rendez-vous" entry (file "inbox.org")
           ,(string-join '("* %? :@appointment:"
                           "<%<%Y-%m-%d %a %H:00-%H:50>>"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ))


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
  ;;       '((((("inbox.org") . (:regexp . "Projets"))) ;; example
  ;;          ((lambda () (string= (org-find-top-headline) "Inbox")))
  ;;          )
  ;;         ;; 6: Notes without a project go to notes.org
  ;;         (((("inbox.org") . (:regexp . "Notes")))
  ;;          ;;((lambda () (string= (org-element-property :my_type (org-element-at-point)) "NOTE")))
  ;;          ((lambda () ('regexp ":my_type:")))
  ;;          )
  ;;         ))
  ;; ORG Agenda

  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-tags-column -120)
  ;;(setq org-tags-column -80)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down todo-state-up category-keep)
          (tags priority-down todo-state-up category-keep)
          (search category-keep)))

  (defun log-todo-next-creation-date (&rest ignore)
    "Log COUR (en cours) creation time in the property drawer under the key 'ACTIVÉ'"
    (when (and (string= (org-get-todo-state) "COUR")
               (not (org-entry-get nil "ACTIVÉ")))
      (org-entry-put nil "ACTIVÉ" (format-time-string "[%Y-%m-%d]"))))

  (defun my/org-pomodoro-update-tag ()
    (when (org-get-todo-state)
      (org-todo "COUR")))
  (add-hook 'org-pomodoro-started-hook #'my/org-pomodoro-update-tag)

  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
  ;; (setq org-agenda-files (list "inbox.org" "agenda.org"
  ;;                              "notes.org" "projects.org"))
  ;;
  ;;(define-key global-map "\C-c n c" 'org-capture)
  (setq org-agenda-custom-commands
        '(("g" "Faire avancer les choses (GTD)"
           ((agenda ""
                    ((org-agenda-span 5)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)
                     (org-agenda-overriding-header "\nBoîte de Réception: clarifier et organiser\n")
                     ))
            (tags-todo "@importante"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Importantes\n")))
            (tags-todo "@urgente"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Urgentes\n")))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* COUR"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            ;; Show tasks that can be started and their estimates, do not show inbox
            (tags-todo "-@importante-@urgente-@meeting"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline 'scheduled))
                        (org-agenda-files (list "agenda.org" "inbox.org"))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-max-entries 5)
                        (org-agenda-overriding-header "\nTâches peut être fait\n")))
            (todo "WAIT"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches en attente\n")))
            ;; Show tasks that I completed today
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nTerminé aujourd'hui\n")))
            )
           (
            ;; The list of items is already filtered by this tag, no point in showing that it exists
            (org-agenda-hide-tags-regexp "inbox")
            ))
          ("G" "Toutes les tâches réalisables"
           ((todo "TODO|COUR|PROJ"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "inbox.org" "agenda.org"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: Réalisables\n")))
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))))


  ;; (setq org-agenda-custom-commands
  ;;     '(
  ;;       ("i" "Boîte de Réception" tags-todo "+TODO=\"TODO\""
  ;;        ((org-agenda-files (file-expand-wildcards "~/org/personal/inbox.org"))))
  ;;       ("n" "Next actions" tags-todo "+TODO=\"TODO\"")
  ;;       ("p" "Projects" tags-todo "+TODO=\"PROJ\"")
  ;;       ("w" "En Attendant" tags-todo "+TODO=\"WAIT\"")
  ;;       ("s" "Un Jour" tags-todo "+TODO=\"TODO\"|TODO=\"PROJ\"")
  ;;       ("o" "Actions and Projects" tags-todo "+TODO=\"TODO\"|TODO=\"PROJ\"")
  ;;      ))
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

(map! :leader
      (:prefix ("j" . "journal") ;; org-journal bindings
       :desc "Create new journal entry" "j" #'org-journal-new-entry
       :desc "Open previous entry" "p" #'org-journal-open-previous-entry
       :desc "Open next entry" "n" #'org-journal-open-next-entry
       :desc "Search journal" "s" #'org-journal-search-forever))


;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq org-agenda-skip-scheduled-if-done t
;;         org-agenda-skip-deadline-if-done t
;;         org-agenda-include-deadlines t
;;         org-agenda-block-separator nil
;;         org-agenda-compact-blocks t
;;         org-agenda-start-day nil ;; i.e. today
;;         org-agenda-span 1
;;         org-agenda-start-on-weekday nil)
;;   (setq org-agenda-custom-commands
;;         '(("c" "Super view"
;;            ((agenda "" ((org-agenda-overriding-header "")
;;                         (org-super-agenda-groups
;;                          '((:name "Today"
;;                             :time-grid t
;;                             :date today
;;                             :order 1)))))
;;             (alltodo "" ((org-agenda-overriding-header "")
;;                          (org-super-agenda-groups
;;                           '((:log t)
;;                             (:name "To refile"
;;                              :file-path "refile\\.org")
;;                             (:name "Next to do"
;;                              :todo "NEXT"
;;                              :order 1)
;;                             (:name "Important"
;;                              :priority "A"
;;                              :order 6)
;;                             (:name "Today's tasks"
;;                              :file-path "journal/")
;;                             (:name "Due Today"
;;                              :deadline today
;;                              :order 2)
;;                             (:name "Scheduled Soon"
;;                              :scheduled future
;;                              :order 8)
;;                             (:name "Overdue"
;;                              :deadline past
;;                              :order 7)
;;                             (:name "Meetings"
;;                              :and (:todo "MEET" :scheduled future)
;;                              :order 10)
;;                             (:discard (:not (:todo "TODO")))))))))))
;;   :config
;;   (org-super-agenda-mode))

(provide '+org-mode)
;; End of Org-mode Configuration
;; ---------------------------------------
