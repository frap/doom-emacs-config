;;; +org-mode.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Org-mode configuration
;; - configuration being tested, may need further adjusting for Doom


(after! org
  (setq
   ;; Default location of org files
   org-directory "~/org/personal"
   ;; Define the location of the file to hold tasks
   org-default-notes-file "~/org/personal/todo.org"

   ;; Define stages for todo tasks
   org-todo-keywords
   '((sequence "TODO(t)" "STRT(s)" "HOLD(h)" "|" "DONE(d)" "CNCL(c)"))
   ;;org-todo-keywords '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))
   ;;
   org-todo-keyword-faces '(("STRT" . +org-todo-active)
                            ("HOLD" . +org-todo-onhold)
                            ("CNCL" . +org-todo-cancel))

   ;; When item enters DONE, add a CLOSED: property with current date-time stamp
   org-log-done 'time

   ;; Make TODO states easier to distinguish by using different colours
   ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
   hl-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("STRT" . "DarkOrchid")
     ("HOLD" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("CNCL" .  "SlateBlue"))

   org-todo-keyword-faces
   '(("TODO" . "SlateGray")
     ("STRT" . "DarkOrchid")
     ("HOLD" . "Firebrick")
     ("REVIEW" . "Teal")
     ("DONE" . "ForestGreen")
     ("CNCL" .  "SlateBlue"))
   )

  ;; GTD setup
  (setq org-capture-templates
        `(("t" "Task" entry (file "inbox.org")
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("n" "Note" entry (file "inbox.org")
           ,(string-join '("* %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("m" "Meeting" entry (file "inbox.org")
           ,(string-join '("* %? :meeting:"
                           "<%<%Y-%m-%d %a %H:00>>"
                           ""
                           "/Met with: /")
                         "\n"))
          ("a" "Appointment" entry (file "inbox.org")
           ,(string-join '("* %? :appointment:"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ))


  (setq org-agenda-custom-commands
        '(("g" "Faire avancer les choses (GTD)"
           ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
           ((tags "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   ;; The list of items is already filtered by this tag, no point in showing that it exists
                   (org-agenda-hide-tags-regexp "inbox")
                   ;; The header of this section should be "Inbox: clarify and organize"
                   (org-agenda-overriding-header "\nInbox: clarifier et organiser\n")))))))
  ;; I will be using add-to-list to shadow the previous values in org-capture-templates. The new versions of a task
  ;; and note will have an extra line linking to the context they were taken in (placeholder %a)
  (add-to-list 'org-capture-templates
               `("t" "Task" entry (file "inbox.org")
                 ,(string-join '("* TODO %?"
                                 ":PROPERTIES:"
                                 ":CREATED: %U"
                                 ":END:"
                                 "/Context:/ %a")
                               "\n"
                               )))
  (add-to-list 'org-capture-templates
               `("n" "Note" entry (file "inbox.org")
                 ,(string-join '("* %?"
                                 ":PROPERTIES:"
                                 ":CREATED: %U"
                                 ":END:"
                                 "/Context:/ %a")
                               "\n")))

  (require 'dash)

  (defvar org-refile-contexts "Contexts for `org-capture'.

Takes the same values as `org-capture-templates-contexts' except
that the first value of each entry should be a valid setting for
`org-refile-targets'.")

  (defun org-refile--get-context-targets ()
    "Get the refile targets for the current headline.

Returns the first set of targets in `org-refile-contexts' that
the current headline satisfies, or `org-refile-targets' if there
are no such."
    (or (car (-first (lambda (x)
                       (org-contextualize-validate-key
                        (car x)
                        org-refile-contexts))
                     org-refile-contexts
                     ))
        org-refile-targets)
    )

  (defun org-refile-with-context (&optional arg default-buffer rfloc msg)
    "Refile the headline to a location based on `org-refile-targets'.

Changes the set of available refile targets based on `org-refile-contexts', but is otherwise identical to `org-refile'"
    (interactive "P")
    (let ((org-refile-targets (org-refile--get-context-targets)))
      (org-refile arg default-buffer rfloc msg)
      ))

  (setq org-refile-contexts
        '((((("inbox.org") . (:regexp . "Projects"))) ;; example
           ((lambda () (string= (org-find-top-headline) "Inbox")))
           )
          ;; 6: Notes without a project go to notes.org
          (((("inbox.org") . (:regexp . "Notes")))
           ;;((lambda () (string= (org-element-property :my_type (org-element-at-point)) "NOTE")))
           ((lambda () ('regexp ":my_type:")))
           )
          ))

  (setq org-agenda-files (list "inbox.org" "agenda.org"
                               "notes.org" "projects.org"))
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
           ((tags "inbox"
                  ((org-agenda-prefix-format "  %?-12t% s")
                   ;; The header of this section should be "Inbox: clarify and organize"
                   (org-agenda-overriding-header "\nInbox: clarify and organize\n")))
            ;; Show tasks that can be started and their estimates, do not show inbox
            (todo "TODO"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "agenda.org" "notes.org" "projects.org"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-max-entries 5)
                   (org-agenda-overriding-header "\nTasks: Can be done\n")))
            ;; Show agenda around today
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))
            ;; Show tasks on hold
            (todo "HOLD"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks: on hold\n")))
            ;; Show tasks that are in progress
            (todo "STRT"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks: in progress\n")))

            ;; Show tasks that I completed today
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n"))))
           (
            ;; The list of items is already filtered by this tag, no point in showing that it exists
            (org-agenda-hide-tags-regexp "inbox")))
          ("G" "All tasks that can be done"
           ((todo "TODO"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "agenda.org" "notes.org" "projects.org")) (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks: Can be done\n")))
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))))

  ;; To show the agenda in a more compact manner and skip a time line when something is scheduled:

  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800 2000)
          "......"
          "----------------"))

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

  ;; reveal
  (load-library "ox-reveal")
  ;; Wrap <img> tag in a <figure> tag
  (setq org-html-html5-fancy t
        org-html-doctype "html5")
  )


;; customize org-mode's checkboxes with unicode symbols
(add-hook! org-mode
  (lambda ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" . "☐") prettify-symbols-alist)
    (push '("[X]" . "☑" ) prettify-symbols-alist)
    (push '("[-]" . "❍" ) prettify-symbols-alist)
    (prettify-symbols-mode)))

;; End of Org-mode Configuration
;; ---------------------------------------
