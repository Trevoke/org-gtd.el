          ;; agenda      The daily/weekly agenda.
          ;; agenda*     Appointments for current week/day.
          ;; todo        Entries with a specific TODO keyword, in all agenda files.
          ;; search      Entries containing search words entry or headline.
          ;; tags        Tags/Property/TODO match in all agenda files.
          ;; tags-todo   Tags/P/T match in all agenda files, TODO entries only.
          ;; todo-tree   Sparse tree of specific TODO keyword in *current* file.
          ;; tags-tree   Sparse tree with all tags matches in *current* file.
          ;; occur-tree  Occur sparse tree for *current* file.
          ;; alltodo     The global TODO list.
          ;; stuck       Stuck projects.
          ;; ...         A user-defined function.

;; match    What to search for:
;;           - a single keyword for TODO keyword searches
;;           - a tags/property/todo match expression for searches
;;           - a word search expression for text searches.
;;           - a regular expression for occur searches
;;           For all other commands, this should be the empty string.

;; org-get-repeat
;; org-entry-get

(let ((org-agenda-files '("~/my-life/orgnotes/gtd/"))
      (org-agenda-custom-commands
       '(("z" "My things"
          ((agenda ""
                   ((org-agenda-overriding-header "Calendar")
                    (org-agenda-show-current-time-in-grid t)
                    (org-agenda-span 1)
                    (org-agenda-entry-types '(:scheduled))
                    (org-agenda-include-deadlines t)))
           (tags "SCHEDULED=\"<-1d>\""
                 ((org-agenda-overriding-header "Yesterday's undone things")
                  (org-agenda-start-day "-1d")
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-span 1)
                                        ;(org-agenda-entry-types '(:timestamp))
                  ))
           (agenda ""
                   ((org-agenda-overriding-header "Unstarted scheduled things")
                    (org-agenda-span 1)
                    (org-agenda-entry-types '(:scheduled))
                    ))
           )
          ((org-agenda-todo-ignore-deadlines t) ;; something for sub-blocks
           ;(org-agenda-todo-ignore-scheduled t) ;; something for sub-blocks
           (org-agenda-todo-ignore-timestamp t) ;; ?
           (org-agenda-todo-ignore-with-date t) ;; ?
           (org-agenda-show-future-repeats t) ;; show me what this is
           (org-agenda-skip-scheduled-if-deadline-is-shown t) ;; focus on finishining
           (org-agenda-skip-timestamp-if-done t)
           (org-agenda-skip-timestamp-if-deadline-is-shown t)
           (org-agenda-include-inactive-timestamps nil)
           (org-agenda-show-outline-path t)
           (org-agenda-restore-windows-after-quit t))))))
  (org-agenda nil "z")
  (goto-char (point-min)))
