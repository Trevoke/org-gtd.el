;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



;; Load test helpers
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "GTD View Language Translation"

 (before-each
  (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "GTD View Language Specification"

  (it "can define a simple GTD view for delegated items with past timestamps"
      (let ((gtd-view-spec
             '((name . "Missed Delegated Check-ins")
               (filters . ((category . delegated)
                           (timestamp . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Delegated")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "can define a GTD view for calendar items with past timestamps"
      (let ((gtd-view-spec
             '((name . "Missed Appointments")
               (filters . ((category . calendar)
                           (level . 2)
                           (timestamp . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")
                      (level 2)
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "can define a simple GTD view for action items"
      (let ((gtd-view-spec
             '((name . "All Actions")
               (filters . ((category . actions))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Actions")))))

  (it "translates timestamp today filter"
      (let ((gtd-view-spec
             '((name . "Today Items")
               (filters . ((type . calendar)
                           (timestamp . today))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Calendar")
                      (property-ts= "ORG_GTD_TIMESTAMP" ,(format-time-string "%Y-%m-%d"))))))

  (it "can define a GTD view for overdue deadlines"
      (let ((gtd-view-spec
             '((name . "Overdue Deadlines")
               (filters . ((deadline . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (deadline :to "today")
                      (not (done))))))

  (it "can define a GTD view for overdue scheduled items excluding habits"
      (let ((gtd-view-spec
             '((name . "Overdue Scheduled Items")
               (filters . ((scheduled . past)
                           (not-habit . t))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (scheduled :to "today")
                      (not (property "STYLE" "habit"))
                      (not (done)))))))

 (describe
  "GTD View Language Complex Scenarios"

  (it "can combine multiple time-based filters with category filters"
      (let ((gtd-view-spec
             '((name . "Complex View")
               (filters . ((category . projects)
                           (level . 2)
                           (deadline . past)
                           (scheduled . future))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Projects")
                      (level 2)
                      (deadline :to "today")
                      (scheduled :from "today")))))

  (it "can handle area-of-focus filtering"
      (let ((gtd-view-spec
             '((name . "Area Focus View")
               (filters . ((area-of-focus . "Work")
                           (todo . ("TODO" "NEXT")))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "CATEGORY" "Work")
                      (todo "TODO" "NEXT"))))))

 (describe
  "Error Handling and Validation"

  (it "rejects invalid filter specifications"
      (let ((invalid-gtd-view-spec
             '((name . "Invalid View")
               (filters . ((invalid-filter . "bad-value"))))))
        (expect (org-gtd-view-lang--translate-to-org-ql invalid-gtd-view-spec)
                :to-throw
                'error))))

 (describe
  "Integration with Existing System"

  (it "can generate equivalent org-ql query for current oops view patterns"
      ;; This tests that our new system can replicate existing functionality
      (let ((delegated-oops-spec
             '((name . "Missed check-ins on delegated items")
               (filters . ((category . delegated)
                           (timestamp . past))))))
        ;; The translated query should be functionally equivalent to:
        ;; items with ORG_GTD = "Delegated" and past timestamp
        (expect (org-gtd-view-lang--translate-to-org-ql delegated-oops-spec)
                :to-equal
                '(and (property "ORG_GTD" "Delegated")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done)))))))

 (describe
  "Incubated category filters"

  (it "can define a GTD view for incubated projects only"
      (let ((incubated-projects-spec
             '((name . "Incubated Projects")
               (filters . ((category . incubated-projects))))))
        (expect (org-gtd-view-lang--translate-to-org-ql incubated-projects-spec)
                :to-equal
                '(and (and (property "ORG_GTD" "Incubated")
                           (property "PREVIOUS_ORG_GTD" "Projects"))))))

  (it "can define a GTD view for all incubated items"
      (let ((incubated-spec
             '((name . "All Incubated")
               (filters . ((category . incubated))))))
        (expect (org-gtd-view-lang--translate-to-org-ql incubated-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated"))))))

 (describe
  "Agenda-Specific View Features"

  (it "can detect agenda view type and create agenda block instead of org-ql block"
      ;; Unit test for view-type handling
      (let ((agenda-view-spec
             '((name . "Simple Agenda View")
               (view-type . agenda))))
        (expect (org-gtd-view-lang--create-agenda-block agenda-view-spec)
                :to-equal
                '(agenda ""
                         ((org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t)
                          (org-agenda-skip-function 'org-gtd-skip-unless-in-progress))))))

  (it "can define a daily agenda view with span and NEXT actions"
      ;; This tests the org-gtd-engage equivalent functionality
      (let ((engage-view-spec
             '((name . "Today's GTD Engage View")
               (view-type . agenda)
               (agenda-span . 1)
               (show-habits . nil)
               (additional-blocks . ((todo . "NEXT"))))))
        (expect (org-gtd-view-lang--create-agenda-block engage-view-spec)
                :to-equal
                '(agenda ""
                         ((org-agenda-include-all-todo nil)
                          (org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t)
                          (org-agenda-skip-function 'org-gtd-skip-unless-in-progress))))

        ;; Should also generate the NEXT block
        (expect (org-gtd-view-lang--create-additional-blocks engage-view-spec)
                :to-equal
                '((todo "NEXT"
                        ((org-agenda-overriding-header "All actions ready to be executed.")))))))

  (describe
   "Tag Filtering Features"

   (it "can filter items by specific tags"
       ;; Unit test for tag filtering
       (let ((tag-view-spec
              '((name . "Context View")
                (filters . ((tags . ("@work" "@computer"))
                            (todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--translate-to-org-ql tag-view-spec)
                 :to-equal
                 '(and (tags "@work" "@computer")
                       (todo "NEXT")))))

   (it "can filter items by tag patterns using tags-match"
       ;; Unit test for tag pattern matching
       (let ((tag-match-spec
              '((name . "Context Pattern View")
                (filters . ((tags-match . "{^@}")
                            (todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--translate-to-org-ql tag-match-spec)
                 :to-equal
                 '(and (tags "{^@}")
                       (todo "NEXT")))))

   (it "can create simple grouped views by pre-defined contexts"
       ;; Unit test for basic grouping functionality
       (let ((grouped-spec
              '((name . "Actions by Context")
                (view-type . tags-grouped)
                (group-contexts . ("@work" "@home"))
                (filters . ((todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--create-grouped-views grouped-spec)
                 :to-equal
                 '((tags "+@work+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@work")))
                   (tags "+@home+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@home")))))))

   (it "can create grouped context views for org-gtd-engage-grouped-by-context"
       ;; Acceptance test for grouped context functionality
       ;; Set up test agenda files with context tags
       (let ((test-file (make-temp-file "test-agenda" nil ".org")))
         (with-temp-file test-file
           (insert "* NEXT Task at work :@work:\n")
           (insert "* NEXT Task at home :@home:\n"))
         (setq org-agenda-files (list test-file))

         (let ((grouped-context-spec
                '((name . "Actions by Context")
                  (view-type . tags-grouped)
                  (group-by . context)
                  (filters . ((tags-match . "{^@}")
                              (todo . ("NEXT")))))))
           (expect (org-gtd-view-lang--create-grouped-views grouped-context-spec)
                   :to-equal
                   '(("@work" . ((tags "+@work+TODO=\"NEXT\""
                                       ((org-agenda-overriding-header "@work")))))
                     ("@home" . ((tags "+@home+TODO=\"NEXT\""
                                       ((org-agenda-overriding-header "@home"))))))))

         ;; Clean up
         (delete-file test-file)
         (setq org-agenda-files nil)))))

 (describe
  "Active Projects View"

  (it "can define a GTD view for active projects only"
      ;; Unit test for active-projects filter translation
      (let ((active-projects-spec
             '((name . "Active Projects")
               (filters . ((category . active-projects))))))
        (expect (org-gtd-view-lang--translate-to-org-ql active-projects-spec)
                :to-equal
                '(and (and (property "ORG_GTD" "Projects")
                           (project-has-active-tasks))))))

  (it "detects projects with at least one active task"
      ;; Test org-gtd-projects--has-active-tasks-p returns true for active projects
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Active Project
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-1
:ORG_GTD_FIRST_TASKS: task-1
:END:
*** TODO First task
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-1
:ORG_GTD_PROJECT_IDS: proj-1
:END:
*** DONE Second task
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-2
:ORG_GTD_PROJECT_IDS: proj-1
:END:
""")
        (basic-save-buffer)
        ;; Register IDs with org-id system
        (org-id-update-id-locations (list (buffer-file-name))))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Active Project")
        (org-back-to-heading t)
        (expect (org-gtd-projects--has-active-tasks-p (point-marker))
                :to-be-truthy)))

  (it "detects projects with all tasks completed as inactive"
      ;; Test org-gtd-projects--has-active-tasks-p returns false for completed projects
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Completed Project
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-2
:ORG_GTD_FIRST_TASKS: task-3
:END:
*** DONE First task
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-3
:ORG_GTD_BLOCKS: task-4
:END:
*** DONE Second task
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-4
:ORG_GTD_DEPENDS_ON: task-3
:END:
""")
        (basic-save-buffer)
        ;; Register IDs with org-id system
        (org-id-update-id-locations (list (buffer-file-name))))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Completed Project")
        (org-back-to-heading t)
        (expect (org-gtd-projects--has-active-tasks-p (point-marker))
                :to-be nil)))

  (it "handles projects with no tasks"
      ;; Edge case: project heading with no tasks defined yet
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Empty Project
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-3
:END:
""")
        (basic-save-buffer)
        ;; Register IDs with org-id system
        (org-id-update-id-locations (list (buffer-file-name))))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Empty Project")
        (org-back-to-heading t)
        (expect (org-gtd-projects--has-active-tasks-p (point-marker))
                :to-be nil))))

 (describe
  "Completion and Closed Item Views"

  (it "can define a view for done items"
      (let ((done-spec
             '((name . "Completed Items")
               (filters . ((done . t))))))
        (expect (org-gtd-view-lang--translate-to-org-ql done-spec)
                :to-equal
                '(and (done)))))

  (it "can define a view for recently closed items"
      (let ((closed-spec
             '((name . "Recently Completed")
               (filters . ((closed . recent))))))
        (expect (org-gtd-view-lang--translate-to-org-ql closed-spec)
                :to-equal
                '(and (closed :from "-7d")))))

  (it "can define a view for items closed today"
      (let ((closed-today-spec
             '((name . "Completed Today")
               (filters . ((closed . today))))))
        (expect (org-gtd-view-lang--translate-to-org-ql closed-today-spec)
                :to-equal
                '(and (closed :on "today")))))

  (it "can define a view for completed projects"
      (let ((completed-projects-spec
             '((name . "Completed Projects")
               (filters . ((category . completed-projects))))))
        (expect (org-gtd-view-lang--translate-to-org-ql completed-projects-spec)
                :to-equal
                '(and (and (property "ORG_GTD" "Projects")
                           (level 2)
                           (not (project-has-active-tasks)))))))

  (it "can combine done and closed filters"
      (let ((recent-completed-spec
             '((name . "Recently Completed Items")
               (filters . ((done . t)
                           (closed . past-week))))))
        (expect (org-gtd-view-lang--translate-to-org-ql recent-completed-spec)
                :to-equal
                '(and (done)
                      (closed :from "-1w"))))))

 (describe
  "Project type filters without level constraints"

  (it "translates stuck-project type without level constraint"
      (let ((gtd-view-spec
             '((name . "Stuck Projects")
               (filters . ((type . stuck-project))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (and (property "ORG_GTD" "Projects")
                           (project-is-stuck))))))

  (it "translates completed-project type without level constraint"
      (let ((gtd-view-spec
             '((name . "Completed Projects")
               (filters . ((type . completed-project))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (and (property "ORG_GTD" "Projects")
                           (not (project-has-active-tasks))))))))

 (describe
  "Flat filter structure support"

  (it "supports flat filter structure without filters wrapper"
      (let ((gtd-view-spec
             '((name . "Stuck Projects")
               (type . stuck-project))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (and (property "ORG_GTD" "Projects")
                           (project-is-stuck))))))

  (it "supports flat filter structure with multiple filters"
      (let ((gtd-view-spec
             '((name . "Next Actions in Work")
               (type . next-action)
               (area-of-focus . "Work"))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Actions")
                      (todo ,(org-gtd-keywords--next))
                      (property "CATEGORY" "Work")))))

  (it "still supports legacy filters wrapper format"
      (let ((gtd-view-spec
             '((name . "Stuck Projects")
               (filters . ((type . stuck-project))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (and (property "ORG_GTD" "Projects")
                           (project-is-stuck)))))))

 (describe
  "Invalid Timestamp and Stuck Item Detection"

  (it "can define a view for items with invalid timestamps"
      (let ((invalid-ts-spec
             '((name . "Items with Invalid Timestamps")
               (filters . ((invalid-timestamp . t))))))
        (expect (org-gtd-view-lang--translate-to-org-ql invalid-ts-spec)
                :to-equal
                '(and (property-invalid-timestamp "ORG_GTD_TIMESTAMP")))))

  (it "can define a view for stuck calendar items"
      (let ((stuck-calendar-spec
             '((name . "Stuck Calendar Items")
               (filters . ((category . calendar)
                           (invalid-timestamp . t))))))
        (expect (org-gtd-view-lang--translate-to-org-ql stuck-calendar-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")
                      (property-invalid-timestamp "ORG_GTD_TIMESTAMP")))))

  (it "detects items with missing ORG_GTD_TIMESTAMP"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* TODO Task without timestamp
:PROPERTIES:
:ORG_GTD: Actions
:END:
""")
        (basic-save-buffer))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task without timestamp")
        (org-back-to-heading t)
        (let ((has-invalid-ts (not (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))
          (expect has-invalid-ts :to-be-truthy))))

  (it "detects items with invalid ORG_GTD_TIMESTAMP format"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* TODO Task with invalid timestamp
:PROPERTIES:
:ORG_GTD: Actions
:ORG_GTD_TIMESTAMP: not a valid date
:END:
""")
        (basic-save-buffer))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task with invalid timestamp")
        (org-back-to-heading t)
        (let ((ts-value (org-entry-get (point) "ORG_GTD_TIMESTAMP")))
          (expect (org-string-match-p org-ts-regexp-both ts-value)
                  :to-be nil))))))

(describe "org-gtd-view-show"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "displays an agenda view from a view spec"
    ;; Create some test data
    (with-current-buffer (org-gtd--default-file)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (basic-save-buffer))

    ;; Call org-gtd-view-show
    (org-gtd-view-show
     '((name . "Test View")
       (filters . ((category . projects)))))

    ;; Verify agenda buffer was created
    (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
      (expect agenda-buffer :to-be-truthy)
      (with-current-buffer agenda-buffer
        (expect (buffer-string) :to-match "My Project"))))

  (it "uses the name from the view spec as the agenda title"
    ;; Create test data
    (with-current-buffer (org-gtd--default-file)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (basic-save-buffer))

    ;; Call with specific name
    (org-gtd-view-show
     '((name . "My Custom View Name")
       (filters . ((category . projects)))))

    ;; Verify the name appears in the buffer
    (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
      (expect agenda-buffer :to-be-truthy)
      (with-current-buffer agenda-buffer
        (expect (buffer-string) :to-match "My Custom View Name"))))

  (it "positions cursor at beginning of buffer"
    ;; Create test data
    (with-current-buffer (org-gtd--default-file)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (basic-save-buffer))

    ;; Call org-gtd-view-show
    (org-gtd-view-show
     '((name . "Test View")
       (filters . ((category . projects)))))

    ;; Verify cursor is at beginning
    (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
      (with-current-buffer agenda-buffer
        (expect (point) :to-equal (point-min))))))

(describe
 "Type-based filters using org-gtd-types"

 (before-each
  (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "type filter"

  (it "translates (type . next-action) to include NEXT keyword"
      (let ((view-spec
             '((name . "Next Actions")
               (filters . ((type . next-action))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Actions")
                      (todo ,(org-gtd-keywords--next))))))

  (it "translates (type . delegated) to include WAIT keyword"
      (let ((view-spec
             '((name . "Delegated Items")
               (filters . ((type . delegated))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Delegated")
                      (todo ,(org-gtd-keywords--wait))))))

  (it "translates (type . calendar) to ORG_GTD=Calendar query"
      (let ((view-spec
             '((name . "Calendar Items")
               (filters . ((type . calendar))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")))))

  (it "translates (type . incubated) to ORG_GTD=Incubated query"
      (let ((view-spec
             '((name . "Incubated Items")
               (filters . ((type . incubated))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")))))

  (it "translates (type . project) to ORG_GTD=Projects query"
      (let ((view-spec
             '((name . "Projects")
               (filters . ((type . project))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Projects")))))

  (it "translates (type . reference) to ORG_GTD=Reference query"
      (let ((view-spec
             '((name . "Reference Items")
               (filters . ((type . reference))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Reference")))))

  (it "translates (type . trash) to ORG_GTD=Trash query"
      (let ((view-spec
             '((name . "Trash")
               (filters . ((type . trash))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Trash")))))

  (it "translates (type . quick-action) to ORG_GTD=Quick query"
      (let ((view-spec
             '((name . "Quick Actions")
               (filters . ((type . quick-action))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Quick")))))

  (it "translates (type . habit) to ORG_GTD=Habit query"
      (let ((view-spec
             '((name . "Habits")
               (filters . ((type . habit))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Habit")))))

  (it "signals error for unknown type"
      (let ((view-spec
             '((name . "Unknown")
               (filters . ((type . nonexistent-type))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-throw 'user-error))))

 (describe
  "semantic :when property filter"

  (it "translates (:when . past) with delegated type to ORG_GTD_TIMESTAMP"
      (let ((view-spec
             '((name . "Missed Delegated")
               (filters . ((type . delegated)
                           (:when . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Delegated")
                      (todo ,(org-gtd-keywords--wait))
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "translates (:when . past) with calendar type to ORG_GTD_TIMESTAMP"
      (let ((view-spec
             '((name . "Missed Calendar")
               (filters . ((type . calendar)
                           (:when . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "translates (:when . past) with incubated type to ORG_GTD_TIMESTAMP"
      (let ((view-spec
             '((name . "Past Incubated")
               (filters . ((type . incubated)
                           (:when . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "translates (:when . past) with habit type to SCHEDULED"
      (let ((view-spec
             '((name . "Past Habits")
               (filters . ((type . habit)
                           (:when . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Habit")
                      (property-ts< "SCHEDULED" "today")
                      (not (done))))))

  (it "translates (:when . future) with calendar type"
      (let ((view-spec
             '((name . "Future Calendar")
               (filters . ((type . calendar)
                           (:when . future))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")
                      (property-ts> "ORG_GTD_TIMESTAMP" "today"))))))

 (describe
  "when filter using semantic property lookup"

  (it "translates when filter using semantic property lookup"
      (let ((gtd-view-spec
             '((name . "Delegated Due Today")
               (type . delegated)
               (when . today))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Delegated")
                      (todo ,(org-gtd-keywords--wait))
                      (property-ts= "ORG_GTD_TIMESTAMP" ,(format-time-string "%Y-%m-%d"))))))

  (it "translates (when . past) with delegated type"
      (let ((gtd-view-spec
             '((name . "Missed Delegated")
               (type . delegated)
               (when . past))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                `(and (property "ORG_GTD" "Delegated")
                      (todo ,(org-gtd-keywords--wait))
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "translates (when . future) with incubated type"
      (let ((gtd-view-spec
             '((name . "Future Incubated")
               (type . incubated)
               (when . future))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")
                      (property-ts> "ORG_GTD_TIMESTAMP" "today")))))

  (it "requires type filter to be present"
      (let ((gtd-view-spec
             '((name . "Invalid When")
               (when . today))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-throw 'user-error)))

  (it "errors when type doesn't have :when property"
      (let ((gtd-view-spec
             '((name . "Invalid Type")
               (type . next-action)
               (when . today))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-throw 'user-error))))

 (describe
  "previous-type filter for incubated items"

  (it "translates (previous-type . delegated) to PREVIOUS_ORG_GTD=Delegated"
      (let ((view-spec
             '((name . "Incubated Delegated")
               (filters . ((type . incubated)
                           (previous-type . delegated))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")
                      (property "PREVIOUS_ORG_GTD" "Delegated")))))

  (it "translates (previous-type . next-action) to PREVIOUS_ORG_GTD=Actions"
      (let ((view-spec
             '((name . "Incubated Actions")
               (filters . ((type . incubated)
                           (previous-type . next-action))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")
                      (property "PREVIOUS_ORG_GTD" "Actions")))))

  (it "translates (previous-type . project) to PREVIOUS_ORG_GTD=Projects"
      (let ((view-spec
             '((name . "Incubated Projects")
               (filters . ((type . incubated)
                           (previous-type . project))))))
        (expect (org-gtd-view-lang--translate-to-org-ql view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Incubated")
                      (property "PREVIOUS_ORG_GTD" "Projects"))))))

 (describe
  "Multi-block view support"

  (it "creates agenda blocks from multi-block view spec"
      (let ((multi-block-spec
             '((name . "Multi View")
               (blocks . (((name . "Block 1")
                           (type . next-action))
                          ((name . "Block 2")
                           (type . delegated)))))))
        (let ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec))))
          (expect (length (caddr (car commands))) :to-equal 2))))

  (it "processes each block separately in multi-block spec"
      (let ((multi-block-spec
             '((name . "Multi View")
               (blocks . (((name . "Next Actions Block")
                           (type . next-action))
                          ((name . "Delegated Block")
                           (type . delegated)))))))
        (let* ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec)))
               (blocks (caddr (car commands))))
          ;; First block should be for next-action
          (expect (caddr (car blocks)) :to-equal
                  '((org-ql-block-header "Next Actions Block")))
          ;; Second block should be for delegated
          (expect (caddr (cadr blocks)) :to-equal
                  '((org-ql-block-header "Delegated Block"))))))

  (it "still supports single-block specs without blocks key"
      (let ((single-block-spec
             '((name . "Single View")
               (type . next-action))))
        (let ((commands (org-gtd-view-lang--create-custom-commands (list single-block-spec))))
          (expect (length (caddr (car commands))) :to-equal 1))))))
