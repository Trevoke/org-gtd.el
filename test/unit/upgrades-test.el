;;; upgrades-test.el --- Tests for org-gtd upgrades -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd upgrade functionality.
;;
;; Test Coverage:
;; - v3 upgrades (4 tests): calendar, delegated, incubate, habits migration
;; - Property-based project system migration (4 tests)
;; - Complete v3 to v4 user upgrade path (6 tests)
;;
;; Migrated from test/upgrades-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; v3 Upgrade Tests

(deftest upgrade-v3/moves-calendar-items-away-from-scheduled ()
  "Moves calendar items away from using SCHEDULED."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Calendared
:PROPERTIES:
:ORG_GTD:  Calendar
:END:
** Twitch Affiliate anniversary
:PROPERTIES:
:LAST_REPEAT: [2023-01-03 Tue 21:59]
:END:
<2023-12-11 Mon +1y>

** Workout                                                        :@workout:
SCHEDULED: <2023-01-04 Wed .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2023-01-03 Tue 21:58]
:CATEGORY: Health
:Effort:   30min
:END:

I think the text goes here

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
")
    (basic-save-buffer))
  (org-gtd-upgrades-calendar-items-to-v3)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Twitch")
    (assert-nil (org-entry-get (point) org-gtd-timestamp))
    (search-forward "Workout")
    (assert-nil (org-entry-get (point) org-gtd-timestamp))
    (search-forward "memories")
    (assert-equal "<2023-04-03>" (org-entry-get (point) org-gtd-timestamp))))

(deftest upgrade-v3/moves-delegated-items-away-from-scheduled ()
  "Moves delegated items away from using SCHEDULED."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Incubated
:PROPERTIES:
:ORG_GTD:  Actions
:END:

** NEXT take a nice nap

** WAIT record meaningful memories
SCHEDULED: <2023-04-03>
:PROPERTIES:
:DELEGATED_TO: Someone
:END:

Do that thing.
")
    (basic-save-buffer))
  (org-gtd-upgrades-delegated-items-to-v3)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "nice nap")
    (assert-nil (org-entry-get (point) org-gtd-timestamp))
    (search-forward "memories")
    (assert-equal "<2023-04-03>" (org-entry-get (point) org-gtd-timestamp))))

(deftest upgrade-v3/moves-incubate-items-away-from-scheduled ()
  "Moves incubate items away from using SCHEDULED."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Incubated
:PROPERTIES:
:ORG_GTD:  Incubated
:END:

** take a nice nap

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
")
    (basic-save-buffer))
  (org-gtd-upgrades-incubated-items-to-v3)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "nice nap")
    (assert-nil (org-entry-get (point) org-gtd-timestamp))
    (assert-equal org-gtd-tickler (org-entry-get (point) "ORG_GTD" t))
    (search-forward "memories")
    (assert-equal org-gtd-tickler (org-entry-get (point) "ORG_GTD" t))
    (assert-equal "<2023-04-03>" (org-entry-get (point) org-gtd-timestamp))))

(deftest upgrade-v3/moves-habits-to-their-own-tree ()
  "Moves habits to their own tree."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Calendared
:PROPERTIES:
:ORG_GTD:  Calendar
:END:
** Twitch Affiliate anniversary
:PROPERTIES:
:LAST_REPEAT: [2023-01-03 Tue 21:59]
:END:
<2023-12-11 Mon +1y>

** Workout                                                        :@workout:
SCHEDULED: <2023-01-04 Wed .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2023-01-03 Tue 21:58]
:CATEGORY: Health
:Effort:   30min
:END:

I think the text goes here

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
")
    (basic-save-buffer))
  (org-gtd-upgrades-habits-to-v3)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Workout")
    (assert-equal org-gtd-habit (org-entry-get nil "ORG_GTD" t))

    (goto-char (point-min))
    (search-forward "Twitch Affiliate")
    (assert-equal org-gtd-calendar (org-entry-get nil "ORG_GTD" t))

    (goto-char (point-min))
    (search-forward "record meaningful")
    (assert-equal org-gtd-calendar (org-entry-get nil "ORG_GTD" t))))

;;; Property-based Project System Migration Tests

(deftest upgrade-property/removes-org-gtd-from-level-1-adds-to-level-2 ()
  "Removes ORG_GTD from level 1 category headings while adding to level 2 children."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Design database
")
    (basic-save-buffer))

  ;; Run property addition (Step 1 only)
  (org-gtd-upgrade--add-org-gtd-properties)

  (with-current-buffer (org-gtd--default-file)
    ;; Verify level 1 heading does NOT have ORG_GTD
    (goto-char (point-min))
    (re-search-forward "^\\* Projects$")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Verify level 2 heading HAS ORG_GTD="Projects"
    (goto-char (point-min))
    (search-forward "Build a webapp")
    (org-back-to-heading t)
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))

    ;; Verify level 3 heading HAS ORG_GTD="Actions"
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-property/handles-non-projects-categories-correctly ()
  "Handles non-Projects categories correctly (e.g., Actions)."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
** Call mom
")
    (basic-save-buffer))

  ;; Run property addition (Step 1 only)
  (org-gtd-upgrade--add-org-gtd-properties)

  (with-current-buffer (org-gtd--default-file)
    ;; Verify level 1 Actions heading does NOT have ORG_GTD
    (goto-char (point-min))
    (re-search-forward "^\\* Actions$")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Verify level 2 item HAS ORG_GTD="Actions"
    (goto-char (point-min))
    (search-forward "Call mom")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-property/migrates-level-based-to-property-based ()
  "Migrates level-based projects to property-based system."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Set up development environment
*** Design the user interface
*** Implement backend API
**** Create user authentication
**** Set up database
** Write documentation
*** Create user manual
*** Document API endpoints
")
    (basic-save-buffer))

  ;; Run the migration function (Step 1 only - property addition)
  (org-gtd-upgrade--add-org-gtd-properties)

  (with-current-buffer (org-gtd--default-file)
    ;; Verify project headings have correct ORG_GTD property
    (goto-char (point-min))
    (search-forward "Build a webapp")
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))

    (goto-char (point-min))
    (search-forward "Write documentation")
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))

    ;; Verify project tasks have ORG_GTD=Actions property
    (goto-char (point-min))
    (search-forward "Set up development environment")
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

    (goto-char (point-min))
    (search-forward "Create user authentication")
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

    (goto-char (point-min))
    (search-forward "Set up database")
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-property/allows-property-queries-regardless-of-level ()
  "Allows project tasks to be collected via property queries regardless of level."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-webapp
:ORG_GTD_FIRST_TASKS: task-setup task-design
:END:
*** Set up development environment
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-setup
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
*** Design the user interface
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-design
:ORG_GTD_BLOCKS: task-wireframes task-colors
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
**** Create wireframes
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-wireframes
:ORG_GTD_DEPENDS_ON: task-design
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
**** Choose color scheme
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-colors
:ORG_GTD_DEPENDS_ON: task-design
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
")
    (basic-save-buffer))

  ;; Test that we can find all project tasks via property query
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Build a webapp")
    (org-back-to-heading t)
    (let ((tasks (org-gtd-dependencies-collect-project-tasks (point-marker))))
      ;; Should find all 4 tasks regardless of their level
      (assert-equal 4 (length tasks))
      ;; Verify we can access all tasks by their properties
      (let ((task-ids '()))
        (dolist (task tasks)
          (save-excursion
            (goto-char task)
            (let ((id (org-entry-get (point) "ID"))
                  (org-gtd (org-entry-get (point) "ORG_GTD")))
              (assert-equal "Actions" org-gtd)
              (push id task-ids))))
        ;; All task IDs should be present
        (assert-true (member "task-setup" task-ids))
        (assert-true (member "task-design" task-ids))
        (assert-true (member "task-wireframes" task-ids))
        (assert-true (member "task-colors" task-ids))))))

;;; Complete v3 to v4 User Upgrade Path Tests

(deftest upgrade-v3-to-v4/removes-org-gtd-from-level-1-categories ()
  "Removes ORG_GTD property from level 1 category headings."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Design database
:PROPERTIES:
:ID: task-1
:END:

* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
** Call mom
:PROPERTIES:
:ID: action-1
:END:
")
    (basic-save-buffer))

  ;; Run full migration (stub yes-or-no-p to return t)
  (with-stub yes-or-no-p t
    (org-gtd-upgrade-v3-to-v4))

  ;; Verify level 1 category headings do NOT have ORG_GTD property
  (with-current-buffer (org-gtd--default-file)
    ;; Check Projects category heading
    (goto-char (point-min))
    (re-search-forward "^\\* Projects$")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Check Actions category heading
    (goto-char (point-min))
    (re-search-forward "^\\* Actions$")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Verify level 2 items DO have ORG_GTD property
    (goto-char (point-min))
    (search-forward "Build a webapp")
    (org-back-to-heading t)
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))

    (goto-char (point-min))
    (search-forward "Call mom")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

    ;; Verify level 3 under Projects has ORG_GTD="Actions"
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-v3-to-v4/converts-sequential-to-dependency-based ()
  "Full migration converts v3 sequential projects to v4 dependency-based."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
")
    (basic-save-buffer))

  ;; Verify v3 state
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

  ;; Run full migration
  (with-stub yes-or-no-p t
    (org-gtd-upgrade-v3-to-v4))

  ;; Verify v4 state
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-true blocks)
      (assert-true (member "task-2" blocks)))
    (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO"))))

(deftest upgrade-v3-to-v4/preserves-user-data ()
  "Preserves user data during migration."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Project
*** TODO Task                                             :urgent:
:PROPERTIES:
:ID: t1
:CUSTOM: value
:END:
Content here.
")
    (basic-save-buffer))

  (let (orig-tags orig-custom)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      ;; Search for the unique TODO heading, not just "Task"
      ;; (After migration, "Task" appears in :ORG_GTD_FIRST_TASKS: property too)
      (search-forward "TODO Task")
      (org-back-to-heading t)
      (setq orig-tags (org-get-tags nil t)
            orig-custom (org-entry-get (point) "CUSTOM")))

    (with-stub yes-or-no-p t
      (org-gtd-upgrade-v3-to-v4))

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      ;; After migration, TODO becomes NEXT, so search for "NEXT Task"
      (search-forward "NEXT Task")
      (org-back-to-heading t)
      (assert-equal orig-tags (org-get-tags nil t))
      (assert-equal orig-custom (org-entry-get (point) "CUSTOM")))))

(deftest upgrade-v3-to-v4/respects-backup-confirmation ()
  "Respects backup confirmation."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Proj
*** TODO Task
:PROPERTIES:
:ID: t1
:END:
")
    (basic-save-buffer))

  ;; Stub yes-or-no-p to return nil (user declined)
  (with-stub yes-or-no-p nil
    (org-gtd-upgrade-v3-to-v4))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Proj$")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-v3-to-v4/adds-project-ids-to-all-tasks ()
  "Adds ORG_GTD_PROJECT_IDS property to all project tasks."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ID: proj-webapp-123
:END:
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
**** TODO Create endpoint
:PROPERTIES:
:ID: task-3
:END:
** Write documentation
:PROPERTIES:
:ID: proj-docs-456
:END:
*** TODO Create user manual
:PROPERTIES:
:ID: task-4
:END:
")
    (basic-save-buffer))

  ;; Run full migration
  (with-stub yes-or-no-p t
    (org-gtd-upgrade-v3-to-v4))

  ;; Verify all tasks have ORG_GTD_PROJECT_IDS set to their project's ID
  (with-current-buffer (org-gtd--default-file)
    ;; Check first project's tasks
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-true project-ids)
      (assert-true (member "proj-webapp-123" project-ids)))

    (goto-char (point-min))
    (search-forward "Implement API")
    (org-back-to-heading t)
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-true project-ids)
      (assert-true (member "proj-webapp-123" project-ids)))

    ;; Check nested task
    (goto-char (point-min))
    (search-forward "Create endpoint")
    (org-back-to-heading t)
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-true project-ids)
      (assert-true (member "proj-webapp-123" project-ids)))

    ;; Check second project's task
    (goto-char (point-min))
    (search-forward "Create user manual")
    (org-back-to-heading t)
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-true project-ids)
      (assert-true (member "proj-docs-456" project-ids)))))

(deftest upgrade-v3-to-v4/adds-trigger-to-all-tasks ()
  "Adds TRIGGER property to all project tasks."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ID: proj-webapp-123
:END:
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
**** TODO Create endpoint
:PROPERTIES:
:ID: task-3
:END:
** Write documentation
:PROPERTIES:
:ID: proj-docs-456
:END:
*** TODO Create user manual
:PROPERTIES:
:ID: task-4
:END:
")
    (basic-save-buffer))

  ;; Run full migration
  (with-stub yes-or-no-p t
    (org-gtd-upgrade-v3-to-v4))

  ;; Verify all tasks have TRIGGER property set
  (with-current-buffer (org-gtd--default-file)
    ;; Check first project's tasks
    (goto-char (point-min))
    (search-forward "Design database")
    (org-back-to-heading t)
    (assert-equal "self org-gtd-update-project-after-task-done!" (org-entry-get (point) "TRIGGER"))

    (goto-char (point-min))
    (search-forward "Implement API")
    (org-back-to-heading t)
    (assert-equal "self org-gtd-update-project-after-task-done!" (org-entry-get (point) "TRIGGER"))

    ;; Check nested task
    (goto-char (point-min))
    (search-forward "Create endpoint")
    (org-back-to-heading t)
    (assert-equal "self org-gtd-update-project-after-task-done!" (org-entry-get (point) "TRIGGER"))

    ;; Check second project's task
    (goto-char (point-min))
    (search-forward "Create user manual")
    (org-back-to-heading t)
    (assert-equal "self org-gtd-update-project-after-task-done!" (org-entry-get (point) "TRIGGER"))))

(deftest upgrade-v3-to-v4/migrates-incubated-with-timestamp-to-tickler ()
  "Migrates Incubated items with ORG_GTD_TIMESTAMP to Tickler."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Incubated
:PROPERTIES:
:ORG_GTD: Incubated
:END:
** Check on investment
:PROPERTIES:
:ORG_GTD_TIMESTAMP: <2024-06-01>
:END:
** Someday learn piano
:PROPERTIES:
:END:
")
    (basic-save-buffer))

  ;; Run the incubated migration
  (org-gtd-upgrade--migrate-incubated-items)

  (with-current-buffer (org-gtd--default-file)
    ;; Level 1 heading should have ORG_GTD_REFILE (first child has timestamp)
    (goto-char (point-min))
    (re-search-forward "^\\* Incubated$")
    (org-back-to-heading t)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD_REFILE"))
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Item with timestamp should be Tickler
    (goto-char (point-min))
    (search-forward "Check on investment")
    (org-back-to-heading t)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))

    ;; Item without timestamp should be Someday
    (goto-char (point-min))
    (search-forward "Someday learn piano")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-v3-to-v4/migrates-incubated-first-child-no-timestamp ()
  "Sets ORG_GTD_REFILE to Someday when first child has no timestamp."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Incubated
:PROPERTIES:
:ORG_GTD: Incubated
:END:
** Someday learn guitar
:PROPERTIES:
:END:
** Check on savings
:PROPERTIES:
:ORG_GTD_TIMESTAMP: <2024-12-01>
:END:
")
    (basic-save-buffer))

  ;; Run the incubated migration
  (org-gtd-upgrade--migrate-incubated-items)

  (with-current-buffer (org-gtd--default-file)
    ;; Level 1 heading should have ORG_GTD_REFILE: Someday (first child has no timestamp)
    (goto-char (point-min))
    (re-search-forward "^\\* Incubated$")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD_REFILE"))
    (assert-nil (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-v3-to-v4/fixes-habits-plural-to-singular ()
  "Fixes habits with ORG_GTD=Habits (plural) to ORG_GTD=Habit (singular).
The v2->v3 migration set ORG_GTD to the constant `org-gtd-habit' which is
\"Habits\" (plural), but the v4 type system expects \"Habit\" (singular)."
  (with-current-buffer (org-gtd--default-file)
    (insert "
* Habits
:PROPERTIES:
:ORG_GTD:  Habits
:END:
** NEXT Morning meditation
SCHEDULED: <2023-12-23 Sat 07:20 ++1d>
:PROPERTIES:
:STYLE:    habit
:ORG_GTD:  Habits
:END:
")
    (basic-save-buffer))

  ;; Run the habit migration
  (org-gtd-upgrade--migrate-habits)

  ;; Verify habit now has singular "Habit"
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Morning meditation")
    (org-back-to-heading t)
    (assert-equal "Habit" (org-entry-get (point) "ORG_GTD"))))

(deftest upgrade-v3-to-v4/adds-habit-property-when-missing ()
  "Adds ORG_GTD=Habit to habits that have STYLE=habit but no ORG_GTD property."
  (with-current-buffer (org-gtd--default-file)
    (insert "
** NEXT Evening exercise
SCHEDULED: <2023-12-23 Sat 19:00 ++1d>
:PROPERTIES:
:STYLE:    habit
:END:
")
    (basic-save-buffer))

  ;; Run the habit migration
  (org-gtd-upgrade--migrate-habits)

  ;; Verify habit now has ORG_GTD="Habit"
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Evening exercise")
    (org-back-to-heading t)
    (assert-equal "Habit" (org-entry-get (point) "ORG_GTD"))))

(provide 'upgrades-test)

;;; upgrades-test.el ends here
