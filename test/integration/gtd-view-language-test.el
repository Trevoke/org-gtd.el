;;; gtd-view-language-test.el --- Integration tests for GTD view language -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for org-gtd view language that require file system access.
;; These tests use mock-fs via ogt-eunit-with-mock-gtd macro.
;;
;; Migrated from test/gtd-view-language-test.el (buttercup).
;; Pure unit tests are in test-eunit/unit/gtd-view-language-test.el.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Grouped Context Views

(deftest view-lang-int/grouped-context-views-with-files ()
  "Creates grouped context views scanning actual agenda files."
  ;; Create test data in GTD file
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Task at work :@work:\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (insert "* NEXT Task at home :@home:\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (basic-save-buffer))

  (let ((grouped-context-spec
         '((name . "Actions by Context")
           (view-type . tags-grouped)
           (group-by . context)
           (filters . ((todo . ("NEXT")))))))
    (assert-equal
     '(("@work" . ((tags "+@work+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@work")))))
       ("@home" . ((tags "+@home+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@home"))))))
     (org-gtd-view-lang--create-grouped-views grouped-context-spec))))

;;; Active Projects View - Project Detection

(deftest view-lang-int/detects-active-project ()
  "Detects projects with at least one active task."
  (with-current-buffer (org-gtd--default-file)
    (insert "* Projects
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
")
    (basic-save-buffer)
    (org-id-update-id-locations (list (buffer-file-name))))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Active Project")
    (org-back-to-heading t)
    (assert-true (org-gtd-projects--has-active-tasks-p (point-marker)))))

(deftest view-lang-int/detects-inactive-completed-project ()
  "Detects projects with all tasks completed as inactive."
  (with-current-buffer (org-gtd--default-file)
    (insert "* Projects
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
")
    (basic-save-buffer)
    (org-id-update-id-locations (list (buffer-file-name))))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Completed Project")
    (org-back-to-heading t)
    (assert-nil (org-gtd-projects--has-active-tasks-p (point-marker)))))

(deftest view-lang-int/handles-empty-projects ()
  "Handles projects with no tasks defined yet."
  (with-current-buffer (org-gtd--default-file)
    (insert "* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Empty Project
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-3
:END:
")
    (basic-save-buffer)
    (org-id-update-id-locations (list (buffer-file-name))))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Empty Project")
    (org-back-to-heading t)
    (assert-nil (org-gtd-projects--has-active-tasks-p (point-marker)))))

;;; Stuck Item Detection - Timestamp Validation

(deftest view-lang-int/detects-missing-timestamp ()
  "Detects items with missing ORG_GTD_TIMESTAMP."
  (with-current-buffer (org-gtd--default-file)
    (insert "* TODO Task without timestamp
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task without timestamp")
    (org-back-to-heading t)
    (let ((has-invalid-ts (not (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))
      (assert-true has-invalid-ts))))

(deftest view-lang-int/detects-invalid-timestamp-format ()
  "Detects items with invalid ORG_GTD_TIMESTAMP format."
  (with-current-buffer (org-gtd--default-file)
    (insert "* TODO Task with invalid timestamp
:PROPERTIES:
:ORG_GTD: Actions
:ORG_GTD_TIMESTAMP: not a valid date
:END:
")
    (basic-save-buffer))

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task with invalid timestamp")
    (org-back-to-heading t)
    (let ((ts-value (org-entry-get (point) "ORG_GTD_TIMESTAMP")))
      (assert-nil (org-string-match-p org-ts-regexp-both ts-value)))))

;;; org-gtd-view-show Integration

(deftest view-lang-int/view-show-displays-agenda ()
  "Displays an agenda view from a view spec."
  (with-current-buffer (org-gtd--default-file)
    (insert "* My Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":END:\n")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Test View")
     (type . project)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "My Project" (buffer-string)))))

(deftest view-lang-int/view-show-uses-spec-name ()
  "Uses the name from the view spec as the agenda title."
  (with-current-buffer (org-gtd--default-file)
    (insert "* My Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":END:\n")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "My Custom View Name")
     (type . project)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "My Custom View Name" (buffer-string)))))

(deftest view-lang-int/view-show-positions-cursor ()
  "Positions cursor at beginning of buffer."
  (with-current-buffer (org-gtd--default-file)
    (insert "* My Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":END:\n")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Test View")
     (type . project)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (with-current-buffer agenda-buffer
      (assert-equal (point-min) (point)))))

;;; Calendar-Day Block Skip Function Tests

(deftest view-lang-int/skip-function-keeps-calendar ()
  "Skip function returns nil for Calendar items (don't skip)."
  (with-current-buffer (org-gtd--default-file)
    (insert "* Calendar Event
:PROPERTIES:
:ORG_GTD: Calendar
:END:
")
    (basic-save-buffer)
    (goto-char (point-min))
    (search-forward "Calendar Event")
    (org-back-to-heading t)
    (assert-nil (org-gtd-view-lang--skip-unless-calendar-or-habit))))

(deftest view-lang-int/skip-function-keeps-habit ()
  "Skip function returns nil for Habit items (don't skip)."
  (with-current-buffer (org-gtd--default-file)
    (insert (format "* Daily Habit
:PROPERTIES:
:ORG_GTD: %s
:END:
" (org-gtd-type-org-gtd-value 'habit)))
    (basic-save-buffer)
    (goto-char (point-min))
    (search-forward "Daily Habit")
    (org-back-to-heading t)
    (assert-nil (org-gtd-view-lang--skip-unless-calendar-or-habit))))

(deftest view-lang-int/skip-function-skips-actions ()
  "Skip function skips non-Calendar non-Habit items."
  (with-current-buffer (org-gtd--default-file)
    (insert "* Some Action
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer)
    (goto-char (point-min))
    (search-forward "Some Action")
    (org-back-to-heading t)
    (assert-true (org-gtd-view-lang--skip-unless-calendar-or-habit))))

;;; Done Filter Integration Tests

(deftest view-lang-int/done-recent-displays-closed-items ()
  "Displays recently completed items using done=recent."
  (with-current-buffer (org-gtd--default-file)
    (insert "* DONE Completed Task
CLOSED: ")
    ;; Insert a recent closing timestamp
    (insert (format-time-string "[%Y-%m-%d %a %H:%M]\n"))
    (insert ":PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Recently Completed")
     (done . recent)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "Completed Task" (buffer-string)))))

;;; Tags Filter Integration Tests

(deftest view-lang-int/tags-filter-includes-matching-items ()
  "View with tags filter shows items with matching tags."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Work task :@work:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Home task :@home:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Errands task :@errands:
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Work and Home Tasks")
     (type . next-action)
     (tags . ("@work" "@home"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include work and home tasks
        (assert-match "Work task" content)
        (assert-match "Home task" content)
        ;; Should NOT include errands task
        (assert-nil (string-match-p "Errands task" content))))))

(deftest view-lang-int/tags-filter-excludes-non-matching-items ()
  "View with tags filter excludes items without matching tags."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Tagged task :@work:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Untagged task
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Work Tasks Only")
     (type . next-action)
     (tags . ("@work"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include tagged task
        (assert-match "Tagged task" content)
        ;; Should NOT include untagged task
        (assert-nil (string-match-p "Untagged task" content))))))

;;; Who Filter Integration Tests

(deftest view-lang-int/who-filter-shows-delegated-to-person ()
  "View with who filter shows items delegated to specified person."
  (with-current-buffer (org-gtd--default-file)
    (insert "* WAIT Task for Alice
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Alice
:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>
:END:
* WAIT Task for Bob
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Alice's Tasks")
     (type . delegated)
     (who . "Alice")))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include Alice's task
        (assert-match "Task for Alice" content)
        ;; Should NOT include Bob's task
        (refute-match "Task for Bob" content)))))

;;; Deadline Filter Integration Tests

(deftest view-lang-int/deadline-filter-shows-overdue-items ()
  "View with deadline=past filter shows items with overdue deadlines."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Overdue task
DEADLINE: <2020-01-01 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Future task
DEADLINE: <2099-12-31 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT No deadline task
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Overdue Tasks")
     (type . next-action)
     (deadline . past)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include overdue task
        (assert-match "Overdue task" content)
        ;; Should NOT include future or no-deadline tasks
        (refute-match "Future task" content)
        (refute-match "No deadline task" content)))))

;;; Scheduled Filter Integration Tests

(deftest view-lang-int/scheduled-filter-shows-past-scheduled ()
  "View with scheduled=past filter shows items scheduled in the past."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Past scheduled task
SCHEDULED: <2020-01-01 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Future scheduled task
SCHEDULED: <2099-12-31 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT No scheduled task
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Past Scheduled")
     (type . next-action)
     (scheduled . past)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include past scheduled task
        (assert-match "Past scheduled task" content)
        ;; Should NOT include future or no-scheduled tasks
        (refute-match "Future scheduled task" content)
        (refute-match "No scheduled task" content)))))

;;; Todo Keyword Filter Integration Tests

(deftest view-lang-int/todo-filter-shows-specific-keywords ()
  "View with todo filter shows only items with specified keywords."
  (with-current-buffer (org-gtd--default-file)
    (insert "* TODO Task in TODO state
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Task in NEXT state
:PROPERTIES:
:ORG_GTD: Actions
:END:
* WAIT Task in WAIT state
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "TODO and NEXT only")
     (type . next-action)
     (todo . ("TODO" "NEXT"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        (assert-match "Task in TODO state" content)
        (assert-match "Task in NEXT state" content)
        (assert-nil (string-match-p "Task in WAIT state" content))))))

(provide 'gtd-view-language-integration-test)

;;; gtd-view-language-test.el ends here
