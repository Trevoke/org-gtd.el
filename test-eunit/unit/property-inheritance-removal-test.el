;;; property-inheritance-removal-test.el --- Tests for v4 property inheritance removal -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; These tests verify that v4 items have direct ORG_GTD properties
;; and do NOT need property inheritance to function correctly.
;;
;; The `t` argument in `org-entry-get` calls should be removable.
;;
;; Test Coverage:
;; - Single actions have direct ORG_GTD property (1 test)
;; - Calendar items have direct ORG_GTD property (1 test)
;; - Project headings have direct ORG_GTD property (1 test)
;; - Project tasks have direct ORG_GTD property (1 test)
;; - Tickler items have direct ORG_GTD property (1 test)
;; - Delegated items have direct ORG_GTD property (1 test)
;; - Habits have direct ORG_GTD property (1 test)
;; - Areas-of-focus validation works without inheritance (1 test)
;; - Skip functions work without inheritance (1 test)
;; - Refile target verification works without inheritance (1 test)
;; - Agenda CATEGORY lookup for project tasks (1 test)
;;
;; Migrated from test/property-inheritance-removal-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; ORG_GTD property is directly on items (not inherited)

(deftest property-inheritance/single-action-has-direct-property ()
  "Single actions have direct ORG_GTD property."
  (create-single-action "Test action")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test action")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/calendar-item-has-direct-property ()
  "Calendar items have direct ORG_GTD property."
  (create-calendar-item "Test calendar" (calendar-current-date))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test calendar")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/project-heading-has-direct-property ()
  "Project headings have direct ORG_GTD property."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/project-task-has-direct-property ()
  "Project tasks have direct ORG_GTD property."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/tickler-item-has-direct-property ()
  "Tickler items have direct ORG_GTD property."
  (create-deferred-item "Test tickler" (calendar-current-date))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test tickler")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/delegated-item-has-direct-property ()
  "Delegated items have direct ORG_GTD property."
  (create-delegated-item "Test delegate" "John Doe" (calendar-current-date))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test delegate")
    ;; WITHOUT inheritance (no t argument)
    (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD"))))

(deftest property-inheritance/habit-has-direct-property ()
  "Habits have direct ORG_GTD property."
  (create-habit "Test habit" ".+1d")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test habit")
    ;; WITHOUT inheritance (no t argument)
    ;; Note: Config sets "Habit" (singular), not "Habits"
    (assert-equal "Habit" (org-entry-get (point) "ORG_GTD"))))

;;; Areas-of-focus validation works without inheritance

(deftest property-inheritance/areas-of-focus-validation-works ()
  "Can validate item has ORG_GTD without t argument."
  (create-single-action "Test action")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test action")
    ;; This is the check in org-gtd-areas-of-focus.el:71
    ;; WITHOUT inheritance should still work
    (assert-true (org-entry-get nil "ORG_GTD"))))

;;; Skip functions work without inheritance

(deftest property-inheritance/calendar-skip-works ()
  "Calendar skip works without inheritance."
  (create-calendar-item "Test calendar" (calendar-current-date))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test calendar")
    ;; org-gtd-skip.el:101 - Check ORG_GTD directly without inheritance
    (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))))

;;; Refile target verification works without inheritance

(deftest property-inheritance/project-heading-verification-works ()
  "Project heading verification works without inheritance."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    ;; org-gtd-refile.el:103 - Check ORG_GTD directly without inheritance
    (assert-equal "Projects" (org-entry-get nil "ORG_GTD"))))

;;; Agenda CATEGORY lookup for project tasks

(deftest property-inheritance/project-task-category-lookup ()
  "Project task can look up CATEGORY from project heading via ID."
  ;; Create a project and set CATEGORY on it
  (create-project "Test project with category")
  (with-current-buffer (org-gtd--default-file)
    ;; Set CATEGORY on project heading
    (goto-char (point-min))
    (search-forward "Test project with category")
    (org-entry-put (point) "CATEGORY" "Work")
    (basic-save-buffer)

    ;; Now check that a project task can get the CATEGORY via lookup
    (goto-char (point-min))
    (search-forward "Task 1")
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      ;; Verify the task has project IDs
      (assert-true project-ids)
      ;; Look up project's CATEGORY via the new helper function
      (let ((category (org-gtd-agenda-get-category-for-task)))
        (assert-equal "Work" category)))))

(provide 'property-inheritance-removal-test)

;;; property-inheritance-removal-test.el ends here
