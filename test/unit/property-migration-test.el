;;; property-migration-test.el --- Tests for property name migration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for property name migration to ORG_GTD_* namespace.
;;
;; Test Coverage:
;; - ORG_GTD_FIRST_TASKS property on project creation (1 test)
;; - ORG_GTD_BLOCKS property on dependency creation (1 test)
;; - ORG_GTD_DEPENDS_ON property on dependency creation (1 test)
;; - ORG_GTD_PROJECT_IDS property on task organization (1 test)
;;
;; Migrated from test/property-migration-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; ORG_GTD_FIRST_TASKS property on project creation

(deftest property-migration/first-tasks-uses-namespaced-property ()
  "Sets ORG_GTD_FIRST_TASKS instead of FIRST_TASKS on new projects."
  ;; Create a project with 3 tasks
  (capture-inbox-item "Test Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2\n** Task 3")
  (organize-as-project)

  ;; Verify ORG_GTD_FIRST_TASKS is set, not FIRST_TASKS
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test Project")
    (org-back-to-heading t)

    ;; Should have ORG_GTD_FIRST_TASKS property
    (assert-true (org-entry-get (point) "ORG_GTD_FIRST_TASKS"))

    ;; Should NOT have old FIRST_TASKS property
    (assert-nil (org-entry-get (point) "FIRST_TASKS"))))

;;; ORG_GTD_BLOCKS and ORG_GTD_DEPENDS_ON properties on dependency creation

(deftest property-migration/blocks-uses-namespaced-property ()
  "Sets ORG_GTD_BLOCKS instead of BLOCKS when creating dependencies."
  ;; Create a project with 3 tasks
  (capture-inbox-item "Dependency Test Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2\n** Task 3")
  (organize-as-project)

  ;; Check that tasks have sequential dependencies with new properties
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)

    ;; Task 1 should have ORG_GTD_BLOCKS, not BLOCKS
    (assert-true (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
    (assert-nil (org-entry-get-multivalued-property (point) "BLOCKS"))))

(deftest property-migration/depends-on-uses-namespaced-property ()
  "Sets ORG_GTD_DEPENDS_ON instead of DEPENDS_ON when creating dependencies."
  ;; Create a project with 3 tasks
  (capture-inbox-item "Dependency Test Project 2")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2\n** Task 3")
  (organize-as-project)

  ;; Check that tasks have sequential dependencies with new properties
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)

    ;; Task 2 should have ORG_GTD_DEPENDS_ON, not DEPENDS_ON
    (assert-true (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
    (assert-nil (org-entry-get-multivalued-property (point) "DEPENDS_ON"))))

;;; ORG_GTD_PROJECT_IDS property on task organization

(deftest property-migration/project-ids-uses-namespaced-property ()
  "Sets ORG_GTD_PROJECT_IDS instead of ORG_GTD_PROJECT when organizing tasks into projects."
  ;; Create a project
  (capture-inbox-item "Project with IDs")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2")
  (organize-as-project)

  ;; Check that tasks have ORG_GTD_PROJECT_IDS
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)

    ;; Task should have ORG_GTD_PROJECT_IDS with the project ID (for multi-project support)
    (assert-true (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))

    ;; Should ALSO have ORG_GTD_PROJECT property (for agenda display)
    (assert-equal "Project with IDs" (org-entry-get (point) "ORG_GTD_PROJECT"))))

(provide 'property-migration-test)

;;; property-migration-test.el ends here
