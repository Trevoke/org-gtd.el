;;; property-based-system-test.el --- Tests for property-based GTD system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for property-based GTD system design.
;;
;; Test Coverage:
;; - Project task identification by ORG_GTD property (2 tests)
;; - Migration from level-based to property-based (1 test)
;; - View language without level filters (1 test)
;; - Property-based dependency system (1 test)
;;
;; Migrated from test/property-based-system-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Project task identification by ORG_GTD property

(deftest property-based/identifies-tasks-by-property-not-level ()
  "Identifies project tasks by ORG_GTD=org-gtd-action property not by level."
  ;; Create a project structure where tasks have ORG_GTD property but are not at level 2
  (with-temp-buffer
    (org-mode)
    ;; Create project root using builder
    (make-project "Project Root"
                  :id "root-id"
                  :level 1)
    ;; Create tasks at non-standard levels using builder
    (make-task "Deep Task 1"
               :id "task1-id"
               :level 3
               :properties '(("TODO" . "TODO")))
    (make-task "Even Deeper Task 2"
               :id "task2-id"
               :level 4
               :properties '(("TODO" . "TODO")))

    ;; Test that tasks are identified by property, not level
    (goto-char (point-min))
    (let ((task-count 0))
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
           (setq task-count (1+ task-count))))
       nil
       nil)
      (assert-equal 2 task-count))))

(deftest property-based/collects-tasks-via-graph-traversal ()
  "Collects tasks through graph traversal via ID relationships."
  ;; This test verifies that task collection follows BLOCKS/DEPENDS_ON chains
  ;; Uses real project creation API to ensure FIRST_TASKS is set correctly

  ;; Create project using real API
  (create-project "Graph Project")

  ;; Now modify the created tasks to add dependency relationships
  (let* ((gtd-file (org-gtd--default-file))
         (project-marker nil)
         (task-ids '()))

    (with-current-buffer gtd-file
      ;; Find the project and its tasks
      (goto-char (point-min))
      (search-forward "Graph Project")
      (org-back-to-heading t)
      (setq project-marker (point-marker))

      ;; Collect task IDs
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
           (let ((id (org-entry-get (point) "ID")))
             (when id (push id task-ids)))))
       nil
       'tree)

      (setq task-ids (nreverse task-ids))

      ;; Add dependency relationships: Task 1 -> Task 2 -> Task 3
      ;; Task 1 blocks Task 2
      (goto-char (point-min))
      (org-find-entry-with-id (nth 0 task-ids))
      (org-entry-put (point) "ORG_GTD_BLOCKS" (nth 1 task-ids))

      ;; Task 2 depends on Task 1 and blocks Task 3
      (goto-char (point-min))
      (org-find-entry-with-id (nth 1 task-ids))
      (org-entry-put (point) "ORG_GTD_DEPENDS_ON" (nth 0 task-ids))
      (org-entry-put (point) "ORG_GTD_BLOCKS" (nth 2 task-ids))

      ;; Task 3 depends on Task 2
      (goto-char (point-min))
      (org-find-entry-with-id (nth 2 task-ids))
      (org-entry-put (point) "ORG_GTD_DEPENDS_ON" (nth 1 task-ids))

      ;; Update FIRST_TASKS to reflect new dependency structure
      ;; Only Task 1 should be in FIRST_TASKS now
      (goto-char project-marker)
      (org-entry-put (point) "FIRST_TASKS" (nth 0 task-ids))

      (basic-save-buffer))

    ;; Update org-id locations so org-id-find works
    (org-id-update-id-locations (list (buffer-file-name gtd-file)))

    ;; Test graph traversal
    (with-current-buffer gtd-file
      (goto-char project-marker)
      (let ((project-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
        (assert-equal 3 (length project-tasks))))))

;;; Migration from level-based to property-based

(deftest property-based/migrates-level-2-tasks-to-have-property ()
  "Migrates existing level 2 project tasks to have ORG_GTD property."
  ;; Create old-style project with level 2 tasks but no ORG_GTD properties
  ;; Note: This test deliberately creates tasks WITHOUT ORG_GTD property to test migration
  (with-temp-buffer
    (org-mode)
    ;; Create project heading
    (insert "* Legacy Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":ID: legacy-project-id\n")
    (insert ":END:\n")
    ;; Create tasks WITHOUT ORG_GTD property (simulating old format)
    (insert "** Legacy Task 1\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: legacy-task1-id\n")
    (insert ":TODO: TODO\n")
    (insert ":END:\n")
    (insert "** Legacy Task 2\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: legacy-task2-id\n")
    (insert ":TODO: TODO\n")
    (insert ":END:\n")

    ;; Apply migration function core logic (adapted for temp buffer)
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (org-back-to-heading t)
      (when (string= (org-entry-get (point) "ORG_GTD") "Projects")
        (let ((project-level (org-current-level)))
          (outline-next-heading)
          (while (and (not (eobp))
                      (> (org-current-level) project-level))
            (when (= (org-current-level) (1+ project-level))
              ;; This is a direct child (level 2 under project)
              (unless (org-entry-get (point) "ORG_GTD")
                (org-entry-put (point) "ORG_GTD" "Actions")))
            (outline-next-heading)))))

    ;; Verify all level 2 tasks under Projects now have ORG_GTD property
    (goto-char (point-min))
    (search-forward "Legacy Task 1")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

    (goto-char (point-min))
    (search-forward "Legacy Task 2")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

;;; Property-based dependency system

(deftest property-based/uses-explicit-dependency-properties ()
  "Uses explicit dependency properties (ORG_GTD_DEPENDS_ON, ORG_GTD_BLOCKS)."
  ;; Test that dependency relationships work using explicit properties
  ;; (not relying on org-edna inheritance)
  (with-temp-buffer
    (org-mode)
    ;; Create project with dependent tasks using builders
    (make-project "Project"
                  :id "proj-id"
                  :level 1)
    (make-task "Task 1"
               :id "task1-id"
               :level 2
               :blocks '("task2-id")
               :properties '(("TODO" . "TODO")))
    (make-task "Task 2"
               :id "task2-id"
               :level 2
               :depends-on '("task1-id")
               :properties '(("TODO" . "TODO")))

    ;; Task relationships should work without inheritance
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (let ((deps (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
      (assert-equal 1 (length deps))
      (assert-equal "task1-id" (car deps)))))

(provide 'property-based-system-test)

;;; property-based-system-test.el ends here
