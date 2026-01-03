;;; multi-project-test.el --- Tests for multi-project task support -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for multi-project task support, including archiving logic.
;;
;; Test Coverage:
;; - Removes project ID from ORG_GTD_PROJECT_IDS when archiving (1 test)
;; - Archives task when ORG_GTD_PROJECT_IDS becomes empty (1 test)
;; - Does not archive task when ORG_GTD_PROJECT_IDS still has values (1 test)
;; - Shared task archiving behavior acceptance test (1 test)
;;
;; Migrated from test/multi-project-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Unit tests: multi-project archiving logic

(deftest multi-project/removes-project-id-when-archiving ()
  "Removes project ID from ORG_GTD_PROJECT_IDS when archiving."
  ;; Create a single task with multiple project IDs
  (capture-inbox-item "Test Task")
  (org-gtd-process-inbox)
  (organize-as-single-action)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test Task")
    (org-back-to-heading t)

    ;; Manually add multiple project IDs to simulate shared task
    (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")
    (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-2")

    ;; Verify both IDs are present
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-equal 2 (length project-ids))
      (assert-true (member "proj-id-1" project-ids))
      (assert-true (member "proj-id-2" project-ids)))

    ;; Call helper function to remove one project ID
    (require 'org-gtd-archive)
    (org-gtd--remove-project-id-from-task (point) "proj-id-1")

    ;; Verify only proj-id-2 remains
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-equal 1 (length project-ids))
      (assert-true (member "proj-id-2" project-ids))
      (assert-nil (member "proj-id-1" project-ids)))))

(deftest multi-project/archives-task-when-project-ids-empty ()
  "Archives task when ORG_GTD_PROJECT_IDS becomes empty."
  ;; Create a single task with one project ID
  (capture-inbox-item "Test Task")
  (org-gtd-process-inbox)
  (organize-as-single-action)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test Task")
    (org-back-to-heading t)
    (org-todo "DONE")

    ;; Add single project ID
    (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")

    ;; Verify task exists in tasks file
    (let ((tasks-content-before (buffer-string)))
      (assert-match "Test Task" tasks-content-before))

    ;; Call helper to remove project ID and archive if empty
    (require 'org-gtd-archive)
    (org-gtd--archive-task-if-no-projects (point) "proj-id-1")

    ;; Verify task is archived
    (let ((tasks-content (ogt--buffer-string (org-gtd--default-file)))
          (archived-content (ogt--archive-string)))
      (refute-match "Test Task" tasks-content)
      (assert-match "Test Task" archived-content))))

(deftest multi-project/no-archive-when-project-ids-remain ()
  "Does not archive task when ORG_GTD_PROJECT_IDS still has values."
  ;; Create a single task with multiple project IDs
  (capture-inbox-item "Test Task")
  (org-gtd-process-inbox)
  (organize-as-single-action)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test Task")
    (org-back-to-heading t)
    (org-todo "DONE")

    ;; Add multiple project IDs
    (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")
    (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-2")

    ;; Call helper to remove one project ID
    (require 'org-gtd-archive)
    (org-gtd--archive-task-if-no-projects (point) "proj-id-1")

    ;; Verify task is NOT archived (still in tasks file)
    (let ((tasks-content (ogt--buffer-string (org-gtd--default-file)))
          (archived-content (ogt--archive-string)))
      (assert-match "Test Task" tasks-content)
      (refute-match "Test Task" archived-content))))

;;; Acceptance test: shared task archiving behavior

(deftest multi-project/archives-shared-task-only-when-all-projects-complete ()
  "Archives shared tasks only when all projects are complete."
  ;; Step 1: Create Project A with Task A1, Shared Task, Task A2
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task A1\n** Shared Task\n** Task A2")
  (organize-as-project)

  ;; Step 2: Create Project B with Task B1, Task B2
  (capture-inbox-item "Project B")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task B1\n** Task B2")
  (organize-as-project)

  ;; Step 3: Set up multi-project relationship for Shared Task
  (let (shared-task-id project-a-id project-b-id)
    (with-current-buffer (org-gtd--default-file)
      ;; Get Project A's ID and Shared Task's ID
      (goto-char (point-min))
      (search-forward "Project A")
      (org-back-to-heading t)
      (setq project-a-id (org-entry-get (point) "ID"))
      (search-forward "Shared Task")
      (org-back-to-heading t)
      (setq shared-task-id (org-entry-get (point) "ID"))

      ;; Get Project B's ID
      (goto-char (point-min))
      (search-forward "Project B")
      (org-back-to-heading t)
      (setq project-b-id (org-entry-get (point) "ID"))

      ;; Add Project B's ID to Shared Task's ORG_GTD_PROJECT_IDS
      ;; (Project A's ID should already be there from clarify-as-project)
      (goto-char (point-min))
      (search-forward "Project A")
      (search-forward "Shared Task")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-b-id)

      ;; Add Shared Task to Project B's first tasks
      (goto-char (point-min))
      (search-forward "Project B")
      (org-back-to-heading t)
      (let ((existing-first-tasks (org-entry-get (point) "ORG_GTD_FIRST_TASKS")))
        (org-entry-put (point) "ORG_GTD_FIRST_TASKS"
                      (concat existing-first-tasks " " shared-task-id)))))

  ;; Step 4: Complete all tasks in Project A
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")

    ;; Mark Task A1 as DONE
    (search-forward "Task A1")
    (org-back-to-heading t)
    (org-todo "DONE")

    ;; Mark Shared Task as DONE
    (goto-char (point-min))
    (search-forward "Project A")
    (search-forward "Shared Task")
    (org-back-to-heading t)
    (org-todo "DONE")

    ;; Mark Task A2 as DONE
    (goto-char (point-min))
    (search-forward "Project A")
    (search-forward "Task A2")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 5: Archive completed items
  (org-gtd-archive-completed-items)

  ;; Step 6: Verify Project A is archived but Shared Task is NOT
  ;; (because Project B still references it)
  (let ((archived-content (ogt--archive-string))
        (tasks-content (ogt--buffer-string (org-gtd--default-file))))

    ;; Project A should be archived
    (assert-match "Project A" archived-content)

    ;; Project B should still be in tasks file
    (assert-match "Project B" tasks-content)

    ;; Shared Task should NOT be archived yet (Project B still needs it)
    (refute-match "Shared Task" archived-content)
    (assert-match "Shared Task" tasks-content))

  ;; Step 7: Complete all tasks in Project B
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project B")

    ;; Mark Task B1 as DONE
    (search-forward "Task B1")
    (org-back-to-heading t)
    (org-todo "DONE")

    ;; Mark Task B2 as DONE
    (goto-char (point-min))
    (search-forward "Project B")
    (search-forward "Task B2")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 8: Archive completed items again
  (org-gtd-archive-completed-items)

  ;; Step 9: Verify both projects and shared task are now archived
  (let ((archived-content (ogt--archive-string)))
    ;; Both projects should be archived
    (assert-match "Project A" archived-content)
    (assert-match "Project B" archived-content)

    ;; Shared Task should NOW be archived (all projects complete)
    (assert-match "Shared Task" archived-content)))

(provide 'multi-project-test)

;;; multi-project-test.el ends here
