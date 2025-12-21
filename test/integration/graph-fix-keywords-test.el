;;; graph-fix-keywords-test.el --- Tests for graph operations fixing TODO keywords -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for graph transient operations to verify they
;; correctly fix TODO keywords after modifying dependencies.
;;
;; These tests verify the EXPECTED behavior (they will FAIL initially
;; because graph operations don't yet call org-gtd-projects-fix-todo-keywords).
;;
;; Once we add the fix-keywords calls to graph operations, these tests
;; should pass.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'org-gtd-projects)

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Tests

(deftest graph-fix/add-blocker-changes-next-to-todo ()
  "Adding a blocker to a NEXT task should change it to TODO.
When a task that was NEXT (no blockers) gets a blocker added,
it should become TODO since it now has dependencies."

  ;; Setup: Create project with Task A (NEXT, no blockers)
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state - Task A should be NEXT (no blockers)
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state: Task A is NEXT
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO")))

      ;; Action: Add Task B as blocker to Task A using internal function
      (org-gtd-graph--add-blocker-internal "Task B" (list task-a-id) project-marker)

      ;; Assert: Task A should now be TODO (has blocker)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Assert: Task B should be NEXT (no blockers)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/add-successor-changes-blocked-to-todo ()
  "Adding a successor that blocks an existing NEXT task changes it to TODO.
When we add a new task that blocks an existing NEXT task,
the blocked task should become TODO."

  ;; Setup: Create project with Task A (NEXT, no blockers)
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state - Task A should be NEXT
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state: Task A is NEXT
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO")))

      ;; Action: Add Task B as successor that blocks Task A
      (org-gtd-graph--add-successor-internal "Task B" (list task-a-id) project-marker)

      ;; Assert: Task A should now be TODO (blocked by B)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Assert: Task B should be NEXT (no blockers)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/modify-blockers-remove-all-changes-to-next ()
  "Removing all blockers from a TODO task should change it to NEXT.
When we use modify-blockers to remove all blockers from a task,
it should become NEXT since it's now ready to work on."

  ;; Setup: Create project with A → B chain
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A" "Task B"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state: Task A is NEXT, Task B is TODO
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Action: Remove all blockers from Task B (empty list)
      (org-gtd-graph--modify-blockers-internal task-b-id '() project-marker)

      ;; Assert: Task B should now be NEXT (no blockers)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/modify-blockers-add-blocker-changes-next-to-todo ()
  "Adding blockers via modify-blockers changes NEXT task to TODO.
When we use modify-blockers to add a blocker to a NEXT task,
it should become TODO."

  ;; Setup: Create project with two independent tasks (both NEXT)
  (let ((project-data (make-parallel-project "Test Project"
                                             :tasks '("Task A" "Task B"))))
    (let ((task-a-id (alist-get 'task-a-id project-data))
          (task-b-id (alist-get 'task-b-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state - both should be NEXT
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state: Both tasks are NEXT
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO")))

      ;; Action: Make Task A block Task B using modify-blockers
      (org-gtd-graph--modify-blockers-internal task-b-id (list task-a-id) project-marker)

      ;; Assert: Task A should still be NEXT (no blockers)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO")))

      ;; Assert: Task B should now be TODO (blocked by A)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/modify-successors-remove-all-changes-blocked-to-next ()
  "Removing all successors from a task that was blocking another changes blocked task to NEXT.
When we remove the last blocker relationship, the previously blocked task
should become NEXT if it has no other blockers."

  ;; Setup: Create project with A → B chain
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A" "Task B"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state: Task A is NEXT, Task B is TODO
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Action: Remove Task B from Task A's successors (A no longer blocks B)
      (org-gtd-graph--modify-successors-internal task-a-id '() project-marker)

      ;; Assert: Task B should now be NEXT (no blockers)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/add-root-task-is-next ()
  "Adding a root task via graph transient creates NEXT task.
When we add a new root task to a project, it should be NEXT
since root tasks have no blockers."

  ;; Setup: Create simple project with one task
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A"))))
    (let ((project-marker (alist-get 'project-marker project-data)))

      ;; Action: Add root task using the internal function
      ;; (this is what org-gtd-graph-transient-add-root uses internally)
      (org-gtd-graph--add-root-internal "Root Task" project-marker)

      ;; Assert: Root Task should be NEXT (this will FAIL until we add fix-keywords call)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Root Task")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/remove-task-unblocks-successors ()
  "Removing a blocker task should unblock its successors.
When we remove a task from a project using org-gtd-graph--remove-from-project,
its successors should become NEXT if they have no other blockers."

  ;; Setup: Create project with A → B chain (simpler case)
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A" "Task B"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-marker (alist-get 'project-marker project-data))
          (project-id (alist-get 'project-id project-data)))

      ;; Fix keywords to set initial state - A is NEXT, B is TODO
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Action: Remove Task A from project (the blocker)
      (org-gtd-graph--remove-from-project task-a-id project-id)

      ;; Assert: Task B should now be NEXT (no longer blocked by A)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(deftest graph-fix/trash-task-unblocks-successors ()
  "Trashing a blocker task should unblock its successors.
When we trash a task using org-gtd-graph--trash-task,
its successors should become NEXT if they have no other blockers."

  ;; Setup: Create project with A → B chain
  (let ((project-data (make-chain-project "Test Project"
                                          :tasks '("Task A" "Task B"))))
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-marker (alist-get 'project-marker project-data)))

      ;; Fix keywords to set initial state - A is NEXT, B is TODO
      (org-gtd-projects-fix-todo-keywords project-marker)

      ;; Verify initial state
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "TODO" (org-entry-get (point) "TODO")))

      ;; Action: Trash Task A (the blocker)
      (org-gtd-graph--trash-task task-a-id)

      ;; Assert: Task B should now be NEXT (no longer blocked by A)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "NEXT" (org-entry-get (point) "TODO"))))))

(provide 'graph-fix-keywords-test)

;;; graph-fix-keywords-test.el ends here
