;;; graph-remove-task-test.el --- Tests for removing tasks from project graphs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for remove/trash operations on project graph tasks.
;;
;; Test Coverage:
;; - Remove from simple chain (A → B → C)
;; - Remove root task
;; - Remove from multiple projects
;; - Keep as independent
;; - Trash from multiple projects
;;
;; Migrated from test/org-gtd-graph-remove-task-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;;; Test helpers

(defun graph-remove-test--create-chain (project-title)
  "Create a test project with chain A → B → C.
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id."
  (let ((data (make-chain-project project-title
                                  :tasks '("Task A" "Task B" "Task C"))))
    `((project-marker . ,(cdr (assoc 'project-marker data)))
      (project-id . ,(cdr (assoc 'project-id data)))
      (task-a-id . ,(nth 0 (cdr (assoc 'task-ids data))))
      (task-b-id . ,(nth 1 (cdr (assoc 'task-ids data))))
      (task-c-id . ,(nth 2 (cdr (assoc 'task-ids data)))))))

(defun graph-remove-test--create-diamond (project-title)
  "Create a test project with diamond structure: A → C ← B, C → D.
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id, task-d-id."
  (make-diamond-project project-title
                        :root-tasks '("Task A" "Task B")
                        :middle-task "Task C"
                        :leaf-task "Task D"))

;;;; Test 1: Remove from Simple Chain

(deftest graph-remove/removes-middle-task-and-rewires-chain ()
  "Removes middle task and rewires chain A → B → C to A → C."
  (let* ((project-data (graph-remove-test--create-chain "Chain Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Verify initial state: A → B → C
    (let ((b-deps (org-gtd-get-task-dependencies task-b-id))
          (c-deps (org-gtd-get-task-dependencies task-c-id))
          (a-blocks (org-gtd-get-task-blockers task-a-id))
          (b-blocks (org-gtd-get-task-blockers task-b-id)))
      (assert-same-items (list task-a-id) b-deps)
      (assert-same-items (list task-b-id) c-deps)
      (assert-same-items (list task-b-id) a-blocks)
      (assert-same-items (list task-c-id) b-blocks))

    ;; Remove Task B
    (org-gtd-graph--remove-from-project task-b-id project-id)

    ;; Verify rewiring: A → C
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))

    ;; Verify B removed from project
    (assert-nil (member project-id (org-gtd-get-task-projects task-b-id)))

    ;; Verify B no longer blocks C or depends on A
    (assert-nil (member task-a-id (org-gtd-get-task-dependencies task-b-id)))
    (assert-nil (member task-c-id (org-gtd-get-task-blockers task-b-id)))))

;;;; Test 2: Remove Root Task

(deftest graph-remove/removes-root-task-and-promotes-successor ()
  "Removes root task and promotes successor to root."
  (let* ((project-data (graph-remove-test--create-chain "Root Remove Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Verify initial state: FIRST_TASKS = [A], A → B → C
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-same-items (list task-a-id) first-tasks))

    ;; Remove Task A (root)
    (org-gtd-graph--remove-from-project task-a-id project-id)

    ;; Verify B is now root
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-same-items (list task-b-id) first-tasks))

    ;; Verify A no longer in FIRST_TASKS
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-nil (member task-a-id first-tasks)))

    ;; Verify B no longer depends on A
    (assert-nil (member task-a-id (org-gtd-get-task-dependencies task-b-id)))

    ;; Verify A removed from project
    (assert-nil (member project-id (org-gtd-get-task-projects task-a-id)))))

;;;; Test 3: Remove Task with Multiple Parents

(deftest graph-remove/removes-task-with-two-parents-and-rewires ()
  "Removes task with two parents and rewires both to successor."
  (let* ((project-data (graph-remove-test--create-diamond "Diamond Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data)))
         (task-d-id (cdr (assoc 'task-d-id project-data))))

    ;; Verify initial state: A → C, B → C, C → D
    (assert-same-items (list task-c-id) (org-gtd-get-task-blockers task-a-id))
    (assert-same-items (list task-c-id) (org-gtd-get-task-blockers task-b-id))
    (assert-same-items (list task-a-id task-b-id) (org-gtd-get-task-dependencies task-c-id))
    (assert-same-items (list task-d-id) (org-gtd-get-task-blockers task-c-id))
    (assert-same-items (list task-c-id) (org-gtd-get-task-dependencies task-d-id))

    ;; Remove Task C (which has two parents)
    (org-gtd-graph--remove-from-project task-c-id project-id)

    ;; Verify both parents now block D: A → D, B → D
    (assert-same-items (list task-d-id) (org-gtd-get-task-blockers task-a-id))
    (assert-same-items (list task-d-id) (org-gtd-get-task-blockers task-b-id))

    ;; Verify D now depends on both A and B
    (assert-same-items (list task-a-id task-b-id) (org-gtd-get-task-dependencies task-d-id))

    ;; Verify C removed from project
    (assert-nil (member project-id (org-gtd-get-task-projects task-c-id)))

    ;; Verify C's dependencies cleaned up
    (assert-nil (member task-a-id (org-gtd-get-task-dependencies task-c-id)))
    (assert-nil (member task-b-id (org-gtd-get-task-dependencies task-c-id)))
    (assert-nil (member task-d-id (org-gtd-get-task-blockers task-c-id)))

    ;; Verify A and B no longer reference C
    (assert-nil (member task-c-id (org-gtd-get-task-blockers task-a-id)))
    (assert-nil (member task-c-id (org-gtd-get-task-blockers task-b-id)))))

;;;; Test 4: Keep as Independent

(deftest graph-remove/keeps-task-as-independent ()
  "Removes task from all projects and makes it independent."
  (let* ((project-data (graph-remove-test--create-chain "Independence Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Keep B as independent
    (org-gtd-graph--keep-as-independent task-b-id)

    ;; Verify B not in any project
    (let ((b-projects (org-gtd-get-task-projects task-b-id)))
      (assert-true (or (null b-projects) (equal b-projects '()))))

    ;; Verify project rewired: A → C
    (assert-true (member task-c-id (org-gtd-get-task-blockers task-a-id)))
    (assert-true (member task-a-id (org-gtd-get-task-dependencies task-c-id)))

    ;; Verify B's dependencies PRESERVED (orphaned but kept for future)
    (assert-true (member task-a-id (org-gtd-get-task-dependencies task-b-id)))
    (assert-true (member task-c-id (org-gtd-get-task-blockers task-b-id)))))

;;;; Test 5: Trash Task

(deftest graph-remove/trashes-task-and-cleans-dependencies ()
  "Removes task from all projects, cleans all dependencies, and archives it."
  (let* ((project-data (graph-remove-test--create-chain "Trash Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Verify initial state
    (assert-true (member task-a-id (org-gtd-get-task-dependencies task-b-id)))
    (assert-true (member task-c-id (org-gtd-get-task-blockers task-b-id)))

    ;; Trash Task B
    (org-gtd-graph--trash-task task-b-id)

    ;; Verify project rewired: A → C
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))

    ;; Verify A no longer blocks B, C no longer depends on B
    (assert-nil (member task-b-id (org-gtd-get-task-blockers task-a-id)))
    (assert-nil (member task-b-id (org-gtd-get-task-dependencies task-c-id)))

    ;; Verify B's dependencies completely cleaned up
    (let ((b-deps (org-gtd-get-task-dependencies task-b-id))
          (b-blocks (org-gtd-get-task-blockers task-b-id)))
      (assert-true (or (null b-deps) (equal b-deps '())))
      (assert-true (or (null b-blocks) (equal b-blocks '()))))

    ;; Verify B not in any project
    (let ((b-projects (org-gtd-get-task-projects task-b-id)))
      (assert-true (or (null b-projects) (equal b-projects '()))))

    ;; Verify B is archived/canceled
    (let ((marker (org-id-find task-b-id t)))
      (if marker
          (org-with-point-at marker
            (let ((todo-state (org-get-todo-state)))
              (assert-equal (org-gtd-keywords--canceled) todo-state)))
        ;; Task not found means it was archived to a different file
        (assert-nil marker)))))

(deftest graph-remove/does-not-orphan-successors-when-trashing ()
  "Does not orphan successors when trashing middle task in chain."
  (let* ((project-data (graph-remove-test--create-chain "Orphan Test Project"))
         (project-id (cdr (assoc 'project-id project-data)))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Verify initial state: A → B → C, FIRST_TASKS = [A]
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-same-items (list task-a-id) first-tasks))

    ;; Trash Task B (middle task)
    (org-gtd-graph--trash-task task-b-id)

    ;; Verify rewiring happened: A → C
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))

    ;; CRITICAL: C should NOT be in FIRST_TASKS because it has predecessor A
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-same-items (list task-a-id) first-tasks)
      (assert-nil (member task-c-id first-tasks)))

    ;; Verify A is still the only root task
    (let ((first-tasks (org-with-point-at project-marker
                        (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
      (assert-equal 1 (length first-tasks))
      (assert-equal task-a-id (car first-tasks)))))

(provide 'graph-remove-task-test)

;;; graph-remove-task-test.el ends here
