;;; graph-modify-relationships-test.el --- Tests for modifying task relationships -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; TDD tests for modify-blockers and modify-successors operations.
;;
;; Test Coverage:
;; - Add new blocker to task with no blockers (1 test)
;; - Remove existing blocker (1 test)
;; - Mix of add and remove (1 test)
;; - Cycle prevention (1 test)
;; - Add single successor (1 test)
;; - Remove existing successor (1 test)
;;
;; Migrated from test/org-gtd-graph-modify-relationships-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;;; Test Setup

(defun org-gtd-graph-modify-test--create-simple-project ()
  "Create a test project with three independent tasks A, B, C.
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id."
  (make-parallel-project "Simple Project"
                         :tasks '("Task A" "Task B" "Task C")))

;;; Modify Blockers Tests

(deftest graph-modify/adds-single-blocker-to-task ()
  "Adds a single blocker to task with no blockers."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data))))

    ;; Verify initial state: B has no blockers
    (assert-nil (org-gtd-get-task-dependencies task-b-id))

    ;; Mock the interactive input to select A as blocker for B
    ;; We'll implement the actual function to accept programmatic input for testing
    (org-gtd-graph--modify-blockers-internal task-b-id (list task-a-id) project-marker)

    ;; Verify: A now blocks B
    (assert-equal (list task-b-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))))

(deftest graph-modify/removes-existing-blocker ()
  "Removes an existing blocker."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data))))

    ;; Setup: A blocks B
    (org-gtd-dependencies-create task-a-id task-b-id)
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))

    ;; Remove blocker: B now has no blockers
    (org-gtd-graph--modify-blockers-internal task-b-id '() project-marker)

    ;; Verify: A no longer blocks B
    (assert-nil (org-gtd-get-task-blockers task-a-id))
    (assert-nil (org-gtd-get-task-dependencies task-b-id))))

(deftest graph-modify/handles-mix-of-add-and-remove ()
  "Handles mix of add and remove."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data)))
         (task-c-id (cdr (assoc 'task-c-id project-data))))

    ;; Setup: A blocks B
    (org-gtd-dependencies-create task-a-id task-b-id)
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))

    ;; Change blocker: Remove A, add C
    (org-gtd-graph--modify-blockers-internal task-b-id (list task-c-id) project-marker)

    ;; Verify: C blocks B, A does not
    (assert-nil (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-b-id) (org-gtd-get-task-blockers task-c-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-dependencies task-b-id))))

(deftest graph-modify/prevents-cycle-when-adding-blocker ()
  "Prevents adding successor as blocker (cycle)."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data))))

    ;; Setup: A blocks B (A → B)
    (org-gtd-dependencies-create task-a-id task-b-id)
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))

    ;; Try to make B block A (which would create A → B → A cycle)
    ;; This should throw a user-error
    (assert-raises 'user-error
      (org-gtd-graph--modify-blockers-internal task-a-id (list task-b-id) project-marker))))

;;; Modify Successors Tests

(deftest graph-modify/adds-single-successor ()
  "Adds a single successor to task with no successors."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data))))

    ;; Verify initial state: A has no successors
    (assert-nil (org-gtd-get-task-blockers task-a-id))

    ;; Make A block B
    (org-gtd-graph--modify-successors-internal task-a-id (list task-b-id) project-marker)

    ;; Verify: A now blocks B
    (assert-equal (list task-b-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))))

(deftest graph-modify/removes-existing-successor ()
  "Removes an existing successor."
  (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
         (project-marker (cdr (assoc 'project-marker project-data)))
         (task-a-id (cdr (assoc 'task-a-id project-data)))
         (task-b-id (cdr (assoc 'task-b-id project-data))))

    ;; Setup: A blocks B
    (org-gtd-dependencies-create task-a-id task-b-id)
    (assert-equal (list task-b-id) (org-gtd-get-task-blockers task-a-id))

    ;; Remove successor: A now has no successors
    (org-gtd-graph--modify-successors-internal task-a-id '() project-marker)

    ;; Verify: A no longer blocks B
    (assert-nil (org-gtd-get-task-blockers task-a-id))
    (assert-nil (org-gtd-get-task-dependencies task-b-id))))

(provide 'graph-modify-relationships-test)

;;; graph-modify-relationships-test.el ends here
