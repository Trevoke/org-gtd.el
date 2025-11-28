;; -*- lexical-binding: t; coding: utf-8 -*-

;;; org-gtd-graph-modify-relationships-test.el --- Tests for modifying task relationships -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; TDD tests for modify-blockers and modify-successors operations.
;;
;; Test Coverage:
;; - Add new blocker to task with no blockers
;; - Add multiple blockers at once
;; - Remove existing blocker
;; - Mix of add and remove
;; - Cycle prevention
;;

;;; Code:

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)

;;;; Test Setup
;; Uses graph topology builders for clean project creation

(defun org-gtd-graph-modify-test--create-simple-project ()
  "Create a test project with three independent tasks A, B, C.
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id."
  (make-parallel-project "Simple Project"
                         :tasks '("Task A" "Task B" "Task C")))

;;;; Test 1: Add single blocker to task with no blockers

(describe "org-gtd-graph-modify-blockers"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "adds a single blocker to task with no blockers"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data))))

      ;; Verify initial state: B has no blockers
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal nil)

      ;; Mock the interactive input to select A as blocker for B
      ;; We'll implement the actual function to accept programmatic input for testing
      (org-gtd-graph--modify-blockers-internal task-b-id (list task-a-id) project-marker)

      ;; Verify: A now blocks B
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-b-id))
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))))

  (it "removes an existing blocker"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data))))

      ;; Setup: A blocks B
      (org-gtd-dependencies-create task-a-id task-b-id)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))

      ;; Remove blocker: B now has no blockers
      (org-gtd-graph--modify-blockers-internal task-b-id '() project-marker)

      ;; Verify: A no longer blocks B
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal nil)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal nil)))

  (it "handles mix of add and remove"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Setup: A blocks B
      (org-gtd-dependencies-create task-a-id task-b-id)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))

      ;; Change blocker: Remove A, add C
      (org-gtd-graph--modify-blockers-internal task-b-id (list task-c-id) project-marker)

      ;; Verify: C blocks B, A does not
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal nil)
      (expect (org-gtd-get-task-blockers task-c-id) :to-equal (list task-b-id))
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-c-id))))

  (it "prevents adding successor as blocker (cycle)"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data))))

      ;; Setup: A blocks B (A → B)
      (org-gtd-dependencies-create task-a-id task-b-id)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))

      ;; Try to make B block A (which would create A → B → A cycle)
      ;; This should throw an error
      (expect (org-gtd-graph--modify-blockers-internal task-a-id (list task-b-id) project-marker)
              :to-throw))))

;;;; Test Modify-Successors

(describe "org-gtd-graph-modify-successors"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "adds a single successor to task with no successors"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data))))

      ;; Verify initial state: A has no successors
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal nil)

      ;; Make A block B
      (org-gtd-graph--modify-successors-internal task-a-id (list task-b-id) project-marker)

      ;; Verify: A now blocks B
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-b-id))
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))))

  (it "removes an existing successor"
    (let* ((project-data (org-gtd-graph-modify-test--create-simple-project))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data))))

      ;; Setup: A blocks B
      (org-gtd-dependencies-create task-a-id task-b-id)
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-b-id))

      ;; Remove successor: A now has no successors
      (org-gtd-graph--modify-successors-internal task-a-id '() project-marker)

      ;; Verify: A no longer blocks B
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal nil)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal nil))))

(provide 'org-gtd-graph-modify-relationships-test)

;;; org-gtd-graph-modify-relationships-test.el ends here
