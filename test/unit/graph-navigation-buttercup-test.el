;;; graph-navigation-buttercup-test.el --- Tests for graph navigation (from buttercup) -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-navigation keyboard navigation functions.
;; These were migrated from the buttercup test suite.
;;
;; Test Coverage:
;; - Goto navigation with completion (1 test)
;; - Project heading as graph node (2 tests)
;; - Initial node selection in graph view (1 test)
;; - DAG-based navigation with 'n' key (1 test)
;; - 'p' navigation moves up dependency chain (1 test)
;;
;; Migrated from test/org-gtd-graph-navigation-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-navigation)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun org-gtd-graph-navigation-test--create-test-graph ()
  "Create a test graph structure with known topology.
Returns a graph with the following structure:
  A (root)
  |
  +-> B (layer 1)
  |   +-> D (layer 2)
  |   +-> E (layer 2)
  |
  +-> C (layer 1)
      +-> F (layer 2)"
  (let ((graph (org-gtd-graph-create
                :project-id "test-project"
                :project-name "Test Project"))
        (nodes (make-hash-table :test 'equal)))

    ;; Create nodes
    (puthash "A" (org-gtd-graph-node-create
                  :id "A" :title "Task A" :state "TODO" :layer 0)
             nodes)
    (puthash "B" (org-gtd-graph-node-create
                  :id "B" :title "Task B" :state "TODO" :layer 1)
             nodes)
    (puthash "C" (org-gtd-graph-node-create
                  :id "C" :title "Task C" :state "TODO" :layer 1)
             nodes)
    (puthash "D" (org-gtd-graph-node-create
                  :id "D" :title "Task D" :state "TODO" :layer 2)
             nodes)
    (puthash "E" (org-gtd-graph-node-create
                  :id "E" :title "Task E" :state "TODO" :layer 2)
             nodes)
    (puthash "F" (org-gtd-graph-node-create
                  :id "F" :title "Task F" :state "TODO" :layer 2)
             nodes)

    ;; Create edges
    (let ((edges (list
                  (org-gtd-graph-edge-create :from-id "A" :to-id "B" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "A" :to-id "C" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "B" :to-id "D" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "B" :to-id "E" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "C" :to-id "F" :type :blocks))))

      (setf (org-gtd-graph-nodes graph) nodes
            (org-gtd-graph-edges graph) edges
            (org-gtd-graph-root-ids graph) '("A")))

    graph))

;;; Goto Navigation Tests

(deftest graph-nav/goto-selects-node-by-title ()
  "Selects node by title with completion."
  (let* ((graph (org-gtd-graph-navigation-test--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--graph graph
            org-gtd-graph-ui--selected-node-id "A")
      ;; Simulate selecting "Task D"
      (with-simulated-input "Task C-q SPC D RET"
        (org-gtd-graph-nav-goto))
      (assert-equal "D" org-gtd-graph-ui--selected-node-id))))

;;; Project Heading Integration Tests

(deftest graph-nav/project-heading-is-graph-node ()
  "Project heading is included as a node in the graph."
  (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
         project-marker)
    ;; Create a project with some tasks
    (with-current-buffer (find-file-noselect tasks-file)
      (erase-buffer)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":ORG_GTD_FIRST_TASKS: task-1\n")
      (insert ":END:\n")
      (insert "** Task 1\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_BLOCKS: task-2\n")
      (insert ":END:\n")
      (insert "** Task 2\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-2\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (setq project-marker (point-marker))
      (save-buffer))

    ;; Extract graph from project
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           (project-id (org-gtd-graph-project-id graph))
           (project-node (org-gtd-graph-data-get-node graph project-id)))

      ;; Project heading should be a node in the graph
      (assert-true project-node)
      (assert-equal "My Project" (org-gtd-graph-node-title project-node)))))

(deftest graph-nav/project-receives-edges-from-leaf-tasks ()
  "Project heading receives edges from leaf tasks (project at bottom)."
  (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
         project-marker project-id task-1-id task-2-id)
    ;; Create a project with multiple first tasks (which are also leaves)
    (with-current-buffer (find-file-noselect tasks-file)
      (erase-buffer)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Task 1\n")
      (org-back-to-heading t)
      (setq task-1-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Task 2\n")
      (org-back-to-heading t)
      (setq task-2-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      ;; Set first tasks
      (goto-char (point-min))
      (org-back-to-heading t)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (concat task-1-id " " task-2-id))
      (setq project-marker (point-marker))
      (save-buffer))

    ;; Extract graph and check edges
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           ;; Project should have incoming edges (predecessors), not outgoing
           (predecessors (org-gtd-graph-data-get-predecessors graph project-id))
           (successors (org-gtd-graph-data-get-successors graph project-id)))

      ;; Project should receive edges from leaf tasks
      (assert-equal 2 (length predecessors))
      (assert-true (member task-1-id predecessors))
      (assert-true (member task-2-id predecessors))
      ;; Project should NOT have outgoing edges
      (assert-equal 0 (length successors)))))

;;; Initial Node Selection Tests

(deftest graph-nav/initial-selected-node-is-first-task ()
  "Initial selected node should be the first task (project at bottom)."
  (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
         project-marker project-id task-1-id)
    ;; Create a simple project
    (with-current-buffer (find-file-noselect tasks-file)
      (erase-buffer)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Task 1\n")
      (org-back-to-heading t)
      (setq task-1-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      ;; Set first tasks
      (goto-char (point-min))
      (org-back-to-heading t)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-1-id)
      (setq project-marker (point-marker))
      (save-buffer))

    ;; Extract graph and test root node
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           (root-ids (org-gtd-graph-root-ids graph)))

      ;; The root should be the first task, not the project
      (assert-equal 1 (length root-ids))
      (assert-equal task-1-id (car root-ids))
      ;; Project should NOT be a root
      (assert-nil (member project-id root-ids)))))

;;; DAG-based Navigation Tests

(deftest graph-nav/n-navigates-to-first-child-in-dag ()
  "'n' navigates to first child (successor) in DAG, not BFS order."
  (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
         project-marker project-id task-1-id task-2-id task-3-id)
    ;; Create a project with specific structure (project at bottom):
    ;; task-1 (root) -> task-2 (leaf) -> Project
    ;; task-3 (root, also leaf) -> Project
    (with-current-buffer (find-file-noselect tasks-file)
      (erase-buffer)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Task 1\n")
      (org-back-to-heading t)
      (setq task-1-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Task 2\n")
      (org-back-to-heading t)
      (setq task-2-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Task 3\n")
      (org-back-to-heading t)
      (setq task-3-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      ;; Set first tasks
      (goto-char (point-min))
      (org-back-to-heading t)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (concat task-1-id " " task-3-id))
      ;; Create dependency: task-1 blocks task-2
      (org-gtd-dependencies-create task-1-id task-2-id)
      (setq project-marker (point-marker))
      (save-buffer))

    ;; Test DAG navigation using graph data directly
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           (buffer (get-buffer-create "*test-dag-navigation*")))
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--graph graph)

        ;; Root nodes should be task-1 and task-3
        (assert-true (member task-1-id (org-gtd-graph-root-ids graph)))
        (assert-true (member task-3-id (org-gtd-graph-root-ids graph)))

        ;; Start at task-1 (a root node)
        (setq org-gtd-graph-ui--selected-node-id task-1-id)

        ;; Get successors of task-1 - should be task-2
        (let ((task1-successors (org-gtd-graph-data-get-successors graph task-1-id)))
          (assert-equal 1 (length task1-successors))
          (assert-equal task-2-id (car task1-successors)))

        ;; Get successors of task-2 (leaf) - should be the project
        (let ((task2-successors (org-gtd-graph-data-get-successors graph task-2-id)))
          (assert-equal 1 (length task2-successors))
          (assert-equal project-id (car task2-successors)))

        ;; Project should have NO successors (it's the sink)
        (let ((project-successors (org-gtd-graph-data-get-successors graph project-id)))
          (assert-equal 0 (length project-successors)))))))

;;; 'p' Navigation Tests

(deftest graph-nav/p-navigates-to-blocking-task ()
  "'p' navigates to blocking task (predecessor)."
  (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
         project-marker project-id task-1-id task-2-id)
    ;; Create structure (project at bottom):
    ;; task-1 (root) -> task-2 (leaf) -> Project
    ;; task-2 also a first task but blocked by task-1
    (with-current-buffer (find-file-noselect tasks-file)
      (erase-buffer)
      (insert "* My Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Task 1\n")
      (org-back-to-heading t)
      (setq task-1-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Task 2\n")
      (org-back-to-heading t)
      (setq task-2-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      ;; Set first tasks
      (goto-char (point-min))
      (org-back-to-heading t)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (concat task-1-id " " task-2-id))
      ;; Create dependency: task-1 blocks task-2
      (org-gtd-dependencies-create task-1-id task-2-id)
      (setq project-marker (point-marker))
      (save-buffer))

    ;; Test navigation
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           (buffer (get-buffer-create "*test-p-navigation*")))
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--graph graph
              org-gtd-graph-ui--selected-node-id task-2-id)

        ;; Navigate up - should go to task-1 (the blocker)
        (org-gtd-graph-nav-previous)
        (assert-equal task-1-id org-gtd-graph-ui--selected-node-id)))))

(provide 'graph-navigation-buttercup-test)

;;; graph-navigation-buttercup-test.el ends here
