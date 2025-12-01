;; -*- lexical-binding: t; coding: utf-8 -*-

;;; org-gtd-graph-navigation-test.el --- Unit tests for graph navigation -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-navigation keyboard navigation functions.
;;
;; Test Coverage:
;; - BFS traversal order calculation
;; - Sequential navigation (next/previous)
;; - Layer-based navigation (siblings, first/last in layer)
;; - Dependency-based navigation (up/down chains)
;; - Navigation history (back/forward)
;; - Goto with completion
;;

;;; Code:

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-navigation)
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'with-simulated-input)

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

;; Pure unit tests migrated to test-eunit/unit/graph-navigation-test.el:
;; - BFS Traversal Order Tests (1 test)
;; - Sequential Navigation Tests (3 tests)
;; - Layer-Based Navigation Tests (5 tests)
;; - Dependency-Based Navigation Tests (4 tests)

;;;; Goto Navigation Tests

(describe "org-gtd-graph-nav-goto"

  (it "selects node by title with completion"
    (let* ((graph (org-gtd-graph-navigation-test--create-test-graph))
           (buffer (get-buffer-create "*test-graph-nav*")))
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--graph graph
              org-gtd-graph-ui--selected-node-id "A")
        ;; Simulate selecting "Task D"
        (with-simulated-input "Task C-q SPC D RET"
          (org-gtd-graph-nav-goto))
        (expect org-gtd-graph-ui--selected-node-id :to-equal "D")))))

;;;; Project Heading Integration Tests

(describe "Project heading as graph node"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "project heading is included as a node in the graph"
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
        (expect project-node :not :to-be nil)
        (expect (org-gtd-graph-node-title project-node) :to-equal "My Project"))))

  (it "project heading receives edges from leaf tasks (project at bottom)"
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
        (expect (length predecessors) :to-equal 2)
        (expect (member task-1-id predecessors) :to-be-truthy)
        (expect (member task-2-id predecessors) :to-be-truthy)
        ;; Project should NOT have outgoing edges
        (expect (length successors) :to-equal 0)))))

(describe "Initial node selection in graph view"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "initial selected node should be the first task (project at bottom)"
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
        (expect (length root-ids) :to-equal 1)
        (expect (car root-ids) :to-equal task-1-id)
        ;; Project should NOT be a root
        (expect (member project-id root-ids) :to-be nil)))))

(describe "DAG-based navigation with 'n' key"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "'n' navigates to first child (successor) in DAG, not BFS order"
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
          (expect (org-gtd-graph-root-ids graph) :to-contain task-1-id)
          (expect (org-gtd-graph-root-ids graph) :to-contain task-3-id)

          ;; Start at task-1 (a root node)
          (setq org-gtd-graph-ui--selected-node-id task-1-id)

          ;; Get successors of task-1 - should be task-2
          (let ((task1-successors (org-gtd-graph-data-get-successors graph task-1-id)))
            (expect (length task1-successors) :to-equal 1)
            (expect (car task1-successors) :to-equal task-2-id))

          ;; Get successors of task-2 (leaf) - should be the project
          (let ((task2-successors (org-gtd-graph-data-get-successors graph task-2-id)))
            (expect (length task2-successors) :to-equal 1)
            (expect (car task2-successors) :to-equal project-id))

          ;; Project should have NO successors (it's the sink)
          (let ((project-successors (org-gtd-graph-data-get-successors graph project-id)))
            (expect (length project-successors) :to-equal 0)))))))

(describe "'p' navigation moves up dependency chain"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "'p' navigates to blocking task (predecessor)"
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
          (expect org-gtd-graph-ui--selected-node-id :to-equal task-1-id))))))

(provide 'org-gtd-graph-navigation-test)

;;; org-gtd-graph-navigation-test.el ends here
