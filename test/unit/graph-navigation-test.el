;;; graph-navigation-test.el --- Unit tests for graph navigation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-navigation keyboard navigation functions.
;; These are pure unit tests using in-memory graphs.
;;
;; Migrated from test/org-gtd-graph-navigation-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-navigation)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Test Fixtures

(defun graph-nav-eunit--create-test-graph ()
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

;;; BFS Traversal Order Tests

(deftest graph-nav/bfs-traversal-order ()
  "Returns nodes in breadth-first traversal order."
  ;; Expected BFS order: A, B, C, D, E, F
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph)
          (let ((order (org-gtd-graph-nav--get-traversal-order)))
            (assert-equal '("A" "B" "C" "D" "E" "F") order)))
      (kill-buffer buffer))))

;;; Sequential Navigation Tests

(deftest graph-nav/next-moves-to-next-node ()
  "Moves to next node in traversal order."
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          ;; Move to next node
          (org-gtd-graph-nav-next)
          ;; Should now be at B
          (assert-equal "B" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/next-updates-details-panel ()
  "Updates details panel when navigating."
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*"))
         (update-called nil))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          ;; Mock update-details to verify it's called
          (cl-letf (((symbol-function 'org-gtd-graph-ui-update-details)
                     (lambda () (setq update-called t))))
            (org-gtd-graph-nav-next))
          (assert-true update-called))
      (kill-buffer buffer))))

(deftest graph-nav/previous-moves-to-previous-node ()
  "Moves to previous node in traversal order."
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "B")
          ;; Move to previous node
          (org-gtd-graph-nav-previous)
          ;; Should now be at A
          (assert-equal "A" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

;;; Layer-Based Navigation Tests

(deftest graph-nav/next-sibling-moves-in-layer ()
  "Moves to next node in same layer."
  ;; Layer 1 has B and C, so from B we should go to C
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "B")
          (org-gtd-graph-nav-next-sibling)
          (assert-equal "C" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/next-sibling-noop-if-alone ()
  "Does nothing if only node in layer."
  ;; Layer 0 has only A
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          (org-gtd-graph-nav-next-sibling)
          (assert-equal "A" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/previous-sibling-moves-in-layer ()
  "Moves to previous node in same layer."
  ;; Layer 1 has B and C, so from C we should go to B
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "C")
          (org-gtd-graph-nav-previous-sibling)
          (assert-equal "B" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/first-in-layer-jumps-to-first ()
  "Jumps to first node in current layer."
  ;; Layer 2 has D, E, F in BFS order, so from F we should go to D
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "F")
          (org-gtd-graph-nav-first-in-layer)
          (assert-equal "D" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/last-in-layer-jumps-to-last ()
  "Jumps to last node in current layer."
  ;; Layer 2 has D, E, F in BFS order, so from D we should go to F
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "D")
          (org-gtd-graph-nav-last-in-layer)
          (assert-equal "F" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

;;; Dependency-Based Navigation Tests

(deftest graph-nav/down-dependency-navigates-to-successor ()
  "Navigates down the dependency chain to blocked task."
  ;; A blocks B and C, so from A we should go to B (first successor)
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          (org-gtd-graph-nav-down-dependency)
          (assert-equal "B" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/down-dependency-noop-if-no-successors ()
  "Does nothing if no successors."
  ;; F has no successors
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "F")
          (org-gtd-graph-nav-down-dependency)
          (assert-equal "F" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/up-dependency-navigates-to-predecessor ()
  "Navigates up the dependency chain to blocking task."
  ;; B is blocked by A, so from B we should go to A
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "B")
          (org-gtd-graph-nav-up-dependency)
          (assert-equal "A" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/up-dependency-noop-if-no-predecessors ()
  "Does nothing if no predecessors."
  ;; A has no predecessors (it's a root)
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          (org-gtd-graph-nav-up-dependency)
          (assert-equal "A" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(provide 'graph-navigation-test)

;;; graph-navigation-test.el ends here
