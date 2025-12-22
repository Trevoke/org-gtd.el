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

(defun graph-nav-eunit--create-blocker-test-graph ()
  "Create a test graph for testing blocker-based sibling navigation.
Returns a graph with the following structure:
  A (root, layer 0)
  |
  +-> B (layer 1, blocked by A)
  |   +-> D (layer 2, blocked by B)
  |   +-> E (layer 2, blocked by B)
  |
  +-> C (layer 1, blocked by A)
  |   +-> F (layer 2, blocked by C)
  |
  +-> G (layer 2, blocked by A and B) - shares blocker B with D and E
  +-> H (layer 2, blocked by A) - shares blocker A with B and C"
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
    (puthash "G" (org-gtd-graph-node-create
                  :id "G" :title "Task G" :state "TODO" :layer 2)
             nodes)
    (puthash "H" (org-gtd-graph-node-create
                  :id "H" :title "Task H" :state "TODO" :layer 2)
             nodes)

    ;; Create edges
    (let ((edges (list
                  ;; A blocks B and C
                  (org-gtd-graph-edge-create :from-id "A" :to-id "B" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "A" :to-id "C" :type :blocks)
                  ;; B blocks D and E
                  (org-gtd-graph-edge-create :from-id "B" :to-id "D" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "B" :to-id "E" :type :blocks)
                  ;; C blocks F
                  (org-gtd-graph-edge-create :from-id "C" :to-id "F" :type :blocks)
                  ;; G is blocked by both A and B
                  (org-gtd-graph-edge-create :from-id "A" :to-id "G" :type :blocks)
                  (org-gtd-graph-edge-create :from-id "B" :to-id "G" :type :blocks)
                  ;; H is blocked only by A
                  (org-gtd-graph-edge-create :from-id "A" :to-id "H" :type :blocks))))

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
  "Moves to next node in same layer when mode is dag-level."
  ;; Layer 1 has B and C, so from B we should go to C
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*"))
         (org-gtd-graph-sibling-mode 'dag-level))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "B")
          (org-gtd-graph-nav-next-sibling)
          (assert-equal "C" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/next-sibling-noop-if-alone ()
  "Does nothing if only node in layer when mode is dag-level."
  ;; Layer 0 has only A
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*"))
         (org-gtd-graph-sibling-mode 'dag-level))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          (org-gtd-graph-nav-next-sibling)
          (assert-equal "A" org-gtd-graph-ui--selected-node-id))
      (kill-buffer buffer))))

(deftest graph-nav/previous-sibling-moves-in-layer ()
  "Moves to previous node in same layer when mode is dag-level."
  ;; Layer 1 has B and C, so from C we should go to B
  (let* ((graph (graph-nav-eunit--create-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*"))
         (org-gtd-graph-sibling-mode 'dag-level))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "C")
          (org-gtd-graph-nav-previous-sibling)
          (assert-equal "B" org-gtd-graph-ui--selected-node-id))
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

;;; Sibling Mode Configuration Tests

(deftest graph-nav/sibling-mode-defcustom-exists ()
  "Customization variable org-gtd-graph-sibling-mode exists."
  (assert-true (boundp 'org-gtd-graph-sibling-mode)))

(deftest graph-nav/sibling-mode-default-value ()
  "Default value for org-gtd-graph-sibling-mode is any-same-blocker."
  (assert-equal 'any-same-blocker org-gtd-graph-sibling-mode))

;;; Any-Same-Blocker Sibling Tests

(deftest graph-nav/any-same-blocker-finds-siblings-sharing-one-blocker ()
  "Tasks D and E share blocker B, so they are siblings."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "D")
          (let ((siblings (org-gtd-graph-nav--get-siblings-any-same-blocker)))
            ;; D's blockers: [B]
            ;; E's blockers: [B] - shares B, so is a sibling
            ;; G's blockers: [A, B] - shares B, so is a sibling
            ;; BFS traversal order should include E before G
            ;; Verify that both E and G are in the results
            (assert-true (member "E" siblings))
            (assert-true (member "G" siblings))
            (assert-equal 2 (length siblings))))
      (kill-buffer buffer))))

(deftest graph-nav/any-same-blocker-returns-nil-when-no-shared-blockers ()
  "Task F has blocker C, which D doesn't share."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "F")
          (let ((siblings (org-gtd-graph-nav--get-siblings-any-same-blocker)))
            ;; F's blockers: [C]
            ;; No other node shares blocker C
            (assert-equal nil siblings)))
      (kill-buffer buffer))))

(deftest graph-nav/any-same-blocker-returns-nil-when-no-blockers ()
  "Root node A has no blockers."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "A")
          (let ((siblings (org-gtd-graph-nav--get-siblings-any-same-blocker)))
            ;; A has no blockers, so no siblings
            (assert-equal nil siblings)))
      (kill-buffer buffer))))

;;; All-Same-Blockers Sibling Tests

(deftest graph-nav/all-same-blockers-finds-exact-match ()
  "Tasks D and E both have exactly blockers [B], so they are siblings."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "D")
          (let ((siblings (org-gtd-graph-nav--get-siblings-all-same-blockers)))
            ;; D's blockers: [B]
            ;; E's blockers: [B] - exact match, is a sibling
            ;; G's blockers: [A, B] - NOT exact match, not a sibling
            ;; Should return only E
            (assert-true (member "E" siblings))
            (assert-equal 1 (length siblings))))
      (kill-buffer buffer))))

(deftest graph-nav/all-same-blockers-excludes-superset ()
  "Task G has blockers [A, B], which is a superset of D's [B]."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "G")
          (let ((siblings (org-gtd-graph-nav--get-siblings-all-same-blockers)))
            ;; G's blockers: [A, B]
            ;; No other node has exactly [A, B]
            (assert-equal nil siblings)))
      (kill-buffer buffer))))

(deftest graph-nav/all-same-blockers-finds-multiple-exact-matches ()
  "Tasks B and C both have exactly blockers [A]."
  (let* ((graph (graph-nav-eunit--create-blocker-test-graph))
         (buffer (get-buffer-create "*test-graph-nav*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-gtd-graph-view-mode)
          (setq org-gtd-graph-view--graph graph
                org-gtd-graph-ui--selected-node-id "B")
          (let ((siblings (org-gtd-graph-nav--get-siblings-all-same-blockers)))
            ;; B's blockers: [A]
            ;; C's blockers: [A] - exact match
            ;; H's blockers: [A] - exact match
            ;; Should return C and H
            (assert-true (member "C" siblings))
            (assert-true (member "H" siblings))
            (assert-equal 2 (length siblings))))
      (kill-buffer buffer))))

(provide 'graph-navigation-test)

;;; graph-navigation-test.el ends here
