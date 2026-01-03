;;; graph-data-validation-test.el --- Unit tests for graph data validation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-data validation functions.
;; Tests edge filtering for multi-project tasks with cross-project dependencies.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-graph-data)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Test Fixtures

(defun graph-data-validation-test--create-graph-with-invalid-edges ()
  "Create a test graph with edges pointing to non-existent nodes.
This simulates multi-project tasks with cross-project dependencies."
  (let ((graph (org-gtd-graph-create
                :project-id "proj-1"
                :project-name "Test Project")))

    ;; Add nodes: proj-1, task-a, task-b (but NOT external-task)
    (puthash "proj-1" (org-gtd-graph-node-create
                       :id "proj-1"
                       :title "Test Project"
                       :state nil
                       :category "Projects")
             (org-gtd-graph-nodes graph))

    (puthash "task-a" (org-gtd-graph-node-create
                       :id "task-a"
                       :title "Task A"
                       :state "TODO"
                       :category "Actions")
             (org-gtd-graph-nodes graph))

    (puthash "task-b" (org-gtd-graph-node-create
                       :id "task-b"
                       :title "Task B"
                       :state "TODO"
                       :category "Actions")
             (org-gtd-graph-nodes graph))

    ;; Add edges - including one to non-existent node "external-task"
    (setf (org-gtd-graph-edges graph)
          (list
           ;; Valid edge: task-a blocks task-b (both exist)
           (org-gtd-graph-edge-create :from-id "task-a" :to-id "task-b" :type :blocks)
           ;; Invalid edge: task-b blocks external-task (external-task doesn't exist)
           (org-gtd-graph-edge-create :from-id "task-b" :to-id "external-task" :type :blocks)
           ;; Invalid edge: external-task-2 blocks task-a (external-task-2 doesn't exist)
           (org-gtd-graph-edge-create :from-id "external-task-2" :to-id "task-a" :type :blocks)
           ;; Valid edge: task-b blocks proj-1 (leaf edge)
           (org-gtd-graph-edge-create :from-id "task-b" :to-id "proj-1" :type :blocks)))

    graph))

;;; Tests

(deftest graph-data-validation/filter-removes-edges-to-missing-nodes ()
  "Filter removes edges where target node doesn't exist."
  (let ((graph (graph-data-validation-test--create-graph-with-invalid-edges)))
    ;; Before filtering: 4 edges (2 valid, 2 invalid)
    (assert-equal 4 (length (org-gtd-graph-edges graph)))

    ;; Apply filter
    (org-gtd-graph-data--filter-invalid-edges graph)

    ;; After filtering: only 2 valid edges remain
    (assert-equal 2 (length (org-gtd-graph-edges graph)))))

(deftest graph-data-validation/filter-preserves-valid-edges ()
  "Filter preserves edges where both endpoints exist."
  (let ((graph (graph-data-validation-test--create-graph-with-invalid-edges)))
    (org-gtd-graph-data--filter-invalid-edges graph)

    ;; Check that valid edges are preserved
    (let ((edges (org-gtd-graph-edges graph)))
      ;; Should have task-a -> task-b edge
      (assert-true (cl-some (lambda (e)
                              (and (string= (org-gtd-graph-edge-from-id e) "task-a")
                                   (string= (org-gtd-graph-edge-to-id e) "task-b")))
                            edges))
      ;; Should have task-b -> proj-1 edge
      (assert-true (cl-some (lambda (e)
                              (and (string= (org-gtd-graph-edge-from-id e) "task-b")
                                   (string= (org-gtd-graph-edge-to-id e) "proj-1")))
                            edges)))))

(deftest graph-data-validation/filter-removes-edges-from-missing-nodes ()
  "Filter removes edges where source node doesn't exist."
  (let ((graph (graph-data-validation-test--create-graph-with-invalid-edges)))
    (org-gtd-graph-data--filter-invalid-edges graph)

    ;; Should NOT have external-task-2 -> task-a edge
    (let ((edges (org-gtd-graph-edges graph)))
      (assert-false (cl-some (lambda (e)
                               (string= (org-gtd-graph-edge-from-id e) "external-task-2"))
                             edges)))))

(deftest graph-data-validation/validate-reports-invalid-edges ()
  "Validation reports edges referencing non-existent nodes."
  (let* ((graph (graph-data-validation-test--create-graph-with-invalid-edges))
         (issues (org-gtd-graph-data-validate graph)))
    ;; Should report issues for invalid edges
    (assert-true (> (length issues) 0))
    ;; Should mention "non-existent" in the issues
    (assert-true (cl-some (lambda (issue)
                            (string-match-p "non-existent" issue))
                          issues))))

(deftest graph-data-validation/validate-clean-graph-has-no-issues ()
  "Validation returns nil for a graph with only valid edges."
  (let ((graph (graph-data-validation-test--create-graph-with-invalid-edges)))
    ;; Filter out invalid edges first
    (org-gtd-graph-data--filter-invalid-edges graph)
    ;; Now validation should pass (no invalid edge issues)
    (let ((issues (org-gtd-graph-data-validate graph)))
      ;; Filter out non-edge issues (orphans, cycles) - we only care about edge issues here
      (let ((edge-issues (seq-filter (lambda (i) (string-match-p "Edge" i)) issues)))
        (assert-equal 0 (length edge-issues))))))

(provide 'graph-data-validation-test)
;;; graph-data-validation-test.el ends here
