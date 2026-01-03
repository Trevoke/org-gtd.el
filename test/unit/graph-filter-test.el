;;; graph-filter-test.el --- Unit tests for graph filtering -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-filter filtering and zoom functionality.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/org-gtd-graph-filter-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-filter)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Test Fixtures

(defun graph-filter-eunit--create-test-graph ()
  "Create a test graph with various node properties for filtering tests."
  (let* ((graph (org-gtd-graph-create
                 :project-id "proj-1"
                 :project-name "Test Project"))
         ;; Compute dynamic dates relative to today
         (today (format-time-string "%Y-%m-%d"))
         (today-time (org-time-string-to-time today))
         (tomorrow (format-time-string "%Y-%m-%d"
                                       (time-add today-time (days-to-time 1))))
         (two-days-future (format-time-string "%Y-%m-%d"
                                              (time-add today-time (days-to-time 2))))
         (five-days-past (format-time-string "%Y-%m-%d"
                                             (time-subtract today-time (days-to-time 5)))))

    ;; Add nodes with different properties
    (puthash "node-1" (org-gtd-graph-node-create
                       :id "node-1"
                       :title "Task 1"
                       :state "TODO"
                       :category "Actions"
                       :priority "A"
                       :tags '("work" "urgent")
                       :scheduled tomorrow
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-2" (org-gtd-graph-node-create
                       :id "node-2"
                       :title "Task 2"
                       :state "NEXT"
                       :category "Actions"
                       :priority "B"
                       :tags '("personal")
                       :scheduled today
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-3" (org-gtd-graph-node-create
                       :id "node-3"
                       :title "Task 3"
                       :state "DONE"
                       :category "Actions"
                       :priority "C"
                       :tags '("work")
                       :scheduled two-days-future
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-4" (org-gtd-graph-node-create
                       :id "node-4"
                       :title "Task 4"
                       :state "TODO"
                       :category "Actions"
                       :priority nil
                       :tags nil
                       :scheduled nil
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-5" (org-gtd-graph-node-create
                       :id "node-5"
                       :title "Task 5"
                       :state "WAIT"
                       :category "Actions"
                       :priority "A"
                       :tags '("urgent")
                       :scheduled five-days-past
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    ;; Add edges to create hierarchy: node-1 -> node-2 -> node-3
    ;;                                  node-1 -> node-4
    (setf (org-gtd-graph-edges graph)
          (list (org-gtd-graph-edge-create :from-id "node-1" :to-id "node-2" :type :blocks)
                (org-gtd-graph-edge-create :from-id "node-2" :to-id "node-3" :type :blocks)
                (org-gtd-graph-edge-create :from-id "node-1" :to-id "node-4" :type :blocks)))

    (setf (org-gtd-graph-root-ids graph) '("node-1" "node-5"))

    graph))

;;; Filter Creation Tests

(deftest graph-filter/create-empty-filter ()
  "Creates empty filter with all fields nil."
  (let ((filter (org-gtd-graph-filter-create)))
    (assert-true (org-gtd-graph-filter-p filter))
    (assert-nil (org-gtd-graph-filter-todo-states filter))
    (assert-nil (org-gtd-graph-filter-priorities filter))
    (assert-nil (org-gtd-graph-filter-tags filter))
    (assert-nil (org-gtd-graph-filter-scheduled filter))))

;;; Filter by TODO State Tests

(deftest graph-filter/todo-state-single ()
  "Filters by single TODO state."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :todo-states '("TODO")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 2 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-4" visible-ids))))

(deftest graph-filter/todo-state-multiple ()
  "Filters by multiple TODO states."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :todo-states '("TODO" "NEXT")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 3 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-2" visible-ids))
    (assert-true (member "node-4" visible-ids))))

(deftest graph-filter/todo-state-nil-returns-all ()
  "Returns all nodes when todo-states is nil."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 5 (length visible-ids))))

;;; Filter by Priority Tests

(deftest graph-filter/priority-single ()
  "Filters by single priority."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :priorities '("A")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 2 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-5" visible-ids))))

(deftest graph-filter/priority-multiple ()
  "Filters by multiple priorities."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :priorities '("A" "B")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 3 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-2" visible-ids))
    (assert-true (member "node-5" visible-ids))))

(deftest graph-filter/priority-nil-returns-all ()
  "Returns all nodes when priorities is nil."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 5 (length visible-ids))))

;;; Filter by Tags Tests

(deftest graph-filter/tags-single ()
  "Filters by single tag."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :tags '("work")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 2 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-3" visible-ids))))

(deftest graph-filter/tags-multiple-or-logic ()
  "Filters by multiple tags (OR logic)."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :tags '("work" "personal")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 3 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-2" visible-ids))
    (assert-true (member "node-3" visible-ids))))

(deftest graph-filter/tags-nil-returns-all ()
  "Returns all nodes when tags is nil."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 5 (length visible-ids))))

;;; Filter by Scheduled Date Tests

(deftest graph-filter/scheduled-overdue ()
  "Filters overdue tasks."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :scheduled 'overdue))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 1 (length visible-ids))
    (assert-true (member "node-5" visible-ids))))

(deftest graph-filter/scheduled-today ()
  "Filters tasks scheduled for today."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :scheduled 'today))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 1 (length visible-ids))
    (assert-true (member "node-2" visible-ids))))

(deftest graph-filter/scheduled-week ()
  "Filters tasks scheduled within a week."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :scheduled 'week))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 3 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-2" visible-ids))
    (assert-true (member "node-3" visible-ids))))

(deftest graph-filter/scheduled-unscheduled ()
  "Filters unscheduled tasks."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create :scheduled 'unscheduled))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 1 (length visible-ids))
    (assert-true (member "node-4" visible-ids))))

(deftest graph-filter/scheduled-nil-returns-all ()
  "Returns all nodes when scheduled is nil."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 5 (length visible-ids))))

;;; Multiple Filters Tests

(deftest graph-filter/multiple-todo-and-priority ()
  "Combines TODO state and priority filters (AND logic)."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create
                  :todo-states '("TODO")
                  :priorities '("A")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 1 (length visible-ids))
    (assert-true (member "node-1" visible-ids))))

(deftest graph-filter/multiple-todo-priority-tags ()
  "Combines TODO state, priority, and tags filters."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create
                  :todo-states '("TODO" "WAIT")
                  :priorities '("A")
                  :tags '("urgent")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 2 (length visible-ids))
    (assert-true (member "node-1" visible-ids))
    (assert-true (member "node-5" visible-ids))))

(deftest graph-filter/multiple-no-match-returns-empty ()
  "Returns empty list when no nodes match all filters."
  (let* ((graph (graph-filter-eunit--create-test-graph))
         (filter (org-gtd-graph-filter-create
                  :todo-states '("DONE")
                  :priorities '("A")))
         (visible-ids (org-gtd-graph-filter-apply graph filter)))
    (assert-equal 0 (length visible-ids))))

(provide 'graph-filter-test)

;;; graph-filter-test.el ends here
