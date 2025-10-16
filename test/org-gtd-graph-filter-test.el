;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-filter-test.el --- Unit tests for graph filtering -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-filter filtering and zoom functionality.
;;
;; Test Coverage:
;; - Filter by TODO state (single and multiple)
;; - Filter by priority (single and multiple)
;; - Filter by tags (single and multiple)
;; - Filter by scheduled date (overdue, today, week, unscheduled)
;; - Multiple filters combining with AND logic
;; - Zoom to subtree (show descendants only)
;; - Zoom out (return to full view)
;;

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-filter)

;;;; Test Fixtures

(defun org-gtd-graph-filter-test--create-test-graph ()
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
                       :scheduled tomorrow  ; future (tomorrow)
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-2" (org-gtd-graph-node-create
                       :id "node-2"
                       :title "Task 2"
                       :state "NEXT"
                       :category "Actions"
                       :priority "B"
                       :tags '("personal")
                       :scheduled today  ; today
                       :deadline nil)
             (org-gtd-graph-nodes graph))

    (puthash "node-3" (org-gtd-graph-node-create
                       :id "node-3"
                       :title "Task 3"
                       :state "DONE"
                       :category "Actions"
                       :priority "C"
                       :tags '("work")
                       :scheduled two-days-future  ; future (2 days from now)
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
                       :scheduled five-days-past  ; overdue (5 days ago)
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

;;;; Filter Creation Tests

(describe "org-gtd-graph-filter-create"

  (it "creates empty filter with all fields nil"
    (let ((filter (org-gtd-graph-filter-create)))
      (expect (org-gtd-graph-filter-p filter) :to-be-truthy)
      (expect (org-gtd-graph-filter-todo-states filter) :to-be nil)
      (expect (org-gtd-graph-filter-priorities filter) :to-be nil)
      (expect (org-gtd-graph-filter-tags filter) :to-be nil)
      (expect (org-gtd-graph-filter-scheduled filter) :to-be nil)
      (expect (org-gtd-graph-filter-zoom-node-id filter) :to-be nil))))

;;;; Filter by TODO State Tests

(describe "org-gtd-graph-filter-apply with TODO state filter"

  (it "filters by single TODO state"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :todo-states '("TODO")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 2)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-4" visible-ids) :to-be-truthy)))

  (it "filters by multiple TODO states"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :todo-states '("TODO" "NEXT")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 3)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-2" visible-ids) :to-be-truthy)
      (expect (member "node-4" visible-ids) :to-be-truthy)))

  (it "returns all nodes when todo-states is nil"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 5))))

;;;; Filter by Priority Tests

(describe "org-gtd-graph-filter-apply with priority filter"

  (it "filters by single priority"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :priorities '("A")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 2)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-5" visible-ids) :to-be-truthy)))

  (it "filters by multiple priorities"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :priorities '("A" "B")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 3)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-2" visible-ids) :to-be-truthy)
      (expect (member "node-5" visible-ids) :to-be-truthy)))

  (it "returns all nodes when priorities is nil"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 5))))

;;;; Filter by Tags Tests

(describe "org-gtd-graph-filter-apply with tags filter"

  (it "filters by single tag"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :tags '("work")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 2)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-3" visible-ids) :to-be-truthy)))

  (it "filters by multiple tags (OR logic)"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :tags '("work" "personal")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 3)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-2" visible-ids) :to-be-truthy)
      (expect (member "node-3" visible-ids) :to-be-truthy)))

  (it "returns all nodes when tags is nil"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 5))))

;;;; Filter by Scheduled Date Tests

(describe "org-gtd-graph-filter-apply with scheduled filter"

  (it "filters overdue tasks"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :scheduled 'overdue))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 1)
      (expect (member "node-5" visible-ids) :to-be-truthy)))

  (it "filters tasks scheduled for today"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :scheduled 'today))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 1)
      (expect (member "node-2" visible-ids) :to-be-truthy)))

  (it "filters tasks scheduled within a week"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :scheduled 'week))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 3)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-2" visible-ids) :to-be-truthy)
      (expect (member "node-3" visible-ids) :to-be-truthy)))

  (it "filters unscheduled tasks"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :scheduled 'unscheduled))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 1)
      (expect (member "node-4" visible-ids) :to-be-truthy)))

  (it "returns all nodes when scheduled is nil"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 5))))

;;;; Multiple Filters Tests

(describe "org-gtd-graph-filter-apply with multiple filters"

  (it "combines TODO state and priority filters (AND logic)"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create
                    :todo-states '("TODO")
                    :priorities '("A")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 1)
      (expect (member "node-1" visible-ids) :to-be-truthy)))

  (it "combines TODO state, priority, and tags filters"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create
                    :todo-states '("TODO" "WAIT")
                    :priorities '("A")
                    :tags '("urgent")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 2)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-5" visible-ids) :to-be-truthy)))

  (it "returns empty list when no nodes match all filters"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create
                    :todo-states '("DONE")
                    :priorities '("A")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 0))))

;;;; Zoom Tests

(describe "org-gtd-graph-filter-apply with zoom"

  (it "zoom to subtree shows only node and descendants"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :zoom-node-id "node-1"))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      ;; node-1 and all its descendants: node-2, node-3, node-4
      (expect (length visible-ids) :to-equal 4)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-2" visible-ids) :to-be-truthy)
      (expect (member "node-3" visible-ids) :to-be-truthy)
      (expect (member "node-4" visible-ids) :to-be-truthy)
      (expect (member "node-5" visible-ids) :not :to-be-truthy)))

  (it "zoom to leaf node shows only that node"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :zoom-node-id "node-3"))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 1)
      (expect (member "node-3" visible-ids) :to-be-truthy)))

  (it "zoom with nil node-id shows all nodes"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create :zoom-node-id nil))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      (expect (length visible-ids) :to-equal 5)))

  (it "combines zoom with other filters"
    (let* ((graph (org-gtd-graph-filter-test--create-test-graph))
           (filter (org-gtd-graph-filter-create
                    :zoom-node-id "node-1"
                    :todo-states '("TODO")))
           (visible-ids (org-gtd-graph-filter-apply graph filter)))
      ;; Zoomed to node-1 subtree, but only TODO tasks
      (expect (length visible-ids) :to-equal 2)
      (expect (member "node-1" visible-ids) :to-be-truthy)
      (expect (member "node-4" visible-ids) :to-be-truthy))))

(provide 'org-gtd-graph-filter-test)

;;; org-gtd-graph-filter-test.el ends here
