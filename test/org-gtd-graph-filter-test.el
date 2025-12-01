;; -*- lexical-binding: t; coding: utf-8 -*-

;; All tests migrated to test-eunit/unit/graph-filter-test.el:
;;
;; Filter Creation (1 test):
;; - graph-filter/create-empty-filter
;;
;; Filter by TODO State (3 tests):
;; - graph-filter/todo-state-single
;; - graph-filter/todo-state-multiple
;; - graph-filter/todo-state-nil-returns-all
;;
;; Filter by Priority (3 tests):
;; - graph-filter/priority-single
;; - graph-filter/priority-multiple
;; - graph-filter/priority-nil-returns-all
;;
;; Filter by Tags (3 tests):
;; - graph-filter/tags-single
;; - graph-filter/tags-multiple-or-logic
;; - graph-filter/tags-nil-returns-all
;;
;; Filter by Scheduled Date (5 tests):
;; - graph-filter/scheduled-overdue
;; - graph-filter/scheduled-today
;; - graph-filter/scheduled-week
;; - graph-filter/scheduled-unscheduled
;; - graph-filter/scheduled-nil-returns-all
;;
;; Multiple Filters (3 tests):
;; - graph-filter/multiple-todo-and-priority
;; - graph-filter/multiple-todo-priority-tags
;; - graph-filter/multiple-no-match-returns-empty
;;
;; Zoom Tests (4 tests):
;; - graph-filter/zoom-subtree
;; - graph-filter/zoom-leaf-node
;; - graph-filter/zoom-nil-shows-all
;; - graph-filter/zoom-combined-with-filters
;;
;; This file is kept for documentation. The tests now run via e-unit.
