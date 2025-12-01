;;; accessors-test.el --- Unit tests for accessor layer -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; All tests in this file have been migrated to test-eunit/unit/accessors-test.el
;;
;; Test Coverage (now in e-unit):
;; - Property readers (get dependencies, blockers, projects, state, category)
;; - Property writers (set state, add/remove multivalued properties)
;; - Task lookup (find task markers)
;; - Task state predicates (is-done-p, is-active-p)
;;

;;; Code:

;; All tests migrated to test-eunit/unit/accessors-test.el (31 tests total):
;;
;; Property Readers (12 tests):
;; - org-gtd-get-task-dependencies (4 tests)
;; - org-gtd-get-task-blockers (2 tests)
;; - org-gtd-get-task-state (4 tests)
;; - org-gtd-get-task-category (2 tests)
;;
;; Property Writers (8 tests):
;; - org-gtd-set-task-state (3 tests)
;; - org-gtd-add-to-multivalued-property (3 tests)
;; - org-gtd-remove-from-multivalued-property (2 tests)
;;
;; Task Lookup (2 tests):
;; - org-gtd-find-task-in-current-buffer (2 tests)
;;
;; Task State Predicates (9 tests):
;; - org-gtd-task-is-done-p (5 tests)
;; - org-gtd-task-is-active-p (4 tests)

(provide 'accessors-test)

;;; accessors-test.el ends here
