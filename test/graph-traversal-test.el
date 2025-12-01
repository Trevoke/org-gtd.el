;; -*- lexical-binding: t; coding: utf-8 -*-

;;; graph-traversal-test.el --- Tests for project task graph traversal -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; ALL TESTS MIGRATED to test-eunit/unit/graph-traversal-test.el:
;;
;; org-gtd-projects--collect-tasks-by-graph function tests:
;; - exists and can be called
;; - collects tasks by following ID dependency chains from FIRST_TASKS
;; - handles circular dependencies gracefully
;; - returns empty list when FIRST_TASKS is empty or missing
;; - uses ORG_GTD_FIRST_TASKS instead of FIRST_TASKS for graph traversal
;;
;; Multi-project task graph boundaries tests:
;; - respects project boundaries when traversing shared tasks (acceptance test)
;; - excludes tasks that don't have current project ID in ORG_GTD_PROJECT_IDS
;; - includes shared tasks when project ID matches
;;

;;; Code:

;; This file is kept as a placeholder. All tests have been migrated to e-unit.

;;; graph-traversal-test.el ends here
