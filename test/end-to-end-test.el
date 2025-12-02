;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

;; Basic workflow tests migrated to test-eunit/acceptance/basic-workflows-test.el
;; Cancel and archive tests migrated to test-eunit/acceptance/cancel-archive-test.el
;; Review flow tests migrated to test-eunit/acceptance/review-flow-test.el (including multi-file)
;; Habit flow tests migrated to test-eunit/acceptance/habit-flow-test.el
;; Multi-file DAG tests migrated to test-eunit/acceptance/multi-file-dag-test.el
;; NOTE: The buttercup tests had incomplete setup - they didn't set ORG_GTD_PROJECT_IDS
;;       on secondary file tasks. The e-unit versions properly set up bi-directional
;;       relationships so archiving works correctly across files.
;; Advanced project task operations migrated to test-eunit/acceptance/project-task-operations-test.el
;; (Only active tests migrated; xit tests for unimplemented features not migrated)
;; Multi-project task sharing tests migrated to test-eunit/acceptance/multi-project-sharing-test.el
;; Project cancellation and archive tests migrated to test-eunit/acceptance/project-cancellation-test.el
;; Ticklering and reactivating projects migrated to test-eunit/acceptance/tickler-flow-test.el
;; Area of focus review with tickler projects migrated to test-eunit/acceptance/review-flow-test.el

;; Orphaned Task Detection tests removed - bug was fixed, equivalent tests exist:
;; - test-eunit/integration/task-management-commands-test.el: task-mgmt-int/detects-orphaned-tasks

;; Graph Validation Tests - all removed, covered by e-unit:
;; - Circular dependency prevention: org-gtd-task-add-blocker/successor check before adding
;; - Cycle detection: org-gtd-graph-data--find-cycle
;; - Orphaned task detection: task-mgmt-int/detects-orphaned-tasks

;; Multi-file Review and Validation Tests migrated to test-eunit/integration/end-to-end-test.el

;; "Project Task Operation Tests (Manual Workflows)" - all tests migrated to test-eunit/integration/dag-operations-test.el
;; Tests 1-6 (Adding tasks to project DAG) fully migrated))

;; Ticklering and reactivating projects test migrated to test-eunit/integration/end-to-end-test.el

;;; end-to-end-test.el ends here
