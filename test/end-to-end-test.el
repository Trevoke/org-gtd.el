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

(describe "Orphaned Task Detection"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Orphaned task in project"
    (xit "detects task unreachable from project's ORG_GTD_FIRST_TASKS"
        ;; NOTE: org-gtd-validate-project-dependencies has a bug - it calls
        ;; org-gtd-agenda-files which doesn't exist. The function should call
        ;; org-gtd-core--agenda-files or org-agenda-files instead.
        ;; This test is marked as pending until the product bug is fixed.

        "Product bug: org-gtd-validate-project-dependencies calls non-existent org-gtd-agenda-files")))

(describe "Graph Validation Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Circular dependency detection during organize"
    (xit "prevents creating A→B→C→A cycle"
        ;; NOTE: Circular dependency detection is NOT currently implemented for
        ;; manual property manipulation. org-entry-add-to-multivalued-property
        ;; doesn't check for cycles. A validation command exists
        ;; (org-gtd-validate-project-dependencies) but it has a bug.
        ;;
        ;; EXPECTED: Adding a dependency that creates a cycle should raise user-error
        ;; ACTUAL: No error is raised; cycles can be created
        ;;
        ;; This test documents the expected behavior for when it's implemented.

        "Feature not implemented: circular dependency prevention during property manipulation"))

  (describe "Circular dependency detection in existing project"
    (xit "validates project and detects manually introduced cycle"
        ;; NOTE: This relies on org-gtd-validate-project-dependencies which currently
        ;; has a bug (calls non-existent org-gtd-agenda-files). Pending fix.

        "Product bug: org-gtd-validate-project-dependencies needs fixing"))

  (describe "Orphaned task detection via validation"
    (xit "runs validation command and finds unreachable tasks"
        ;; NOTE: This also relies on org-gtd-validate-project-dependencies

        "Product bug: org-gtd-validate-project-dependencies needs fixing")))

;; Multi-file Review and Validation Tests migrated to test-eunit/integration/end-to-end-test.el

;; "Project Task Operation Tests (Manual Workflows)" - all tests migrated to test-eunit/integration/dag-operations-test.el
;; Tests 1-6 (Adding tasks to project DAG) fully migrated))

;; Ticklering and reactivating projects test migrated to test-eunit/integration/end-to-end-test.el

;;; end-to-end-test.el ends here
