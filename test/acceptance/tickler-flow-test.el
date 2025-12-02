;;; tickler-flow-test.el --- Acceptance tests for tickler workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for GTD tickler workflows including:
;; - Ticklering projects (moving to incubation)
;; - Reactivating ticklered projects
;; - State preservation across tickler/reactivation cycle
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Ticklering and Reactivating Projects

(deftest tickler-reactivation-cycle-preserves-project-state ()
  "Verifies full incubation -> reactivation cycle preserves project state."
  ;; 1. CREATE a project with dependencies
  (create-project "Future project")

  ;; 2. VERIFY initial state: Task 1 is NEXT
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO")))

  ;; 3. TICKLER the project (move to incubation)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (org-gtd-tickler "2025-12-01"))

  ;; 4. VERIFY project is ticklered
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "<2025-12-01>" (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

  ;; 5. VERIFY tasks are ticklered (no TODO keywords, previous state saved)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "TODO"))
    (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO")))

  ;; 6. VERIFY ticklered tasks don't appear in engage view
  (org-gtd-engage)
  (refute-match "Task 1" (agenda-raw-text))

  ;; 7. REACTIVATE the project
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (org-gtd-reactivate))

  ;; 8. VERIFY project is reactivated
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

  ;; 9. VERIFY tasks are reactivated with TODO keywords restored
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "NEXT" (org-entry-get (point) "TODO")))

  ;; 10. VERIFY reactivated tasks appear in engage view again
  (org-gtd-engage)
  (assert-match "Task 1" (agenda-raw-text)))

;;; tickler-flow-test.el ends here
