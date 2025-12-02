;;; project-tickler-test.el --- Unit tests for project tickler state preservation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project tickler functionality including state preservation,
;; task collection, incubation and reactivation.
;;
;; Migrated from test/project-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; State Preservation for Tickler

(deftest tickler/saves-state-to-previous-properties ()
  "Saves ORG_GTD and TODO state to PREVIOUS_* properties."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)

    ;; Set up initial state
    (org-entry-put (point) "ORG_GTD" "Actions")
    (org-entry-put (point) "TODO" "NEXT")

    ;; Save state
    (org-gtd-project--save-state (point))

    ;; Verify state was saved
    (assert-equal "Actions" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
    (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO"))

    ;; Verify ORG_GTD changed to Tickler
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))

    ;; Verify TODO keyword was cleared
    (assert-nil (org-entry-get (point) "TODO"))))

(deftest tickler/restores-state-from-previous-properties ()
  "Restores ORG_GTD and TODO state from PREVIOUS_* properties."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)

    ;; Set up tickler state with saved previous values
    (org-entry-put (point) "ORG_GTD" "Tickler")
    (org-entry-put (point) "PREVIOUS_ORG_GTD" "Actions")
    (org-entry-put (point) "PREVIOUS_TODO" "NEXT")
    (org-todo 'none)  ; Clear TODO keyword

    ;; Restore state
    (org-gtd-project--restore-state (point))

    ;; Verify state was restored
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    ;; Verify PREVIOUS_* properties were removed
    (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
    (assert-nil (org-entry-get (point) "PREVIOUS_TODO"))))

(deftest tickler/collects-all-tasks-by-graph-traversal ()
  "Collects all tasks in a project by graph traversal."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker))
          (task-ids '()))

      ;; Get all task IDs
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (push (org-id-get-create) task-ids)
      (goto-char (point-min))
      (search-forward "Task 2")
      (org-back-to-heading t)
      (push (org-id-get-create) task-ids)
      (goto-char (point-min))
      (search-forward "Task 3")
      (org-back-to-heading t)
      (push (org-id-get-create) task-ids)

      (setq task-ids (nreverse task-ids))

      ;; Get tasks via helper
      (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
        ;; Should return 3 task markers
        (assert-equal 3 (length task-markers))
        ;; Each marker should point to a task with matching ID
        (dolist (marker task-markers)
          (org-with-point-at marker
            (assert-true (member (org-id-get) task-ids))))))))

;;; Project Tickler

(deftest tickler/incubates-project-with-review-date ()
  "Ticklers a project with review date."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; Incubate with review date
      (org-gtd-project-incubate project-marker "2025-12-01")

      ;; Verify project heading is incubated
      (org-with-point-at project-marker
        (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
        (assert-equal "<2025-12-01>" (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

      ;; Verify all tasks are incubated
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
      (assert-equal "Actions" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      (assert-nil (org-entry-get (point) "TODO"))
      (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO")))))

(deftest tickler/reactivates-tickler-project ()
  "Reactivates a tickler project."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; First incubate it
      (org-gtd-project-incubate project-marker "2025-12-01")

      ;; Verify it's incubated
      (org-with-point-at project-marker
        (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD")))

      ;; Now reactivate it
      (org-gtd-project-reactivate project-marker)

      ;; Verify project heading is restored
      (org-with-point-at project-marker
        (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
        (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
        (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

      ;; Verify tasks are restored
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
      (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      ;; TODO keyword should be restored (will be NEXT after recalculation)
      (assert-true (org-entry-get (point) "TODO")))))

(provide 'project-tickler-test)

;;; project-tickler-test.el ends here
