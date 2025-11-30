;;; habit-flow-test.el --- Acceptance tests for habit workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for habit workflows including:
;; - Habit capture, organization, and engagement
;; - Habit completion (reschedules instead of archiving)
;; - Habit cancellation behavior (org-mode limitation)
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Habit Workflow

(deftest habit-workflow-with-completion ()
  "Captures, organizes as habit, shows in engage, completes without archiving."
  ;; 1. CAPTURE
  (capture-inbox-item "Exercise daily")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE as habit with daily repeater
  (organize-as-habit "+1d")

  ;; 4. VERIFY shows in engage
  (org-gtd-engage)
  (assert-match "Exercise daily" (agenda-raw-text))

  ;; 5. Mark habit DONE
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Exercise daily")
    (org-todo "DONE"))

  ;; 6. VERIFY habit is NOT archived (it reschedules instead)
  (org-gtd-archive-completed-items)
  (with-current-buffer (org-gtd--default-file)
    ;; Habit should still be in the file (not archived)
    (assert-match "Exercise daily" (current-buffer-raw-text)))

  ;; 7. VERIFY habit still has SCHEDULED timestamp (proof it rescheduled)
  ;; Note: The habit reschedules to tomorrow, so it might not show in today's agenda
  ;; depending on the agenda view configuration. We verify it exists in the file.
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Exercise daily")
    ;; Verify it has a SCHEDULED timestamp
    (refute-nil (org-entry-get (point) "SCHEDULED"))))

;;; Habit Cancellation

(deftest habit-cancellation-removes-from-engage ()
  "Cancels habit, verifies it doesn't show in engage.
NOTE: This test documents a product limitation with habits.
When you mark a habit CNCL, org-mode removes the TODO keyword and logs
it in LAST_REPEAT, similar to when marking it DONE. This means:
1. The habit doesn't actually have CNCL state (state is nil)
2. The habit won't be archived by org-gtd-archive-completed-items
3. Users need to manually delete or archive unwanted habits
This is org-mode behavior, not specific to org-gtd."
  ;; 1. CAPTURE and ORGANIZE habit
  (capture-inbox-item "Read before bed")
  (org-gtd-process-inbox)
  (organize-as-habit "+1d")

  ;; 2. VERIFY shows in engage before cancellation
  (org-gtd-engage)
  (assert-match "Read before bed" (agenda-raw-text))

  ;; 3. CANCEL the habit
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Read before bed")
    (org-todo "CNCL"))

  ;; 4. VERIFY canceled habit does NOT show in engage
  ;; (Good! Habits behave correctly - canceled ones don't show)
  (org-gtd-engage)
  (refute-match "Read before bed" (agenda-raw-text))

  ;; 5. VERIFY habit state is nil (org-mode limitation)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Read before bed")
    (assert-nil (org-get-todo-state)))

  ;; 6. VERIFY habit is NOT archived (because it has no done keyword)
  (org-gtd-archive-completed-items)
  (with-current-buffer (org-gtd--default-file)
    ;; Habit remains in file (not archived due to org-mode behavior)
    (assert-match "Read before bed" (current-buffer-raw-text))))

;;; habit-flow-test.el ends here
