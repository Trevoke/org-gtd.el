;;; processing-test.el --- Tests for org-gtd processing -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd inbox processing functionality.
;;
;; Test Coverage:
;; - Processes and organizes all inbox items (1 test)
;; - Uses configurable decorations (1 test)
;; - Displays organized item in agenda (1 test)
;; - Allows organizing valid project (1 test)
;; - Rejects project with no tasks (1 test)
;;
;; Migrated from test/processing-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Processing Tests

(deftest processing/organizes-all-items-leaving-inbox-empty ()
  "Processes and organizes all inbox items leaving inbox empty."
  ;; Capture inbox items
  (capture-inbox-item "single action")
  (capture-inbox-item "test project")
  (capture-inbox-item "test calendar item")
  (capture-inbox-item "test delegated item")
  (capture-inbox-item "test tickler item")
  (capture-inbox-item "test single action")
  (capture-inbox-item "test knowledge item")

  ;; Process them all using helper functions (7 items total)
  (org-gtd-process-inbox)
  ;; First item from around-each (single action)
  (organize-as-single-action)

  ;; Items we captured in this test - create project using builder
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task 1" :level 2)
    (make-task "Task 2" :level 2)
    (make-task "Task 3" :level 2)
    (organize-as-project))

  (schedule-item (calendar-current-date))
  (delegate-item "Someone" (calendar-current-date))
  (defer-item (calendar-current-date))
  (organize-as-single-action)
  (archive-as-reference)

  ;; Check inbox is empty
  (with-current-buffer (ogt-inbox-buffer)
    (refute-match "test" (current-buffer-raw-text))))

(deftest processing/uses-configurable-decorations ()
  "Uses configurable decorations on the processed items."
  (capture-inbox-item "single action")
  ;; Define a simple test hook that adds a priority
  (defun test-hook-add-priority ()
    "Test hook that adds priority A without user input."
    (org-priority ?A))

  (let ((org-gtd-organize-hooks '(test-hook-add-priority)))
    (org-gtd-process-inbox)
    (organize-as-single-action))

  (org-gtd-engage)
  (let ((ogt-agenda-string (agenda-raw-text)))
    (assert-true (string-match "NEXT \\[#A\\] single action" ogt-agenda-string))))

(deftest processing/displays-organized-item-in-agenda ()
  "Displays organized item in daily agenda after processing."
  (capture-inbox-item "single action")
  (org-gtd-process-inbox)
  (organize-as-single-action)
  (assert-equal t (buffer-modified-p (org-gtd--default-file)))

  (org-gtd-engage)
  (let ((ogt-agenda-string (agenda-raw-text)))
    (assert-true (string-match "single action" ogt-agenda-string))))

;;; Error Management Tests

(deftest processing/allows-organizing-valid-project ()
  "Allows organizing a valid project with tasks."
  (capture-inbox-item "my project")
  ;; This test verifies the happy path works
  (org-gtd-process-inbox)
  ;; Add tasks to make it a valid project
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "First task" :level 2)
    (make-task "Second task" :level 2))
  (organize-as-project)
  ;; Should succeed and not be in WIP buffer
  (refute-match org-gtd-wip--prefix (buffer-name)))

(deftest processing/rejects-project-with-no-tasks ()
  "Rejects project with no tasks and returns to editing."
  (capture-inbox-item "empty project")
  ;; Simulate user interaction with simulated-input
  (with-simulated-input "RET" ;; Press enter to dismiss error message
    (org-gtd-process-inbox)
    ;; Don't add any tasks - project has no child headings
    (organize-as-project)
    ;; Should stay in WIP buffer after error
    (assert-match org-gtd-wip--prefix (buffer-name))))

(provide 'processing-test)

;;; processing-test.el ends here
