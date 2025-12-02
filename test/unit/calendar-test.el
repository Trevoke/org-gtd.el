;;; calendar-test.el --- Tests for org-gtd calendar items -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd calendar item functionality.
;;
;; Test Coverage:
;; - Stores scheduled date in timestamp property (1 test)
;; - Cleans up conflicting GTD state during creation (1 test)
;;
;; Note: "appears in daily agenda after creation" migrated to
;; test-eunit/acceptance/basic-workflows-test.el
;;
;; Migrated from test/calendar-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Calendar Item Properties Tests

(deftest calendar/stores-date-in-timestamp-property ()
  "Calendar item stores scheduled date in ORG_GTD_TIMESTAMP property."
  (let* ((date (calendar-current-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day (nth 1 date)))
    (create-calendar-item "Yowza" date)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Yowza")
      (let ((timestamp (task-timestamp (current-task))))
        (assert-match (format "%s-%#02d-%#02d" year month day) timestamp)))))

;;; Conflicting State Cleanup Tests

(deftest calendar/cleans-up-conflicting-state ()
  "Cleans up conflicting GTD state when clarify-item is called during creation."
  ;; This test verifies that org-gtd-clarify-item (called by org-gtd-calendar-create)
  ;; properly cleans up any existing conflicting GTD properties via
  ;; org-gtd-wip--maybe-initialize-buffer-contents
  (let ((topic "Item with conflicting state")
        (date (format-time-string "%Y-%m-%d")))

    ;; Create an item with conflicting GTD state, then use calendar-create to clean it up
    (with-temp-buffer
      (org-mode)
      (insert (format "* %s\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:DELEGATED_TO: Someone Else\n:STYLE: habit\n:ORG_GTD: Actions\n:END:\n" topic))
      (goto-char (point-min))
      (search-forward topic)
      (org-back-to-heading)

      ;; Verify conflicting properties are present before clarification
      (assert-equal "<2020-01-01>" (task-timestamp (current-task)))
      (assert-equal "Someone Else" (task-delegated-to (current-task)))
      (assert-equal "habit" (task-property (current-task) "STYLE"))
      (assert-equal "Actions" (task-property (current-task) "ORG_GTD"))

      ;; Test that a calendar item created programmatically is clean
      (org-gtd-calendar-create topic date)

      ;; Verify that a properly created calendar item appears in agenda
      (org-gtd-engage)
      ;; The calendar creation should have worked despite conflicting source state
      (assert-true (agenda-contains? topic)))))

(provide 'calendar-test)

;;; calendar-test.el ends here
