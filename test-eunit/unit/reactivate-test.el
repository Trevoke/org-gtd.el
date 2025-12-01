;;; reactivate-test.el --- Tests for org-gtd reactivation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd reactivation functionality.
;;
;; Test Coverage:
;; - org-gtd-save-state saves delegated/calendar state to PREVIOUS_* properties (5 tests)
;; - org-gtd-restore-state restores from PREVIOUS_* properties (2 tests)
;; - org-gtd-reactivate reactivates someday/tickler items (3 tests)
;; - Integration: preserves state through someday/tickler cycles (2 tests)
;;
;; Migrated from test/reactivate-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'org-gtd-reactivate)

(e-unit-initialize)

;; Note: These tests don't need mock-fs since they only use temp buffers.
;; We just need minimal org-gtd configuration for keyword settings.

;;; org-gtd-save-state tests

(deftest save-state/saves-delegated-item-to-previous-properties ()
  "Saves delegated item state to PREVIOUS_* properties."
  (ogt--with-temp-org-buffer
   "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Delegated
:DELEGATED_TO: John Doe
:ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
   (org-back-to-heading t)
   (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
     (org-todo "WAIT"))
   (org-gtd-save-state)
   (assert-equal "Delegated" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
   (assert-equal "WAIT" (org-entry-get (point) "PREVIOUS_TODO"))
   (assert-equal "John Doe" (org-entry-get (point) "PREVIOUS_DELEGATED_TO"))
   (assert-equal "<2024-06-15>" (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP"))))

(deftest save-state/saves-calendar-item-to-previous-properties ()
  "Saves calendar item state to PREVIOUS_* properties."
  (ogt--with-temp-org-buffer
   "* Appointment
:PROPERTIES:
:ID: cal-id
:ORG_GTD: Calendar
:ORG_GTD_TIMESTAMP: <2024-07-20>
:END:"
   (org-back-to-heading t)
   (org-gtd-save-state)
   (assert-equal "Calendar" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
   (assert-equal "<2024-07-20>" (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP"))))

(deftest save-state/does-not-save-for-someday-items ()
  "Does not save state for someday items."
  (ogt--with-temp-org-buffer
   "* Someday item
:PROPERTIES:
:ID: someday-id
:ORG_GTD: Someday
:END:"
   (org-back-to-heading t)
   (org-gtd-save-state)
   (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))))

(deftest save-state/does-not-save-for-tickler-items ()
  "Does not save state for tickler items."
  (ogt--with-temp-org-buffer
   "* Tickler item
:PROPERTIES:
:ID: tickler-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-08-01>
:END:"
   (org-back-to-heading t)
   (org-gtd-save-state)
   (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))))

(deftest save-state/does-not-save-for-items-without-org-gtd ()
  "Does not save state for items without ORG_GTD property."
  (ogt--with-temp-org-buffer
   "* Inbox item
:PROPERTIES:
:ID: inbox-id
:END:"
   (org-back-to-heading t)
   (org-gtd-save-state)
   (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))))

;;; org-gtd-restore-state tests

(deftest restore-state/restores-delegated-item-from-previous-properties ()
  "Restores delegated item from PREVIOUS_* properties."
  (ogt--with-temp-org-buffer
   "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: John Doe
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
   (org-back-to-heading t)
   (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
     ;; Use with-simulated-input to handle prompts
     (with-simulated-input "RET RET RET RET"
       (org-gtd-restore-state))
     ;; Verify restored
     (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD"))
     (assert-equal "WAIT" (org-entry-get (point) "TODO"))
     (assert-equal "John Doe" (org-entry-get (point) "DELEGATED_TO"))
     (assert-equal "<2024-06-15>" (org-entry-get (point) "ORG_GTD_TIMESTAMP"))
     ;; Verify PREVIOUS_* cleaned up
     (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
     (assert-nil (org-entry-get (point) "PREVIOUS_TODO"))
     (assert-nil (org-entry-get (point) "PREVIOUS_DELEGATED_TO"))
     (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP")))))

(deftest restore-state/calls-clarify-when-no-previous-org-gtd ()
  "Calls clarify when no PREVIOUS_ORG_GTD exists."
  (ogt--with-temp-org-buffer
   "* Inbox item went to someday
:PROPERTIES:
:ID: direct-someday-id
:ORG_GTD: Someday
:END:"
   (org-back-to-heading t)
   (with-spy org-gtd-clarify-item spy
     (org-gtd-restore-state)
     ;; Verify ORG_GTD cleared and clarify called
     (assert-nil (org-entry-get (point) "ORG_GTD"))
     (assert-true (spy-called-p spy)))))

;;; org-gtd-reactivate tests

(deftest reactivate/reactivates-someday-item ()
  "Reactivates a someday item."
  (ogt--with-temp-org-buffer
   "* Delegated then someday'd
:PROPERTIES:
:ID: reactivate-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: Jane
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-09-01>
:END:"
   (org-back-to-heading t)
   (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
     (with-simulated-input "RET RET RET RET"
       (org-gtd-reactivate))
     (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD")))))

(deftest reactivate/reactivates-tickler-item ()
  "Reactivates a tickler item."
  (ogt--with-temp-org-buffer
   "* Calendar then tickler'd
:PROPERTIES:
:ID: tickler-reactivate-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-12-01>
:PREVIOUS_ORG_GTD: Calendar
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-10-15>
:END:"
   (org-back-to-heading t)
   (with-simulated-input "RET"
     (org-gtd-reactivate))
   (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
   (assert-equal "<2024-10-15>" (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))

(deftest reactivate/errors-on-non-someday-tickler-item ()
  "Errors on non-someday/tickler item."
  (ogt--with-temp-org-buffer
   "* Active item
:PROPERTIES:
:ID: active-id
:ORG_GTD: Delegated
:END:"
   (org-back-to-heading t)
   (assert-raises 'user-error
     (org-gtd-reactivate))))

;;; Integration tests: someday/tickler cycles

(deftest integration/preserves-delegated-state-through-someday-cycle ()
  "Preserves delegated state through someday cycle."
  (ogt--with-temp-org-buffer
   "* Delegated task
:PROPERTIES:
:ID: cycle-test-id
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:ORG_GTD_TIMESTAMP: <2024-05-01>
:END:"
   (org-back-to-heading t)
   (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
     (org-todo "WAIT")
     ;; Someday the item
     (org-gtd-someday--configure)
     ;; Verify someday state
     (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
     (assert-equal "Delegated" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
     ;; Reactivate with simulated input
     (with-simulated-input "RET RET RET RET"
       (org-gtd-reactivate))
     ;; Verify restored
     (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD"))
     (assert-equal "Bob" (org-entry-get (point) "DELEGATED_TO")))))

(deftest integration/preserves-calendar-state-through-tickler-cycle ()
  "Preserves calendar state through tickler cycle."
  (ogt--with-temp-org-buffer
   "* Calendar event
:PROPERTIES:
:ID: tickler-cycle-id
:ORG_GTD: Calendar
:ORG_GTD_TIMESTAMP: <2024-06-20>
:END:"
   (org-back-to-heading t)
   ;; Tickler the item (stub the date prompt)
   (with-stub org-gtd-prompt-for-active-date "<2025-01-01>"
     (org-gtd-tickler--configure))
   ;; Verify tickler state
   (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
   (assert-equal "Calendar" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
   ;; Reactivate with simulated input
   (with-simulated-input "RET"
     (org-gtd-reactivate))
   ;; Verify restored
   (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
   (assert-equal "<2024-06-20>" (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))

(provide 'reactivate-test)

;;; reactivate-test.el ends here
