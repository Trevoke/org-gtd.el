;;; review-schedule-test.el --- Tests for org-gtd-review-schedule -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-review-schedule', the command that creates a
;; recurring habit reminding the user to run their review, and for
;; `org-gtd-review--reminder-exists-p', the predicate that detects one.
;;
;; Test Coverage:
;; - Scheduling creates a typed, repeating habit (1 test)
;; - Reminder detection before and after scheduling (2 tests)
;; - Session completion tip conditional on a reminder existing (2 tests)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review)
(require 'cl-lib)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-review--state nil
          org-gtd-review--window-config nil)
    (when (get-buffer org-gtd-review--buffer-name)
      (kill-buffer org-gtd-review--buffer-name))
    (let ((state-file (f-join org-gtd-directory "review-state.eld")))
      (when (file-exists-p state-file)
        (delete-file state-file)))
    (funcall proceed context)))

;;; Scheduling Tests

(deftest review-schedule/creates-habit-with-repeater ()
  "Scheduling creates a properly-typed habit in the tasks file."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (let ((text (buffer-string)))
      (assert-match "Weekly Review" text)
      (assert-match ":ORG_GTD: +Habit" text)
      (assert-match "SCHEDULED: <2026-07-10 [A-Za-z]+ \\.\\+1w>" text)
      (assert-match "M-x org-gtd-review" text))))

;;; Reminder Detection Tests

(deftest review-schedule/reminder-exists-p-nil-on-empty-tasks-file ()
  "With no habit in the tasks file, no reminder is detected."
  (assert-nil (org-gtd-review--reminder-exists-p)))

(deftest review-schedule/reminder-exists-p-t-after-scheduling ()
  "After scheduling, the reminder is detected."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (assert-true (org-gtd-review--reminder-exists-p)))

;;; Completion Tip Tests

(defvar review-schedule-test--tiny-profile
  '(("Tiny"
     ("Phase A"
      (:title "Step one" :type prompt))))
  "Minimal one-step profile so a session completes in one keypress.")

(defun review-schedule-test--complete-session-messages ()
  "Run a Tiny session to completion, returning the messages emitted."
  (let ((captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) captured))
                 nil)))
      (org-gtd-review "Tiny")
      (with-current-buffer org-gtd-review--buffer-name
        (org-gtd-review-next)))
    captured))

(deftest review-schedule/finish-tip-shown-without-reminder ()
  "Completing a session with no reminder scheduled shows the tip."
  (let ((org-gtd-review-profiles review-schedule-test--tiny-profile))
    (let ((messages (review-schedule-test--complete-session-messages)))
      (assert-true
       (seq-find (lambda (m) (string-match-p "org-gtd-review-schedule" m))
                 messages)))))

(deftest review-schedule/finish-tip-suppressed-when-reminder-exists ()
  "Completing a session after scheduling omits the one-time tip."
  (let ((org-gtd-review-profiles review-schedule-test--tiny-profile))
    (org-gtd-review-schedule "Tiny" "2026-07-10" ".+1w")
    (let ((messages (review-schedule-test--complete-session-messages)))
      (assert-nil
       (seq-find (lambda (m) (string-match-p "org-gtd-review-schedule" m))
                 messages)))))

(provide 'review-schedule-test)

;;; review-schedule-test.el ends here
