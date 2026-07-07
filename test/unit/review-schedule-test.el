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
;; - Body line targets the created habit, decoys and tags aside (2 tests)
;; - Repeater validation and empty-profiles guard (3 tests)
;; - Duplicate-reminder confirmation guard (2 tests)
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

;;; Heading Targeting Tests

(deftest review-schedule/body-line-lands-under-habit-not-decoy ()
  "A pre-existing heading ending in the profile name is not the target."
  (with-current-buffer (org-gtd--default-file)
    (org-with-wide-buffer
     (goto-char (point-max))
     (insert "\n* Notes on Weekly Review\nScribbles.\n")
     (basic-save-buffer)))
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (search-forward "Run M-x org-gtd-review")
     (assert-equal "Weekly Review" (org-get-heading t t t t)))))

(deftest review-schedule/body-line-added-despite-tags ()
  "Organize hooks that tag the heading do not break body insertion."
  (let ((org-gtd-organize-hooks (list (lambda () (org-set-tags ":review:")))))
    (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w"))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (let ((text (buffer-string)))
      (assert-match ":review:" text)
      (assert-match "Run M-x org-gtd-review when you sit down for this\\."
                    text))))

;;; Input Validation Tests

(deftest review-schedule/rejects-malformed-repeater ()
  "A repeater org cannot parse is refused before anything is written."
  (let ((err (condition-case e
                 (progn
                   (org-gtd-review-schedule "Weekly Review" "2026-07-10" "weekly")
                   nil)
               (user-error e))))
    (assert-true err)
    (assert-match "\\.\\+1w" (cadr err)))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (assert-nil (string-match-p "Weekly Review" (buffer-string)))))

(deftest review-schedule/rejects-zero-interval-repeater ()
  "A zero-interval repeater never re-arms, so it is refused.
Multi-digit intervals like .+10d remain accepted."
  (let ((err (condition-case e
                 (progn
                   (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+0w")
                   nil)
               (user-error e))))
    (assert-true err)
    (assert-match "\\.\\+1w" (cadr err)))
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+10d")
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (assert-match "SCHEDULED: <2026-07-10 [A-Za-z]+ \\.\\+10d>"
                  (buffer-string))))

(deftest review-schedule/no-profiles-configured-errors ()
  "Scheduling with no profiles signals a clean user-error."
  (let ((org-gtd-review-profiles nil))
    (let ((err (condition-case e
                   (progn (org-gtd-review-schedule) nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "No review profiles configured" (cadr err)))))

;;; Duplicate Guard Tests

(deftest review-schedule/declining-duplicate-keeps-single-reminder ()
  "When a reminder exists, answering no schedules nothing new."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
    (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w"))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (assert-equal 1 (count-matches "Run M-x org-gtd-review when"))))

(deftest review-schedule/confirming-duplicate-schedules-again ()
  "Explicit confirmation allows a second reminder."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
    (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w"))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (assert-equal 2 (count-matches "Run M-x org-gtd-review when"))))

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
