;;; delegating-test.el --- Tests for org-gtd delegation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd delegation functionality.
;;
;; Test Coverage:
;; - Delegation through agenda (1 test)
;; - Delegated item stores timestamp (1 test)
;; - Delegation with skip-refile (1 test)
;;
;; Note: "appears in daily agenda with WAIT state after delegation" migrated to
;; test-eunit/acceptance/basic-workflows-test.el
;;
;; Migrated from test/delegating-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Delegation Through Agenda Tests

(deftest delegation/can-be-done-through-agenda ()
  "Delegation can be done through the agenda and show on the agenda."
  (create-single-action "delegateme")
  (ogt--save-all-buffers)
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "delegateme")
    (with-simulated-input "That SPC Guy RET RET"
      (org-gtd-delegate-agenda-item)))

  (ogt--save-all-buffers)
  (org-gtd-engage)
  (assert-true (agenda-contains? "WAIT "))
  (assert-true (agenda-contains? "That Guy")))

;;; Delegated Item Properties Tests

(deftest delegation/stores-checkin-date-in-timestamp ()
  "Delegated item stores check-in date in ORG_GTD_TIMESTAMP property."
  (let* ((date (calendar-current-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day (nth 1 date)))
    (create-delegated-item "TASK DESC" "Someone" date)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "TASK DESC")
      (let ((timestamp (task-timestamp (current-task))))
        (assert-match (format "%s-%#02d-%#02d" year month day) timestamp)))))

;;; Customizing Delegation Input Tests

(deftest delegation/allows-skip-refile ()
  "Delegation allows delegation without refiling via skip-refile."
  (let ((topic "Custom delegate test")
        (checkin-date (format-time-string "%Y-%m-%d")))
    (create-single-action topic)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward topic)
      (org-back-to-heading t)
      (let ((original-pos (point-marker)))
        ;; Clarify with skip-refile
        (let ((current-prefix-arg '(4)))
          (org-gtd-clarify-item))
        ;; Delegate via organize menu
        (with-current-buffer (car (org-gtd-wip--get-buffers))
          (with-simulated-input "Custom SPC Person RET RET"
            (org-gtd-delegate)))
        ;; Verify delegation was set up and item wasn't refiled
        (goto-char (point-min))
        (search-forward topic)
        (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "Custom Person" (org-entry-get (point) "DELEGATED_TO"))))))

(provide 'delegating-test)

;;; delegating-test.el ends here
