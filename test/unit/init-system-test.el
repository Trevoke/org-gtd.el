;;; init-system-test.el --- Tests for org-gtd-init-system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-init-system', the idempotent first-time
;; setup concierge.
;;
;; Test Coverage:
;; - All GTD files are created (1 test)
;; - Running twice neither errors nor duplicates seeds (1 test)
;; - Accepting the offer routes into org-gtd-review-schedule (1 test)
;; - An existing reminder skips the offer entirely (1 test)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-init)
(require 'cl-lib)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; File Creation Tests

(deftest init-system/creates-all-gtd-files ()
  "Init ensures tasks, inbox, and seeded checklists files exist."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
    (org-gtd-init-system))
  (assert-true (file-exists-p (org-gtd--path org-gtd-default-file-name)))
  (assert-true (file-exists-p (org-gtd-inbox-path)))
  (assert-true (file-exists-p (org-gtd-checklist--file-path))))

(deftest init-system/is-idempotent ()
  "Running init twice neither errors nor duplicates seeds."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
    (org-gtd-init-system)
    (org-gtd-init-system))
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (assert-equal 1 (count-matches "^\\* Weekly Review triggers$"))))

;;; Review Schedule Offer Tests

(deftest init-system/offers-review-schedule ()
  "Answering yes routes into org-gtd-review-schedule.
The real schedule command runs, wrapped so it receives concrete
arguments instead of prompting (stubbing `read-string', a C subr,
trips native-comp trampolines under the mock fs)."
  (let ((real-schedule (symbol-function 'org-gtd-review-schedule)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'org-gtd-review-schedule)
               (lambda ()
                 (interactive)
                 (funcall real-schedule "Weekly Review" "2026-07-10" ".+1w"))))
      (org-gtd-init-system)))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (let ((text (buffer-string)))
      (assert-match "Weekly Review" text)
      (assert-match ":ORG_GTD: +Habit" text))))

(deftest init-system/skips-offer-when-reminder-exists ()
  "An existing reminder reports as such and never offers to schedule."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (let ((captured nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (error "y-or-n-p must not be called")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) captured))
                 nil)))
      (org-gtd-init-system))
    (assert-true
     (seq-find (lambda (m) (string-match-p "already scheduled" m))
               captured))))

(provide 'init-system-test)

;;; init-system-test.el ends here
