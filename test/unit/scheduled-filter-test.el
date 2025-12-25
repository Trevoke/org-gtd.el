;;; scheduled-filter-test.el --- Tests for scheduled filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for scheduled predicate used in skip functions.
;; These tests verify that the scheduled matching predicate works correctly
;; for filtering org entries by SCHEDULED timing (past, today, future).
;;

;;; Code:

(require 'e-unit)
(require 'org)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Predicate Unit Tests

(deftest scheduled-pred/past-matches-overdue ()
  "Scheduled predicate matches items scheduled in the past."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Past scheduled task\n")
    (insert "SCHEDULED: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--scheduled-matches 'past)))
      (assert-true (funcall pred)))))

(deftest scheduled-pred/past-no-match-future ()
  "Scheduled predicate does not match items scheduled in future."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future scheduled task\n")
    (insert "SCHEDULED: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--scheduled-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest scheduled-pred/past-no-match-no-scheduled ()
  "Scheduled predicate does not match items without scheduled date."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO No scheduled task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--scheduled-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest scheduled-pred/future-matches-upcoming ()
  "Future scheduled predicate matches items scheduled in future."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future scheduled task\n")
    (insert "SCHEDULED: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--scheduled-matches 'future)))
      (assert-true (funcall pred)))))

(deftest scheduled-pred/future-no-match-past-scheduled ()
  "Future scheduled predicate does not match past scheduled dates."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Past scheduled task\n")
    (insert "SCHEDULED: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--scheduled-matches 'future)))
      (assert-nil (funcall pred)))))

(provide 'scheduled-filter-test)
;;; scheduled-filter-test.el ends here
