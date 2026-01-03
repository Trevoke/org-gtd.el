;;; deadline-filter-test.el --- Tests for deadline filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for deadline predicate used in skip functions.
;; These tests verify that the deadline matching predicate works correctly
;; for filtering org entries by DEADLINE timing (past, today, future).
;;

;;; Code:

(require 'e-unit)
(require 'org)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Predicate Unit Tests

(deftest deadline-pred/past-matches-overdue ()
  "Deadline predicate matches items with deadline in the past."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Overdue task\n")
    (insert "DEADLINE: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-true (funcall pred)))))

(deftest deadline-pred/past-no-match-future-deadline ()
  "Deadline predicate does not match items with future deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future task\n")
    (insert "DEADLINE: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/past-no-match-no-deadline ()
  "Deadline predicate does not match items without deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO No deadline task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/future-matches-upcoming ()
  "Future deadline predicate matches items with future deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future task\n")
    (insert "DEADLINE: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'future)))
      (assert-true (funcall pred)))))

(deftest deadline-pred/future-no-match-past-deadline ()
  "Future deadline predicate does not match past deadlines."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Overdue task\n")
    (insert "DEADLINE: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'future)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/today-matches-deadline-today ()
  "Today deadline predicate matches items with deadline today."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task due today\n")
    (insert (format "DEADLINE: <%s>\n" (format-time-string "%Y-%m-%d %a")))
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'today)))
      (assert-true (funcall pred)))))

(deftest deadline-pred/today-no-match-past-deadline ()
  "Today deadline predicate does not match past deadlines."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Overdue task\n")
    (insert "DEADLINE: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'today)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/today-no-match-future-deadline ()
  "Today deadline predicate does not match future deadlines."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future task\n")
    (insert "DEADLINE: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'today)))
      (assert-nil (funcall pred)))))

(provide 'deadline-filter-test)
;;; deadline-filter-test.el ends here
