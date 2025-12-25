;;; todo-filter-test.el --- Tests for todo keyword filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for todo keyword predicate used in skip functions.
;; These tests verify that the todo matching predicate works correctly
;; for filtering org entries by TODO keywords.
;;

;;; Code:

(require 'e-unit)
(require 'org)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Predicate Unit Tests

(deftest todo-pred/matches-single-keyword ()
  "Todo predicate matches item with specified keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--todo-matches '("TODO"))))
      (assert-true (funcall pred)))))

(deftest todo-pred/matches-any-of-multiple-keywords ()
  "Todo predicate matches if item has any of specified keywords."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--todo-matches '("TODO" "NEXT"))))
      (assert-true (funcall pred)))))

(deftest todo-pred/no-match-different-keyword ()
  "Todo predicate does not match item with different keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--todo-matches '("TODO" "NEXT"))))
      (assert-nil (funcall pred)))))

(deftest todo-pred/no-match-no-keyword ()
  "Todo predicate does not match item without TODO keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* Task without keyword\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--todo-matches '("TODO"))))
      (assert-nil (funcall pred)))))

(deftest todo-pred/no-match-done-keyword ()
  "Todo predicate does not match done items."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Completed task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--todo-matches '("TODO" "NEXT"))))
      (assert-nil (funcall pred)))))

(provide 'todo-filter-test)
;;; todo-filter-test.el ends here
