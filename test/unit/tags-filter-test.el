;;; tags-filter-test.el --- Tests for tags filter predicate -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for tags predicate used in skip functions.
;; These tests verify that the tags matching predicate works correctly
;; for filtering org entries by tags.
;;

;;; Code:

(require 'e-unit)
(require 'org)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Predicate Unit Tests

(deftest tags-pred/matches-single-tag ()
  "Predicate matches when entry has the specified tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/matches-any-of-multiple-tags ()
  "Predicate matches when entry has any of the specified tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@home:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/no-match-when-different-tag ()
  "Predicate returns nil when entry has different tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@errands:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/no-match-when-no-tags ()
  "Predicate returns nil when entry has no tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/matches-among-multiple-entry-tags ()
  "Predicate matches when entry has multiple tags including a match."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:urgent:important:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("urgent"))))
      (assert-true (funcall pred)))))

(provide 'tags-filter-test)
;;; tags-filter-test.el ends here
