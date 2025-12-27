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

(deftest tags-pred/matches-emoji-tag ()
  "Predicate matches when entry has an emoji tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :🏠:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("🏠"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/matches-emoji-among-multiple-tags ()
  "Predicate matches emoji tag among other tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :urgent:🔥:important:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("🔥"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/no-match-wrong-emoji ()
  "Predicate returns nil when emoji doesn't match."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :🏠:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("🏢"))))
      (assert-nil (funcall pred)))))

;;; Skip Function Integration

(deftest tags-filter/skip-function-includes-tags ()
  "Skip function builder includes tags filter predicate."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Next Action :@work:\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((spec '((type . next-action)
                   (tags . ("@work" "@home"))))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Skip function should be a lambda (closure)
      (assert-true (functionp skip-fn))
      ;; Should NOT skip entry with matching tag
      (assert-nil (funcall skip-fn)))))

(provide 'tags-filter-test)
;;; tags-filter-test.el ends here
