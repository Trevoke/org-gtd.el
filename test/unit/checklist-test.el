;;; checklist-test.el --- Tests for org-gtd checklist templates -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd checklist templates.
;;
;; Test Coverage:
;; - Lazy creation and seeding of checklists.org (2 tests)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-checklist)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; File Creation and Seeding Tests

(deftest checklist/file-is-created-with-starter-templates ()
  "First touch creates checklists.org seeded with starter trigger lists."
  (let ((buf (org-gtd-checklist--file-buffer)))
    (with-current-buffer buf
      (assert-match "\\* Weekly Review triggers" (buffer-string))
      (assert-match "\\* Mind sweep prompts" (buffer-string))
      (assert-match "- \\[ \\]" (buffer-string)))))

(deftest checklist/seeding-is-idempotent ()
  "Touching the file twice does not duplicate the starters."
  (org-gtd-checklist--file-buffer)
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (assert-equal 1 (count-matches "^\\* Weekly Review triggers$"))))

;;; Template Names and Items Tests

(deftest checklist/names-lists-top-level-headings ()
  "Template names are the top-level heading titles."
  (assert-equal '("Weekly Review triggers" "Mind sweep prompts")
                (org-gtd-checklist-names)))

(deftest checklist/items-returns-ordered-item-strings ()
  "Items of a named checklist come back as ordered plain strings."
  (let ((items (org-gtd-checklist--items "Mind sweep prompts")))
    (assert-equal "Boss, partners, colleagues?" (car items))
    (assert-equal 8 (length items))))

(deftest checklist/items-ignores-checked-state ()
  "A checked box still yields its item text."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (search-forward "- [ ] Boss")
    (replace-match "- [X] Boss")
    (basic-save-buffer))
  (assert-equal "Boss, partners, colleagues?"
                (car (org-gtd-checklist--items "Mind sweep prompts"))))

(deftest checklist/items-nil-for-unknown-name ()
  "Unknown checklist name returns nil, no error."
  (assert-nil (org-gtd-checklist--items "No such list")))

(provide 'checklist-test)

;;; checklist-test.el ends here
