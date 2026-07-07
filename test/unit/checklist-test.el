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
;; - Template name and item parsing, normalization (6 tests)
;; - Inserting template instances at point (6 tests)
;; - Visiting the checklists file (1 test)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-checklist)
(require 'with-simulated-input)

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

(deftest checklist/tagged-heading-resolves-by-bare-name ()
  "Tags on a template heading do not leak into its name."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-max))
    (insert "* Packing list :travel:\n- [ ] Socks?\n")
    (basic-save-buffer))
  (assert-true (member "Packing list" (org-gtd-checklist-names)))
  (assert-equal '("Socks?") (org-gtd-checklist--items "Packing list")))

(deftest checklist/comment-headings-are-excluded ()
  "COMMENT headings are not offered as templates."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-max))
    (insert "* COMMENT Draft list\n- [ ] Not ready?\n")
    (basic-save-buffer))
  (assert-nil (member "Draft list" (org-gtd-checklist-names)))
  (assert-nil (member "COMMENT Draft list" (org-gtd-checklist-names))))

;;; Insert Tests

(deftest checklist/insert-copies-subtree-at-point ()
  "Insert spawns the named template as a subtree at point."
  (with-temp-buffer
    (org-mode)
    (with-simulated-input "Weekly SPC Review SPC triggers RET"
      (call-interactively #'org-gtd-checklist-insert))
    (assert-match "^\\* Weekly Review triggers" (buffer-string))
    (assert-match "- \\[ \\] Projects started" (buffer-string))))

(deftest checklist/insert-adapts-level-to-context ()
  "Inserting under an existing heading demotes the copy."
  (with-temp-buffer
    (org-mode)
    (insert "* Trip to the beach\n")
    (goto-char (point-max))
    (org-gtd-checklist-insert "Mind sweep prompts")
    (assert-match "^\\*\\* Mind sweep prompts" (buffer-string))))

(deftest checklist/insert-unknown-name-errors-cleanly ()
  "Unknown name signals a user-error naming the file."
  (with-temp-buffer
    (org-mode)
    (let ((err (condition-case e
                   (progn (org-gtd-checklist-insert "Nope") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "checklists\\.org" (cadr err)))))

(deftest checklist/insert-at-heading-bol-inserts-child-below ()
  "Point at bol of a heading inserts the template as a child below it."
  (with-temp-buffer
    (org-mode)
    (insert "* Trip to the beach\nsome body text\n")
    (goto-char (point-min))
    (org-gtd-checklist-insert "Mind sweep prompts")
    (assert-match "^\\* Trip to the beach$" (buffer-string))
    (assert-match "^\\*\\* Mind sweep prompts" (buffer-string))
    ;; The template lands after its parent heading, not above it.
    (goto-char (point-min))
    (assert-true (search-forward "* Trip to the beach" nil t))
    (assert-true (search-forward "** Mind sweep prompts" nil t))))

(deftest checklist/insert-mid-heading-line-keeps-heading-intact ()
  "Point in the middle of a heading line does not split the heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Trip to the beach\n")
    (goto-char (point-min))
    (search-forward "Trip to")
    (org-gtd-checklist-insert "Mind sweep prompts")
    (assert-match "^\\* Trip to the beach$" (buffer-string))
    (assert-match "^\\*\\* Mind sweep prompts" (buffer-string))))

(deftest checklist/insert-requires-org-mode ()
  "Refuses to insert into a non-org buffer."
  (with-temp-buffer
    (assert-true
     (condition-case e
         (progn (org-gtd-checklist-insert "Mind sweep prompts") nil)
       (user-error e)))))

;;; Visit Tests

(deftest checklist/visit-opens-checklists-file ()
  "Visit command selects a buffer visiting the checklists file."
  (org-gtd-checklist-visit)
  (assert-match "checklists\\.org" (buffer-file-name)))

(provide 'checklist-test)

;;; checklist-test.el ends here
