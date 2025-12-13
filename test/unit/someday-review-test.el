;;; someday-review-test.el --- Tests for someday review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for someday/maybe review functionality.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-someday-review)

(e-unit-initialize)

;;; LOGBOOK Entry Tests

(deftest someday-review/adds-reviewed-entry-to-logbook ()
  "Adds 'Reviewed' entry to LOGBOOK drawer."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match ":LOGBOOK:" content)
     (assert-match "- Reviewed \\[" content))))

(deftest someday-review/preserves-existing-logbook-entries ()
  "Preserves existing LOGBOOK entries when adding new one."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:
:LOGBOOK:
- Previous note [2025-01-01 Wed]
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match "Previous note" content)
     (assert-match "- Reviewed \\[" content))))

;;; Finding Someday Items Tests

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest someday-review/finds-all-someday-items ()
  "Finds all items with ORG_GTD: Someday."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-create "Item two")
  (let ((items (org-gtd-someday-review--find-items nil)))
    (assert-equal 2 (length items))))

(deftest someday-review/filters-by-list-property ()
  "Filters items by ORG_GTD_SOMEDAY_LIST property."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    ;; Create items with different lists
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work idea"))
    (with-simulated-input "Personal RET"
      (org-gtd-someday-create "Personal idea")))
  (let ((work-items (org-gtd-someday-review--find-items "Work")))
    (assert-equal 1 (length work-items))))

(deftest someday-review/finds-unassigned-items ()
  "Finds items without ORG_GTD_SOMEDAY_LIST when filtering for unassigned."
  (let ((org-gtd-someday-lists nil))
    (org-gtd-someday-create "Unassigned item"))
  (let ((org-gtd-someday-lists '("Work")))
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work item")))
  (let ((unassigned (org-gtd-someday-review--find-items 'unassigned)))
    (assert-equal 1 (length unassigned))))

(provide 'someday-review-test)

;;; someday-review-test.el ends here
