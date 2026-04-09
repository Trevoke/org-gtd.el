;;; create-item-test.el --- Tests for org-gtd-create-item -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the unified programmatic entry point
;; `org-gtd-create-item'.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest create-item/next-action-creates-heading-with-org-gtd-prop ()
  "Creating a next-action item writes ORG_GTD=Actions to the new heading."
  (org-gtd-create-item 'next-action "Buy milk")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Buy milk")
    (assert-equal "Actions" (org-entry-get nil "ORG_GTD" t))))

(deftest create-item/calendar-forwards-config-when ()
  "Creating a calendar item forwards :when config to the registry."
  (org-gtd-create-item 'calendar "Dentist"
                       '((:when . "<2026-05-01 Fri>")))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Dentist")
    (assert-equal "Calendar" (org-entry-get nil "ORG_GTD" t))
    (assert-equal "<2026-05-01 Fri>"
                  (org-entry-get nil "ORG_GTD_TIMESTAMP" t))))

(deftest create-item/delegated-forwards-who-and-when ()
  "Creating a delegated item forwards :who and :when config."
  (org-gtd-create-item 'delegated "Review draft"
                       '((:who . "Alice")
                         (:when . "<2026-05-10>")))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Review draft")
    (assert-equal "Delegated" (org-entry-get nil "ORG_GTD" t))
    (assert-equal "Alice" (org-entry-get nil "DELEGATED_TO" t))))

(provide 'create-item-test)

;;; create-item-test.el ends here
