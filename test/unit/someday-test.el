;;; someday-test.el --- Unit tests for someday/maybe item organization -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for someday/maybe item organization details.
;;
;; Test Coverage:
;; - No timestamp properties on someday items (1 test)
;; - Refile target has correct ORG_GTD_REFILE property (1 test)
;;
;; Migrated from test/someday-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Someday Lists Configuration Tests

(deftest someday/lists-variable-exists ()
  "org-gtd-someday-lists variable exists and defaults to nil."
  (assert-nil org-gtd-someday-lists))

;;; List Prompt Tests

(deftest someday/prompts-for-list-when-lists-configured ()
  "Prompts for list selection when org-gtd-someday-lists is configured."
  (let ((org-gtd-someday-lists '("Work Ideas" "Personal")))
    (ogt--with-temp-org-buffer
     "* Test item"
     (org-back-to-heading t)
     (with-simulated-input "Work SPC Ideas TAB RET"
       (org-gtd-someday--configure))
     (assert-equal "Work Ideas" (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))))

(deftest someday/skips-list-prompt-when-no-lists-configured ()
  "Skips list prompt when org-gtd-someday-lists is nil."
  (let ((org-gtd-someday-lists nil))
    (ogt--with-temp-org-buffer
     "* Test item"
     (org-back-to-heading t)
     (org-gtd-someday--configure)
     (assert-nil (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))))

;;; Someday/Maybe Organization Tests

(deftest someday/has-no-timestamp-properties ()
  "Someday items have no timestamp properties (no ORG_GTD_TIMESTAMP, SCHEDULED, or DEADLINE)."
  (org-gtd-someday-create "Build a treehouse")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Build a treehouse")
    (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP"))
    (assert-nil (org-entry-get (point) "SCHEDULED"))
    (assert-nil (org-entry-get (point) "DEADLINE"))))

(deftest someday/refiled-to-heading-with-someday-refile-property ()
  "Someday items are refiled to a heading with ORG_GTD_REFILE: Someday property."
  (org-gtd-someday-create "Visit Japan")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Visit Japan")
    ;; Navigate to parent heading
    (org-up-heading-safe)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD_REFILE"))))

(provide 'someday-test)

;;; someday-test.el ends here
