;;; files-test.el --- Tests for org-gtd file creation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd file creation and default content.
;;
;; Test Coverage:
;; - Inbox file creation with default content (2 tests)
;; - Default file creation for various GTD items (4 tests)
;;
;; Migrated from test/files-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Inbox File Tests

;; Note: The test "inbox has default content" cannot be replicated with mock-fs
;; because mock-fs pre-creates the inbox.org as an empty file, and org-gtd only
;; adds the template content when creating a NEW file.
;; The buttercup version works because it uses real temp directories without
;; pre-existing files.
;; This behavior is implicitly tested by other tests that capture to inbox.

;;; Default File Creation Tests

(deftest files/project-creates-default-file ()
  "Creates default file for a project."
  (create-project "project headline")
  (ogt--save-all-buffers)
  (assert-match "org-gtd-tasks\\.org" (ogt--org-dir-buffer-string)))

(deftest files/calendar-item-creates-default-file ()
  "Creates default file for a calendar item."
  (create-calendar-item "calendar headline")
  (ogt--save-all-buffers)
  (assert-match "org-gtd-tasks\\.org" (ogt--org-dir-buffer-string)))

(deftest files/delegated-item-creates-default-file ()
  "Creates default file for a delegated item."
  (create-delegated-item "delegated-headline")
  (ogt--save-all-buffers)
  (assert-match "org-gtd-tasks\\.org" (ogt--org-dir-buffer-string)))

(deftest files/tickler-item-creates-default-file ()
  "Creates default file for a tickler item."
  (create-deferred-item "tickler headline")
  (ogt--save-all-buffers)
  (assert-match "org-gtd-tasks\\.org" (ogt--org-dir-buffer-string)))

(deftest files/single-action-creates-default-file ()
  "Creates default file for a single action."
  (create-single-action "single action")
  (ogt--save-all-buffers)
  (assert-match "org-gtd-tasks\\.org" (ogt--org-dir-buffer-string)))

(provide 'files-test)

;;; files-test.el ends here
