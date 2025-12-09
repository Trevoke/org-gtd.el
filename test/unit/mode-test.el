;;; mode-test.el --- Unit tests for org-gtd-mode -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-mode inbox count and lighter functionality.
;;
;; Test Coverage:
;; - org-gtd-inbox-count (3 tests)
;; - org-gtd-mode-lighter (2 tests)
;;
;; Migrated from test/mode-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; org-gtd-inbox-count tests

(deftest mode/inbox-count-returns-0-when-empty ()
  "Inbox count returns 0 when inbox is empty."
  (assert-equal 0 (org-gtd-inbox-count)))

(deftest mode/inbox-count-counts-items-in-main-inbox ()
  "Inbox count correctly counts items in main inbox."
  (capture-inbox-item "First item")
  (capture-inbox-item "Second item")
  (capture-inbox-item "Third item")
  (assert-equal 3 (org-gtd-inbox-count)))

(deftest mode/inbox-count-includes-additional-inbox-files ()
  "Inbox count includes items from additional inbox files."
  (capture-inbox-item "Main inbox item")

  (let* ((additional-file (f-join org-gtd-directory "extra-inbox.org"))
         (additional-buffer (find-file-noselect additional-file)))
    (with-current-buffer additional-buffer
      (insert "* Extra item 1\n* Extra item 2\n")
      (basic-save-buffer))

    (let ((org-gtd-additional-inbox-files (list additional-file)))
      (assert-equal 3 (org-gtd-inbox-count)))))

;;; org-gtd-mode-lighter tests

(deftest mode/lighter-formats-count-in-string ()
  "Mode lighter formats count in lighter string."
  (capture-inbox-item "Test item")
  (capture-inbox-item "Another item")
  (assert-equal " GTD[2]" (org-gtd-mode-lighter)))

(deftest mode/lighter-shows-zero-when-inbox-empty ()
  "Mode lighter shows zero when inbox is empty."
  (assert-equal " GTD[0]" (org-gtd-mode-lighter)))

;;; CLOSED timestamp auto-logging tests

(deftest mode/closed-timestamp-added-for-gtd-item ()
  "GTD item gets CLOSED timestamp when marked done."
  (org-gtd-mode 1)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test task\n")
        (goto-char (point-min))
        (org-entry-put (point) "ORG_GTD" "Actions")
        ;; Mark as done
        (org-todo "DONE")
        ;; Should have CLOSED timestamp
        (assert-true (org-entry-get (point) "CLOSED")))
    (org-gtd-mode -1)))

(deftest mode/closed-timestamp-not-added-for-non-gtd-item ()
  "Non-GTD item does NOT get CLOSED timestamp from org-gtd hook."
  (org-gtd-mode 1)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Regular task\n")
        (goto-char (point-min))
        ;; No ORG_GTD property
        (let ((org-log-done nil))  ; Ensure org-mode's logging is off
          (org-todo "DONE"))
        ;; Should NOT have CLOSED timestamp (no ORG_GTD property)
        (assert-nil (org-entry-get (point) "CLOSED")))
    (org-gtd-mode -1)))

(deftest mode/closed-timestamp-not-duplicated ()
  "GTD item with existing CLOSED doesn't get double-stamped."
  (org-gtd-mode 1)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test task\n")
        (goto-char (point-min))
        (org-entry-put (point) "ORG_GTD" "Actions")
        ;; Use org-add-planning-info to set CLOSED (org-entry-put doesn't work)
        (org-add-planning-info 'closed "[2025-01-01 Wed 10:00]")
        ;; Mark as done
        (org-todo "DONE")
        ;; Should still have original CLOSED timestamp
        (assert-equal "[2025-01-01 Wed 10:00]" (org-entry-get (point) "CLOSED")))
    (org-gtd-mode -1)))

(deftest mode/closed-hook-added-when-mode-enabled ()
  "Hook is added to org-after-todo-state-change-hook when mode enabled."
  (org-gtd-mode -1)  ; Ensure disabled first
  (assert-nil (memq 'org-gtd--add-closed-timestamp org-after-todo-state-change-hook))
  (org-gtd-mode 1)
  (unwind-protect
      (assert-true (memq 'org-gtd--add-closed-timestamp org-after-todo-state-change-hook))
    (org-gtd-mode -1)))

(deftest mode/closed-hook-removed-when-mode-disabled ()
  "Hook is removed from org-after-todo-state-change-hook when mode disabled."
  (org-gtd-mode 1)
  (assert-true (memq 'org-gtd--add-closed-timestamp org-after-todo-state-change-hook))
  (org-gtd-mode -1)
  (assert-nil (memq 'org-gtd--add-closed-timestamp org-after-todo-state-change-hook)))

(provide 'mode-test)

;;; mode-test.el ends here
