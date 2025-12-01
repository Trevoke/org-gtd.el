;;; additional-inboxes-test.el --- Tests for multiple inbox file processing -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for processing items from multiple inbox files.
;; Exercises mock-fs with dynamic file creation inside GTD directory.
;;
;; Test Coverage:
;; - Processing additional inbox after main inbox empty (1 test)
;; - Skipping empty additional inbox files (1 test)
;; - Processing multiple additional inboxes in order (1 test)
;; - Session state cleared on cancel (1 test)
;;
;; Migrated from test/additional-inboxes-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Additional Inbox Processing Tests

(deftest additional-inboxes/processes-after-main-inbox-empty ()
  "Processes items from additional inbox after main inbox is empty."
  ;; Setup: Create item in main inbox
  (capture-inbox-item "Main inbox item")

  ;; Setup: Create additional inbox file with an item
  (let* ((additional-inbox-file (f-join org-gtd-directory "additional-inbox.org"))
         (additional-buffer (find-file-noselect additional-inbox-file)))
    (with-current-buffer additional-buffer
      (insert "* Additional inbox item\n")
      (basic-save-buffer))

    ;; Configure additional inboxes
    (let ((org-gtd-additional-inbox-files (list additional-inbox-file)))
      ;; Process inbox - should start with main inbox
      (org-gtd-process-inbox)

      ;; Organize the main inbox item
      (organize-as-single-action)

      ;; After main inbox empty, should continue to additional inbox
      ;; We should now be clarifying the additional inbox item
      (assert-true (ogt-get-wip-buffer))
      (with-wip-buffer
        (assert-match "Additional inbox item" (buffer-string)))

      ;; Organize the additional inbox item
      (organize-as-single-action)

      ;; Both inboxes should now be empty
      (assert-nil (file-contains? (org-gtd-inbox-path) "Main inbox item"))
      (assert-nil (file-contains? additional-inbox-file "Additional inbox item")))))

(deftest additional-inboxes/skips-empty-files ()
  "Skips empty additional inbox files."
  ;; Setup: Create item in main inbox only
  (capture-inbox-item "Only main item")

  ;; Setup: Create empty additional inbox file
  (let* ((empty-inbox-file (f-join org-gtd-directory "empty-inbox.org"))
         (empty-buffer (find-file-noselect empty-inbox-file)))
    (with-current-buffer empty-buffer
      (basic-save-buffer))

    ;; Configure additional inboxes
    (let ((org-gtd-additional-inbox-files (list empty-inbox-file)))
      ;; Process inbox
      (org-gtd-process-inbox)

      ;; Organize the only item
      (organize-as-single-action)

      ;; Should have completed without error
      (assert-nil (file-contains? (org-gtd-inbox-path) "Only main item")))))

(deftest additional-inboxes/processes-multiple-in-order ()
  "Processes multiple additional inbox files in order."
  ;; Setup: Main inbox empty, two additional inboxes with items
  (let* ((inbox1-file (f-join org-gtd-directory "inbox1.org"))
         (inbox2-file (f-join org-gtd-directory "inbox2.org"))
         (inbox1-buffer (find-file-noselect inbox1-file))
         (inbox2-buffer (find-file-noselect inbox2-file)))

    (with-current-buffer inbox1-buffer
      (insert "* Item from inbox 1\n")
      (basic-save-buffer))

    (with-current-buffer inbox2-buffer
      (insert "* Item from inbox 2\n")
      (basic-save-buffer))

    ;; Configure additional inboxes
    (let ((org-gtd-additional-inbox-files (list inbox1-file inbox2-file)))
      ;; Process inbox (main inbox is empty)
      (org-gtd-process-inbox)

      ;; Should be processing item from inbox1
      (assert-true (ogt-get-wip-buffer))
      (with-wip-buffer
        (assert-match "Item from inbox 1" (buffer-string)))

      ;; Organize first item
      (organize-as-single-action)

      ;; Should now be processing item from inbox2
      (assert-true (ogt-get-wip-buffer))
      (with-wip-buffer
        (assert-match "Item from inbox 2" (buffer-string)))

      ;; Organize second item
      (organize-as-single-action)

      ;; All inboxes should be empty
      (assert-nil (file-contains? inbox1-file "Item from inbox 1"))
      (assert-nil (file-contains? inbox2-file "Item from inbox 2")))))

(deftest additional-inboxes/clears-session-state-on-cancel ()
  "Clears session state when user cancels with C-c C-k."
  ;; Setup: Create items in main inbox and additional inbox
  (capture-inbox-item "Main item")

  (let* ((additional-inbox-file (f-join org-gtd-directory "cancel-test-inbox.org"))
         (additional-buffer (find-file-noselect additional-inbox-file)))
    (with-current-buffer additional-buffer
      (insert "* Additional item\n")
      (basic-save-buffer))

    (let ((org-gtd-additional-inbox-files (list additional-inbox-file)))
      ;; Start processing
      (org-gtd-process-inbox)

      ;; Session should be active
      (assert-true org-gtd-process--session-active)

      ;; Cancel clarification
      (org-gtd-clarify-stop)

      ;; Session state should be cleared
      (assert-nil org-gtd-process--session-active)
      (assert-nil org-gtd-process--pending-inboxes)

      ;; Starting again should re-initialize properly
      (org-gtd-process-inbox)
      (assert-true org-gtd-process--session-active)
      (assert-equal (list additional-inbox-file) org-gtd-process--pending-inboxes)

      ;; Clean up by canceling again
      (org-gtd-clarify-stop))))

(provide 'additional-inboxes-test)

;;; additional-inboxes-test.el ends here
