;;; task-management-commands-test.el --- Integration tests for task management -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for task management commands that require filesystem access.
;; These tests use mock-fs via ogt-eunit-with-mock-gtd macro.
;;
;; Migrated from test/task-management-commands-test.el (buttercup).
;; Pure unit tests are in test-eunit/unit/task-management-commands-test.el.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Automatic Next Action Updates (Story 15)

(deftest task-mgmt-int/blocker-done-makes-dependents-next ()
  "Automatically makes dependent tasks NEXT when blocker is marked DONE."
  ;; Test the core acceptance criteria: When I mark a task as DONE,
  ;; all tasks that were blocked by this task automatically become NEXT
  (let ((test-file (concat org-gtd-directory "test-deps.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
      (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

      ;; Add ORG_GTD_BLOCKS properties to Task A (bidirectional relationship)
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" "task-b-id")
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" "task-c-id")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Verify initial state: Task B and C should be TODO
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      ;; Mark Task A as DONE - this should trigger automatic updates
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task B and C automatically became NEXT
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO"))

      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

(deftest task-mgmt-int/partial-blocker-done-keeps-todo ()
  "Leaves tasks with remaining dependencies as TODO when only one blocker is completed."
  ;; Test that tasks with multiple dependencies only become NEXT when ALL blockers are complete
  (let ((test-file (concat org-gtd-directory "test-multi-deps.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
      (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Mark only Task A as DONE (Task B still TODO)
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task C remains TODO (still blocked by Task B)
      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      ;; Now mark Task B as DONE too
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Now Task C should become NEXT (all dependencies satisfied)
      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

(deftest task-mgmt-int/state-changes-reflected-immediately ()
  "Updates state changes immediately when blocker marked DONE."
  ;; Test that changes happen in the same transaction
  (let ((test-file (concat org-gtd-directory "test-immediate.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Mark Task A as DONE
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task B became NEXT immediately (same buffer, no file reload needed)
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

(provide 'task-management-commands-integration-test)

;;; task-management-commands-test.el ends here
