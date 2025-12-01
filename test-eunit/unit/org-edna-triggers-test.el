;;; org-edna-triggers-test.el --- Tests for org-edna trigger integration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-edna trigger behavior in org-gtd projects.
;;
;; org-edna provides automatic state transitions based on TRIGGER properties.
;; These tests verify that org-edna triggers behave correctly when users
;; manually change task states, particularly around WAIT states.
;;
;; Bug context (from v3):
;; When user changed a NEXT task to WAIT, org-edna's triggers would incorrectly
;; promote child tasks from TODO to NEXT, even though the blocking task wasn't
;; completed.
;;
;; Migrated from test/org-edna-triggers-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; WAIT State Handling Tests

(deftest edna/no-child-promotion-when-parent-changes-to-wait ()
  "Child NOT promoted to NEXT when user changes parent from NEXT to WAIT.
This tests the v3 bug:
User manually changes Task A from NEXT → WAIT.
org-edna should NOT trigger Task B to become NEXT because WAIT means
\"blocked, waiting for something external\"."
  (capture-inbox-item "Project with dependency")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task A" :level 2)
    (make-task "Task B" :level 2)
    (organize-as-project))

  ;; Set up dependency: Task A blocks Task B
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (let ((task-a-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (let ((task-b-id (org-id-get-create)))
        ;; Set up A → B dependency
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id))))

  ;; Verify initial state: Task A should be NEXT, Task B should be TODO
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO")))

  ;; USER ACTION: Manually change Task A from NEXT to WAIT
  ;; This simulates the user pressing C-c C-t and selecting WAIT
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (org-todo "WAIT"))  ; This triggers org-edna

  ;; VERIFICATION: After org-edna triggers run:
  ;; - Task A should be WAIT (user's explicit choice)
  ;; - Task B should STILL BE TODO (not NEXT!)
  ;;   WAIT is not DONE, so dependent tasks should remain blocked
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (assert-equal "WAIT" (org-entry-get (point) "TODO"))

    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

(provide 'org-edna-triggers-test)

;;; org-edna-triggers-test.el ends here
