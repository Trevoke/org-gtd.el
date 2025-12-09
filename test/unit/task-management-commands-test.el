;;; task-management-commands-test.el --- Unit tests for task management commands -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for task dependency commands (BLOCKS/DEPENDS_ON relationships).
;; These tests use with-temp-buffer and don't require mock-fs.
;;
;; Migrated from test/task-management-commands-test.el (buttercup).
;; Integration tests that require GTD file setup are in a separate file.
;;

;;; Code:

(require 'e-unit)
(require 'e-unit-mock)
(require 'org-gtd)
(require 'org-gtd-task-management)

;; Initialize e-unit-mock
(e-unit-mock-initialize)

;;; org-gtd-task-add-blockers command tests

(deftest task-mgmt/add-blockers-creates-bidirectional-relationship ()
  "Creates bidirectional BLOCKS/DEPENDS_ON relationship between tasks."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

    ;; Go to Task B and add Task A as blocker
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    ;; Mock the task selection to return single task as list
    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-a-id")
      (org-gtd-task-add-blockers)

      ;; Verify Task B has DEPENDS_ON property
      (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
        (assert-equal '("task-a-id") depends-on))

      ;; Verify Task A has BLOCKS property
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
        (assert-equal '("task-b-id") blocks)))))

(deftest task-mgmt/add-blockers-multiple-creates-bidirectional ()
  "Creates bidirectional relationships with multiple blockers."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

    ;; Go to Task C and add both Task A and B as blockers
    (goto-char (point-min))
    (re-search-forward "Task C")
    (org-back-to-heading t)

    ;; Mock the task selection to return multiple IDs
    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-a-id" "task-b-id")
      (org-gtd-task-add-blockers)

      ;; Verify Task C has DEPENDS_ON property with both IDs
      (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
        (assert-equal '("task-a-id" "task-b-id") (sort depends-on 'string<)))

      ;; Verify Task A has BLOCKS property
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
        (assert-equal '("task-c-id") blocks))

      ;; Verify Task B has BLOCKS property
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
        (assert-equal '("task-c-id") blocks)))))

;;; Circular dependency detection tests

(deftest task-mgmt/prevents-direct-circular-dependency ()
  "Prevents direct circular dependency (A blocks B, B cannot block A)."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

    ;; First create A blocks B relationship
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-a-id")
      (org-gtd-task-add-blockers))

    ;; Verify A blocks B relationship was created
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (assert-equal '("task-b-id") (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal '("task-a-id") (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

    ;; Now try to create reverse relationship: B blocks A (should fail)
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)

    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-b-id")
      ;; This should signal an error due to circular dependency
      (assert-raises 'user-error
        (org-gtd-task-add-blockers)))

    ;; Verify that the circular relationship was NOT created
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))))

(deftest task-mgmt/prevents-indirect-circular-dependency ()
  "Prevents indirect circular dependency (A -> B -> C -> A)."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

    ;; Create A blocks B
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)
    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-a-id")
      (org-gtd-task-add-blockers))

    ;; Create B blocks C
    (goto-char (point-min))
    (re-search-forward "Task C")
    (org-back-to-heading t)
    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-b-id")
      (org-gtd-task-add-blockers))

    ;; Now try to create C blocks A (would create cycle)
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-c-id")
      (assert-raises 'user-error
        (org-gtd-task-add-blockers)))

    ;; Verify that the circular relationship was NOT created
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

    (goto-char (point-min))
    (re-search-forward "Task C")
    (org-back-to-heading t)
    (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-nil (member "task-a-id" blocks)))))

;;; Remove blockers tests

(deftest task-mgmt/remove-blockers-updates-relationships ()
  "Removes selected blocking relationships from task with existing blockers."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

    ;; Go to Task C (which has blockers A and B)
    (goto-char (point-min))
    (re-search-forward "Task C")
    (org-back-to-heading t)

    ;; Mock the task selection to remove just Task A as blocker
    (with-spy org-gtd-task-management--select-multiple-blocking-task-ids calls '("task-a-id")
      (org-gtd-task-remove-blockers)

      ;; Verify Task C no longer depends on Task A, but still depends on Task B
      (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
        (assert-equal '("task-b-id") depends-on))

      ;; Verify Task A no longer blocks Task C
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

      ;; Verify Task B still blocks Task C
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
        (assert-equal '("task-c-id") blocks)))))

(deftest task-mgmt/remove-blockers-handles-no-blockers ()
  "Handles task with no blockers gracefully."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")

    ;; Go to Task A (which has no blockers)
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)

    ;; This should not error, just show a helpful message
    (with-spy message calls nil
      (org-gtd-task-remove-blockers)
      (assert-true (spy-called-p calls))
      (let* ((call-args (spy-first-call calls))
             (format-string (car call-args)))
        (assert-match "has no blockers to remove" format-string)))))

;;; Add dependents tests

(deftest task-mgmt/add-dependents-creates-bidirectional ()
  "Creates bidirectional relationship where current task blocks selected tasks."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

    ;; Go to Task A and add Tasks B and C as dependents
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)

    ;; Mock both task selection and message
    (with-spy org-gtd-task-management--select-multiple-task-ids task-calls '("task-b-id" "task-c-id")
      (with-spy message msg-calls nil
        (org-gtd-task-add-dependents)

        ;; Verify Task A has BLOCKS property with both Task B and C
        (assert-equal '("task-b-id" "task-c-id")
                      (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

        ;; Verify Task B has DEPENDS_ON property with Task A
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal '("task-a-id")
                      (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

        ;; Verify Task C has DEPENDS_ON property with Task A
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (assert-equal '("task-a-id")
                      (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))))))

;;; Regular org file tests

(deftest task-mgmt/add-blockers-works-in-regular-files ()
  "Adds task dependencies in regular org files with confirmation message."
  (with-temp-buffer
    (org-mode)
    (insert "* Project Example\n")
    (insert "** Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "** Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

    ;; Go to Task B and add Task A as blocker
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    (with-spy message msg-calls nil
      (with-spy org-gtd-task-management--select-multiple-task-ids task-calls '("task-a-id")
        (org-gtd-task-add-blockers)

        ;; Verify confirmation message was shown
        (assert-true (spy-called-p msg-calls))

        ;; Verify the relationships were created
        (assert-true (member "task-a-id"
                             (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

        ;; Task A should have BLOCKS
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (assert-true (member "task-b-id"
                             (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))))))

;;; Clear relationships tests

(deftest task-mgmt/clear-relationships-removes-all ()
  "Removes all BLOCKS and DEPENDS_ON properties and updates related tasks."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id task-c-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:ORG_GTD_BLOCKS: task-d-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
    (insert "* Task D\n:PROPERTIES:\n:ID: task-d-id\n:ORG_GTD_DEPENDS_ON: task-b-id\n:END:\n\n")

    ;; Move to Task B (which has both BLOCKS and DEPENDS_ON)
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    (org-gtd-task-clear-relationships)

    ;; Verify Task B has no BLOCKS or DEPENDS_ON properties
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

    ;; Verify Task A no longer blocks Task B (but still blocks Task C)
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-nil (member "task-b-id" blocks-list))
      (assert-true (member "task-c-id" blocks-list)))

    ;; Verify Task D no longer depends on Task B
    (goto-char (point-min))
    (re-search-forward "Task D")
    (org-back-to-heading t)
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

    ;; Verify Task C is unchanged (still depends on Task A)
    (goto-char (point-min))
    (re-search-forward "Task C")
    (org-back-to-heading t)
    (assert-equal '("task-a-id") (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))))

(deftest task-mgmt/clear-relationships-shows-confirmation ()
  "Shows confirmation message about cleared relationships."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

    ;; Move to Task A
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)

    (with-spy message calls nil
      (org-gtd-task-clear-relationships)
      (assert-true (spy-called-p calls)))))

(deftest task-mgmt/clear-relationships-handles-no-relationships ()
  "Handles task with no relationships gracefully."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")

    ;; Move to Task A
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)

    (with-spy message calls nil
      (org-gtd-task-clear-relationships)
      (assert-true (spy-called-p calls))
      (let* ((call-args (spy-first-call calls))
             (format-string (car call-args)))
        (assert-match "has no relationships to clear" format-string)))))

;;; Relationship visualization tests

(deftest task-mgmt/show-relationships-formatted-display ()
  "Shows task dependency relationships in a formatted display."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id task-c-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:ORG_GTD_BLOCKS: task-d-id\n:END:\n\n")
    (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
    (insert "* Task D\n:PROPERTIES:\n:ID: task-d-id\n:ORG_GTD_DEPENDS_ON: task-b-id\n:END:\n\n")

    ;; Position on Task B (has both blocking and blocked relationships)
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    ;; Mock task name resolution function with a fake
    (with-fake org-gtd-task-management--get-heading-for-id
        (lambda (id)
          (cond
           ((string= id "task-a-id") "Task A")
           ((string= id "task-b-id") "Task B")
           ((string= id "task-c-id") "Task C")
           ((string= id "task-d-id") "Task D")
           (t "Unknown Task")))
      (let ((relationship-display (org-gtd-task-show-relationships)))
        ;; Expect formatted display showing blockers and dependents
        (assert-match "Task B Dependencies:" relationship-display)
        (assert-match "Blocked by:.*Task A" relationship-display)
        (assert-match "Blocks:.*Task D" relationship-display)
        ;; Task C is not directly related to Task B
        (assert-nil (string-match-p "Task C" relationship-display))))))

(deftest task-mgmt/show-relationships-no-relationships ()
  "Shows clear message when task has no relationships."
  (with-temp-buffer
    (org-mode)
    (insert "* Isolated Task\n:PROPERTIES:\n:ID: isolated-task-id\n:END:\n\n")

    ;; Position on the isolated task
    (goto-char (point-min))
    (re-search-forward "Isolated Task")
    (org-back-to-heading t)

    (let ((relationship-display (org-gtd-task-show-relationships)))
      ;; Should show clear message for no relationships
      (assert-match "Isolated Task.*no dependency relationships" relationship-display))))

;;; Lazy ID Creation (Story 9)

(deftest task-mgmt/lazy-id-creation-for-all-tasks ()
  "Automatically creates IDs for tasks without them when adding dependencies."
  (with-temp-buffer
    (org-mode)
    ;; Create tasks WITHOUT IDs initially
    (insert "* Task A\n\n")
    (insert "* Task B\n\n")

    ;; Go to Task B (no ID) and add Task A (no ID) as blocker
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    ;; Capture the initial state - no IDs should exist
    (assert-nil (org-entry-get (point) "ID"))
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ID"))

    ;; Go back to Task B to run the command
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    (let* ((task-a-id nil)
           (task-b-id nil))
      (with-fake org-gtd-task-management--select-multiple-task-ids
          (lambda (_prompt)
            ;; First trigger the real task collection to create IDs
            (org-gtd-task-management--collect-all-task-info)
            ;; Now capture Task A's ID after it gets created during collection
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "Task A")
              (org-back-to-heading t)
              (setq task-a-id (org-entry-get (point) "ID")))
            (list task-a-id))
        (org-gtd-task-add-blockers)

        ;; After the command, both tasks should have IDs
        ;; Task B should have an ID (current task)
        (setq task-b-id (org-entry-get (point) "ID"))
        (assert-true task-b-id)
        (assert-equal 0 (string-match "^[a-z0-9-]+$" task-b-id))

        ;; Task A should have an ID (selected as blocker)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (setq task-a-id (org-entry-get (point) "ID"))
        (assert-true task-a-id)
        (assert-equal 0 (string-match "^[a-z0-9-]+$" task-a-id))

        ;; Verify the blocking relationships were created correctly
        (assert-true (member task-b-id (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (assert-true (member task-a-id (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))))))

(deftest task-mgmt/lazy-id-creation-mixed-scenario ()
  "Creates IDs automatically for tasks without them when some already have IDs."
  (with-temp-buffer
    (org-mode)
    ;; Task A has no ID, Task B has pre-existing ID
    (insert "* Task A\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: existing-task-b-id\n:END:\n\n")

    ;; Go to Task B (has ID) and add Task A (no ID) as blocker
    (goto-char (point-min))
    (re-search-forward "Task B")
    (org-back-to-heading t)

    ;; Verify initial state
    (assert-equal "existing-task-b-id" (org-entry-get (point) "ID"))
    (goto-char (point-min))
    (re-search-forward "Task A")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ID"))

    (let ((task-a-id nil))
      (with-fake org-gtd-task-management--select-multiple-task-ids
          (lambda (_prompt)
            ;; First trigger the real task collection to create IDs
            (org-gtd-task-management--collect-all-task-info)
            ;; The selection should create ID for Task A during collection
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "Task A")
              (org-back-to-heading t)
              (setq task-a-id (org-entry-get (point) "ID")))
            (list task-a-id))

        ;; Go back to Task B to run the command
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (org-gtd-task-add-blockers)

        ;; Task A should now have an ID
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (setq task-a-id (org-entry-get (point) "ID"))
        (assert-true task-a-id)

        ;; Task B should keep its pre-existing ID
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal "existing-task-b-id" (org-entry-get (point) "ID"))

        ;; Verify relationships were created
        (assert-true (member task-a-id (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (assert-true (member "existing-task-b-id" (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))))))

;;; Dependency Helper Window (Story 11)

(deftest task-mgmt/dependency-helper-displays-in-clarify-buffer ()
  "Displays live dependency view in clarify buffer when org-gtd-clarify-display-helper-buffer is enabled."
  (with-temp-buffer
    ;; Create a clarify buffer with multiple tasks
    (org-gtd-clarify-mode)

    ;; Insert project structure with dependencies
    (insert "* Build Deck\n")
    (insert "** Get permits\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD_BLOCKS: get-materials\n")
    (insert ":ID: get-permits\n")
    (insert ":END:\n")
    (insert "** Get materials\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD_DEPENDS_ON: get-permits\n")
    (insert ":ORG_GTD_BLOCKS: install-decking\n")
    (insert ":ID: get-materials\n")
    (insert ":END:\n")
    (insert "** Install decking\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD_DEPENDS_ON: get-materials\n")
    (insert ":ID: install-decking\n")
    (insert ":END:\n")
    (insert "** Paint deck (orphaned task)\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: paint-deck\n")
    (insert ":END:\n")

    ;; Enable the helper buffer feature
    (let ((org-gtd-clarify-display-helper-buffer t))
      ;; Trigger the helper window display
      (org-gtd-clarify-display-dependency-helper)

      ;; Check that helper window was created
      (let ((helper-buffer (get-buffer "*Org GTD Project Dependencies*")))
        (assert-true helper-buffer)
        (with-current-buffer helper-buffer
          (let ((helper-content (buffer-string)))
            ;; Should show project name
            (assert-match "Project name: Build Deck" helper-content)
            ;; Should show task relationships in format: (depends_on, ...) -> task -> (blocks, ...)
            (assert-match "() -> Get permits -> (Get materials)" helper-content)
            (assert-match "(Get permits) -> Get materials -> (Install decking)" helper-content)
            (assert-match "(Get materials) -> Install decking -> ()" helper-content)
            ;; Should show orphaned tasks section
            (assert-match "Orphaned tasks:" helper-content)
            (assert-match "Paint deck (orphaned task)" helper-content)))))))

(provide 'task-management-commands-test)

;;; task-management-commands-test.el ends here
