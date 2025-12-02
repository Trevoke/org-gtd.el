;;; project-fix-keywords-test.el --- Integration tests for fix TODO keywords -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for `org-gtd-projects-fix-all-todo-keywords`.
;; Tests the global fix function for multi-project tasks, WAIT preservation,
;; and DAG dependency handling.
;;
;; Migrated from test/project-test.el (buttercup).
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

;;; Single Project Baseline

(deftest fix-kw/single-project-baseline ()
  "Handles single project correctly (baseline - ensures no regression).
When Task 1 is marked DONE, Task 2 should become NEXT, Task 3 stays TODO."
  (create-project "Single Project")

  ;; Manually mark Task 1 as DONE
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "DONE"))

  ;; Run the global fix function
  (org-gtd-projects-fix-all-todo-keywords)

  ;; Verify results
  (with-current-buffer (org-gtd--default-file)
    ;; Task 1: DONE (preserved)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "DONE" (org-entry-get (point) "TODO"))

    ;; Task 2: should be NEXT (dependency satisfied)
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    ;; Task 3: should be TODO (depends on Task 2)
    (goto-char (point-min))
    (search-forward "Task 3")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

;;; Multi-Project Task Tests

(deftest fix-kw/multi-project-task-next-when-ready-in-all ()
  "Marks multi-project task NEXT when ready in ALL projects (AND semantics).
Shared Task belongs to both Project Alpha and Project Beta; should be NEXT."
  ;; Create Project Alpha with shared task
  (capture-inbox-item "Project Alpha")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Shared Task" :level 2)
    (make-task "Alpha Task" :level 2)
    (organize-as-project))

  ;; Create Project Beta with separate task
  (capture-inbox-item "Project Beta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Beta Task" :level 2)
    (organize-as-project))

  ;; Share "Shared Task" with Project Beta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared Task")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Project Beta")
      (org-back-to-heading t)
      (let ((beta-id (org-id-get-create)))
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)
        (goto-char (point-min))
        (search-forward "Shared Task")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" beta-id))))

  ;; Run the global fix function
  (org-gtd-projects-fix-all-todo-keywords)

  ;; Verify: Shared Task should be NEXT (ready in BOTH projects)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared Task")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

;;; WAIT Task Preservation

(deftest fix-kw/preserves-wait-tasks ()
  "Preserves WAIT tasks (never changes them to TODO or NEXT).
WAIT is user-explicitly set and should never be automatically changed."
  ;; Create a project with 3 tasks in sequence
  (create-project "Project With Wait")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (let ((task-1-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Task 2")
      (org-back-to-heading t)
      (let ((task-2-id (org-id-get-create)))
        (goto-char (point-min))
        (search-forward "Task 3")
        (org-back-to-heading t)
        (let ((task-3-id (org-id-get-create)))
          ;; Set up dependencies: Task 1 -> Task 2 -> Task 3
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-2-id)
          (goto-char (point-min))
          (search-forward "Task 2")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-3-id)
          (goto-char (point-min))
          (search-forward "Task 3")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-2-id)))))

  ;; Mark Task 2 as WAIT (user explicitly marked it)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "WAIT"))

  ;; Complete Task 1, so Task 2 would normally become NEXT
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "DONE"))

  ;; Run the global fix function
  (org-gtd-projects-fix-all-todo-keywords)

  ;; Verify results
  (with-current-buffer (org-gtd--default-file)
    ;; Task 2 should still be WAIT (never auto-changed)
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (assert-equal "WAIT" (org-entry-get (point) "TODO"))

    ;; Task 3 should be TODO (blocked by WAIT task)
    (goto-char (point-min))
    (search-forward "Task 3")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

;;; DAG Branching Tests

(deftest fix-kw/simple-branching-paths ()
  "Handles simple branching paths (A -> B and A -> D).
When A is DONE, both B and D should become NEXT (independent paths)."
  (create-project "Simple Branch")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (let ((task-1-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Task 2")
      (org-back-to-heading t)
      (let ((task-2-id (org-id-get-create)))
        (goto-char (point-min))
        (search-forward "Task 3")
        (org-back-to-heading t)
        (let ((task-3-id (org-id-get-create)))

          ;; Clear default sequential dependencies created by organize-as-project
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (org-entry-delete (point) "ORG_GTD_BLOCKS")
          (goto-char (point-min))
          (search-forward "Task 2")
          (org-back-to-heading t)
          (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
          (org-entry-delete (point) "ORG_GTD_BLOCKS")
          (goto-char (point-min))
          (search-forward "Task 3")
          (org-back-to-heading t)
          (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

          ;; Set up: Task 1 -> Task 2, Task 1 -> Task 3 (branching paths)
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-2-id)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-3-id)

          (goto-char (point-min))
          (search-forward "Task 2")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)

          (goto-char (point-min))
          (search-forward "Task 3")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)

          ;; Set Task 1 as the only first task
          (goto-char (point-min))
          (search-forward "Simple Branch")
          (org-back-to-heading t)
          (org-entry-delete (point) "ORG_GTD_FIRST_TASKS")
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-1-id)))))

  ;; Complete Task 1
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "DONE"))

  ;; Run fix
  (org-gtd-projects-fix-all-todo-keywords)

  ;; Verify: Both Task 2 and Task 3 should be NEXT
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    (goto-char (point-min))
    (search-forward "Task 3")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

(deftest fix-kw/independent-dag-paths-with-wait ()
  "Allows NEXT in independent DAG paths when another path has WAIT.
DAG: A -> B (WAIT) -> C, A -> D.
When A is DONE: B stays WAIT, C stays TODO (blocked), D becomes NEXT."
  (capture-inbox-item "Project With Branches")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task A" :level 2)
    (make-task "Task B" :level 2)
    (make-task "Task C" :level 2)
    (make-task "Task D" :level 2)
    (organize-as-project))

  ;; Set up the DAG structure
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (let ((task-a-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (let ((task-b-id (org-id-get-create)))
        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (let ((task-c-id (org-id-get-create)))
          (goto-char (point-min))
          (search-forward "Task D")
          (org-back-to-heading t)
          (let ((task-d-id (org-id-get-create)))

            ;; Clear default sequential dependencies
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_BLOCKS")
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
            (org-entry-delete (point) "ORG_GTD_BLOCKS")
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
            (org-entry-delete (point) "ORG_GTD_BLOCKS")
            (goto-char (point-min))
            (search-forward "Task D")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

            ;; A -> B -> C
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-d-id)

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-b-id)

            ;; A -> D (independent path)
            (goto-char (point-min))
            (search-forward "Task D")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

            ;; Set Task A as the only first task of the project
            (goto-char (point-min))
            (search-forward "Project With Branches")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_FIRST_TASKS")
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id))))))

  ;; Complete Task A
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "DONE"))

  ;; Mark Task B as WAIT
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "WAIT"))

  ;; Run the global fix function
  (org-gtd-projects-fix-all-todo-keywords)

  ;; Verify results
  (with-current-buffer (org-gtd--default-file)
    ;; Task B stays WAIT
    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal "WAIT" (org-entry-get (point) "TODO"))

    ;; Task C stays TODO (blocked by WAIT B)
    (goto-char (point-min))
    (search-forward "Task C")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))

    ;; Task D becomes NEXT (independent path from B)
    (goto-char (point-min))
    (search-forward "Task D")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

(provide 'project-fix-keywords-test)

;;; project-fix-keywords-test.el ends here
