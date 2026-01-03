;;; task-removal-test.el --- Tests for removing tasks from projects -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for removing tasks from projects while maintaining dependency graph integrity.
;;
;; Migrated from test/task-removal-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-task-management)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;;; Remove leaf task tests

(deftest task-removal/removes-leaf-task-without-reconnection ()
  "Removes task from project without reconnection when task has no children."
  ;; Scenario: Project A with T1 → T2 → T3 (leaf)
  ;; Remove T3: Just remove from project, no reconnection needed
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2\n** Task 3")
  (organize-as-project)

  ;; Get the project and task IDs
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))

      ;; Verify Task 3 exists and belongs to project
      (goto-char (point-min))
      (search-forward "Project A")
      (search-forward "Task 3")
      (org-back-to-heading t)
      (let ((task3-id (org-entry-get (point) "ID"))
            (task3-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))

        ;; Verify Task 3 belongs to Project A
        (assert-true (member project-id task3-projects))

        ;; Remove Task 3 from Project A
        (org-gtd-remove-task-from-project)

        ;; Verify Task 3 no longer has Project A in ORG_GTD_PROJECT_IDS
        (let ((updated-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (assert-nil (member project-id updated-projects)))

        ;; Verify Project A no longer references Task 3 in its graph
        (goto-char (point-min))
        (search-forward "Project A")
        (org-back-to-heading t)
        (let ((first-tasks (split-string (or (org-entry-get (point) "ORG_GTD_FIRST_TASKS") ""))))
          (assert-nil (member task3-id first-tasks)))))))

;;;; Remove middle task tests

(deftest task-removal/reconnects-child-to-parent-when-removing-middle-task ()
  "Reconnects child to parent when removing middle task with user confirmation."
  ;; Scenario: Project A with T1 → T2 → T3
  ;; Remove T2 with reconnection: Should connect T1 → T3
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2\n** Task 3")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))

      ;; Get task IDs
      (goto-char (point-min))
      (search-forward "Project A")
      (search-forward "Task 1")
      (org-back-to-heading t)
      (let ((task1-id (org-entry-get (point) "ID")))

        (goto-char (point-min))
        (search-forward "Project A")
        (search-forward "Task 2")
        (org-back-to-heading t)
        (let ((task2-id (org-entry-get (point) "ID"))
              (task2-parents (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
              (task2-children (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

          ;; Verify initial state: Task 2 has Task 1 as parent and Task 3 as child
          (assert-true (member task1-id task2-parents))
          (assert-equal 1 (length task2-children))

          (let ((task3-id (car task2-children)))

            ;; Mock user interaction: answer yes to reconnection
            (with-stub y-or-n-p t
              ;; Remove Task 2 from Project A
              (org-gtd-remove-task-from-project))

            ;; Verify Task 2 no longer belongs to Project A
            (let ((task2-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
              (assert-nil (member project-id task2-projects)))

            ;; Verify Task 1 now blocks Task 3 directly
            (goto-char (point-min))
            (search-forward "Project A")
            (search-forward "Task 1")
            (org-back-to-heading t)
            (let ((task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
              (assert-true (member task3-id task1-blocks))
              (assert-nil (member task2-id task1-blocks)))

            ;; Verify Task 3 now depends on Task 1 directly
            (goto-char (point-min))
            (search-forward "Project A")
            (search-forward "Task 3")
            (org-back-to-heading t)
            (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
              (assert-true (member task1-id task3-depends))
              (assert-nil (member task2-id task3-depends)))))))))

;;;; Multi-project task tests

(deftest task-removal/preserves-task-in-other-projects ()
  "Removes project ID while preserving task in other projects."
  ;; Scenario: Task T1 belongs to both Project A and Project B
  ;; Remove T1 from Project A: T1 should still exist in Project B
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Shared Task")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    ;; Get Project A's ID and Shared Task's ID
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-entry-get (point) "ID"))
          (project-b-id "fake-project-b-id"))

      (search-forward "Shared Task")
      (org-back-to-heading t)

      ;; Manually add a second project ID to simulate multi-project scenario
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-b-id)

      ;; Verify Shared Task belongs to both projects
      (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
        (assert-true (member project-a-id task-projects))
        (assert-true (member project-b-id task-projects)))

      ;; Remove Shared Task from Project A
      (org-gtd-remove-task-from-project)

      ;; Verify Shared Task no longer belongs to Project A
      (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
        (assert-nil (member project-a-id task-projects))
        (assert-true (member project-b-id task-projects)))

      ;; Verify Shared Task still exists (not archived)
      (goto-char (point-min))
      (assert-true (search-forward "Shared Task" nil t)))))

;;;; Extract task from project tests

(deftest task-removal/extract-multi-project-keeps-org-gtd-actions ()
  "Removes only the specified project ID and keeps task with ORG_GTD='Actions'."
  ;; Given: A task belongs to multiple projects
  ;; When: I extract/remove from one project
  ;; Then: Task should remove only that project ID, stay in place, keep ORG_GTD="Actions"
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Multi-project Task")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    ;; Get Project A's ID
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-entry-get (point) "ID"))
          (project-b-id "fake-project-b-id"))

      ;; Find the task and add a second project ID
      (search-forward "Multi-project Task")
      (org-back-to-heading t)
      (let ((task-id (org-entry-get (point) "ID"))
            (initial-position (point)))

        ;; Manually add a second project ID to simulate multi-project scenario
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-b-id)

        ;; Verify task belongs to both projects
        (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (assert-true (member project-a-id task-projects))
          (assert-true (member project-b-id task-projects)))

        ;; Verify initial ORG_GTD property is "Actions" (project task)
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

        ;; Extract task from Project A (current project)
        (org-gtd-remove-task-from-project)

        ;; Verify task removed from Project A but still in Project B
        (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (assert-nil (member project-a-id task-projects))
          (assert-true (member project-b-id task-projects)))

        ;; Verify ORG_GTD property is still "Actions" (remains a project task)
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

        ;; Verify task stayed in the same location (not moved/refiled)
        (assert-equal initial-position (point))

        ;; Verify task still exists in the buffer
        (goto-char (point-min))
        (assert-true (search-forward "Multi-project Task" nil t))))))

(deftest task-removal/extract-single-project-converts-to-single-action ()
  "Converts task to single action when removing from last/only project."
  ;; Given: A task belongs to only one project
  ;; When: I extract/remove from that project
  ;; Then: Task should keep ORG_GTD as "Actions", change TODO to NEXT, stay in place
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Single-project Task")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    ;; Get Project A's ID
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-entry-get (point) "ID")))

      ;; Find the task
      (search-forward "Single-project Task")
      (org-back-to-heading t)
      (let ((task-id (org-entry-get (point) "ID"))
            (initial-position (point)))

        ;; Verify task belongs to only one project
        (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (assert-equal 1 (length task-projects))
          (assert-true (member project-a-id task-projects)))

        ;; Verify initial ORG_GTD property is "Actions" (project task)
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

        ;; Extract task from Project A (the only project)
        (org-gtd-remove-task-from-project)

        ;; Verify task no longer has any project IDs
        (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (assert-true (or (null task-projects) (equal task-projects '("")))))

        ;; Verify ORG_GTD property stayed as "Actions"
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

        ;; Verify TODO state changed to NEXT (single action)
        (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO"))

        ;; Verify task stayed in the same location (not moved/refiled)
        (assert-equal initial-position (point))

        ;; Verify task still exists in the buffer at original location
        (goto-char (point-min))
        (search-forward "Project A")
        (assert-true (search-forward "Single-project Task" nil t))))))

(provide 'task-removal-test)

;;; task-removal-test.el ends here
