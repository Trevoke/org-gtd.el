;;; task-removal-test.el --- Tests for removing tasks from projects -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for removing tasks from projects while maintaining dependency graph integrity
;;

;;; Code:

;;;; Requirements

(require 'org-gtd)
(require 'org-gtd-task-management)
(require 'buttercup)

;; Load test helpers
(load (expand-file-name "helpers/setup.el"
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))

;;;; Test Setup

(describe
 "removing tasks from projects"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "remove leaf task (no children)"

  (it "removes task from project without reconnection when task has no children"
      ;; Scenario 1: Project A with T1 → T2 → T3 (leaf)
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
            (expect (member project-id task3-projects) :to-be-truthy)

            ;; Remove Task 3 from Project A
            ;; Mock user interaction to not reconnect (task has no children, so shouldn't ask)
            (org-gtd-remove-task-from-project)

            ;; Verify Task 3 no longer has Project A in ORG_GTD_PROJECT_IDS
            (let ((updated-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
              (expect (member project-id updated-projects) :not :to-be-truthy))

            ;; Verify Project A no longer references Task 3 in its graph
            ;; (Task 3 should not be in ORG_GTD_FIRST_TASKS or blocked by any task in project)
            (goto-char (point-min))
            (search-forward "Project A")
            (org-back-to-heading t)
            (let ((first-tasks (split-string (or (org-entry-get (point) "ORG_GTD_FIRST_TASKS") ""))))
              (expect (member task3-id first-tasks) :not :to-be-truthy)))))))

 (describe
  "remove middle task with one parent - auto-reconnect"

  (it "reconnects child to parent when removing middle task with user confirmation"
      ;; Scenario 2: Project A with T1 → T2 → T3
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
              (expect (member task1-id task2-parents) :to-be-truthy)
              (expect (length task2-children) :to-equal 1)

              (let ((task3-id (car task2-children)))

                ;; Mock user interaction: answer yes to reconnection
                (spy-on 'y-or-n-p :and-return-value t)

                ;; Remove Task 2 from Project A
                (org-gtd-remove-task-from-project)

                ;; Verify Task 2 no longer belongs to Project A
                (let ((task2-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
                  (expect (member project-id task2-projects) :not :to-be-truthy))

                ;; Verify Task 1 now blocks Task 3 directly
                (goto-char (point-min))
                (search-forward "Project A")
                (search-forward "Task 1")
                (org-back-to-heading t)
                (let ((task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
                  (expect (member task3-id task1-blocks) :to-be-truthy)
                  (expect (member task2-id task1-blocks) :not :to-be-truthy))

                ;; Verify Task 3 now depends on Task 1 directly
                (goto-char (point-min))
                (search-forward "Project A")
                (search-forward "Task 3")
                (org-back-to-heading t)
                (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
                  (expect (member task1-id task3-depends) :to-be-truthy)
                  (expect (member task2-id task3-depends) :not :to-be-truthy))))))))))

(describe
 "remove task from multi-project task"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "removes project ID while preserving task in other projects"
      ;; Scenario 5: Task T1 belongs to both Project A and Project B
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
            (expect (member project-a-id task-projects) :to-be-truthy)
            (expect (member project-b-id task-projects) :to-be-truthy))

          ;; Remove Shared Task from Project A
          (org-gtd-remove-task-from-project)

          ;; Verify Shared Task no longer belongs to Project A
          (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
            (expect (member project-a-id task-projects) :not :to-be-truthy)
            (expect (member project-b-id task-projects) :to-be-truthy))

          ;; Verify Shared Task still exists (not archived)
          (goto-char (point-min))
          (expect (search-forward "Shared Task" nil t) :to-be-truthy)))))

(describe
 "extract task from project - Story 1: multi-project task"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "removes only the specified project ID and keeps task in place with ORG_GTD='Actions'"
      ;; Given: A task belongs to multiple projects (has multiple IDs in ORG_GTD_PROJECT_IDS)
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
              (expect (member project-a-id task-projects) :to-be-truthy)
              (expect (member project-b-id task-projects) :to-be-truthy))

            ;; Verify initial ORG_GTD property is "Actions" (project task)
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

            ;; Extract task from Project A (current project)
            (org-gtd-remove-task-from-project)

            ;; Verify task removed from Project A but still in Project B
            (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
              (expect (member project-a-id task-projects) :not :to-be-truthy)
              (expect (member project-b-id task-projects) :to-be-truthy))

            ;; Verify ORG_GTD property is still "Actions" (remains a project task)
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

            ;; Verify task stayed in the same location (not moved/refiled)
            (expect (point) :to-equal initial-position)

            ;; Verify task still exists in the buffer
            (goto-char (point-min))
            (expect (search-forward "Multi-project Task" nil t) :to-be-truthy))))))

(describe
 "extract task from project - Story 2: single-project task (last project)"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "converts task to single action when removing from last/only project"
      ;; Given: A task belongs to only one project (has single ID in ORG_GTD_PROJECT_IDS)
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
              (expect (length task-projects) :to-equal 1)
              (expect (member project-a-id task-projects) :to-be-truthy))

            ;; Verify initial ORG_GTD property is "Actions" (project task)
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

            ;; Extract task from Project A (the only project)
            (org-gtd-remove-task-from-project)

            ;; Verify task no longer has any project IDs
            (let ((task-projects (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
              (expect (or (null task-projects) (equal task-projects '(""))) :to-be-truthy))

            ;; Verify ORG_GTD property stayed as "Actions" (both project tasks and single actions use "Actions")
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

            ;; Verify TODO state changed to NEXT (single action)
            (expect (org-entry-get (point) "TODO") :to-equal (org-gtd-keywords--next))

            ;; Verify task stayed in the same location (not moved/refiled)
            (expect (point) :to-equal initial-position)

            ;; Verify task still exists in the buffer at original location
            (goto-char (point-min))
            (search-forward "Project A")
            (expect (search-forward "Single-project Task" nil t) :to-be-truthy))))))

;;; task-removal-test.el ends here
