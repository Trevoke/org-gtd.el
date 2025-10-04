;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Multi-project task support"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              ;; Clear org-id locations to prevent pollution from previous tests
              (setq org-id-locations nil)
              (setq org-id-files nil))

 (after-each (ogt--close-and-delete-files)
             ;; Clear org-id locations after tests
             (setq org-id-locations nil)
             (setq org-id-files nil)
             (remove-hook 'post-command-hook 'org-add-log-note))

 (describe
  "Unit tests: multi-project archiving logic"

  (it "removes project ID from ORG_GTD_PROJECT_IDS when archiving"
      ;; Create a single task with multiple project IDs
      (ogt-capture-single-item "Test Task")
      (org-gtd-process-inbox)
      (ogt-clarify-as-single-action)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test Task")
        (org-back-to-heading t)

        ;; Manually add multiple project IDs to simulate shared task
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-2")

        ;; Verify both IDs are present
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect (length project-ids) :to-equal 2)
          (expect (member "proj-id-1" project-ids) :to-be-truthy)
          (expect (member "proj-id-2" project-ids) :to-be-truthy))

        ;; Call helper function to remove one project ID
        (require 'org-gtd-archive)
        (org-gtd--remove-project-id-from-task (point) "proj-id-1")

        ;; Verify only proj-id-2 remains
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect (length project-ids) :to-equal 1)
          (expect (member "proj-id-2" project-ids) :to-be-truthy)
          (expect (member "proj-id-1" project-ids) :not :to-be-truthy))))

  (it "archives task when ORG_GTD_PROJECT_IDS becomes empty"
      ;; Create a single task with one project ID
      (ogt-capture-single-item "Test Task")
      (org-gtd-process-inbox)
      (ogt-clarify-as-single-action)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test Task")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Add single project ID
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")

        ;; Verify task exists in tasks file
        (let ((tasks-content-before (buffer-string)))
          (expect tasks-content-before :to-match "Test Task"))

        ;; Call helper to remove project ID and archive if empty
        (require 'org-gtd-archive)
        (org-gtd--archive-task-if-no-projects (point) "proj-id-1")

        ;; Verify task is archived
        (let ((tasks-content (ogt--buffer-string (org-gtd--default-file)))
              (archived-content (ogt--archive-string)))
          (expect tasks-content :not :to-match "Test Task")
          (expect archived-content :to-match "Test Task"))))

  (it "does not archive task when ORG_GTD_PROJECT_IDS still has values"
      ;; Create a single task with multiple project IDs
      (ogt-capture-single-item "Test Task")
      (org-gtd-process-inbox)
      (ogt-clarify-as-single-action)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test Task")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Add multiple project IDs
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-1")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "proj-id-2")

        ;; Call helper to remove one project ID
        (require 'org-gtd-archive)
        (org-gtd--archive-task-if-no-projects (point) "proj-id-1")

        ;; Verify task is NOT archived (still in tasks file)
        (let ((tasks-content (ogt--buffer-string (org-gtd--default-file)))
              (archived-content (ogt--archive-string)))
          (expect tasks-content :to-match "Test Task")
          (expect archived-content :not :to-match "Test Task")))))

 (describe
  "Acceptance test: shared task archiving behavior"

  (it "archives shared tasks only when all projects are complete"
      ;; Step 1: Create Project A with Task A1, Shared Task, Task A2
      (ogt-capture-single-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1\n** Shared Task\n** Task A2")
      (ogt-clarify-as-project)

      ;; Step 2: Create Project B with Task B1, Task B2
      (ogt-capture-single-item "Project B")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task B1\n** Task B2")
      (ogt-clarify-as-project)

      ;; Step 3: Set up multi-project relationship for Shared Task
      (let (shared-task-id project-a-id project-b-id)
        (with-current-buffer (org-gtd--default-file)
          ;; Get Project A's ID and Shared Task's ID
          (goto-char (point-min))
          (search-forward "Project A")
          (org-back-to-heading t)
          (setq project-a-id (org-entry-get (point) "ID"))
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (setq shared-task-id (org-entry-get (point) "ID"))

          ;; Get Project B's ID
          (goto-char (point-min))
          (search-forward "Project B")
          (org-back-to-heading t)
          (setq project-b-id (org-entry-get (point) "ID"))

          ;; Add Project B's ID to Shared Task's ORG_GTD_PROJECT_IDS
          ;; (Project A's ID should already be there from clarify-as-project)
          (goto-char (point-min))
          (search-forward "Project A")
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-b-id)

          ;; Add Shared Task to Project B's first tasks
          (goto-char (point-min))
          (search-forward "Project B")
          (org-back-to-heading t)
          (let ((existing-first-tasks (org-entry-get (point) "ORG_GTD_FIRST_TASKS")))
            (org-entry-put (point) "ORG_GTD_FIRST_TASKS"
                          (concat existing-first-tasks " " shared-task-id)))))

      ;; Step 4: Complete all tasks in Project A
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Project A")

        ;; Mark Task A1 as DONE
        (search-forward "Task A1")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Mark Shared Task as DONE
        (goto-char (point-min))
        (search-forward "Project A")
        (search-forward "Shared Task")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Mark Task A2 as DONE
        (goto-char (point-min))
        (search-forward "Project A")
        (search-forward "Task A2")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 5: Archive completed items
      (org-gtd-archive-completed-items)

      ;; Step 6: Verify Project A is archived but Shared Task is NOT
      ;; (because Project B still references it)
      (let ((archived-content (ogt--archive-string))
            (tasks-content (ogt--buffer-string (org-gtd--default-file))))

        ;; Project A should be archived
        (expect archived-content :to-match "Project A")

        ;; Project B should still be in tasks file
        (expect tasks-content :to-match "Project B")

        ;; Shared Task should NOT be archived yet (Project B still needs it)
        (expect archived-content :not :to-match "Shared Task")
        (expect tasks-content :to-match "Shared Task"))

      ;; Step 7: Complete all tasks in Project B
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Project B")

        ;; Mark Task B1 as DONE
        (search-forward "Task B1")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Mark Task B2 as DONE
        (goto-char (point-min))
        (search-forward "Project B")
        (search-forward "Task B2")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 8: Archive completed items again
      (org-gtd-archive-completed-items)

      ;; Step 9: Verify both projects and shared task are now archived
      (let ((archived-content (ogt--archive-string)))
        ;; Both projects should be archived
        (expect archived-content :to-match "Project A")
        (expect archived-content :to-match "Project B")

        ;; Shared Task should NOW be archived (all projects complete)
        (expect archived-content :to-match "Shared Task")))))
