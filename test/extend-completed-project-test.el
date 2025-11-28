;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

(describe
 "Extending completed projects"

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
  "Unit test: org-gtd-projects--set-project-name-on-task"

  (it "sets both ORG_GTD_PROJECT and ORG_GTD_PROJECT_IDS when extending project"
      ;; Create a simple project
      (capture-inbox-item "Test Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1")
      (organize-as-project)

      ;; Get the project ID
      (let (project-id)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)
          (setq project-id (org-entry-get (point) "ID"))
          (expect project-id :to-be-truthy)

          ;; Create a new task heading under the project manually
          (org-end-of-subtree)
          (insert "\n*** New Extended Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")

          ;; Go to the new task and call the function
          (goto-char (point-min))
          (search-forward "New Extended Task")
          (org-back-to-heading t)

          ;; Call the function under test
          (org-gtd-projects--set-project-name-on-task)

          ;; Verify both properties are set
          (let ((project-name (org-entry-get (point) "ORG_GTD_PROJECT"))
                (project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
            ;; Should set ORG_GTD_PROJECT
            (expect project-name :to-be-truthy)
            (expect project-name :to-match "Test Project")

            ;; Should also set ORG_GTD_PROJECT_IDS
            (expect (length project-ids) :to-be 1)
            (expect (car project-ids) :to-equal project-id))))))

  (it "works when task is in different file than project (multi-file DAG)"
      ;; Create a project in main file
      (capture-inbox-item "Multi-file Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task in main file")
      (organize-as-project)

      ;; Get the project ID
      (let (project-id second-file)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Multi-file Project")
          (org-back-to-heading t)
          (setq project-id (org-entry-get (point) "ID"))
          (expect project-id :to-be-truthy))

        ;; Create a task in a DIFFERENT file
        (setq second-file (org-gtd--path "other-file"))
        (with-temp-file second-file
          (insert "* Task in other file\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-other-file\n")
          (insert ":ORG_GTD: Actions\n")
          (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
          (insert ":END:\n"))

        ;; Now call org-gtd-projects--set-project-name-on-task from the other file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode)
          (goto-char (point-min))
          (search-forward "Task in other file")
          (org-back-to-heading t)

          ;; This should work even though task is in different file
          (org-gtd-projects--set-project-name-on-task)

          ;; Verify ORG_GTD_PROJECT was set
          (let ((project-name (org-entry-get (point) "ORG_GTD_PROJECT")))
            (expect project-name :to-be-truthy)
            (expect project-name :to-match "Multi-file Project")))))

 (describe
  "Acceptance test: extending a completed but not archived project"

  (it "allows adding a new task to a completed project"
      ;; Step 1: Create a simple project with 2 tasks
      (capture-inbox-item "Completed Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1\n** Task 2")
      (organize-as-project)

      ;; Step 2: Mark all tasks as DONE (complete the project)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (org-todo "DONE")

        (goto-char (point-min))
        (search-forward "Task 2")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 3: Verify project is complete (all tasks DONE)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Completed Project")
        (org-back-to-heading t)
        (expect (org-gtd--all-project-tasks-done-p) :to-be-truthy))

      ;; Step 4: Extend the completed project with a new task
      ;; Using the refile path format: "Projects/Completed Project"
      (add-task-to-existing-project
       "New Task"
       "Projects/Completed SPC Project TAB RET")

      ;; Step 5: Verify the new task was added to the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "New Task")
        (org-back-to-heading t)

        ;; Verify task has project ID
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect (length project-ids) :to-be 1)
          (expect (car project-ids) :to-be-truthy))

        ;; Verify task is properly configured as a project task
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "ORG_GTD_PROJECT") :to-be-truthy))

      ;; Step 6: Verify project is no longer complete (has incomplete task)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Completed Project")
        (org-back-to-heading t)
        (expect (org-gtd--all-project-tasks-done-p) :not :to-be-truthy)))

  (it "does not archive extended project when only new task is incomplete"
      ;; Step 1: Create and complete a project
      (capture-inbox-item "Project To Extend")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Original Task")
      (organize-as-project)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Original Task")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 2: Extend with new task
      (add-task-to-existing-project
       "Extension Task"
       "Projects/Project SPC To SPC Extend TAB RET")

      ;; Step 3: Run archive
      (org-gtd-archive-completed-items)

      ;; Step 4: Verify project and tasks are NOT archived
      (with-current-buffer (org-gtd--default-file)
        (let ((content (buffer-string)))
          (expect content :to-match "Project To Extend")
          (expect content :to-match "Original Task")
          (expect content :to-match "Extension Task")))

      ;; Verify nothing was archived
      (let ((archived-content (ogt--archive-string)))
        (expect archived-content :not :to-match "Project To Extend")
        (expect archived-content :not :to-match "Extension Task")))

  (it "archives project after completing the extension task"
      ;; Step 1: Create and complete a project
      (capture-inbox-item "Full Cycle Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Initial Task")
      (organize-as-project)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Initial Task")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 2: Extend with new task
      (add-task-to-existing-project
       "Final Task"
       "Projects/Full SPC Cycle SPC Project TAB RET")

      ;; Step 3: Complete the extension task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Final Task")
        (org-back-to-heading t)
        (org-todo "DONE"))

      ;; Step 4: Run archive
      (org-gtd-archive-completed-items)

      ;; Step 5: Verify project is archived
      (with-current-buffer (org-gtd--default-file)
        (let ((content (buffer-string)))
          (expect content :not :to-match "Full Cycle Project")))

      ;; Verify project was archived
      (let ((archived-content (ogt--archive-string)))
        (expect archived-content :to-match "Full Cycle Project")
        (expect archived-content :to-match "Initial Task")
        (expect archived-content :to-match "Final Task")))))
