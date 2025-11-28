;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

(describe
 "Multi-project agenda display"

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
  "Unit test: ORG_GTD_PROJECT property for multi-project tasks"

  (it "sets ORG_GTD_PROJECT to the first project when task belongs to multiple projects"
      ;; Create Project A with tasks
      (capture-inbox-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1\n** Shared Task\n** Task A2")
      (organize-as-project)

      ;; Create Project B with tasks
      (capture-inbox-item "Project B")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task B1\n** Task B2")
      (organize-as-project)

      ;; Set up multi-project relationship for Shared Task
      (let (shared-task-id project-a-id project-b-id project-a-name project-b-name)
        (with-current-buffer (org-gtd--default-file)
          ;; Get Project A's ID and name
          (goto-char (point-min))
          (search-forward "Project A")
          (org-back-to-heading t)
          (setq project-a-id (org-entry-get (point) "ID"))
          (setq project-a-name (org-get-heading t t t t))
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (setq shared-task-id (org-entry-get (point) "ID"))

          ;; Get Project B's ID and name
          (goto-char (point-min))
          (search-forward "Project B")
          (org-back-to-heading t)
          (setq project-b-id (org-entry-get (point) "ID"))
          (setq project-b-name (org-get-heading t t t t))

          ;; Add Project B's ID to Shared Task's ORG_GTD_PROJECT_IDS
          ;; (Project A's ID should already be there from clarify-as-project)
          (goto-char (point-min))
          (search-forward "Project A")
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-b-id)

          ;; Verify that Shared Task has both project IDs
          (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
            (expect (length project-ids) :to-equal 2)
            (expect (member project-a-id project-ids) :to-be-truthy)
            (expect (member project-b-id project-ids) :to-be-truthy))

          ;; Now configure Project B which will also try to set ORG_GTD_PROJECT on Shared Task
          ;; This simulates what happens when both projects are processed
          (goto-char (point-min))
          (search-forward "Project B")
          (org-back-to-heading t)
          (org-gtd-projects--configure-all-tasks)

          ;; Verify that Shared Task's ORG_GTD_PROJECT is STILL "Project A"
          ;; (should not be overwritten by Project B)
          (goto-char (point-min))
          (search-forward "Project A")
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (let ((task-project-name (org-entry-get (point) "ORG_GTD_PROJECT")))
            ;; The task's project name should start with "Project A" (the project-a-name may have progress cookie added later)
            ;; We check if "Project A" (base name) is in the task's project name
            ;; Note: task-project-name might be "Project A" or "Project A [0/3]" depending on when it was set
            (expect (or (string= task-project-name "Project A")
                        (string-prefix-p "Project A " task-project-name))
                    :to-be-truthy)
            ;; Also verify it's NOT "Project B"
            (expect (not (or (string= task-project-name "Project B")
                             (string-prefix-p "Project B " task-project-name)))
                    :to-be-truthy)))))))
