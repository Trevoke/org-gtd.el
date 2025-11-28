;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe
 "Property name migration to ORG_GTD_* namespace"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              (setq org-id-locations nil)
              (setq org-id-files nil))

 (after-each (ogt--close-and-delete-files)
             (setq org-id-locations nil)
             (setq org-id-files nil))

 (describe
  "ORG_GTD_FIRST_TASKS property on project creation"

  (it "sets ORG_GTD_FIRST_TASKS instead of FIRST_TASKS on new projects"
      ;; Create a project with 3 tasks
      (capture-inbox-item "Test Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1\n** Task 2\n** Task 3")
      (organize-as-project)

      ;; Verify ORG_GTD_FIRST_TASKS is set, not FIRST_TASKS
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test Project")
        (org-back-to-heading t)

        ;; Should have ORG_GTD_FIRST_TASKS property
        (expect (org-entry-get (point) "ORG_GTD_FIRST_TASKS") :to-be-truthy)

        ;; Should NOT have old FIRST_TASKS property
        (expect (org-entry-get (point) "FIRST_TASKS") :to-be nil))))

 (describe
  "ORG_GTD_BLOCKS and ORG_GTD_DEPENDS_ON properties on dependency creation"

  (it "sets ORG_GTD_BLOCKS instead of BLOCKS when creating dependencies"
      ;; Create a project with 3 tasks
      (capture-inbox-item "Dependency Test Project")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1\n** Task 2\n** Task 3")
      (organize-as-project)

      ;; Check that tasks have sequential dependencies with new properties
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)

        ;; Task 1 should have ORG_GTD_BLOCKS, not BLOCKS
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-be-truthy)
        (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-be nil)))

  (it "sets ORG_GTD_DEPENDS_ON instead of DEPENDS_ON when creating dependencies"
      ;; Create a project with 3 tasks
      (capture-inbox-item "Dependency Test Project 2")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1\n** Task 2\n** Task 3")
      (organize-as-project)

      ;; Check that tasks have sequential dependencies with new properties
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 2")
        (org-back-to-heading t)

        ;; Task 2 should have ORG_GTD_DEPENDS_ON, not DEPENDS_ON
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-be-truthy)
        (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-be nil))))

 (describe
  "ORG_GTD_PROJECT_IDS property on task organization"

  (it "sets ORG_GTD_PROJECT_IDS instead of ORG_GTD_PROJECT when organizing tasks into projects"
      ;; Create a project
      (capture-inbox-item "Project with IDs")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task 1\n** Task 2")
      (organize-as-project)

      ;; Check that tasks have ORG_GTD_PROJECT_IDS
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)

        ;; Task should have ORG_GTD_PROJECT_IDS with the project ID (for multi-project support)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS") :to-be-truthy)

        ;; Should ALSO have ORG_GTD_PROJECT property (for agenda display)
        (expect (org-entry-get (point) "ORG_GTD_PROJECT") :to-equal "Project with IDs")))))
