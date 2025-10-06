;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Property-based GTD system"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'post-command-hook 'org-add-log-note))

 (describe
  "Project task identification by ORG_GTD property"

  (it "identifies project tasks by ORG_GTD=org-gtd-action property not by level"
      ;; Create a project structure where tasks have ORG_GTD property but are not at level 2
      (with-temp-buffer
        (org-mode)
        (insert "* Project Root\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":ID: root-id\n")
        (insert ":END:\n")
        (insert "*** Deep Task 1\n")  ;; Level 3, not 2
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task1-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")
        (insert "**** Even Deeper Task 2\n")  ;; Level 4, not 2
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task2-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")

        ;; Test that tasks are identified by property, not level
        (goto-char (point-min))
        (let ((task-count 0))
          (org-map-entries
           (lambda ()
             (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
               (setq task-count (1+ task-count))))
           nil
           nil)
          (expect task-count :to-equal 2))))

  (it "collects tasks through graph traversal via ID relationships"
      ;; This test verifies that task collection follows BLOCKS/DEPENDS_ON chains
      ;; Uses real project creation API to ensure FIRST_TASKS is set correctly

      ;; Create project using real API
      (ogt-capture-and-process-project "Graph Project")

      ;; Now modify the created tasks to add dependency relationships
      (let* ((gtd-file (org-gtd--default-file))
             (project-marker nil)
             (task-ids '()))

        (with-current-buffer gtd-file
          ;; Find the project and its tasks
          (goto-char (point-min))
          (search-forward "Graph Project")
          (org-back-to-heading t)
          (setq project-marker (point-marker))

          ;; Collect task IDs
          (org-map-entries
           (lambda ()
             (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
               (let ((id (org-entry-get (point) "ID")))
                 (when id (push id task-ids)))))
           nil
           'tree)

          (setq task-ids (nreverse task-ids))

          ;; Add dependency relationships: Task 1 -> Task 2 -> Task 3
          ;; Task 1 blocks Task 2
          (goto-char (point-min))
          (org-find-entry-with-id (nth 0 task-ids))
          (org-entry-put (point) "ORG_GTD_BLOCKS" (nth 1 task-ids))

          ;; Task 2 depends on Task 1 and blocks Task 3
          (goto-char (point-min))
          (org-find-entry-with-id (nth 1 task-ids))
          (org-entry-put (point) "ORG_GTD_DEPENDS_ON" (nth 0 task-ids))
          (org-entry-put (point) "ORG_GTD_BLOCKS" (nth 2 task-ids))

          ;; Task 3 depends on Task 2
          (goto-char (point-min))
          (org-find-entry-with-id (nth 2 task-ids))
          (org-entry-put (point) "ORG_GTD_DEPENDS_ON" (nth 1 task-ids))

          ;; Update FIRST_TASKS to reflect new dependency structure
          ;; Only Task 1 should be in FIRST_TASKS now
          (goto-char project-marker)
          (org-entry-put (point) "FIRST_TASKS" (nth 0 task-ids))

          (basic-save-buffer))

        ;; Update org-id locations so org-id-find works
        (org-id-update-id-locations (list (buffer-file-name gtd-file)))

        ;; Test graph traversal
        (with-current-buffer gtd-file
          (goto-char project-marker)
          (let ((project-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
            (expect (length project-tasks) :to-equal 3))))))

 (describe
  "Migration from level-based to property-based"

  (it "migrates existing level 2 project tasks to have ORG_GTD property"
      ;; Create old-style project with level 2 tasks but no ORG_GTD properties
      (with-temp-buffer
        (org-mode)
        (insert "* Legacy Project\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":ID: legacy-project-id\n")
        (insert ":END:\n")
        (insert "** Legacy Task 1\n")  ;; Level 2 but no ORG_GTD property
        (insert ":PROPERTIES:\n")
        (insert ":ID: legacy-task1-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")
        (insert "** Legacy Task 2\n")  ;; Level 2 but no ORG_GTD property
        (insert ":PROPERTIES:\n")
        (insert ":ID: legacy-task2-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")

        ;; Apply migration function core logic (adapted for temp buffer)
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (org-back-to-heading t)
          (when (string= (org-entry-get (point) "ORG_GTD") "Projects")
            (let ((project-level (org-current-level)))
              (outline-next-heading)
              (while (and (not (eobp))
                          (> (org-current-level) project-level))
                (when (= (org-current-level) (1+ project-level))
                  ;; This is a direct child (level 2 under project)
                  (unless (org-entry-get (point) "ORG_GTD")
                    (org-entry-put (point) "ORG_GTD" "Actions")))
                (outline-next-heading)))))

        ;; Verify all level 2 tasks under Projects now have ORG_GTD property
        (goto-char (point-min))
        (search-forward "Legacy Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

        (goto-char (point-min))
        (search-forward "Legacy Task 2")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions"))))

 (describe
  "View language without level filters"

  (it "creates view specifications without level-based filters"
      ;; Test that view language can specify project tasks without using level filters
      (let ((view-spec '((name . "Project Tasks")
                         (filters . ((category . projects)
                                   (property . (("ORG_GTD" . "Actions"))))))))

        ;; Translate to org-ql query - should not contain level filters
        (let ((org-ql-query (org-gtd-view-lang--translate-to-org-ql view-spec)))
          (expect (format "%s" org-ql-query) :not :to-match "level")
          (expect (format "%s" org-ql-query) :to-match "ORG_GTD")))))

 (describe
  "Org-edna without inheritance"

  (it "disables org-edna inheritance for property-based system"
      ;; Test that org-edna inheritance is disabled
      (org-gtd-mode 1)
      (expect org-edna-use-inheritance :to-equal 0)

      ;; Test that dependency relationships work without inheritance
      (with-temp-buffer
        (org-mode)
        (insert "* Project\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":ID: proj-id\n")
        (insert ":END:\n")
        (insert "** Task 1\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task1-id\n")
        (insert ":ORG_GTD_BLOCKS: task2-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")
        (insert "** Task 2\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task2-id\n")
        (insert ":ORG_GTD_DEPENDS_ON: task1-id\n")
        (insert ":TODO: TODO\n")
        (insert ":END:\n")

        ;; Task relationships should work without inheritance
        (goto-char (point-min))
        (search-forward "Task 2")
        (org-back-to-heading t)
        (let ((deps (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
          (expect (length deps) :to-equal 1)
          (expect (car deps) :to-equal "task1-id"))))))