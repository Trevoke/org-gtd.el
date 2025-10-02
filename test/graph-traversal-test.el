;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Graph traversal for project tasks"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              ;; Clear org-id locations to prevent pollution from previous tests
              (setq org-id-locations nil)
              (setq org-id-files nil))
 (after-each (ogt--close-and-delete-files)
             ;; Clear org-id locations after tests
             (setq org-id-locations nil)
             (setq org-id-files nil))

 (describe
  "org-gtd-projects--collect-tasks-by-graph function"

  (it "exists and can be called"
      ;; Test that the function exists
      (expect (fboundp 'org-gtd-projects--collect-tasks-by-graph) :to-be-truthy))

  (it "collects tasks by following ID dependency chains from FIRST_TASKS"
      ;; Create a temp file with a project and tasks connected via ID properties
      (let* ((temp-file (make-temp-file "graph-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)
              ;; Project heading with FIRST_TASKS
              (insert "* Test Project\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: project-id\n")
              (insert ":FIRST_TASKS: task-a-id\n")
              (insert ":END:\n")

              ;; Task A - root task (no DEPENDS_ON)
              (insert "** Task A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-a-id\n")
              (insert ":BLOCKS: task-b-id\n")
              (insert ":END:\n")

              ;; Task B - depends on A
              (insert "*** Task B\n")  ;; Different level
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-b-id\n")
              (insert ":DEPENDS_ON: task-a-id\n")
              (insert ":END:\n")

              ;; Task C - not connected to graph (no DEPENDS_ON or BLOCKS to/from project tasks)
              (insert "** Task C\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-c-id\n")
              (insert ":END:\n")

              (basic-save-buffer)
              ;; Update org-id locations
              (org-id-update-id-locations (list temp-file))

              ;; Test the graph traversal starting from project heading
              (goto-char (point-min))
              (search-forward "Test Project")
              (org-back-to-heading t)
              (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
                ;; Should find Task A and Task B (via FIRST_TASKS -> BLOCKS chain)
                ;; but not Task C (not in graph)
                (expect (length connected-tasks) :to-equal 2)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

  (it "handles circular dependencies gracefully"
      ;; Test that circular dependencies don't cause infinite loops
      (let* ((temp-file (make-temp-file "circular-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)
              ;; Project heading
              (insert "* Circular Project\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: circular-proj-id\n")
              (insert ":FIRST_TASKS: task-a-id\n")
              (insert ":END:\n")

              ;; Task A blocks B, depends on C (circular)
              (insert "** Task A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-a-id\n")
              (insert ":BLOCKS: task-b-id\n")
              (insert ":DEPENDS_ON: task-c-id\n")  ;; Circular: A depends on C
              (insert ":END:\n")

              ;; Task B blocks C, depends on A
              (insert "** Task B\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-b-id\n")
              (insert ":DEPENDS_ON: task-a-id\n")
              (insert ":BLOCKS: task-c-id\n")
              (insert ":END:\n")

              ;; Task C completes the cycle
              (insert "** Task C\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-c-id\n")
              (insert ":DEPENDS_ON: task-b-id\n")
              (insert ":BLOCKS: task-a-id\n")  ;; Circular: C blocks A
              (insert ":END:\n")

              (basic-save-buffer)
              ;; Update org-id locations
              (org-id-update-id-locations (list temp-file))

              ;; Should handle circular dependencies without infinite loop
              (goto-char (point-min))
              (search-forward "Circular Project")
              (org-back-to-heading t)
              (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
                ;; Should find all 3 tasks in the circular graph by following BLOCKS from A
                (expect (length connected-tasks) :to-equal 3)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

  (it "returns empty list when FIRST_TASKS is empty or missing"
      ;; Test behavior when project has no FIRST_TASKS
      (with-temp-buffer
        (org-mode)
        (insert "* Empty Project\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":ID: empty-proj-id\n")
        (insert ":END:\n")

        (goto-char (point-min))
        (org-back-to-heading t)
        (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
          (expect connected-tasks :to-equal nil))))))
