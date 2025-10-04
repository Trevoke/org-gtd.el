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
              (insert ":ORG_GTD_FIRST_TASKS: task-a-id\n")
              (insert ":END:\n")

              ;; Task A - root task (no ORG_GTD_DEPENDS_ON)
              (insert "** Task A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-a-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
              (insert ":ORG_GTD_BLOCKS: task-b-id\n")
              (insert ":END:\n")

              ;; Task B - depends on A
              (insert "*** Task B\n")  ;; Different level
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-b-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-a-id\n")
              (insert ":END:\n")

              ;; Task C - not connected to graph (no DEPENDS_ON or BLOCKS to/from project tasks)
              (insert "** Task C\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-c-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
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
              (insert ":ORG_GTD_FIRST_TASKS: task-a-id\n")
              (insert ":END:\n")

              ;; Task A blocks B, depends on C (circular)
              (insert "** Task A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-a-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: circular-proj-id\n")
              (insert ":ORG_GTD_BLOCKS: task-b-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-c-id\n")  ;; Circular: A depends on C
              (insert ":END:\n")

              ;; Task B blocks C, depends on A
              (insert "** Task B\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-b-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: circular-proj-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-a-id\n")
              (insert ":ORG_GTD_BLOCKS: task-c-id\n")
              (insert ":END:\n")

              ;; Task C completes the cycle
              (insert "** Task C\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-c-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: circular-proj-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-b-id\n")
              (insert ":ORG_GTD_BLOCKS: task-a-id\n")  ;; Circular: C blocks A
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
          (expect connected-tasks :to-equal nil))))

  (it "uses ORG_GTD_FIRST_TASKS instead of FIRST_TASKS for graph traversal"
      ;; Test that graph traversal reads ORG_GTD_FIRST_TASKS property
      (let* ((temp-file (make-temp-file "new-property-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)
              ;; Project heading with ORG_GTD_FIRST_TASKS
              (insert "* New Property Project\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: new-proj-id\n")
              (insert ":ORG_GTD_FIRST_TASKS: task-1-id\n")
              (insert ":END:\n")

              ;; Task 1 - root task
              (insert "** Task 1\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-1-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: new-proj-id\n")
              (insert ":ORG_GTD_BLOCKS: task-2-id\n")
              (insert ":END:\n")

              ;; Task 2
              (insert "** Task 2\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-2-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: new-proj-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-1-id\n")
              (insert ":END:\n")

              (basic-save-buffer)
              ;; Update org-id locations
              (org-id-update-id-locations (list temp-file))

              ;; Test the graph traversal
              (goto-char (point-min))
              (search-forward "New Property Project")
              (org-back-to-heading t)
              (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
                ;; Should find both tasks via ORG_GTD_FIRST_TASKS
                (expect (length connected-tasks) :to-equal 2)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file))))))

 (describe
  "Multi-project task graph boundaries"

  (it "respects project boundaries when traversing shared tasks (acceptance test)"
      ;; ACCEPTANCE TEST: Graph traversal should only include tasks that have
      ;; the current project ID in their ORG_GTD_PROJECT_IDS property
      (let* ((temp-file (make-temp-file "multi-project-boundary-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)

              ;; Project A with tasks T1, T2, T3, T4
              (insert "* Project A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: proj-a\n")
              (insert ":ORG_GTD_FIRST_TASKS: t1-id t2-id\n")
              (insert ":END:\n")

              ;; T1: Only in Project A, blocks T3
              (insert "** T1\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t1-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a\n")
              (insert ":ORG_GTD_BLOCKS: t3-id\n")
              (insert ":END:\n")

              ;; T2: Only in Project A, blocks T4
              (insert "** T2\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t2-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a\n")
              (insert ":ORG_GTD_BLOCKS: t4-id\n")
              (insert ":END:\n")

              ;; T3: SHARED with Project B (has both proj-a and proj-b), blocks T6
              (insert "** T3\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t3-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a proj-b\n")
              (insert ":ORG_GTD_DEPENDS_ON: t1-id\n")
              (insert ":ORG_GTD_BLOCKS: t6-id\n")
              (insert ":END:\n")

              ;; T4: Only in Project A
              (insert "** T4\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t4-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a\n")
              (insert ":ORG_GTD_DEPENDS_ON: t2-id\n")
              (insert ":END:\n")

              ;; Project B with tasks T3, T5, T6, T7
              (insert "* Project B\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: proj-b\n")
              (insert ":ORG_GTD_FIRST_TASKS: t3-id t5-id\n")
              (insert ":END:\n")

              ;; T5: Only in Project B, blocks T7
              (insert "** T5\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t5-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-b\n")
              (insert ":ORG_GTD_BLOCKS: t7-id\n")
              (insert ":END:\n")

              ;; T6: Only in Project B (T3 blocks T6, but T6 is not in proj-a)
              (insert "** T6\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t6-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-b\n")
              (insert ":ORG_GTD_DEPENDS_ON: t3-id\n")
              (insert ":END:\n")

              ;; T7: Only in Project B
              (insert "** T7\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: t7-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-b\n")
              (insert ":ORG_GTD_DEPENDS_ON: t5-id\n")
              (insert ":END:\n")

              (basic-save-buffer)
              ;; Update org-id locations
              (org-id-update-id-locations (list temp-file))

              ;; TEST PROJECT A: Should include T1, T2, T3, T4 but NOT T6
              (goto-char (point-min))
              (search-forward "Project A")
              (org-back-to-heading t)
              (let* ((proj-a-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker)))
                     (proj-a-task-ids (mapcar (lambda (marker)
                                                (org-with-point-at marker
                                                  (org-entry-get (point) "ID")))
                                              proj-a-tasks)))
                ;; Should have exactly 4 tasks: T1, T2, T3, T4
                (expect (length proj-a-tasks) :to-equal 4)
                ;; Should include T1, T2, T3, T4
                (expect (member "t1-id" proj-a-task-ids) :to-be-truthy)
                (expect (member "t2-id" proj-a-task-ids) :to-be-truthy)
                (expect (member "t3-id" proj-a-task-ids) :to-be-truthy)
                (expect (member "t4-id" proj-a-task-ids) :to-be-truthy)
                ;; Should NOT include T6 (even though T3 blocks T6, T6 is not in proj-a)
                (expect (member "t6-id" proj-a-task-ids) :not :to-be-truthy))

              ;; TEST PROJECT B: Should include T3, T5, T6, T7
              (goto-char (point-min))
              (search-forward "Project B")
              (org-back-to-heading t)
              (let* ((proj-b-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker)))
                     (proj-b-task-ids (mapcar (lambda (marker)
                                                (org-with-point-at marker
                                                  (org-entry-get (point) "ID")))
                                              proj-b-tasks)))
                ;; Should have exactly 4 tasks: T3, T5, T6, T7
                (expect (length proj-b-tasks) :to-equal 4)
                ;; Should include T3, T5, T6, T7
                (expect (member "t3-id" proj-b-task-ids) :to-be-truthy)
                (expect (member "t5-id" proj-b-task-ids) :to-be-truthy)
                (expect (member "t6-id" proj-b-task-ids) :to-be-truthy)
                (expect (member "t7-id" proj-b-task-ids) :to-be-truthy)
                ;; Should NOT include T1, T2, T4 (not in proj-b)
                (expect (member "t1-id" proj-b-task-ids) :not :to-be-truthy)
                (expect (member "t2-id" proj-b-task-ids) :not :to-be-truthy)
                (expect (member "t4-id" proj-b-task-ids) :not :to-be-truthy)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

  (it "excludes tasks that don't have current project ID in ORG_GTD_PROJECT_IDS"
      ;; UNIT TEST: When following BLOCKS relationship, only include tasks
      ;; that have the current project ID in their ORG_GTD_PROJECT_IDS
      (let* ((temp-file (make-temp-file "exclude-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)

              ;; Project A
              (insert "* Project A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: proj-a\n")
              (insert ":ORG_GTD_FIRST_TASKS: task-1-id\n")
              (insert ":END:\n")

              ;; Task 1: In Project A, blocks Task 2
              (insert "** Task 1\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-1-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a\n")
              (insert ":ORG_GTD_BLOCKS: task-2-id\n")
              (insert ":END:\n")

              ;; Task 2: NOT in Project A (only in proj-b)
              (insert "** Task 2\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-2-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-b\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-1-id\n")
              (insert ":END:\n")

              (basic-save-buffer)
              (org-id-update-id-locations (list temp-file))

              ;; Test: Project A should only include Task 1, not Task 2
              (goto-char (point-min))
              (search-forward "Project A")
              (org-back-to-heading t)
              (let* ((proj-a-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker)))
                     (proj-a-task-ids (mapcar (lambda (marker)
                                                (org-with-point-at marker
                                                  (org-entry-get (point) "ID")))
                                              proj-a-tasks)))
                ;; Should only have 1 task: Task 1
                (expect (length proj-a-tasks) :to-equal 1)
                (expect (member "task-1-id" proj-a-task-ids) :to-be-truthy)
                ;; Should NOT include Task 2 (even though Task 1 blocks it)
                (expect (member "task-2-id" proj-a-task-ids) :not :to-be-truthy)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))

  (it "includes shared tasks when project ID matches"
      ;; UNIT TEST: When a task has multiple project IDs, include it if one
      ;; of those IDs matches the current project
      (let* ((temp-file (make-temp-file "include-shared-test" nil ".org"))
             (buf (find-file-noselect temp-file)))
        (unwind-protect
            (with-current-buffer buf
              (org-mode)

              ;; Project A
              (insert "* Project A\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Projects\n")
              (insert ":ID: proj-a\n")
              (insert ":ORG_GTD_FIRST_TASKS: task-1-id\n")
              (insert ":END:\n")

              ;; Task 1: In Project A, blocks Shared Task
              (insert "** Task 1\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: task-1-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a\n")
              (insert ":ORG_GTD_BLOCKS: shared-task-id\n")
              (insert ":END:\n")

              ;; Shared Task: In BOTH Project A and Project B
              (insert "** Shared Task\n")
              (insert ":PROPERTIES:\n")
              (insert ":ORG_GTD: Actions\n")
              (insert ":ID: shared-task-id\n")
              (insert ":ORG_GTD_PROJECT_IDS: proj-a proj-b\n")
              (insert ":ORG_GTD_DEPENDS_ON: task-1-id\n")
              (insert ":END:\n")

              (basic-save-buffer)
              (org-id-update-id-locations (list temp-file))

              ;; Test: Project A should include both Task 1 and Shared Task
              (goto-char (point-min))
              (search-forward "Project A")
              (org-back-to-heading t)
              (let* ((proj-a-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker)))
                     (proj-a-task-ids (mapcar (lambda (marker)
                                                (org-with-point-at marker
                                                  (org-entry-get (point) "ID")))
                                              proj-a-tasks)))
                ;; Should have 2 tasks: Task 1 and Shared Task
                (expect (length proj-a-tasks) :to-equal 2)
                (expect (member "task-1-id" proj-a-task-ids) :to-be-truthy)
                ;; Should include Shared Task (has proj-a in its ORG_GTD_PROJECT_IDS)
                (expect (member "shared-task-id" proj-a-task-ids) :to-be-truthy)))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))))
