;;; graph-traversal-test.el --- Tests for project task graph traversal -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-projects--collect-tasks-by-graph function.
;; Tests graph traversal for project tasks including dependency chains,
;; circular dependencies, and multi-project boundaries.
;;
;; Migrated from test/graph-traversal-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'org-gtd-projects)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    ;; Clear org-id locations to prevent pollution from previous tests
    (setq org-id-locations nil)
    (setq org-id-files nil)
    (unwind-protect
        (funcall proceed context)
      ;; Clear org-id locations after tests
      (setq org-id-locations nil)
      (setq org-id-files nil)
      ;; Save cleared state to disk to prevent test pollution
      (org-id-locations-save))))

;;; org-gtd-projects--collect-tasks-by-graph Tests

(deftest graph-traversal/function-exists ()
  "Test that the function exists."
  (assert-true (fboundp 'org-gtd-projects--collect-tasks-by-graph)))

(deftest graph-traversal/collects-tasks-by-dependency-chains ()
  "Collects tasks by following ID dependency chains from FIRST_TASKS."
  (let* ((temp-file (make-temp-file "graph-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project heading with FIRST_TASKS
          (make-project "Test Project"
                        :id "project-id"
                        :first-tasks '("task-a-id"))

          ;; Task A - root task (no ORG_GTD_DEPENDS_ON)
          (make-task "Task A"
                     :id "task-a-id"
                     :level 2
                     :project-ids '("project-id")
                     :blocks '("task-b-id"))

          ;; Task B - depends on A
          (make-task "Task B"
                     :id "task-b-id"
                     :level 3  ;; Different level
                     :project-ids '("project-id")
                     :depends-on '("task-a-id"))

          ;; Task C - not connected to graph (no DEPENDS_ON or BLOCKS to/from project tasks)
          (make-task "Task C"
                     :id "task-c-id"
                     :level 2
                     :project-ids '("project-id"))

          (org-mode-restart)
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
            (assert-equal 2 (length connected-tasks))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-traversal/handles-circular-dependencies ()
  "Handles circular dependencies gracefully without infinite loops."
  (let* ((temp-file (make-temp-file "circular-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project heading
          (make-project "Circular Project"
                        :id "circular-proj-id"
                        :first-tasks '("task-a-id"))

          ;; Task A blocks B, depends on C (circular)
          (make-task "Task A"
                     :id "task-a-id"
                     :level 2
                     :project-ids '("circular-proj-id")
                     :blocks '("task-b-id")
                     :depends-on '("task-c-id"))  ;; Circular: A depends on C

          ;; Task B blocks C, depends on A
          (make-task "Task B"
                     :id "task-b-id"
                     :level 2
                     :project-ids '("circular-proj-id")
                     :depends-on '("task-a-id")
                     :blocks '("task-c-id"))

          ;; Task C completes the cycle
          (make-task "Task C"
                     :id "task-c-id"
                     :level 2
                     :project-ids '("circular-proj-id")
                     :depends-on '("task-b-id")
                     :blocks '("task-a-id"))  ;; Circular: C blocks A

          (org-mode-restart)
          (basic-save-buffer)
          ;; Update org-id locations
          (org-id-update-id-locations (list temp-file))

          ;; Should handle circular dependencies without infinite loop
          (goto-char (point-min))
          (search-forward "Circular Project")
          (org-back-to-heading t)
          (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
            ;; Should find all 3 tasks in the circular graph by following BLOCKS from A
            (assert-equal 3 (length connected-tasks))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-traversal/empty-when-no-first-tasks ()
  "Returns empty list when FIRST_TASKS is empty or missing."
  (with-temp-buffer
    (org-mode)
    (make-project "Empty Project"
                  :id "empty-proj-id")

    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
      (assert-nil connected-tasks))))

(deftest graph-traversal/uses-org-gtd-first-tasks-property ()
  "Uses ORG_GTD_FIRST_TASKS instead of FIRST_TASKS for graph traversal."
  (let* ((temp-file (make-temp-file "new-property-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project heading with ORG_GTD_FIRST_TASKS
          (make-project "New Property Project"
                        :id "new-proj-id"
                        :first-tasks '("task-1-id"))

          ;; Task 1 - root task
          (make-task "Task 1"
                     :id "task-1-id"
                     :level 2
                     :project-ids '("new-proj-id")
                     :blocks '("task-2-id"))

          ;; Task 2
          (make-task "Task 2"
                     :id "task-2-id"
                     :level 2
                     :project-ids '("new-proj-id")
                     :depends-on '("task-1-id"))

          (org-mode-restart)
          (basic-save-buffer)
          ;; Update org-id locations
          (org-id-update-id-locations (list temp-file))

          ;; Test the graph traversal
          (goto-char (point-min))
          (search-forward "New Property Project")
          (org-back-to-heading t)
          (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
            ;; Should find both tasks via ORG_GTD_FIRST_TASKS
            (assert-equal 2 (length connected-tasks))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Multi-project task graph boundaries tests

(deftest graph-traversal/respects-project-boundaries ()
  "Respects project boundaries when traversing shared tasks."
  (let* ((temp-file (make-temp-file "multi-project-boundary-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)

          ;; Project A with tasks T1, T2, T3, T4
          (make-project "Project A"
                        :id "proj-a"
                        :first-tasks '("t1-id" "t2-id"))

          ;; T1: Only in Project A, blocks T3
          (make-task "T1"
                     :id "t1-id"
                     :level 2
                     :project-ids '("proj-a")
                     :blocks '("t3-id"))

          ;; T2: Only in Project A, blocks T4
          (make-task "T2"
                     :id "t2-id"
                     :level 2
                     :project-ids '("proj-a")
                     :blocks '("t4-id"))

          ;; T3: SHARED with Project B (has both proj-a and proj-b), blocks T6
          (make-task "T3"
                     :id "t3-id"
                     :level 2
                     :project-ids '("proj-a" "proj-b")
                     :depends-on '("t1-id")
                     :blocks '("t6-id"))

          ;; T4: Only in Project A
          (make-task "T4"
                     :id "t4-id"
                     :level 2
                     :project-ids '("proj-a")
                     :depends-on '("t2-id"))

          ;; Project B with tasks T3, T5, T6, T7
          (make-project "Project B"
                        :id "proj-b"
                        :first-tasks '("t3-id" "t5-id"))

          ;; T5: Only in Project B, blocks T7
          (make-task "T5"
                     :id "t5-id"
                     :level 2
                     :project-ids '("proj-b")
                     :blocks '("t7-id"))

          ;; T6: Only in Project B (T3 blocks T6, but T6 is not in proj-a)
          (make-task "T6"
                     :id "t6-id"
                     :level 2
                     :project-ids '("proj-b")
                     :depends-on '("t3-id"))

          ;; T7: Only in Project B
          (make-task "T7"
                     :id "t7-id"
                     :level 2
                     :project-ids '("proj-b")
                     :depends-on '("t5-id"))

          (org-mode-restart)
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
            (assert-equal 4 (length proj-a-tasks))
            ;; Should include T1, T2, T3, T4
            (assert-true (member "t1-id" proj-a-task-ids))
            (assert-true (member "t2-id" proj-a-task-ids))
            (assert-true (member "t3-id" proj-a-task-ids))
            (assert-true (member "t4-id" proj-a-task-ids))
            ;; Should NOT include T6 (even though T3 blocks T6, T6 is not in proj-a)
            (assert-nil (member "t6-id" proj-a-task-ids)))

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
            (assert-equal 4 (length proj-b-tasks))
            ;; Should include T3, T5, T6, T7
            (assert-true (member "t3-id" proj-b-task-ids))
            (assert-true (member "t5-id" proj-b-task-ids))
            (assert-true (member "t6-id" proj-b-task-ids))
            (assert-true (member "t7-id" proj-b-task-ids))
            ;; Should NOT include T1, T2, T4 (not in proj-b)
            (assert-nil (member "t1-id" proj-b-task-ids))
            (assert-nil (member "t2-id" proj-b-task-ids))
            (assert-nil (member "t4-id" proj-b-task-ids))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-traversal/excludes-tasks-without-project-id ()
  "Excludes tasks that don't have current project ID in ORG_GTD_PROJECT_IDS."
  (let* ((temp-file (make-temp-file "exclude-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)

          ;; Project A
          (make-project "Project A"
                        :id "proj-a"
                        :first-tasks '("task-1-id"))

          ;; Task 1: In Project A, blocks Task 2
          (make-task "Task 1"
                     :id "task-1-id"
                     :level 2
                     :project-ids '("proj-a")
                     :blocks '("task-2-id"))

          ;; Task 2: NOT in Project A (only in proj-b)
          (make-task "Task 2"
                     :id "task-2-id"
                     :level 2
                     :project-ids '("proj-b")
                     :depends-on '("task-1-id"))

          (org-mode-restart)
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
            (assert-equal 1 (length proj-a-tasks))
            (assert-true (member "task-1-id" proj-a-task-ids))
            ;; Should NOT include Task 2 (even though Task 1 blocks it)
            (assert-nil (member "task-2-id" proj-a-task-ids))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-traversal/includes-shared-tasks ()
  "Includes shared tasks when project ID matches."
  (let* ((temp-file (make-temp-file "include-shared-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)

          ;; Project A
          (make-project "Project A"
                        :id "proj-a"
                        :first-tasks '("task-1-id"))

          ;; Task 1: In Project A, blocks Shared Task
          (make-task "Task 1"
                     :id "task-1-id"
                     :level 2
                     :project-ids '("proj-a")
                     :blocks '("shared-task-id"))

          ;; Shared Task: In BOTH Project A and Project B
          (make-task "Shared Task"
                     :id "shared-task-id"
                     :level 2
                     :project-ids '("proj-a" "proj-b")
                     :depends-on '("task-1-id"))

          (org-mode-restart)
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
            (assert-equal 2 (length proj-a-tasks))
            (assert-true (member "task-1-id" proj-a-task-ids))
            ;; Should include Shared Task (has proj-a in its ORG_GTD_PROJECT_IDS)
            (assert-true (member "shared-task-id" proj-a-task-ids))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'graph-traversal-test)

;;; graph-traversal-test.el ends here
