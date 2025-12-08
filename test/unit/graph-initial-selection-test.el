;;; graph-initial-selection-test.el --- Tests for graph initial selection -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-graph-data-find-first-actionable function.
;; Tests BFS traversal to find first actionable task (NEXT, WAIT, etc.)
;; for initial selection when opening project graph view.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-data)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-id-locations nil)
    (setq org-id-files nil)
    (unwind-protect
        (funcall proceed context)
      (setq org-id-locations nil)
      (setq org-id-files nil)
      (org-id-locations-save))))

;;; Tests go here

(deftest graph-initial/function-exists ()
  "Test that the function exists."
  (assert-true (fboundp 'org-gtd-graph-data-find-first-actionable)))

(deftest graph-initial/finds-next-task-in-root ()
  "Finds NEXT task when it's a root task."
  (let* ((temp-file (make-temp-file "graph-initial-next" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project with NEXT"
                        :id "proj-next-id"
                        :first-tasks '("task-next-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-next-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with NEXT")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/finds-wait-task-in-root ()
  "Finds WAIT task when it's a root task."
  (let* ((temp-file (make-temp-file "graph-initial-wait" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project with WAIT"
                        :id "proj-wait-id"
                        :first-tasks '("task-wait-id"))

          (make-task "Task with WAIT state"
                     :id "task-wait-id"
                     :status 'wait
                     :level 2
                     :project-ids '("proj-wait-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with WAIT")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-wait-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/skips-todo-tasks ()
  "Skips tasks in TODO state, finds NEXT."
  (let* ((temp-file (make-temp-file "graph-initial-skip-todo" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip TODO"
                        :id "proj-skip-todo-id"
                        :first-tasks '("task-todo-id" "task-next-id"))

          (make-task "Task with TODO state"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-skip-todo-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-todo-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip TODO")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/skips-done-tasks ()
  "Skips tasks in DONE state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-done" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip DONE"
                        :id "proj-skip-done-id"
                        :first-tasks '("task-done-id" "task-next-id"))

          (make-task "Task with DONE state"
                     :id "task-done-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-skip-done-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-done-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip DONE")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/skips-cncl-tasks ()
  "Skips tasks in CNCL state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-cncl" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip CNCL"
                        :id "proj-skip-cncl-id"
                        :first-tasks '("task-cncl-id" "task-next-id"))

          (make-task "Task with CNCL state"
                     :id "task-cncl-id"
                     :status 'cncl
                     :level 2
                     :project-ids '("proj-skip-cncl-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-cncl-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip CNCL")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/returns-nil-when-no-tasks ()
  "Returns nil when graph has only project heading (no tasks)."
  (let* ((temp-file (make-temp-file "graph-initial-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with no tasks
          (make-project "Empty Project" :id "empty-proj-id")
          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Empty Project")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/bfs-finds-actionable-in-successors ()
  "BFS traverses to successors when roots are not actionable."
  (let* ((temp-file (make-temp-file "graph-initial-bfs-succ" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project BFS successors"
                        :id "proj-bfs-succ-id"
                        :first-tasks '("task-a-id"))

          (make-task "Task A - done root"
                     :id "task-a-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-bfs-succ-id")
                     :blocks '("task-b-id"))

          (make-task "Task B - next successor"
                     :id "task-b-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-succ-id")
                     :depends-on '("task-a-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project BFS successors")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-b-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/bfs-searches-breadth-first ()
  "Searches all nodes at current depth before going deeper."
  (let* ((temp-file (make-temp-file "graph-initial-bfs-order" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project BFS order"
                        :id "proj-bfs-order-id"
                        :first-tasks '("task-a-id" "task-b-id"))

          (make-task "Task A - TODO root"
                     :id "task-a-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-bfs-order-id")
                     :blocks '("task-c-id"))

          (make-task "Task B - NEXT root"
                     :id "task-b-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-order-id"))

          (make-task "Task C - NEXT at depth 2"
                     :id "task-c-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-order-id")
                     :depends-on '("task-a-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project BFS order")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-b-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/returns-nil-when-all-non-actionable ()
  "Returns nil when all tasks are DONE, CNCL, or TODO."
  (let* ((temp-file (make-temp-file "graph-initial-all-non-action" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project all non-actionable"
                        :id "proj-all-na-id"
                        :first-tasks '("task-todo-id" "task-done-id" "task-cncl-id"))

          (make-task "Task TODO"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (make-task "Task DONE"
                     :id "task-done-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (make-task "Task CNCL"
                     :id "task-cncl-id"
                     :status 'cncl
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project all non-actionable")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest graph-initial/skips-project-heading ()
  "Does not return project heading even if it has actionable state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-proj" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (insert "* NEXT Project with actionable state\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: proj-actionable-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-todo-id\n")
          (insert ":END:\n")

          (make-task "Task TODO only"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-actionable-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with actionable state")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'graph-initial-selection-test)

;;; graph-initial-selection-test.el ends here
