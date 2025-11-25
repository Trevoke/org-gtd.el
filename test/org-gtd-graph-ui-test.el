;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-ui-test.el --- Unit tests for graph UI split-window layout -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-ui split-window layout and task details panel.
;;
;; Test Coverage:
;; - org-gtd-graph-ui-setup-windows (split-window layout creation)
;; - org-gtd-graph-ui-get-windows (window retrieval)
;; - org-gtd-graph-ui-cleanup-windows (window cleanup)
;; - org-gtd-graph-ui-select-node (node selection and details update)
;; - org-gtd-graph-ui-update-details (details panel update)
;; - org-gtd-graph-ui--format-task-details (task details formatting)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-mode)
(require 'org-gtd-files)
(require 'org-gtd-core)

;;;; Test Setup

(defun org-gtd-graph-ui-test--setup ()
  "Set up minimal test environment for graph UI tests."
  (setq org-gtd-directory (make-temp-file "org-gtd-graph-ui-test" t)
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL")))
  ;; Create the tasks file
  (let ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org")))
    (with-temp-file tasks-file
      (insert ""))))

(defun org-gtd-graph-ui-test--teardown ()
  "Clean up after graph UI tests."
  (when (and org-gtd-directory (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

;;;; org-gtd-graph-ui-setup-windows Tests

(describe "org-gtd-graph-ui-setup-windows"

  (before-each (org-gtd-graph-ui-test--setup))
  (after-each (org-gtd-graph-ui-test--teardown))

  (it "creates a split-window layout"
    (let ((graph-buffer (get-buffer-create "*test-graph*")))
      ;; Display the buffer in a window first
      (switch-to-buffer graph-buffer)
      (let ((windows (org-gtd-graph-ui-setup-windows graph-buffer)))
        (expect (car windows) :to-be-truthy)
        (expect (cdr windows) :to-be-truthy)
        (expect (windowp (car windows)) :to-be-truthy)
        (expect (windowp (cdr windows)) :to-be-truthy))
      (kill-buffer graph-buffer)))

  (it "creates an atomic window group when parent is not root"
    (let ((graph-buffer (get-buffer-create "*test-graph*"))
          (other-buffer (get-buffer-create "*other*")))
      ;; Create a non-root parent by splitting the frame first
      (switch-to-buffer other-buffer)
      (delete-other-windows)
      (let ((other-window (split-window-below)))
        (select-window other-window)
        (switch-to-buffer graph-buffer)
        (let* ((windows (org-gtd-graph-ui-setup-windows graph-buffer))
               (graph-window (car windows))
               (details-window (cdr windows))
               (parent-window (window-parent graph-window)))
          ;; Parent window should have window-atom parameter set
          ;; since it's not the root window
          (expect parent-window :to-be-truthy)
          (expect (eq parent-window (frame-root-window)) :not :to-be-truthy)
          (expect (window-parameter parent-window 'window-atom) :to-be-truthy)))
      (kill-buffer graph-buffer)
      (kill-buffer other-buffer)))

  (it "quit command closes both graph and details windows"
    (let ((graph-buffer (get-buffer-create "*test-graph*")))
      ;; Display the buffer in a window first
      (switch-to-buffer graph-buffer)
      (delete-other-windows)
      (with-current-buffer graph-buffer
        (setq org-gtd-graph-ui--details-buffer nil))
      (let* ((windows (org-gtd-graph-ui-setup-windows graph-buffer))
             (graph-window (car windows))
             (details-window (cdr windows))
             (details-buffer (with-current-buffer graph-buffer
                              org-gtd-graph-ui--details-buffer)))
        ;; Both windows should exist
        (expect (window-live-p graph-window) :to-be-truthy)
        (expect (window-live-p details-window) :to-be-truthy)
        ;; Call org-gtd-graph-quit from the graph window
        (with-selected-window graph-window
          (with-current-buffer graph-buffer
            (org-gtd-graph-quit)))
        ;; Details window should be closed
        (expect (window-live-p details-window) :not :to-be-truthy)
        ;; Details buffer should be killed
        (expect (buffer-live-p details-buffer) :not :to-be-truthy)))))

;;;; Cross-project dependency display tests

(describe "org-gtd-graph-ui--format-task-details with cross-project dependencies"

  (before-each (org-gtd-graph-ui-test--setup))
  (after-each (org-gtd-graph-ui-test--teardown))

  (it "shows blockers from other projects in task details"
    (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
           project-x-id task-a-id project-y-id task-b-id)
      ;; Create two projects with tasks
      (with-current-buffer (find-file-noselect tasks-file)
        (erase-buffer)
        (insert "* Project X\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (org-back-to-heading)
        (setq project-x-id (org-id-get-create))
        (goto-char (point-max))
        (insert "** TODO Task A in Project X\n")
        (org-back-to-heading)
        (setq task-a-id (org-id-get-create))

        (goto-char (point-max))
        (insert "* Project Y\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (org-back-to-heading)
        (setq project-y-id (org-id-get-create))
        (goto-char (point-max))
        (insert "** TODO Task B in Project Y\n")
        (org-back-to-heading)
        (setq task-b-id (org-id-get-create))

        ;; Create cross-project dependency: Task A blocks Task B
        (org-gtd-dependencies-create task-a-id task-b-id)
        (save-buffer))

      ;; Now check the task details for Task B
      (let ((details (org-gtd-graph-ui--format-task-details task-b-id)))
        ;; Should show Task A as a blocker
        (expect details :to-match "Blocked by")
        (expect details :to-match "Task A in Project X"))))

  (it "separates cross-project blockers into their own section"
    (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
           project-y-id task-b-id task-c-id task-a-id)
      ;; Create Project Y with two tasks, one blocked by same-project task, one by external
      (with-current-buffer (find-file-noselect tasks-file)
        (erase-buffer)
        ;; Project X with Task A (will be external blocker)
        (insert "* Project X\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (goto-char (point-max))
        (insert "** TODO Task A in Project X\n")
        (org-back-to-heading)
        (setq task-a-id (org-id-get-create))

        ;; Project Y with Task B and Task C
        (goto-char (point-max))
        (insert "* Project Y\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (org-back-to-heading)
        (setq project-y-id (org-id-get-create))
        (goto-char (point-max))
        (insert "** TODO Task B in Project Y\n")
        (org-back-to-heading)
        (setq task-b-id (org-id-get-create))
        ;; Add project membership
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-y-id)
        (goto-char (point-max))
        (insert "** TODO Task C in Project Y\n")
        (org-back-to-heading)
        (setq task-c-id (org-id-get-create))
        ;; Add project membership
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-y-id)

        ;; Set up project's first tasks
        (goto-char (point-min))
        (re-search-forward "^\\* Project Y")
        (org-back-to-heading)
        (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-b-id)

        ;; Task B blocks Task C (same project)
        (org-gtd-dependencies-create task-b-id task-c-id)
        ;; Task A blocks Task C (cross-project)
        (org-gtd-dependencies-create task-a-id task-c-id)
        (save-buffer))

      ;; View Task C details in context of Project Y's graph
      (let* ((project-marker (org-id-find project-y-id t))
             (graph (org-gtd-graph-data--extract-from-project project-marker))
             (details (org-gtd-graph-ui--format-task-details task-c-id graph)))
        ;; Should have separate sections
        (expect details :to-match "Blocked by:")
        (expect details :to-match "Task B in Project Y")
        (expect details :to-match "Blocked by (other projects):")
        (expect details :to-match "Task A in Project X"))))

  (it "separates cross-project dependents into their own section"
    (let* ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org"))
           project-x-id task-a-id task-b-id task-c-id)
      ;; Create Project X with Task A that blocks tasks in both same and other projects
      (with-current-buffer (find-file-noselect tasks-file)
        (erase-buffer)
        ;; Project X with Task A and Task B
        (insert "* Project X\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (org-back-to-heading)
        (setq project-x-id (org-id-get-create))
        (goto-char (point-max))
        (insert "** TODO Task A in Project X\n")
        (org-back-to-heading)
        (setq task-a-id (org-id-get-create))
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-x-id)
        (goto-char (point-max))
        (insert "** TODO Task B in Project X\n")
        (org-back-to-heading)
        (setq task-b-id (org-id-get-create))
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-x-id)

        ;; Set up project's first tasks
        (goto-char (point-min))
        (re-search-forward "^\\* Project X")
        (org-back-to-heading)
        (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-a-id)

        ;; Project Y with Task C (will be blocked by Task A)
        (goto-char (point-max))
        (insert "* Project Y\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Projects\n")
        (insert ":END:\n")
        (goto-char (point-max))
        (insert "** TODO Task C in Project Y\n")
        (org-back-to-heading)
        (setq task-c-id (org-id-get-create))

        ;; Task A blocks Task B (same project)
        (org-gtd-dependencies-create task-a-id task-b-id)
        ;; Task A blocks Task C (cross-project)
        (org-gtd-dependencies-create task-a-id task-c-id)
        (save-buffer))

      ;; View Task A details in context of Project X's graph
      (let* ((project-marker (org-id-find project-x-id t))
             (graph (org-gtd-graph-data--extract-from-project project-marker))
             (details (org-gtd-graph-ui--format-task-details task-a-id graph)))
        ;; Should have separate sections for what this task blocks
        (expect details :to-match "Blocks:")
        (expect details :to-match "Task B in Project X")
        (expect details :to-match "Blocks (other projects):")
        (expect details :to-match "Task C in Project Y")))))

(provide 'org-gtd-graph-ui-test)

;;; org-gtd-graph-ui-test.el ends here
