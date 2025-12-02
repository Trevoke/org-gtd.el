;;; graph-ui-test.el --- Unit tests for graph UI task details formatting -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-ui--format-task-details function.
;; Tests cross-project dependency display in task details panel.
;;
;; Window manipulation tests (setup-windows, quit, etc.) remain in
;; test/org-gtd-graph-ui-test.el as they require interactive Emacs.
;;
;; Migrated from test/org-gtd-graph-ui-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-data)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Cross-project dependency display tests

(deftest graph-ui/format-shows-cross-project-blockers ()
  "Shows blockers from other projects in task details."
  (let* ((tasks-file (concat org-gtd-directory "org-gtd-tasks.org"))
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
      (assert-match "Blocked by" details)
      (assert-match "Task A in Project X" details))))

(deftest graph-ui/format-separates-cross-project-blockers ()
  "Separates cross-project blockers into their own section."
  (let* ((tasks-file (concat org-gtd-directory "org-gtd-tasks.org"))
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
      (assert-match "Blocked by:" details)
      (assert-match "Task B in Project Y" details)
      (assert-match "Blocked by (other projects):" details)
      (assert-match "Task A in Project X" details))))

(deftest graph-ui/format-separates-cross-project-dependents ()
  "Separates cross-project dependents into their own section."
  (let* ((tasks-file (concat org-gtd-directory "org-gtd-tasks.org"))
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
      (assert-match "Blocks:" details)
      (assert-match "Task B in Project X" details)
      (assert-match "Blocks (other projects):" details)
      (assert-match "Task C in Project Y" details))))

(provide 'graph-ui-test)

;;; graph-ui-test.el ends here
