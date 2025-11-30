;;; multi-project-sharing-test.el --- Acceptance tests for multi-project task sharing -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for multi-project task sharing workflows including:
;; - Sharing a task between two projects via ORG_GTD_FIRST_TASKS
;; - Completing a shared task and verifying both projects recognize it
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Share Task Between Projects

(deftest share-task-between-projects-via-first-tasks ()
  "Creates two projects with same task ID in both ORG_GTD_FIRST_TASKS."
  ;; Create first project with task
  (capture-inbox-item "Project Alpha")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Design database schema" :level 2)
    (organize-as-project))

  ;; Create second project with its own task
  (capture-inbox-item "Project Beta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Implement API" :level 2)
    (organize-as-project))

  ;; Share "Design database schema" with Project Beta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Design database schema")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create)))
      ;; Add shared task to Project Beta's FIRST_TASKS
      (goto-char (point-min))
      (search-forward "Project Beta")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

      ;; Verify both tasks show in engage
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Design database schema" agenda-content)
        (assert-match "Implement API" agenda-content))

      ;; Verify Project Alpha has shared task in FIRST_TASKS
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Project Alpha")
        (org-back-to-heading t)
        (let ((alpha-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (refute-nil (member shared-task-id alpha-tasks))))

      ;; Verify Project Beta has shared task in FIRST_TASKS
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Project Beta")
        (org-back-to-heading t)
        (let ((beta-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (refute-nil (member shared-task-id beta-tasks)))))))

;;; Complete Shared Task

(deftest complete-shared-task-verifies-both-projects ()
  "Marks shared task complete and verifies both projects recognize it as done."
  ;; Create two projects and share a task between them via ORG_GTD_FIRST_TASKS
  (capture-inbox-item "Project Alpha")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Shared Task" :level 2)
    (organize-as-project))

  (capture-inbox-item "Project Beta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Another Task" :level 2)
    (organize-as-project))

  ;; Share "Shared Task" with Project Beta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared Task")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create))
          (project-beta-id nil))

      ;; Get Project Beta's ID
      (goto-char (point-min))
      (search-forward "Project Beta")
      (org-back-to-heading t)
      (setq project-beta-id (org-id-get-create))

      ;; Add shared task to Project Beta's FIRST_TASKS
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

      ;; Add Project Beta's ID to shared task's PROJECT_IDS (bi-directional link)
      (goto-char (point-min))
      (search-forward "Shared Task")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-beta-id)

      ;; Complete the shared task
      (org-todo "DONE")

      ;; Verify shared task doesn't appear in engage (it's DONE)
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (refute-match "Shared Task" agenda-content)
        ;; "Another Task" from Project Beta should still appear
        (assert-match "Another Task" agenda-content)))))

;;; multi-project-sharing-test.el ends here
