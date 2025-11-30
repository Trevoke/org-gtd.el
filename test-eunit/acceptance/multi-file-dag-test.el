;;; multi-file-dag-test.el --- Acceptance tests for multi-file DAG workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for multi-file DAG (Directed Acyclic Graph) workflows including:
;; - Projects with tasks spanning multiple files
;; - Completion and archiving across files
;; - Cancellation in multi-file scenarios
;; - Project extension with tasks in different files
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;
;; NOTE: The original buttercup tests had incomplete setup - they didn't set
;; ORG_GTD_PROJECT_IDS on secondary file tasks. The e-unit versions properly
;; set up bi-directional relationships (project -> task via FIRST_TASKS,
;; task -> project via PROJECT_IDS) so archiving works correctly.
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Multi-file Project Complete Workflow

(deftest multi-file-project-organizes-engages-completes-archives ()
  "Organizes project, engages, completes tasks, and archives across files.
NOTE: This test documents a product limitation - org-gtd-archive-completed-items
currently only archives items from the main GTD files, NOT from secondary
org-agenda-files. Tasks in secondary files remain even after being DONE.
This is a known limitation that should be fixed in the future."
  ;; 1. CAPTURE and ORGANIZE project in main file
  (capture-inbox-item "Multi-file project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task in main file" :level 2)
    (organize-as-project))

  ;; 2. Create second file with related task
  (let ((second-file (org-gtd--path "secondary-tasks")))
    (with-temp-file second-file
      (insert "* TODO Task in second file\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-second-id\n")
      (insert ":ORG_GTD: Actions\n")
      (insert ":END:\n"))

    ;; 3. Get project ID and set up bi-directional link
    (let ((project-id nil))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Multi-file project")
        (org-back-to-heading t)
        (setq project-id (org-id-get-create))
        ;; Link project to task
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "task-second-id"))

      ;; Set up task with back-link to project
      (with-current-buffer (find-file-noselect second-file)
        (org-mode)
        (goto-char (point-min))
        (search-forward "Task in second file")
        (org-back-to-heading t)
        (org-id-add-location "task-second-id" second-file)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-todo "NEXT")))

    ;; Add second file to org-agenda-files
    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; 4. VERIFY both tasks show in engage
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        ;; Multi-file projects may not show project heading, just task names
        (assert-match "Task in main file" agenda-content)
        (assert-match "Task in second file" agenda-content))

      ;; 5. COMPLETE both tasks
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task in main file")
        (org-todo "DONE"))

      (with-current-buffer (find-file-noselect second-file)
        (goto-char (point-min))
        (search-forward "Task in second file")
        (org-todo "DONE"))

      ;; 6. ARCHIVE - project should archive
      (org-gtd-archive-completed-items)

      ;; 7. VERIFY project is archived from main file
      (with-current-buffer (org-gtd--default-file)
        (refute-match "Multi-file project" (current-buffer-raw-text)))

      ;; 8. VERIFY task in second file IS archived
      (with-current-buffer (find-file-noselect second-file)
        (refute-match "Task in second file" (current-buffer-raw-text))))))

;;; Multi-file Project Cancellation

(deftest multi-file-project-cancellation-with-archiving ()
  "Cancels tasks in different files and verifies archiving works."
  ;; 1. CAPTURE and ORGANIZE project
  (capture-inbox-item "Distributed project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Local task one" :level 2)
    (make-task "Local task two" :level 2)
    (organize-as-project))

  ;; 2. Create second file with task
  (let ((second-file (org-gtd--path "other-tasks")))
    (with-temp-file second-file
      (insert "* TODO Remote task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: remote-task-id\n")
      (insert ":ORG_GTD: Actions\n")
      (insert ":END:\n"))

    ;; 3. Get project ID and set up bi-directional link
    (let ((project-id nil))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Distributed project")
        (org-back-to-heading t)
        (setq project-id (org-id-get-create))
        ;; Link project to task
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "remote-task-id"))

      ;; Set up task with back-link to project
      (with-current-buffer (find-file-noselect second-file)
        (org-mode)
        (goto-char (point-min))
        (search-forward "Remote task")
        (org-back-to-heading t)
        (org-id-add-location "remote-task-id" second-file)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-todo "NEXT")))

    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; 4. CANCEL one local task and the remote task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Local task one")
        (org-todo "CNCL"))

      (with-current-buffer (find-file-noselect second-file)
        (goto-char (point-min))
        (search-forward "Remote task")
        (org-todo "CNCL"))

      ;; 5. COMPLETE remaining local task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Local task two")
        (org-todo "DONE"))

      ;; 6. ARCHIVE - project should archive with mix of CNCL and DONE
      (org-gtd-archive-completed-items)

      ;; 7. VERIFY project is archived
      (with-current-buffer (org-gtd--default-file)
        (refute-match "Distributed project" (current-buffer-raw-text)))

      ;; 8. VERIFY remote canceled task IS archived
      (with-current-buffer (find-file-noselect second-file)
        (refute-match "Remote task" (current-buffer-raw-text))))))

;;; Multi-file Project Extension

(deftest multi-file-project-extension-after-archiving ()
  "Adds task to completed project where original tasks are in different files.
NOTE: This test verifies that after archiving a project with multi-file tasks,
we can create a NEW project and link tasks from multiple files to it."
  ;; 1. CAPTURE and ORGANIZE initial project
  (capture-inbox-item "Expandable project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Initial task" :level 2)
    (organize-as-project))

  ;; 2. Create second file with another task
  (let ((second-file (org-gtd--path "additional-tasks")))
    (with-temp-file second-file
      (insert "* TODO External task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: external-task-id\n")
      (insert ":ORG_GTD: Actions\n")
      (insert ":END:\n"))

    ;; 3. Get project ID and set up bi-directional link
    (let ((project-id nil))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Expandable project")
        (org-back-to-heading t)
        (setq project-id (org-id-get-create))
        ;; Link project to task
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "external-task-id"))

      ;; Set up task with back-link to project
      (with-current-buffer (find-file-noselect second-file)
        (org-mode)
        (goto-char (point-min))
        (search-forward "External task")
        (org-back-to-heading t)
        (org-id-add-location "external-task-id" second-file)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-todo "NEXT")))

    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; 4. COMPLETE both tasks
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Initial task")
        (org-todo "DONE"))

      (with-current-buffer (find-file-noselect second-file)
        (goto-char (point-min))
        (search-forward "External task")
        (org-todo "DONE"))

      ;; 5. ARCHIVE - project should be complete and archived
      (org-gtd-archive-completed-items)

      (with-current-buffer (org-gtd--default-file)
        (refute-match "Expandable project" (current-buffer-raw-text)))

      ;; 6. Now create a NEW project that links tasks from multiple files
      ;; First, create a new task in a third file
      (let ((third-file (org-gtd--path "new-extension-tasks")))
        (with-temp-file third-file
          (insert "* TODO New extension task\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: extension-task-id\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":END:\n"))

        ;; Extended org-agenda-files to include third file
        (let ((org-agenda-files (append org-agenda-files (list third-file))))

          ;; 7. Create new project with task in main file
          (capture-inbox-item "New multi-file project")
          (org-gtd-process-inbox)

          (with-wip-buffer
            (goto-char (point-max))
            (newline)
            (make-task "Another new task" :level 2)
            (organize-as-project))

          ;; 8. Get new project ID and set up bi-directional link
          (let ((new-project-id nil))
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "New multi-file project")
              (org-back-to-heading t)
              (setq new-project-id (org-id-get-create))
              ;; Link project to task
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "extension-task-id"))

            ;; Set up task with back-link to project
            (with-current-buffer (find-file-noselect third-file)
              (org-mode)
              (goto-char (point-min))
              (search-forward "New extension task")
              (org-back-to-heading t)
              (org-id-add-location "extension-task-id" third-file)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" new-project-id)
              (org-todo "NEXT")))

          ;; 9. VERIFY new project shows in engage with tasks from two files
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            ;; Multi-file projects may not show project heading, just task names
            (assert-match "Another new task" agenda-content)
            (assert-match "New extension task" agenda-content))

          ;; 10. COMPLETE the new tasks
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Another new task")
            (org-todo "DONE"))

          (with-current-buffer (find-file-noselect third-file)
            (goto-char (point-min))
            (search-forward "New extension task")
            (org-todo "DONE"))

          ;; 11. ARCHIVE - new project should archive
          (org-gtd-archive-completed-items)

          (with-current-buffer (org-gtd--default-file)
            (refute-match "New multi-file project" (current-buffer-raw-text)))

          ;; 12. VERIFY task in third file IS archived
          (with-current-buffer (find-file-noselect third-file)
            (refute-match "New extension task" (current-buffer-raw-text))))))))

;;; multi-file-dag-test.el ends here
