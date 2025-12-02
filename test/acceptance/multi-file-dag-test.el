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
;; These tests simulate the scenario where a user has tasks in multiple
;; org-agenda-files and links them to projects. This is a supported workflow
;; via org-gtd-graph-transient-add-root or similar commands.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Helper to create a task file and link it to a project
;; This simulates the result of using org-gtd-graph-transient-add-root
;; to link an existing task to a project.

(defun ogt-eunit--create-linked-task-in-file (filepath task-description project-id)
  "Create a task in FILEPATH and link it to project with PROJECT-ID.
Returns the task's generated ID.

This simulates what happens when a user has an existing task in their
org-agenda-files and uses graph commands to link it to a project."
  ;; Create the file with a task
  (with-current-buffer (find-file-noselect filepath)
    (erase-buffer)
    (org-mode)
    (insert (format "* TODO %s\n" task-description))
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-back-to-heading t)
    ;; Generate ID (not hardcoded)
    (let ((task-id (org-id-get-create)))
      ;; Link task to project (bi-directional)
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (org-todo "NEXT")
      (save-buffer)
      ;; Let org-id discover the ID naturally
      (org-id-update-id-locations (list filepath))
      task-id)))

;;; Multi-file Project Complete Workflow

(deftest multi-file-project-organizes-engages-completes-archives ()
  "Organizes project, engages, completes tasks, and archives across files."
  ;; 1. CAPTURE and ORGANIZE project in main file
  (capture-inbox-item "Multi-file project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task in main file" :level 2)
    (organize-as-project))

  ;; 2. Get project ID
  (let ((project-id nil))
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi-file project")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (save-buffer))

    ;; 3. Create second file with linked task (simulates existing task linked via graph commands)
    (let* ((second-file (org-gtd--path "secondary-tasks"))
           (task-id (ogt-eunit--create-linked-task-in-file
                     second-file "Task in second file" project-id)))

      ;; 4. Link project to task (bi-directional)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Multi-file project")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))

      (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

        ;; 5. VERIFY both tasks show in engage
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-match "Task in main file" agenda-content)
          (assert-match "Task in second file" agenda-content))

        ;; 6. COMPLETE both tasks
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task in main file")
          (org-todo "DONE"))

        (with-current-buffer (find-file-noselect second-file)
          (goto-char (point-min))
          (search-forward "Task in second file")
          (org-todo "DONE"))

        ;; 7. ARCHIVE - project should archive
        (org-gtd-archive-completed-items)

        ;; 8. VERIFY project is archived from main file
        (with-current-buffer (org-gtd--default-file)
          (refute-match "Multi-file project" (current-buffer-raw-text)))

        ;; 9. VERIFY task in second file IS archived
        (with-current-buffer (find-file-noselect second-file)
          (refute-match "Task in second file" (current-buffer-raw-text)))))))

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

  ;; 2. Get project ID
  (let ((project-id nil))
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Distributed project")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (save-buffer))

    ;; 3. Create second file with linked task
    (let* ((second-file (org-gtd--path "other-tasks"))
           (task-id (ogt-eunit--create-linked-task-in-file
                     second-file "Remote task" project-id)))

      ;; Link project to task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Distributed project")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))

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
          (refute-match "Remote task" (current-buffer-raw-text)))))))

;;; Multi-file Project Extension

(deftest multi-file-project-extension-after-archiving ()
  "Creates projects with multi-file tasks and verifies archiving."
  ;; 1. CAPTURE and ORGANIZE initial project
  (capture-inbox-item "Expandable project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Initial task" :level 2)
    (organize-as-project))

  ;; 2. Get project ID
  (let ((project-id nil))
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Expandable project")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (save-buffer))

    ;; 3. Create second file with linked task
    (let* ((second-file (org-gtd--path "additional-tasks"))
           (task-id (ogt-eunit--create-linked-task-in-file
                     second-file "External task" project-id)))

      ;; Link project to task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Expandable project")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))

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

        ;; 5. ARCHIVE first project
        (org-gtd-archive-completed-items)

        (with-current-buffer (org-gtd--default-file)
          (refute-match "Expandable project" (current-buffer-raw-text)))

        ;; 6. Create a NEW project with multi-file tasks
        (capture-inbox-item "New multi-file project")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Another new task" :level 2)
          (organize-as-project))

        ;; 7. Get new project ID and link a task from a third file
        (let ((new-project-id nil))
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "New multi-file project")
            (org-back-to-heading t)
            (setq new-project-id (org-id-get-create))
            (save-buffer))

          (let* ((third-file (org-gtd--path "new-extension-tasks"))
                 (new-task-id (ogt-eunit--create-linked-task-in-file
                               third-file "New extension task" new-project-id)))

            ;; Link project to task
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "New multi-file project")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
              (save-buffer))

            (let ((org-agenda-files (append org-agenda-files (list third-file))))

              ;; 8. VERIFY new project shows in engage
              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (assert-match "Another new task" agenda-content)
                (assert-match "New extension task" agenda-content))

              ;; 9. COMPLETE the new tasks
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Another new task")
                (org-todo "DONE"))

              (with-current-buffer (find-file-noselect third-file)
                (goto-char (point-min))
                (search-forward "New extension task")
                (org-todo "DONE"))

              ;; 10. ARCHIVE new project
              (org-gtd-archive-completed-items)

              (with-current-buffer (org-gtd--default-file)
                (refute-match "New multi-file project" (current-buffer-raw-text)))

              ;; 11. VERIFY task in third file IS archived
              (with-current-buffer (find-file-noselect third-file)
                (refute-match "New extension task" (current-buffer-raw-text))))))))))

;;; multi-file-dag-test.el ends here
