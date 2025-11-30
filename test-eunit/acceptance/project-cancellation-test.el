;;; project-cancellation-test.el --- Acceptance tests for project cancellation workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for project cancellation and archiving workflows including:
;; - Cancel one task in project, complete others, archive
;; - Cancel all tasks in project (same file), archive
;; - Cancel all tasks in project (multiple files), archive
;; - Cancel shared task between multiple projects
;; - Shared task partial cancellation scenarios
;; - Stuck projects should NOT be archived
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

;;; Cancel One Task, Complete Others

(deftest project-cancel-one-task-complete-others-archives ()
  "Organizes project, cancels one task, completes others, archives successfully."
  ;; Create project with tasks
  (capture-inbox-item "Organize conference")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Book conference room" :level 2)
    (make-task "Send speaker invites" :level 2)
    (make-task "Order catering" :level 2)
    (organize-as-project))

  ;; Verify tasks appear in engage
  (org-gtd-engage)
  (assert-match "Book conference room" (agenda-raw-text))

  ;; Cancel one task
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Order catering")
    (org-todo "CNCL"))

  ;; Verify canceled task doesn't show in engage
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (refute-match "Order catering" agenda-content)
    (assert-match "Book conference room" agenda-content))

  ;; Complete remaining tasks
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Book conference room")
    (org-todo "DONE")
    (goto-char (point-min))
    (re-search-forward "Send speaker invites")
    (org-todo "DONE"))

  ;; Archive - project should archive even with one canceled task
  (org-gtd-archive-completed-items)

  ;; Verify project is archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Organize conference" (current-buffer-raw-text))))

;;; Cancel All Tasks - Same File

(deftest project-cancel-all-tasks-same-file-archives ()
  "Cancels all tasks in project (same file), verifies nothing in engage, archives."
  ;; Create project
  (capture-inbox-item "Abandoned initiative")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Research market" :level 2)
    (make-task "Build prototype" :level 2)
    (make-task "Pitch to investors" :level 2)
    (organize-as-project))

  ;; Verify tasks appear
  (org-gtd-engage)
  (assert-match "Research market" (agenda-raw-text))

  ;; Cancel all tasks
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research market")
    (org-todo "CNCL")
    (goto-char (point-min))
    (re-search-forward "Build prototype")
    (org-todo "CNCL")
    (goto-char (point-min))
    (re-search-forward "Pitch to investors")
    (org-todo "CNCL"))

  ;; Verify nothing shows in engage
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (refute-match "Abandoned" agenda-content)
    (refute-match "Research market" agenda-content)
    (refute-match "Build prototype" agenda-content)
    (refute-match "Pitch to investors" agenda-content))

  ;; Archive
  (org-gtd-archive-completed-items)

  ;; Verify project and tasks are archived from main file
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (refute-match "Abandoned initiative" content)
      (refute-match "Research market" content)
      (refute-match "Build prototype" content)
      (refute-match "Pitch to investors" content))))

;;; Cancel All Tasks - Multiple Files

(deftest project-cancel-all-tasks-multi-file-archives ()
  "Cancels all tasks across multiple files, archives entire project.
NOTE: Properly sets up bi-directional relationships (project -> task via FIRST_TASKS,
task -> project via PROJECT_IDS) so archiving works correctly across files."
  ;; Create project in main file
  (capture-inbox-item "Cross-file abandoned project")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Main file task" :level 2)
    (organize-as-project))

  ;; Get project ID first
  (let ((project-id nil)
        (second-file (org-gtd--path "abandoned-secondary")))
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Cross-file abandoned project")
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (save-buffer))

    ;; Create second file with task linked to project
    (with-current-buffer (find-file-noselect second-file)
      (erase-buffer)
      (org-mode)
      (insert "* TODO Secondary file task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Actions\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      ;; Generate ID (not hardcoded)
      (let ((task-id (org-id-get-create)))
        ;; Link task to project
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-todo "NEXT")
        (save-buffer)
        (org-id-update-id-locations (list second-file))

        ;; Link project to task
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Cross-file abandoned project")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
          (save-buffer))))

    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; Verify both tasks show in engage
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Main file task" agenda-content)
        (assert-match "Secondary file task" agenda-content))

      ;; Cancel all tasks
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Main file task")
        (org-todo "CNCL"))

      (with-current-buffer (find-file-noselect second-file)
        (goto-char (point-min))
        (search-forward "Secondary file task")
        (org-todo "CNCL"))

      ;; Verify nothing shows in engage
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (refute-match "Cross-file abandoned" agenda-content)
        (refute-match "Main file task" agenda-content)
        (refute-match "Secondary file task" agenda-content))

      ;; Archive
      (org-gtd-archive-completed-items)

      ;; Verify project is archived from main file
      (with-current-buffer (org-gtd--default-file)
        (refute-match "Cross-file abandoned project" (current-buffer-raw-text)))

      ;; Verify task in second file is archived
      (with-current-buffer (find-file-noselect second-file)
        (refute-match "Secondary file task" (current-buffer-raw-text))))))

;;; Cancel Shared Task Between Projects

(deftest project-cancel-shared-task-between-projects ()
  "Cancels shared task between two projects, verifies both continue working."
  ;; Create first project
  (capture-inbox-item "Project Alpha")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Alpha task 1" :level 2)
    (make-task "Shared task" :level 2)
    (organize-as-project))

  ;; Create second project
  (capture-inbox-item "Project Beta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Beta task 1" :level 2)
    (organize-as-project))

  ;; Share "Shared task" with Project Beta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared task")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create))
          (project-alpha-id nil)
          (project-beta-id nil))

      ;; Get Project Alpha's ID
      (goto-char (point-min))
      (search-forward "Project Alpha")
      (org-back-to-heading t)
      (setq project-alpha-id (org-id-get-create))

      ;; Get Project Beta's ID
      (goto-char (point-min))
      (search-forward "Project Beta")
      (org-back-to-heading t)
      (setq project-beta-id (org-id-get-create))

      ;; Add shared task to Project Beta's FIRST_TASKS
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

      ;; Add both project IDs to shared task's PROJECT_IDS
      (goto-char (point-min))
      (search-forward "Shared task")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-alpha-id)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-beta-id)
      ;; Set task to NEXT so it appears in engage
      (org-todo "NEXT")

      ;; Verify shared task appears in engage
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Shared task" agenda-content)
        (assert-match "Alpha task 1" agenda-content)
        (assert-match "Beta task 1" agenda-content))

      ;; Cancel the shared task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Shared task")
        (org-todo "CNCL"))

      ;; Verify shared task doesn't appear, but other tasks do
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (refute-match "Shared task" agenda-content)
        (assert-match "Alpha task 1" agenda-content)
        (assert-match "Beta task 1" agenda-content))

      ;; Complete remaining tasks
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Alpha task 1")
        (org-todo "DONE")
        (goto-char (point-min))
        (search-forward "Beta task 1")
        (org-todo "DONE"))

      ;; Archive
      (org-gtd-archive-completed-items)

      ;; Verify both projects are archived
      (with-current-buffer (org-gtd--default-file)
        (refute-match "Project Alpha" (current-buffer-raw-text))
        (refute-match "Project Beta" (current-buffer-raw-text))))))

;;; Shared Task Partial Cancellation

(deftest project-shared-task-partial-cancel-archives-correctly ()
  "Two projects share task, cancel one project's tasks, verify partial archive."
  ;; Create first project
  (capture-inbox-item "Project Gamma")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Gamma unique task" :level 2)
    (make-task "Shared infrastructure task" :level 2)
    (organize-as-project))

  ;; Create second project
  (capture-inbox-item "Project Delta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Delta unique task" :level 2)
    (organize-as-project))

  ;; Share task with Project Delta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared infrastructure task")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create))
          (project-gamma-id nil)
          (project-delta-id nil))

      ;; Get project IDs
      (goto-char (point-min))
      (search-forward "Project Gamma")
      (org-back-to-heading t)
      (setq project-gamma-id (org-id-get-create))

      (goto-char (point-min))
      (search-forward "Project Delta")
      (org-back-to-heading t)
      (setq project-delta-id (org-id-get-create))

      ;; Add shared task to Project Delta's FIRST_TASKS
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

      ;; Add both project IDs to shared task's PROJECT_IDS
      (goto-char (point-min))
      (search-forward "Shared infrastructure task")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-gamma-id)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-delta-id)

      ;; Cancel all tasks in Project Gamma only
      (goto-char (point-min))
      (search-forward "Gamma unique task")
      (org-todo "CNCL")
      (goto-char (point-min))
      (search-forward "Shared infrastructure task")
      (org-todo "CNCL")

      ;; Verify shared task doesn't appear (it's canceled), but Delta's task does
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (refute-match "Gamma unique task" agenda-content)
        (refute-match "Shared infrastructure task" agenda-content)
        (assert-match "Delta unique task" agenda-content))

      ;; Archive - Project Gamma should archive
      (org-gtd-archive-completed-items)

      (with-current-buffer (org-gtd--default-file)
        (let ((content (current-buffer-raw-text)))
          ;; Project Gamma should be archived
          (refute-match "\\* Project Gamma" content)
          ;; Shared task should REMAIN (Project Delta still references it)
          (assert-match "Shared infrastructure task" content)
          ;; Project Delta should still exist
          (assert-match "Project Delta" content)
          (assert-match "Delta unique task" content)))

      ;; Now cancel Project Delta's remaining task
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Delta unique task")
        (org-todo "CNCL"))

      ;; Archive again
      (org-gtd-archive-completed-items)

      ;; Verify everything is archived
      (with-current-buffer (org-gtd--default-file)
        (let ((content (current-buffer-raw-text)))
          (refute-match "Project Gamma" content)
          (refute-match "Project Delta" content)
          (refute-match "Gamma unique task" content)
          (refute-match "Delta unique task" content)
          (refute-match "Shared infrastructure task" content))))))

;;; Stuck Projects Do NOT Get Archived

(deftest stuck-project-not-archived-until-completed ()
  "Verifies stuck project remains in main file, archives only when completed."
  ;; Create project
  (capture-inbox-item "Marketing Campaign")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Research target audience" :level 2)
    (make-task "Create content" :level 2)
    (make-task "Launch campaign" :level 2)
    (organize-as-project))

  ;; Make project STUCK by setting all tasks to TODO (not NEXT or WAIT)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research target audience")
    (org-todo "TODO")
    (goto-char (point-min))
    (re-search-forward "Create content")
    (org-todo "TODO")
    (goto-char (point-min))
    (re-search-forward "Launch campaign")
    (org-todo "TODO"))

  ;; Verify project appears in stuck projects review
  (org-gtd-review-stuck-projects)
  (assert-match "Marketing" (agenda-raw-text))

  ;; Try to archive - stuck project should NOT be archived
  (org-gtd-archive-completed-items)

  ;; Verify project is still in main file
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (assert-match "Marketing Campaign" content)
      (assert-match "Research target audience" content)
      (assert-match "Create content" content)
      (assert-match "Launch campaign" content)))

  ;; Now complete all tasks to make project archivable
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research target audience")
    (org-todo "DONE")
    (goto-char (point-min))
    (re-search-forward "Create content")
    (org-todo "DONE")
    (goto-char (point-min))
    (re-search-forward "Launch campaign")
    (org-todo "DONE"))

  ;; Verify project does NOT appear in stuck projects anymore
  (org-gtd-review-stuck-projects)
  (refute-match "Marketing" (agenda-raw-text))

  ;; Archive - completed project should now archive
  (org-gtd-archive-completed-items)

  ;; Verify project is no longer in main file
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (refute-match "Marketing Campaign" content)
      (refute-match "Research target audience" content)
      (refute-match "Create content" content)
      (refute-match "Launch campaign" content))))

;;; project-cancellation-test.el ends here
