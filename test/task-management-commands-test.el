;;; task-management-commands-test.el --- Tests for task management commands -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for interactive commands that manage task dependencies (BLOCKS/BLOCKED_BY relationships)
;;

;;; Code:

;;;; Requirements

(require 'org-gtd)
(require 'org-gtd-task-management)
(require 'buttercup)

;; Load test helpers
(load (expand-file-name "helpers/setup.el"
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))
(load (expand-file-name "helpers/builders.el"
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))

;;;; Test Setup

(describe
 "task dependency commands"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 ;; Unit tests for org-gtd-task-add-blockers moved to test-eunit/unit/task-management-commands-test.el

 (describe
  "cross-project dependencies (Story 5)"

  (it "shows tasks from different projects with project labels when selecting blockers"
      ;; This test verifies the acceptance criteria: "tasks from all existing agenda files clearly labeled by project"
      ;; Set up Project A in main GTD tasks file
      (capture-inbox-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (organize-as-project)

      ;; Create a second GTD file with Project B
      (let ((second-file (org-gtd--path "secondary-project")))
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))

        ;; Ensure the ID is registered in the org-id system by visiting the file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode) ; Make sure org-mode is active
          (goto-char (point-min))
          (search-forward "Task B1")
          (org-back-to-heading t)
          ;; Force org-id to recognize this ID
          (org-id-add-location "task-b1-id" second-file))

        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; Capture what task selection interface shows
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)

            ;; Mock the completing-read to capture the options presented
            (let ((presented-options nil)
                  (call-count 0))
              (spy-on 'completing-read
                      :and-call-fake
                      (lambda (_prompt collection &rest _args)
                        (when (= call-count 0) ; Only capture on first call
                          (setq presented-options collection))
                        (setq call-count (1+ call-count))
                        ;; Return empty string on second call to exit multi-select loop
                        (if (= call-count 1) "task-b1-id" "")))
              (org-gtd-task-management--select-multiple-task-ids "Test prompt: ")

              ;; Verify that tasks are labeled with project information
              ;; The collection should include project context for cross-project tasks
              (let ((has-project-labels (cl-some (lambda (option)
                                                   ;; option is a cons cell (display . value)
                                                   (string-match-p "Project B" (car option)))
                                                 presented-options)))
                (expect has-project-labels :to-be-truthy)))))))


  (it "creates dependencies between tasks in different projects"
      ;; Test cross-project blocking relationships
      ;; Set up Project A in main GTD tasks file
      (capture-inbox-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (organize-as-project)

      ;; Create a second GTD file manually
      (let ((second-file (org-gtd--path "secondary-project")))
        ;; Set up Project B in the secondary file
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))

        ;; Ensure the ID is registered in the org-id system by visiting the file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode) ; Make sure org-mode is active
          (goto-char (point-min))
          (search-forward "Task B1")
          (org-back-to-heading t)
          ;; Force org-id to recognize this ID
          (org-id-add-location "task-b1-id" second-file))

        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; Now create cross-project dependency: Task A1 depends on Task B1
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)

            ;; Mock task selection to return Task B1 from different project
            (spy-on 'org-gtd-task-management--select-multiple-task-ids
                    :and-return-value '("task-b1-id"))
            (org-gtd-task-add-blockers)

            ;; Verify Task A1 has DEPENDS_ON pointing to Task B1
            (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
              (expect depends-on :to-equal '("task-b1-id"))))

          ;; Verify Task B1 in the other file has BLOCKS pointing to Task A1
          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task B1")
            (org-back-to-heading t)
            (let ((task-a1-id (with-current-buffer (org-gtd--default-file)
                                (goto-char (point-min))
                                (search-forward "Task A1")
                                (org-back-to-heading t)
                                (org-entry-get (point) "ID")))
                  (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
              (expect blocks :to-equal (list task-a1-id))))))))



 ;; Automatic next action updates tests moved to test-eunit/integration/task-management-commands-test.el

 ;; Circular dependency detection tests moved to test-eunit/unit/task-management-commands-test.el

 ;; Remove task blockers tests moved to test-eunit/unit/task-management-commands-test.el

 ;; org-gtd-task-add-dependents tests moved to test-eunit/unit/task-management-commands-test.el

 ;; Direct task dependency management tests moved to test-eunit/unit/task-management-commands-test.el

 ;; Clear all task relationships tests moved to test-eunit/unit/task-management-commands-test.el

 ) ; Close the main "task dependency commands" describe block

(describe
 "lazy ID creation (Story 9)"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "automatically creates IDs for tasks without them when adding dependencies"
     ;; Test that tasks without pre-existing IDs get IDs created automatically
     (with-temp-buffer
       (org-mode)
       ;; Create tasks WITHOUT IDs initially
       (insert "* Task A\n\n")
       (insert "* Task B\n\n")

       ;; Go to Task B (no ID) and add Task A (no ID) as blocker
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       ;; Capture the initial state - no IDs should exist
       (expect (org-entry-get (point) "ID") :to-be nil)
       (goto-char (point-min))
       (re-search-forward "Task A")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "ID") :to-be nil)

       ;; Mock the task selection - first we need to collect available tasks
       ;; The system should create IDs as needed during this process
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       (let* ((task-a-id nil)
              (task-b-id nil))
         (spy-on 'org-gtd-task-management--select-multiple-task-ids
                 :and-call-fake
                 (lambda (_prompt)
                   ;; First trigger the real task collection to create IDs
                   (org-gtd-task-management--collect-all-task-info)
                   ;; Now capture Task A's ID after it gets created during collection
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "Task A")
                     (org-back-to-heading t)
                     (setq task-a-id (org-entry-get (point) "ID")))
                   (list task-a-id)))
         (org-gtd-task-add-blockers)

         ;; After the command, both tasks should have IDs
         ;; Task B should have an ID (current task)
         (setq task-b-id (org-entry-get (point) "ID"))
         (expect task-b-id :not :to-be nil)
         (expect (string-match "^[a-z0-9-]+$" task-b-id) :to-be 0) ; org-gtd ID format

         ;; Task A should have an ID (selected as blocker)
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (setq task-a-id (org-entry-get (point) "ID"))
         (expect task-a-id :not :to-be nil)
         (expect (string-match "^[a-z0-9-]+$" task-a-id) :to-be 0) ; org-gtd ID format

         ;; Verify the blocking relationships were created correctly
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-contain task-b-id)

         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-contain task-a-id))))

 (it "creates IDs automatically for tasks without them when adding dependencies"
     ;; Test mixed scenario: some tasks have IDs, some don't
     (with-temp-buffer
       (org-mode)
       ;; Task A has no ID, Task B has pre-existing ID
       (insert "* Task A\n\n")
       (insert "* Task B\n:PROPERTIES:\n:ID: existing-task-b-id\n:END:\n\n")

       ;; Go to Task B (has ID) and add Task A (no ID) as blocker
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       ;; Verify initial state
       (expect (org-entry-get (point) "ID") :to-equal "existing-task-b-id")
       (goto-char (point-min))
       (re-search-forward "Task A")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "ID") :to-be nil)

       (let ((task-a-id nil))
         (spy-on 'org-gtd-task-management--select-multiple-task-ids
                 :and-call-fake
                    (lambda (_prompt)
                      ;; First trigger the real task collection to create IDs
                      (org-gtd-task-management--collect-all-task-info)
                      ;; The selection should create ID for Task A during collection
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "Task A")
                        (org-back-to-heading t)
                        (setq task-a-id (org-entry-get (point) "ID")))
                      (list task-a-id)))

         ;; Go back to Task B to run the command
         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (org-gtd-task-add-blockers)

         ;; Task A should now have an ID
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (setq task-a-id (org-entry-get (point) "ID"))
         (expect task-a-id :not :to-be nil)

         ;; Task B should keep its pre-existing ID
         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (expect (org-entry-get (point) "ID") :to-equal "existing-task-b-id")

         ;; Verify relationships were created
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-contain task-a-id)
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-contain "existing-task-b-id"))))

     ) ; Close lazy ID creation describe block

 ;; Relationship visualization tests moved to test-eunit/unit/task-management-commands-test.el

(describe "dependency helper window (Story 11)"
  (it "displays live dependency view in WIP buffer when org-gtd-clarify-display-helper-buffer is enabled"
    (with-temp-buffer
      ;; Create a WIP buffer with multiple tasks
      (org-mode)
      (org-gtd-wip-mode)

      ;; Insert project structure with dependencies (disable ID overlays during insert)
      (let ((org-gtd-id-overlay-mode nil))
        (insert "* Build Deck\n")
        (insert "** Get permits\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_BLOCKS: get-materials\n")
        (insert ":ID: get-permits\n")
        (insert ":END:\n")
        (insert "** Get materials\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_DEPENDS_ON: get-permits\n")
        (insert ":ORG_GTD_BLOCKS: install-decking\n")
        (insert ":ID: get-materials\n")
        (insert ":END:\n")
        (insert "** Install decking\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_DEPENDS_ON: get-materials\n")
        (insert ":ID: install-decking\n")
        (insert ":END:\n")
        (insert "** Paint deck (orphaned task)\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: paint-deck\n")
        (insert ":END:\n"))

      ;; Enable the helper buffer feature
      (let ((org-gtd-clarify-display-helper-buffer t))
        ;; Trigger the helper window display
        (org-gtd-clarify-display-dependency-helper)

        ;; Check that helper window was created
        (let ((helper-buffer (get-buffer "*Org GTD Project Dependencies*")))
          (expect helper-buffer :not :to-be nil)
          (with-current-buffer helper-buffer
            (let ((helper-content (buffer-string)))
              ;; Should show project name
              (expect helper-content :to-match "Project name: Build Deck")
              ;; Should show task relationships in format: (depends_on, ...) -> task -> (blocks, ...)
              (expect helper-content :to-match "() -> Get permits -> (Get materials)")
              (expect helper-content :to-match "(Get permits) -> Get materials -> (Install decking)")
              (expect helper-content :to-match "(Get materials) -> Install decking -> ()")
              ;; Should show orphaned tasks section
              (expect helper-content :to-match "Orphaned tasks:")
              (expect helper-content :to-match "Paint deck (orphaned task)"))))))))

;; Broken project detection tests (Story 14) moved to test-eunit/integration/task-management-commands-test.el

;; Find-project-heading tests for multi-file DAG moved to test-eunit/integration/task-management-commands-test.el

;;; task-management-commands-test.el ends here
