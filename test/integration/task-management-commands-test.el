;;; task-management-commands-test.el --- Integration tests for task management -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for task management commands that require filesystem access.
;; These tests use mock-fs via ogt-eunit-with-mock-gtd macro.
;;
;; Migrated from test/task-management-commands-test.el (buttercup).
;; Pure unit tests are in test-eunit/unit/task-management-commands-test.el.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Automatic Next Action Updates (Story 15)

(deftest task-mgmt-int/blocker-done-makes-dependents-next ()
  "Automatically makes dependent tasks NEXT when blocker is marked DONE."
  ;; Test the core acceptance criteria: When I mark a task as DONE,
  ;; all tasks that were blocked by this task automatically become NEXT
  (let ((test-file (concat org-gtd-directory "test-deps.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)
      (require 'org-gtd-projects)  ;; Ensure org-edna action is defined

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
      (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Create bidirectional dependencies using the API
      (org-gtd-dependencies-create "task-a-id" "task-b-id")
      (org-gtd-dependencies-create "task-a-id" "task-c-id")

      ;; Verify TRIGGER was set on Task A (with self finder for org-edna)
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (assert-equal "self org-gtd-update-project-after-task-done!" (org-entry-get (point) "TRIGGER"))

      ;; Verify initial state: Task B and C should be TODO
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      ;; Mark Task A as DONE - this should trigger automatic updates via org-edna
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task B and C automatically became NEXT
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO"))

      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

(deftest task-mgmt-int/partial-blocker-done-keeps-todo ()
  "Leaves tasks with remaining dependencies as TODO when only one blocker is completed."
  ;; Test that tasks with multiple dependencies only become NEXT when ALL blockers are complete
  (let ((test-file (concat org-gtd-directory "test-multi-deps.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
      (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Create dependencies using the API: A blocks C, B blocks C
      (org-gtd-dependencies-create "task-a-id" "task-c-id")
      (org-gtd-dependencies-create "task-b-id" "task-c-id")

      ;; Mark only Task A as DONE (Task B still TODO)
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task C remains TODO (still blocked by Task B)
      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal "TODO" (org-entry-get (point) "TODO"))

      ;; Now mark Task B as DONE too
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Now Task C should become NEXT (all dependencies satisfied)
      (goto-char (point-min))
      (re-search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

(deftest task-mgmt-int/state-changes-reflected-immediately ()
  "Updates state changes immediately when blocker marked DONE."
  ;; Test that changes happen in the same transaction
  (let ((test-file (concat org-gtd-directory "test-immediate.org")))
    (with-current-buffer (find-file-noselect test-file)
      (org-mode)
      (require 'org-gtd-task-management)

      (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
      (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

      (save-buffer)

      ;; Register IDs with org-id
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (org-id-add-location (match-string 1) test-file))

      ;; Create dependency using the API
      (org-gtd-dependencies-create "task-a-id" "task-b-id")

      ;; Mark Task A as DONE
      (goto-char (point-min))
      (re-search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE")
      (save-buffer)

      ;; Verify Task B became NEXT immediately (same buffer, no file reload needed)
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--next) (org-entry-get (point) "TODO")))))

;;; Broken Project Detection (Story 14)

(deftest task-mgmt-int/detects-broken-references ()
  "Identifies projects with non-existent task references and provides guidance."
  ;; Use the default GTD tasks file (org-agenda-files already includes org-gtd-directory)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "\n* Build Deck Project\n")
    (insert "** Get permits\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: get-permits-id\n")
    (insert ":ORG_GTD_BLOCKS: non-existent-task-id\n")  ; Broken reference
    (insert ":END:\n")
    (insert "** Install decking\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: install-decking-id\n")
    (insert ":ORG_GTD_DEPENDS_ON: another-missing-task-id\n")  ; Another broken reference
    (insert ":END:\n")
    (save-buffer))

  (let ((health-results (org-gtd-validate-project-dependencies)))
    ;; Should identify broken references
    (assert-true health-results)
    (assert-true (plist-get health-results :broken-references))

    ;; Should identify the specific broken references
    (let ((broken-refs (plist-get health-results :broken-references)))
      (assert-equal 2 (length broken-refs))

      ;; Should contain information about the broken references
      (assert-true (cl-some (lambda (ref)
                              (and (string= (plist-get ref :referencing-task) "get-permits-id")
                                   (string= (plist-get ref :missing-task) "non-existent-task-id")
                                   (string= (plist-get ref :property) "ORG_GTD_BLOCKS")))
                            broken-refs))

      (assert-true (cl-some (lambda (ref)
                              (and (string= (plist-get ref :referencing-task) "install-decking-id")
                                   (string= (plist-get ref :missing-task) "another-missing-task-id")
                                   (string= (plist-get ref :property) "ORG_GTD_DEPENDS_ON")))
                            broken-refs)))

    ;; Should provide guidance for fixing broken references
    (assert-true (plist-get health-results :guidance))
    (let ((guidance (plist-get health-results :guidance)))
      (assert-match "[Bb]roken.*reference" guidance)
      (assert-match "[Rr]emove.*invalid.*property" guidance))))

(deftest task-mgmt-int/detects-orphaned-tasks ()
  "Identifies orphaned tasks with dependencies that aren't in proper projects."
  ;; Use the default GTD tasks file
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "\n* Random Task with Dependencies\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: orphaned-task-id\n")
    (insert ":ORG_GTD_DEPENDS_ON: some-other-task-id\n")
    (insert ":END:\n")
    (insert "* Another Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: some-other-task-id\n")
    (insert ":END:\n")
    (save-buffer))

  (let ((health-results (org-gtd-validate-project-dependencies)))
    ;; Should identify orphaned tasks
    (assert-true health-results)
    (assert-true (plist-get health-results :orphaned-tasks))

    (let ((orphaned-tasks (plist-get health-results :orphaned-tasks)))
      (assert-true (> (length orphaned-tasks) 0))

      ;; Should identify the orphaned task with dependencies
      (assert-true (cl-some (lambda (task)
                              (string= (plist-get task :id) "orphaned-task-id"))
                            orphaned-tasks)))

    ;; Should provide guidance for fixing orphaned tasks
    (assert-true (plist-get health-results :guidance))
    (let ((guidance (plist-get health-results :guidance)))
      (assert-match "[Oo]rphaned.*task" guidance)
      (assert-match "[Oo]rganize.*into.*project" guidance))))

;;; Find Project Heading for Multi-file DAG

(deftest task-mgmt-int/find-project-heading-same-file ()
  "Finds project heading when task is in same file as project."
  ;; Standard case - task is outline child of project
  (capture-inbox-item "Same-file Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (make-task "Task in same file" :level 2)
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task in same file")
    (org-back-to-heading t)

    ;; Should find the project heading via outline navigation
    (let ((project-heading (org-gtd-task-management--find-project-heading)))
      (assert-true project-heading)
      (assert-match "Same-file Project" project-heading))))

(deftest task-mgmt-int/find-project-heading-different-file ()
  "Finds project heading when task is in different file (multi-file DAG)."
  ;; Create project in main file
  (capture-inbox-item "Multi-file DAG Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (make-task "Task in main file" :level 2)
  (organize-as-project)

  ;; Get project ID
  (let (project-id second-file)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi-file DAG Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID")))

    ;; Create task in different file with ORG_GTD_PROJECT_IDS set
    (setq second-file (org-gtd--path "other-tasks.org"))
    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (insert "* Task in other file\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-other-123\n")
      (insert ":ORG_GTD: Actions\n")
      (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
      (insert ":END:\n")
      (save-buffer)

      ;; Should find project heading via ORG_GTD_PROJECT_IDS
      (goto-char (point-min))
      (search-forward "Task in other file")
      (org-back-to-heading t)

      (let ((project-heading (org-gtd-task-management--find-project-heading)))
        (assert-true project-heading)
        (assert-match "Multi-file DAG Project" project-heading)))))

;;; Cross-Project Dependencies (Story 5)

(deftest task-mgmt-int/cross-project-shows-project-labels ()
  "Shows tasks from different projects with project labels when selecting blockers."
  ;; Set up Project A in main GTD tasks file
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task A1")
  (organize-as-project)

  ;; Create a second GTD file with Project B
  (let ((second-file (org-gtd--path "secondary-project.org")))
    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n")
      (save-buffer)
      ;; Register ID
      (goto-char (point-min))
      (search-forward "Task B1")
      (org-back-to-heading t)
      (org-id-add-location "task-b1-id" second-file))

    ;; Add second file to org-agenda-files for this test
    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
      ;; Capture what task selection interface shows
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A1")
        (org-back-to-heading t)

        ;; Mock completing-read to capture the options presented
        (let ((presented-options nil)
              (call-count 0))
          (with-fake completing-read
              (lambda (_prompt collection &rest _args)
                (when (= call-count 0)
                  (setq presented-options collection))
                (setq call-count (1+ call-count))
                (if (= call-count 1) "task-b1-id" ""))
            (org-gtd-task-management--select-multiple-task-ids "Test prompt: ")

            ;; Verify that tasks are labeled with project information
            (let ((has-project-labels (cl-some (lambda (option)
                                                 (string-match-p "Project B" (car option)))
                                               presented-options)))
              (assert-true has-project-labels))))))))

(deftest task-mgmt-int/cross-project-creates-dependencies ()
  "Creates dependencies between tasks in different projects."
  ;; Set up Project A in main GTD tasks file
  (capture-inbox-item "Project A")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task A1")
  (organize-as-project)

  ;; Create a second GTD file with Project B
  (let ((second-file (org-gtd--path "secondary-project.org")))
    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n")
      (save-buffer)
      ;; Register ID
      (goto-char (point-min))
      (search-forward "Task B1")
      (org-back-to-heading t)
      (org-id-add-location "task-b1-id" second-file))

    ;; Add second file to org-agenda-files for this test
    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
      ;; Create cross-project dependency: Task A1 depends on Task B1
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A1")
        (org-back-to-heading t)

        ;; Mock task selection to return Task B1 from different project
        (with-spy org-gtd-task-management--select-multiple-task-ids calls '("task-b1-id")
          (org-gtd-task-add-blockers)

          ;; Verify Task A1 has DEPENDS_ON pointing to Task B1
          (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
            (assert-equal '("task-b1-id") depends-on))))

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
          (assert-equal (list task-a1-id) blocks))))))

(provide 'task-management-commands-integration-test)

;;; task-management-commands-test.el ends here
