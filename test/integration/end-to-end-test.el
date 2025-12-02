;;; end-to-end-test.el --- Integration tests for multi-file and tickler workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for multi-file project validation and tickler workflows.
;; Tests stuck project detection, broken reference detection, and tickler cycles.
;;
;; Migrated from test/end-to-end-test.el (buttercup).
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

;;; Multi-file Review and Validation Tests

(deftest e2e/detects-stuck-project-with-tasks-in-multiple-files ()
  "Detects stuck project with tasks in multiple files.
Verifies that stuck project detection works correctly when:
1. Project heading is in main GTD file
2. Tasks are distributed across multiple files
3. All tasks are in TODO state (none in NEXT), making project stuck."
  ;; 1. CAPTURE and ORGANIZE project in main file
  (capture-inbox-item "Multi-file stuck project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task in main file" :level 2)
    (organize-as-project))

  ;; 2. Create second file with task
  (let ((second-file (org-gtd--path "stuck-secondary")))
    (with-temp-file second-file
      (make-task "Task in second file" :id "stuck-task-id" :level 1))

    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (goto-char (point-min))
      (search-forward "Task in second file")
      (org-back-to-heading t)
      (org-id-add-location "stuck-task-id" second-file)
      (org-todo "TODO"))  ; Make it TODO, not NEXT - this makes project stuck

    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; 3. Link task from second file to project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Multi-file stuck project")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "stuck-task-id"))

      ;; 4. Make main file task TODO (not NEXT) so project is stuck
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task in main file")
        (org-todo "TODO"))

      ;; 5. VERIFY project appears in stuck projects review
      (org-gtd-review-stuck-projects)
      (assert-match "Multi-file stuck project" (agenda-raw-text)))))

(deftest e2e/detects-broken-references-in-multi-file-projects ()
  "Detects broken references in multi-file projects.
Verifies that validation detects broken task references when:
1. Project spans multiple files
2. A task has a BLOCKS property pointing to a non-existent ID
3. Validation correctly identifies the broken reference."
  ;; 1. CAPTURE and ORGANIZE project in main file
  (capture-inbox-item "Multi-file validation test")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Main file task" :level 2)
    (organize-as-project))

  ;; 2. Create second file with task that has broken reference
  (let ((second-file (org-gtd--path "validation-secondary")))
    (with-temp-file second-file
      (make-task "Task with broken ref" :id "valid-task-id" :blocks "non-existent-id" :level 1))

    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (goto-char (point-min))
      (search-forward "Task with broken ref")
      (org-back-to-heading t)
      (org-id-add-location "valid-task-id" second-file)
      (org-todo "NEXT"))

    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

      ;; 3. Link task from second file to project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Multi-file validation test")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "valid-task-id"))

      ;; 4. Run validation
      (let* ((health-results (org-gtd-validate-project-dependencies)))

        ;; 5. VERIFY broken reference is detected
        (assert-true (plist-get health-results :broken-references))
        (let ((broken-refs (plist-get health-results :broken-references)))
          (assert-true (cl-some (lambda (ref)
                                  (string= (plist-get ref :missing-task) "non-existent-id"))
                                broken-refs)))))))

;;; Ticklering and Reactivating Projects

(deftest e2e/tickler-reactivation-cycle-preserves-project-state ()
  "Full incubation → reactivation cycle preserves project state."
  ;; Create a project with dependencies
  (create-project "Future project")

  (with-current-buffer (org-gtd--default-file)
    ;; Verify initial state: Task 1 is NEXT
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    ;; Incubate the project
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (org-gtd-tickler "2025-12-01")

    ;; Verify project is tickler
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "<2025-12-01>" (org-entry-get (point) "ORG_GTD_TIMESTAMP"))

    ;; Verify tasks are tickler (no TODO keywords)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "TODO"))
    (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO")))

  ;; Verify it doesn't appear in engage view
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (refute-match "Task 1" (buffer-string)))

  ;; Reactivate the project
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Future project")
    (org-back-to-heading t)
    (org-gtd-reactivate)

    ;; Verify project is reactivated
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP"))

    ;; Verify tasks are reactivated with TODO keywords
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "NEXT" (org-entry-get (point) "TODO")))

  ;; Verify it appears in engage view again
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (assert-match "Task 1" (buffer-string))))

(provide 'end-to-end-integration-test)

;;; end-to-end-test.el ends here
