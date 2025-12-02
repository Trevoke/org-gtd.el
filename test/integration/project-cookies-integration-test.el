;;; project-cookies-integration-test.el --- Integration tests for project progress cookies -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for project progress cookie functionality including
;; counting tasks, updating cookies on state changes, and full lifecycle.
;;
;; Migrated from test/project-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context with cookies enabled.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil)
        (org-gtd-project-progress-cookie-position 'end))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; org-gtd-project--count-tasks

(deftest cookies-int/count-tasks-returns-completed-total ()
  "Returns (completed . total) for project tasks."
  (create-project "Count test")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Count test")
    (org-back-to-heading t)
    (let* ((project-id (org-entry-get (point) "ID"))
           (counts (org-gtd-project--count-tasks project-id)))
      ;; Initially all tasks are TODO/NEXT, none completed
      (assert-equal 0 (car counts))
      (assert-equal 3 (cdr counts)))))

(deftest cookies-int/count-tasks-counts-completed-correctly ()
  "Counts completed tasks correctly."
  (create-project "Done test")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Done test")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))
      ;; Mark first task as DONE
      (search-forward "Task 1")
      (org-todo "DONE")
      (goto-char (point-min))
      (search-forward "Done test")
      (let ((counts (org-gtd-project--count-tasks project-id)))
        (assert-equal 1 (car counts))
        (assert-equal 3 (cdr counts))))))

;;; org-gtd-project-update-cookies

(deftest cookies-int/update-cookies-on-project-heading ()
  "Updates cookies on project heading."
  (create-project "Cookie test")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Cookie test")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))
      (org-gtd-project-update-cookies project-id)
      (assert-match "\\[0/3\\]\\[0%\\]"
                    (buffer-substring-no-properties (point) (line-end-position))))))

;;; org-gtd-project--maybe-update-cookies

(deftest cookies-int/updates-cookies-on-task-state-change ()
  "Updates cookies when task state changes."
  (create-project "Auto cookie")
  (with-current-buffer (org-gtd--default-file)
    ;; Get project ID
    (goto-char (point-min))
    (search-forward "Auto cookie")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))
      ;; Set initial cookies manually
      (org-gtd-project-update-cookies project-id)
      (goto-char (point-min))
      (search-forward "Auto cookie")
      (org-back-to-heading t)
      ;; Verify initial state - 0 tasks done
      (assert-match "\\[0/3\\]\\[0%\\]"
                    (buffer-substring-no-properties (point) (line-end-position)))

      ;; Mark first task DONE - this SHOULD trigger hook and update cookies
      (goto-char (point-min))
      (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
      (org-back-to-heading t)
      (org-todo "DONE")

      ;; With hook, cookies SHOULD update to show 1 task done
      (goto-char (point-min))
      (search-forward "Auto cookie")
      (org-back-to-heading t)
      (assert-match "\\[1/3\\]\\[33%\\]"
                    (buffer-substring-no-properties (point) (line-end-position)))

      ;; Mark another task DONE
      (goto-char (point-min))
      (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
      (org-back-to-heading t)
      (org-todo "DONE")

      ;; Cookies should now show 2 tasks done
      (goto-char (point-min))
      (search-forward "Auto cookie")
      (org-back-to-heading t)
      (assert-match "\\[2/3\\]\\[66%\\]"
                    (buffer-substring-no-properties (point) (line-end-position))))))

;;; org-gtd-project-update-all-cookies

(deftest cookies-int/updates-cookies-for-all-projects ()
  "Updates cookies for all projects."
  (create-project "Project A")
  (create-project "Project B")
  (org-gtd-project-update-all-cookies)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (assert-match "\\[[0-9]+/[0-9]+\\]"
                  (buffer-substring-no-properties (point) (line-end-position)))
    (goto-char (point-min))
    (search-forward "Project B")
    (assert-match "\\[[0-9]+/[0-9]+\\]"
                  (buffer-substring-no-properties (point) (line-end-position)))))

;;; fix todo keywords updates cookies

(deftest cookies-int/updates-cookies-after-fixing-keywords ()
  "Updates cookies after fixing keywords."
  ;; Create project - it should have cookies initially
  (create-project "Fix keywords test")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Fix keywords test")
    (org-back-to-heading t)
    (let ((line-after-creation (buffer-substring-no-properties (point) (line-end-position))))
      ;; Project should have cookies from creation
      (assert-match "\\[[0-9]+/[0-9]+\\]" line-after-creation)))
  ;; Now call fix-all-todo-keywords - cookies should still be there
  (org-gtd-projects-fix-all-todo-keywords)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Fix keywords test")
    (org-back-to-heading t)
    (let ((line-after-fix (buffer-substring-no-properties (point) (line-end-position))))
      ;; Cookies should still be present after fix
      (assert-match "\\[[0-9]+/[0-9]+\\]" line-after-fix))))

;;; project progress cookies integration

(deftest cookies-int/full-lifecycle-create-complete-verify ()
  "Full lifecycle: create, complete tasks, verify cookies."
  ;; Create project
  (create-project "Lifecycle test")

  (with-current-buffer (org-gtd--default-file)
    ;; Count how many tasks the project has
    (goto-char (point-min))
    (search-forward "Lifecycle test")
    (org-back-to-heading t)
    (let* ((project-id (org-entry-get (point) "ID"))
           (initial-counts (org-gtd-project--count-tasks project-id))
           (total-tasks (cdr initial-counts)))
      ;; Verify initial cookies show 0 completed
      (assert-match (format "\\[0/%d\\]\\[0%%\\]" total-tasks)
                    (buffer-substring-no-properties (point) (line-end-position)))

      ;; Find and complete first task
      (goto-char (point-min))
      (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
      (org-back-to-heading t)
      ;; Make sure we're on a task belonging to this project
      (when (member project-id (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
        (org-todo "DONE"))

      ;; Verify cookies updated to show 1 completed
      (goto-char (point-min))
      (search-forward "Lifecycle test")
      (org-back-to-heading t)
      (assert-match (format "\\[1/%d\\]" total-tasks)
                    (buffer-substring-no-properties (point) (line-end-position)))

      ;; Complete all remaining tasks
      (let ((completed 1))
        (while (< completed total-tasks)
          (goto-char (point-min))
          (when (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) " nil t)
            (org-back-to-heading t)
            (when (member project-id (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
              (org-todo "DONE")
              (setq completed (1+ completed))))))

      ;; Verify 100% completion
      (goto-char (point-min))
      (search-forward "Lifecycle test")
      (org-back-to-heading t)
      (assert-match (format "\\[%d/%d\\]\\[100%%\\]" total-tasks total-tasks)
                    (buffer-substring-no-properties (point) (line-end-position))))))

(provide 'project-cookies-integration-test)

;;; project-cookies-integration-test.el ends here
