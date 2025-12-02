;;; project-cancel-test.el --- Integration tests for project cancellation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for project cancellation functionality including
;; canceling from agenda, from heading, error handling for non-project tasks,
;; and multi-file DAG project cancellation.
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
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Project Cancellation

(deftest cancel/from-agenda-task ()
  "Cancels project from a task in the agenda."
  (create-project "project headline")
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-gtd-project-cancel-from-agenda)
    (org-gtd-archive-completed-items))

  (let ((archived-projects (archive-string)))
    (assert-match "project headline" archived-projects)))

(deftest cancel/from-project-heading ()
  "Cancels project when on the heading."
  (create-project "project tailline")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "project tailline")
    (org-gtd-project-cancel)
    (org-gtd-archive-completed-items)
    (basic-save-buffer))

  (let ((archived-projects (archive-string)))
    (assert-match "project tailline" archived-projects)))

(deftest cancel/errors-on-single-action ()
  "Errors when called on a single action (not a project task)."
  ;; Bug fix: calling project-cancel on a single action should error,
  ;; not try to cancel the parent Actions heading
  (create-single-action "not a project task")
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "not a project task")
    ;; Should error because this is not a project task
    (let ((error-thrown nil))
      (condition-case err
          (org-gtd-project-cancel-from-agenda)
        (user-error (setq error-thrown t)))
      (assert-true error-thrown))))

(deftest cancel/multi-file-project-from-task-in-other-file ()
  "Cancels project with task in different file (not outline child).
Multi-file DAG: project task doesn't have to be outline child of project.
Must find project via ORG_GTD_PROJECT_IDS, not via org-up-heading-safe."
  (capture-inbox-item "Multi-file project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Task in main file" :level 2)
    (organize-as-project))

  ;; Create task in different file
  (let ((second-file (org-gtd--path "other-file")))
    (with-temp-file second-file
      (make-task "Task in other file" :id "other-file-task" :level 1 :status 'next))

    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (goto-char (point-min))
      (search-forward "Task in other file")
      (org-back-to-heading t)
      (org-id-add-location "other-file-task" second-file))

    ;; Link task from second file to project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi-file project")
      (org-back-to-heading t)
      (let ((project-id (org-id-get-create)))
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "other-file-task")
        ;; Add project ID to task in other file
        (with-current-buffer (find-file-noselect second-file)
          (goto-char (point-min))
          (search-forward "Task in other file")
          (org-back-to-heading t)
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
          (basic-save-buffer))))

    ;; Cancel project from agenda via task in OTHER file
    (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Task in other file")
        ;; This should cancel the whole project, even though task is not outline child
        (org-gtd-project-cancel-from-agenda))

      ;; Verify project was canceled by checking that tasks are CNCL
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task in main file")
        (org-back-to-heading t)
        (assert-equal "CNCL" (org-entry-get (point) "TODO")))

      ;; Also verify task in OTHER file was canceled
      (with-current-buffer (find-file-noselect second-file)
        (goto-char (point-min))
        (search-forward "Task in other file")
        (org-back-to-heading t)
        (assert-equal "CNCL" (org-entry-get (point) "TODO"))))))

(provide 'project-cancel-test)

;;; project-cancel-test.el ends here
