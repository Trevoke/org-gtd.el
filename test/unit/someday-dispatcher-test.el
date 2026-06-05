;;; someday-dispatcher-test.el --- Tests for org-gtd-someday smart dispatch -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-someday smart dispatcher detecting project headings
;; and project tasks.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Dispatcher Tests

(deftest someday-dispatch/on-project-heading-somedays-whole-project ()
  "Calling org-gtd-someday on a project heading somedays the whole project."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)

    ;; Call org-gtd-someday on project heading
    (org-gtd-someday)

    ;; Verify project heading is someday
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))

    ;; Verify all tasks are someday
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

(deftest someday-dispatch/on-project-task-somedays-whole-project ()
  "Calling org-gtd-someday on a project task somedays the whole project."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    ;; Navigate to a task, not the project heading
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)

    ;; Call org-gtd-someday on a project task
    (org-gtd-someday)

    ;; Verify the entire project is someday
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))

    ;; Verify all tasks are someday
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

(deftest someday-dispatch/on-single-action-somedays-only-that-item ()
  "Calling org-gtd-someday on a single action moves only that item,
even when no Someday refile target exists yet (issue #288).

Regression: standalone re-organize from within the default file used to
raise \"Cannot refile to position inside the tree or region\" because
creating the missing refile target left point at end-of-buffer."
  (create-single-action "First action")
  (create-single-action "Second action")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "First action")
    (org-back-to-heading t)

    ;; Standalone someday on the single action (no Someday target exists yet).
    (org-gtd-someday)

    ;; First action is now someday.
    (goto-char (point-min))
    (search-forward "First action")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))

    ;; Second action is untouched: still a next action.
    (goto-char (point-min))
    (search-forward "Second action")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(provide 'someday-dispatcher-test)

;;; someday-dispatcher-test.el ends here
