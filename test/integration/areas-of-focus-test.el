;;; areas-of-focus-test.el --- Tests for areas of focus functionality -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd areas of focus functionality.
;; Areas of focus are GTD "horizons" that help categorize tasks.
;;
;; Migrated from test/areas-of-focus.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'with-simulated-input)

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context with areas of focus configured."
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-areas-of-focus '("Health" "Home" "Career"))
    (unwind-protect
        (funcall proceed context)
      (setq org-gtd-areas-of-focus nil))))

;;; CATEGORY Tests - During Organization

(deftest areas-of-focus/set-during-clarification ()
  "Sets CATEGORY on clarified item from a customizable list."
  (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
  (unwind-protect
      (progn
        (capture-inbox-item "Medical Appointment")
        (org-gtd-process-inbox)
        ;; Use direct function call with simulated input instead of execute-kbd-macro
        ;; which doesn't work reliably with transient menus in CI batch mode
        (with-wip-buffer
          (goto-char (point-min))
          (when (org-before-first-heading-p)
            (org-next-visible-heading 1))
          (with-simulated-input "Health RET"
            (org-gtd-single-action)))
        (org-gtd-engage)
        (assert-match "Health.*Medical"
                      (ogt--buffer-string org-agenda-buffer)))
    (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)))

(deftest areas-of-focus/set-on-item-at-point ()
  "Sets CATEGORY on item at point from the areas of focus decoration."
  (with-current-buffer (get-buffer-create "temp.org")
    (org-mode)
    (insert "* A heading")
    (with-simulated-input "Health RET"
      (org-gtd-area-of-focus-set-on-item-at-point))
    (assert-equal "Health" (org-entry-get (point) "CATEGORY"))
    (kill-buffer)))

;;; CATEGORY Tests - Through Agenda

(deftest areas-of-focus/agenda-sets-on-single-action ()
  "Sets area of focus on single action task from agenda view."
  (create-single-action "foobar")
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "foobar")
    (with-simulated-input "Home RET"
      (org-gtd-area-of-focus-set-on-agenda-item)))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "foobar")
    (assert-equal "Home" (org-entry-get (point) "CATEGORY"))))

(deftest areas-of-focus/agenda-does-not-set-on-parent ()
  "Does NOT set category on Actions parent when setting on single action.
This test verifies the bug fix: single actions should get CATEGORY set
on themselves, NOT on the parent * Actions heading."
  (create-single-action "my single action")
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "my single action")
    (with-simulated-input "Career RET"
      (org-gtd-area-of-focus-set-on-agenda-item)))
  (with-current-buffer (org-gtd--default-file)
    ;; Verify CATEGORY is on the single action
    (goto-char (point-min))
    (search-forward "my single action")
    (assert-equal "Career" (org-entry-get (point) "CATEGORY" nil))
    ;; Verify CATEGORY on parent is NOT "Career"
    (org-up-heading-safe)
    (assert-equal "Actions" (org-get-heading t t t t))
    (let ((parent-category (org-entry-get (point) "CATEGORY" nil)))
      (assert-not-equal "Career" parent-category))))

(deftest areas-of-focus/agenda-sets-on-all-project-tasks ()
  "Sets category on ALL tasks in project when setting on project task.
Changed behavior: instead of setting on project heading,
set CATEGORY on all tasks in the project DAG."
  (create-project "my project")
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "Task 1")
    (with-simulated-input "Home RET"
      (org-gtd-area-of-focus-set-on-agenda-item)))
  (with-current-buffer (org-gtd--default-file)
    ;; Verify CATEGORY is set on all three tasks
    (goto-char (point-min))
    (search-forward "Task 1")
    (assert-equal "Home" (org-entry-get (point) "CATEGORY" nil))
    (goto-char (point-min))
    (search-forward "Task 2")
    (assert-equal "Home" (org-entry-get (point) "CATEGORY" nil))
    (goto-char (point-min))
    (search-forward "Task 3")
    (assert-equal "Home" (org-entry-get (point) "CATEGORY" nil))))

(deftest areas-of-focus/agenda-prompts-for-multi-project-task ()
  "Prompts for project when task belongs to multiple projects."
  ;; Create two projects sharing a task
  (capture-inbox-item "Project Alpha")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Shared task" :level 2)
    (make-task "Alpha unique" :level 2)
    (organize-as-project))

  (capture-inbox-item "Project Beta")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Beta unique" :level 2)
    (organize-as-project))

  ;; Share "Shared task" with Project Beta
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared task")
    (org-back-to-heading t)
    (let ((shared-task-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Project Beta")
      (org-back-to-heading t)
      (let ((beta-id (org-id-get-create)))
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)
        (goto-char (point-min))
        (search-forward "Shared task")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" beta-id))))

  ;; Try to set area of focus on shared task - should prompt for project
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "Shared task")
    (with-simulated-input "Project SPC Alpha RET Career RET"
      (org-gtd-area-of-focus-set-on-agenda-item)))

  ;; Verify CATEGORY is set on Project Alpha's tasks only
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Shared task")
    (assert-equal "Career" (org-entry-get (point) "CATEGORY" nil))
    (goto-char (point-min))
    (search-forward "Alpha unique")
    (assert-equal "Career" (org-entry-get (point) "CATEGORY" nil))
    ;; Beta's task should NOT have "Career" CATEGORY
    (goto-char (point-min))
    (search-forward "Beta unique")
    (let ((beta-category (org-entry-get (point) "CATEGORY" nil)))
      (assert-not-equal "Career" beta-category))))

(deftest areas-of-focus/errors-when-no-org-gtd-property ()
  "Errors when item has no ORG_GTD property.
Tests that `org-gtd-area-of-focus-set-on-agenda-item' rejects items
that lack proper GTD metadata."
  ;; Create a single action, then corrupt it by removing ORG_GTD
  (create-single-action "corrupted action")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "corrupted action")
    (org-back-to-heading t)
    ;; Remove ORG_GTD property to corrupt the item
    (org-entry-delete (point) "ORG_GTD"))

  ;; Use raw todo agenda (not GTD engage view) which shows all TODOs
  (org-todo-list)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "corrupted action")
    (assert-raises 'user-error
      (org-gtd-area-of-focus-set-on-agenda-item))))

(provide 'areas-of-focus-test)

;;; areas-of-focus-test.el ends here
