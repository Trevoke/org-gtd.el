;;; clarify-test.el --- Tests for org-gtd clarify flow -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd clarify flow.
;;
;; Test Coverage:
;; - Source heading marker tracking (1 test)
;; - Skip-refile flag with prefix arg (2 tests)
;; - Organize help buffer content and behavior (3 tests)
;; - Clarify through agenda view (1 test)
;;
;; Migrated from test/clarify-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Source Heading Tracking Tests

(deftest clarify/stores-source-heading-marker ()
  "Stores a marker to the original heading as local variable in WIP buffer."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((task-id (with-current-buffer source-buffer (org-id-get)))
          (wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (assert-equal task-id
                      (org-entry-get org-gtd-clarify--source-heading-marker "ID"))))))

;;; Skip-Refile Flag Tests

(deftest clarify/sets-skip-refile-flag-with-prefix-arg ()
  "Sets skip-refile flag when called with prefix arg."
  (create-single-action "Test item")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test item")
    (org-back-to-heading t)
    (let ((current-prefix-arg '(4)))
      (org-gtd-clarify-item))
    ;; Find the WIP buffer and check the flag
    (assert-true (ogt-get-wip-buffer))
    (with-wip-buffer
      (assert-true org-gtd-clarify--skip-refile))))

;;; Organize Help Buffer Tests

(deftest clarify/help-content-has-all-gtd-types ()
  "Has a content constant with all GTD organize types."
  (assert-true (boundp 'org-gtd-clarify-organize-help-content))
  (assert-match "Quick Action" org-gtd-clarify-organize-help-content)
  (assert-match "Single Action" org-gtd-clarify-organize-help-content)
  (assert-match "Project" org-gtd-clarify-organize-help-content)
  (assert-match "Calendar" org-gtd-clarify-organize-help-content)
  (assert-match "Delegate" org-gtd-clarify-organize-help-content)
  (assert-match "Habit" org-gtd-clarify-organize-help-content)
  (assert-match "Tickler" org-gtd-clarify-organize-help-content)
  (assert-match "Someday" org-gtd-clarify-organize-help-content)
  (assert-match "Knowledge" org-gtd-clarify-organize-help-content)
  (assert-match "Trash" org-gtd-clarify-organize-help-content))

(deftest clarify/help-buffer-org-mode-read-only ()
  "Creates a buffer with help content in org-mode and read-only."
  (let ((buffer (org-gtd-clarify--get-or-create-organize-help-buffer)))
    (assert-true buffer)
    (assert-equal "*Org GTD Organize Help*" (buffer-name buffer))
    (with-current-buffer buffer
      (assert-match "Quick Action" (buffer-string))
      (assert-equal 'org-mode major-mode)
      (assert-true buffer-read-only))
    (kill-buffer buffer)))

(deftest clarify/toggle-organize-help-window ()
  "Toggles the organize help window on and off."
  (let ((org-gtd-clarify-show-organize-help 'right))
    ;; Initially no window
    (assert-nil (get-buffer-window "*Org GTD Organize Help*"))
    ;; Toggle on
    (org-gtd-clarify-toggle-organize-help)
    (assert-true (get-buffer-window "*Org GTD Organize Help*"))
    ;; Toggle off
    (org-gtd-clarify-toggle-organize-help)
    (assert-nil (get-buffer-window "*Org GTD Organize Help*"))
    ;; Cleanup
    (when-let ((buf (get-buffer "*Org GTD Organize Help*")))
      (kill-buffer buf))))

;;; Duplicate Queue Customization Tests

(deftest clarify/duplicate-queue-position-customizable ()
  "Has a customizable variable for queue window position."
  (assert-true (boundp 'org-gtd-clarify-duplicate-queue-position))
  (assert-equal 'bottom (default-value 'org-gtd-clarify-duplicate-queue-position)))

;;; Duplicate Queue Variable Tests

(deftest clarify/duplicate-queue-variable-exists ()
  "Has a buffer-local variable for the duplicate queue."
  (assert-true (boundp 'org-gtd-clarify--duplicate-queue))
  ;; Verify it's buffer-local by default
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '("test"))
    (assert-equal '("test") org-gtd-clarify--duplicate-queue))
  ;; Different buffer should have nil
  (with-temp-buffer
    (assert-nil org-gtd-clarify--duplicate-queue)))

;;; Clarify Through Agenda Tests

(deftest clarify/agenda-converts-tickler-to-project ()
  "Converts tickler item to project with tasks via clarify-agenda-item."
  (create-deferred-item "projectify-me" (calendar-current-date))
  (org-gtd-engage)
  (set-buffer org-agenda-buffer)
  (goto-char (point-min))
  (search-forward "projectify")
  (org-gtd-clarify-agenda-item)
  (execute-kbd-macro (kbd "M-> RET"))
  ;; Create three simple tasks using builder
  (make-task "Task 1" :level 2)
  (make-task "Task 2" :level 2)
  (make-task "Task 3" :level 2)
  (organize-as-project)
  (kill-buffer org-agenda-buffer)
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (assert-match "Task 1" (current-buffer-raw-text)))
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward ":ORG_GTD_REFILE: Tickler")
    (org-narrow-to-subtree)
    (refute-match "projectify" (current-buffer-raw-text))
    (widen)
    (search-forward ":ORG_GTD_REFILE: Projects")
    (org-narrow-to-subtree)
    (assert-match "projectify" (current-buffer-raw-text))
    (widen)))

(deftest clarify/agenda-sets-skip-refile-with-prefix-arg ()
  "Sets skip-refile flag when called with prefix arg through agenda."
  (create-single-action "Agenda test item")
  (ogt--save-all-buffers)
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "Agenda test item")
    ;; Simulate interactive call: in real usage, current-prefix-arg
    ;; is set by the command loop before the function is called
    (let ((current-prefix-arg '(4)))
      (org-gtd-clarify-agenda-item))
    ;; Find the WIP buffer and check the flag
    (assert-true (ogt-get-wip-buffer))
    (with-wip-buffer
      (assert-true org-gtd-clarify--skip-refile))))

(deftest clarify/agenda-converts-single-action-to-calendar ()
  "Converts single action to calendar item via clarify-agenda-item."
  (create-single-action "schedule-me")
  (ogt--save-all-buffers)
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "schedule-me")
    (org-gtd-clarify-agenda-item))
  ;; Reorganize as calendar
  (with-wip-buffer
    (with-simulated-input "2025-06-20 RET"
      (org-gtd-calendar)))
  ;; Verify the item is now a calendar item
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "schedule-me")
    (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
    (assert-match "2025-06-20" (org-entry-get (point) org-gtd-timestamp))))

(deftest clarify/agenda-converts-single-action-to-habit ()
  "Converts single action to habit via clarify-agenda-item."
  (create-single-action "habitize-me")
  (ogt--save-all-buffers)
  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "habitize-me")
    (org-gtd-clarify-agenda-item))
  ;; Reorganize as habit
  (with-wip-buffer
    (with-simulated-input "2025-01-01 RET .+1d RET"
      (org-gtd-habit)))
  ;; Verify the item is now a habit
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "habitize-me")
    (assert-equal "Habit" (org-entry-get (point) "ORG_GTD"))
    (assert-true (org-get-scheduled-time (point)))))

(provide 'clarify-test)

;;; clarify-test.el ends here
