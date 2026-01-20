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

;;; Duplicate Queue Helper Tests

(deftest clarify/queue-empty-p-returns-true-when-empty ()
  "Returns t when queue is empty or nil."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-true (org-gtd-clarify--queue-empty-p))))

(deftest clarify/queue-empty-p-returns-nil-when-has-items ()
  "Returns nil when queue has items."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (assert-nil (org-gtd-clarify--queue-empty-p))))

(deftest clarify/queue-add-appends-to-queue ()
  "Adds item to end of queue."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (org-gtd-clarify--queue-add "First" "* First")
    (org-gtd-clarify--queue-add "Second" "* Second")
    (assert-equal 2 (length org-gtd-clarify--duplicate-queue))
    (assert-equal "First" (plist-get (car org-gtd-clarify--duplicate-queue) :title))))

(deftest clarify/queue-pop-returns-and-removes-first-item ()
  "Pops first item from queue (FIFO)."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue
          '((:title "First" :content "* First")
            (:title "Second" :content "* Second")))
    (let ((item (org-gtd-clarify--queue-pop)))
      (assert-equal "First" (plist-get item :title))
      (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
      (assert-equal "Second" (plist-get (car org-gtd-clarify--duplicate-queue) :title)))))

(deftest clarify/queue-pop-returns-nil-when-empty ()
  "Returns nil when popping from empty queue."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-nil (org-gtd-clarify--queue-pop))))

;;; Queue Display Tests

(deftest clarify/queue-display-creates-buffer ()
  "Creates queue buffer with correct content."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue
          '((:title "Task A" :content "* Task A")
            (:title "Task B" :content "* Task B")))
    (org-gtd-clarify--queue-display)
    (let ((queue-buf (get-buffer "*Org GTD Duplicate Queue*")))
      (assert-true queue-buf)
      (with-current-buffer queue-buf
        (assert-match "Pending (2)" (buffer-string))
        (assert-match "1\\. Task A" (buffer-string))
        (assert-match "2\\. Task B" (buffer-string))
        (assert-true buffer-read-only))
      (kill-buffer queue-buf))))

(deftest clarify/queue-cleanup-kills-buffer ()
  "Cleanup kills the queue buffer."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (org-gtd-clarify--queue-display)
    (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
    (org-gtd-clarify--queue-cleanup)
    (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))))

;;; Content Extraction Tests

(deftest clarify/get-wip-content-extracts-heading ()
  "Extracts title and content from WIP buffer."
  (capture-inbox-item "Test heading")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (let ((result (org-gtd-clarify--get-wip-content)))
      (assert-match "Test heading" (plist-get result :title))
      (assert-match "\\* .*Test heading" (plist-get result :content)))))

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

;;; Keybinding Tests

(deftest clarify/keymap-has-duplicate-bindings ()
  "Clarify mode map has d and D for duplicate commands."
  (assert-equal 'org-gtd-clarify-duplicate
                (lookup-key org-gtd-clarify-mode-map (kbd "d")))
  (assert-equal 'org-gtd-clarify-duplicate-exact
                (lookup-key org-gtd-clarify-mode-map (kbd "D"))))

;;; Duplicate Command Tests

(deftest clarify/duplicate-exact-adds-to-queue ()
  "Exact duplicate adds current content to queue."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
    (assert-equal "Original item"
                  (plist-get (car org-gtd-clarify--duplicate-queue) :title)))
  ;; Cleanup
  (org-gtd-clarify--queue-cleanup))

(deftest clarify/duplicate-exact-shows-queue-window ()
  "Exact duplicate displays queue window."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (assert-true (get-buffer "*Org GTD Duplicate Queue*")))
  ;; Cleanup
  (org-gtd-clarify--queue-cleanup))

(deftest clarify/duplicate-with-rename-uses-new-title ()
  "Duplicate with rename uses provided title."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    ;; C-a moves to beginning of line, C-k kills to end of line, then type new title
    (with-simulated-input "C-a C-k New SPC title RET"
      (org-gtd-clarify-duplicate))
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
    (assert-equal "New title"
                  (plist-get (car org-gtd-clarify--duplicate-queue) :title)))
  ;; Cleanup
  (org-gtd-clarify--queue-cleanup))

(deftest clarify/duplicate-fails-on-empty-buffer ()
  "Duplicate fails when buffer has no content."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (assert-raises 'user-error
      (org-gtd-clarify-duplicate-exact))))

(provide 'clarify-test)

;;; clarify-test.el ends here
