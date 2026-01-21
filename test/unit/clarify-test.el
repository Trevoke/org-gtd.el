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

;;; Queue Processing Tests

(deftest clarify/organize-processes-queue-before-continuation ()
  "After organizing, processes queued duplicates before calling continuation."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  ;; Add a duplicate while clarifying
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  ;; Verify we have one duplicate in queue
  (with-wip-buffer
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue)))
  ;; Organize the original as single action
  (with-wip-buffer
    (organize-as-single-action))
  ;; Should now be clarifying the duplicate - look for a WIP buffer
  (assert-true (ogt-get-wip-buffer))
  ;; Organize the duplicate
  (with-wip-buffer
    (organize-as-single-action))
  ;; Queue should be empty, no more WIP buffers from this session
  (assert-nil (ogt-get-wip-buffer)))

;;; Cancel with Queue Tests

(deftest clarify/stop-with-queue-prompts-discard ()
  "Stopping with pending duplicates - discard clears queue."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (org-gtd-clarify-duplicate-exact))
  ;; Simulate choosing discard
  (with-wip-buffer
    (with-simulated-input "d"
      (org-gtd-clarify-stop)))
  ;; Queue should be cleared, no queue window
  (assert-nil (get-buffer "*Org GTD Duplicate Queue*")))

(deftest clarify/stop-save-to-inbox-preserves-duplicates ()
  "Choosing save-to-inbox preserves duplicates in inbox."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  ;; Simulate choosing save
  (with-wip-buffer
    (with-simulated-input "s"
      (org-gtd-clarify-stop)))
  ;; Check inbox has the duplicate (need to refresh buffer)
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (revert-buffer t t)
    (assert-match "Original item" (buffer-string))))

;;; Kill-Emacs Query Tests

(deftest clarify/pending-duplicates-all-buffers-finds-duplicates ()
  "Finds pending duplicates across all clarify buffers."
  (capture-inbox-item "Item one")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  ;; Should find the duplicate
  (let ((all-duplicates (org-gtd-clarify--pending-duplicates-all-buffers)))
    (assert-equal 1 (length all-duplicates))
    (assert-equal "Item one" (plist-get (car all-duplicates) :title)))
  ;; Cleanup
  (with-wip-buffer
    (org-gtd-clarify--queue-cleanup)))

(deftest clarify/kill-emacs-query-registered ()
  "The kill-emacs query function is registered."
  (assert-true (memq 'org-gtd-clarify--kill-emacs-query kill-emacs-query-functions)))

;;; Other Clarify Buffers Exist Tests

(deftest clarify/other-buffers-exist-returns-nil-when-alone ()
  "Returns nil when current buffer is the only clarify buffer."
  (capture-inbox-item "Test item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (assert-nil (org-gtd-clarify--other-clarify-buffers-exist-p))))

(deftest clarify/other-buffers-exist-returns-t-when-multiple ()
  "Returns t when other clarify buffers exist."
  ;; Create first clarify buffer
  (capture-inbox-item "Item one")
  (org-gtd-process-inbox)
  (let ((first-buf (ogt-get-wip-buffer)))
    ;; Create second clarify buffer manually
    (with-temp-buffer
      (org-gtd-clarify-mode)
      ;; Check from first buffer's perspective
      (with-current-buffer first-buf
        (assert-true (org-gtd-clarify--other-clarify-buffers-exist-p))))))

;;; Kill Buffer Query Tests

(deftest clarify/kill-buffer-query-allows-when-queue-empty ()
  "Query function returns t when queue is empty (test A1)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-true (org-gtd-clarify--kill-buffer-query))))

(deftest clarify/kill-buffer-query-prompts-discard ()
  "Query function prompts and returns t on discard (test A2)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (with-simulated-input "d"
      (assert-true (org-gtd-clarify--kill-buffer-query)))
    ;; Queue unchanged (will be cleared by actual kill)
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue))))

(deftest clarify/kill-buffer-query-prompts-save ()
  "Query function saves to inbox and returns t on save (test A3)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Saved Item" :content "* Saved Item")))
    (with-simulated-input "s"
      (assert-true (org-gtd-clarify--kill-buffer-query)))
    ;; Check inbox has the saved item
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (revert-buffer t t)
      (assert-match "Saved Item" (buffer-string)))))

(deftest clarify/kill-buffer-query-prompts-cancel ()
  "Query function returns nil on cancel, aborting kill (test A4)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (with-simulated-input "c"
      (assert-nil (org-gtd-clarify--kill-buffer-query)))))

;;; Kill Side Window Helper Tests

(deftest clarify/kill-side-window-removes-buffer ()
  "Kills buffer and closes window."
  (let ((test-buf (get-buffer-create "*Test Side Window*")))
    (display-buffer-in-side-window test-buf '((side . right)))
    (assert-true (get-buffer-window test-buf))
    (org-gtd-clarify--kill-side-window "*Test Side Window*")
    (assert-nil (get-buffer "*Test Side Window*"))))

(deftest clarify/kill-side-window-handles-nonexistent ()
  "Does not error when buffer doesn't exist."
  (assert-nil (get-buffer "*Nonexistent Buffer*"))
  ;; Should not error
  (org-gtd-clarify--kill-side-window "*Nonexistent Buffer*"))

;;; Kill Buffer Cleanup Tests

(deftest clarify/kill-buffer-cleanup-kills-queue-window ()
  "Cleanup kills queue window when last clarify buffer (test B1)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (org-gtd-clarify--queue-display)
    (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
    ;; Simulate being the last buffer
    (org-gtd-clarify--kill-buffer-cleanup)
    (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))))

(deftest clarify/kill-buffer-cleanup-no-error-when-empty ()
  "Cleanup handles case with no side windows (test B6)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    ;; Should not error
    (org-gtd-clarify--kill-buffer-cleanup)))

(deftest clarify/kill-buffer-cleanup-preserves-windows-when-other-buffers ()
  "Cleanup preserves side windows when other clarify buffers exist (test C1)."
  (let ((other-buf (get-buffer-create "*Other Clarify*")))
    (with-current-buffer other-buf
      (org-gtd-clarify-mode))
    (unwind-protect
        (with-temp-buffer
          (org-gtd-clarify-mode)
          ;; Create queue window
          (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
          (org-gtd-clarify--queue-display)
          (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
          ;; Cleanup - other buffer exists, so windows should remain
          (org-gtd-clarify--kill-buffer-cleanup)
          (assert-true (get-buffer "*Org GTD Duplicate Queue*")))
      ;; Cleanup test buffer
      (kill-buffer other-buf)
      (when-let ((buf (get-buffer "*Org GTD Duplicate Queue*")))
        (kill-buffer buf)))))

;;; Hook Registration Tests

(deftest clarify/mode-registers-kill-buffer-hooks ()
  "Clarify mode registers buffer-local kill hooks."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (assert-true (memq 'org-gtd-clarify--kill-buffer-query
                       (buffer-local-value 'kill-buffer-query-functions (current-buffer))))
    (assert-true (memq 'org-gtd-clarify--kill-buffer-cleanup
                       (buffer-local-value 'kill-buffer-hook (current-buffer))))))

;;; Integration Tests

(deftest clarify/duplicate-full-workflow ()
  "Test complete duplicate workflow: create, process, verify."
  ;; Start with inbox item
  (capture-inbox-item "Meeting prep")
  (org-gtd-process-inbox)

  ;; Create two duplicates with exact names (rename happens at queue add time)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (org-gtd-clarify-duplicate-exact))

  ;; Verify queue has 2 items
  (with-wip-buffer
    (assert-equal 2 (length org-gtd-clarify--duplicate-queue)))

  ;; Organize original as calendar item
  (with-wip-buffer
    (with-simulated-input "2026-02-01 RET"
      (org-gtd-calendar)))

  ;; Now clarifying first duplicate - look for WIP buffer
  (assert-true (ogt-get-wip-buffer))

  ;; Organize first duplicate as single action
  (with-wip-buffer
    (organize-as-single-action))

  ;; Now clarifying second duplicate
  (assert-true (ogt-get-wip-buffer))

  ;; Organize second duplicate as single action
  (with-wip-buffer
    (organize-as-single-action))

  ;; Should be done - no more WIP buffers
  (assert-nil (ogt-get-wip-buffer))

  ;; Verify items exist in GTD system - all should be "Meeting prep"
  ;; (the original plus two exact duplicates)
  (with-current-buffer (org-gtd--default-file)
    (revert-buffer t t)
    (let ((content (buffer-string)))
      ;; Should have Calendar and Actions sections with "Meeting prep"
      (assert-match "Meeting prep" content)
      (assert-match "Calendar" content)
      (assert-match "Actions" content))))

(provide 'clarify-test)

;;; clarify-test.el ends here
