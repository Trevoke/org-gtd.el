;;; basic-workflows-test.el --- Acceptance tests for core GTD workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for the core GTD workflows: capture, process, organize,
;; engage, and archive. These tests verify the complete user journey through
;; the GTD methodology.
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Single Action Workflow

(deftest single-action-workflow ()
  "Captures, processes, organizes, shows in agenda, and archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Buy groceries")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (single action)
  (organize-as-single-action)

  ;; 4. VERIFY in agenda
  (org-gtd-engage)
  (assert-match "Buy groceries" (agenda-raw-text))

  ;; 5. COMPLETE and verify archival
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Buy groceries")
    (org-todo "DONE"))

  ;; 6. ARCHIVE
  (org-gtd-archive-completed-items)

  ;; Verify item is archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Buy groceries" (current-buffer-raw-text))))

;;; Project Workflow

(deftest project-workflow ()
  "Captures, processes, organizes project with tasks, shows in agenda, and archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Plan vacation")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (project with tasks)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Research destinations" :level 2)
    (make-task "Book flights" :level 2)
    (make-task "Reserve hotel" :level 2)
    (organize-as-project))

  ;; 4. VERIFY in agenda
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Plan vacati" agenda-content)  ; Truncated in agenda display
    (assert-match "Research destinations" agenda-content))

  ;; 5. COMPLETE project and verify archival
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research destinations")
    (org-todo "DONE")
    (re-search-forward "Book flights")
    (org-todo "DONE")
    (re-search-forward "Reserve hotel")
    (org-todo "DONE"))

  ;; 6. ARCHIVE
  (org-gtd-archive-completed-items)

  ;; Verify project is archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Plan vacation" (current-buffer-raw-text))))

;;; Calendar Workflow

(deftest calendar-workflow ()
  "Captures, processes, organizes as calendar item, shows in agenda, and archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Doctor appointment")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (calendar item)
  (schedule-item (calendar-current-date))

  ;; 4. VERIFY in agenda
  (org-gtd-engage)
  (assert-match "Doctor appointment" (agenda-raw-text))

  ;; 5. COMPLETE and archive
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Doctor appointment")
    (org-todo "DONE"))

  (org-gtd-archive-completed-items)

  ;; Verify archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Doctor appointment" (current-buffer-raw-text))))

;;; Delegated Workflow

(deftest delegated-workflow ()
  "Captures, processes, organizes as delegated item, shows in agenda, and archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Get report from John")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (delegate)
  (delegate-item "John" (calendar-current-date))

  ;; 4. VERIFY in agenda
  (org-gtd-engage)
  (assert-match "Get report from John" (agenda-raw-text))

  ;; 5. COMPLETE and archive
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Get report from John")
    (org-todo "DONE"))

  (org-gtd-archive-completed-items)

  ;; Verify archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Get report from John" (current-buffer-raw-text))))

;;; Tickler Workflow

(deftest tickler-workflow ()
  "Captures, processes, organizes, and handles someday/maybe items."
  ;; 1. CAPTURE
  (capture-inbox-item "Learn Spanish")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (tickler)
  (defer-item (calendar-current-date))

  ;; 4. VERIFY stored in main GTD file under Tickler
  (with-current-buffer (org-gtd--default-file)
    (assert-match "Learn Spanish" (current-buffer-raw-text)))

  ;; Tickler items DO show in agenda with their scheduled date
  (org-gtd-engage)
  (assert-match "Learn Spanish" (agenda-raw-text)))

;;; Knowledge Workflow

(deftest knowledge-workflow ()
  "Captures, processes, organizes, and stores reference material."
  ;; 1. CAPTURE
  (capture-inbox-item "Git commands reference")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (knowledge) - ensure we're in WIP buffer
  (with-wip-buffer
    (archive-as-reference))

  ;; 4. VERIFY that knowledge items are archived immediately
  ;; (they don't stay in the main GTD file)
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Git commands reference" (current-buffer-raw-text)))

  ;; Knowledge items don't show in daily agenda (they're archived)
  (org-gtd-engage)
  (refute-match "Git commands reference" (agenda-raw-text)))

;;; Multiple Item Processing

(deftest multiple-item-processing ()
  "Processes multiple different item types in sequence."
  ;; Capture multiple items
  (capture-inbox-item "Call mom")           ; single action
  (capture-inbox-item "Team meeting")       ; calendar
  (capture-inbox-item "Organize garage")    ; project
  (capture-inbox-item "Wine recipe")        ; knowledge

  ;; Process them all
  (org-gtd-process-inbox)

  ;; Organize each appropriately
  (organize-as-single-action)                 ; Call mom
  (schedule-item (calendar-current-date))     ; Team meeting

  ;; Project with subtasks
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Sort items" :level 2)
    (make-task "Donate old items" :level 2)
    (make-task "Clean floor" :level 2)
    (organize-as-project))

  (archive-as-reference)                      ; Wine recipe

  ;; Verify all items are properly organized
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Call mom" agenda-content)
    (assert-match "Team meeting" agenda-content)
    (assert-match "Organize ga" agenda-content)  ; Truncated in agenda display
    (assert-match "Sort items" agenda-content)
    ;; Knowledge item should not appear in agenda (archived immediately)
    (refute-match "Wine recipe" agenda-content))

  ;; Verify inbox is empty after processing
  (with-current-buffer (ogt-inbox-buffer)
    (refute-match "Call mom" (current-buffer-raw-text))
    (refute-match "Team meeting" (current-buffer-raw-text))
    (refute-match "Organize garage" (current-buffer-raw-text))
    (refute-match "Wine recipe" (current-buffer-raw-text))))

;;; Complete GTD Workflow Integration

(deftest complete-gtd-workflow-integration ()
  "Tests capture -> process -> organize -> engage -> archive cycle."
  ;; Start with empty system
  (refute-match "Review budget" (with-current-buffer (ogt-inbox-buffer)
                                      (current-buffer-raw-text)))
  (refute-match "Birthday party" (with-current-buffer (ogt-inbox-buffer)
                                       (current-buffer-raw-text)))
  (refute-match "Emacs manual" (with-current-buffer (ogt-inbox-buffer)
                                     (current-buffer-raw-text)))

  ;; Capture multiple items throughout "day"
  (capture-inbox-item "Review budget")
  (capture-inbox-item "Birthday party planning")
  (capture-inbox-item "Read Emacs manual chapter")

  ;; Process inbox
  (org-gtd-process-inbox)

  ;; Organize items
  (organize-as-single-action)  ; Review budget

  ;; Project with multiple tasks
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Choose venue" :level 2)
    (make-task "Send invitations" :level 2)
    (make-task "Order cake" :level 2)
    (organize-as-project))

  (archive-as-reference)  ; Emacs manual

  ;; Engage with agenda
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Review budget" agenda-content)
    (assert-match "Birthday pa" agenda-content)  ; Truncated in agenda display
    (assert-match "Choose venue" agenda-content))

  ;; Complete some work
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Review budget")
    (org-todo "DONE")
    (re-search-forward "Choose venue")
    (org-todo "DONE"))

  ;; Archive completed items
  (org-gtd-archive-completed-items)

  ;; Verify system state
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      ;; Completed single actions should be archived
      (refute-match "Review budget" content)
      ;; Incomplete items should remain
      (assert-match "Birthday party planning" content)
      (assert-match "Send invitations" content)
      (assert-match "Order cake" content)
      ;; Knowledge items are archived immediately
      (refute-match "Read Emacs manual chapter" content)))

  ;; Inbox should be empty
  (with-current-buffer (ogt-inbox-buffer)
    (refute-match "Review budget" (current-buffer-raw-text))
    (refute-match "Birthday party" (current-buffer-raw-text))
    (refute-match "Emacs manual" (current-buffer-raw-text))))

;;; basic-workflows-test.el ends here
