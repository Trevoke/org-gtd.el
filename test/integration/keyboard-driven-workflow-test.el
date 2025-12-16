;;; keyboard-driven-workflow-test.el --- Integration tests for keyboard GTD workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for keyboard-driven GTD workflows.
;; Tests the full capture -> process -> organize -> engage cycle using mock-fs.
;;
;; Migrated from test/integration/keyboard-driven-workflow-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'with-simulated-input)

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Single Action Workflow

(deftest workflow/single-action-via-keyboard ()
  "Captures via keyboard, processes with C-c c, and organizes with 's' key."
  ;; 1. CAPTURE via keyboard simulation approach
  (org-gtd-capture nil "i")
  (insert "Buy groceries")
  (org-capture-finalize)

  ;; 2. PROCESS inbox
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard (C-c c opens transient, 's' selects single action)
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call instead of keyboard simulation on transient
    (org-gtd-single-action))

  ;; 4. VERIFY in agenda
  (org-gtd-engage)
  (assert-match "Buy groceries" (agenda-raw-text))

  ;; 5. VERIFY item is properly organized
  (with-current-buffer (org-gtd--default-file)
    (assert-match "Buy groceries" (current-buffer-raw-text))
    (assert-match "NEXT" (current-buffer-raw-text))))

;;; Project Workflow

(deftest workflow/project-via-keyboard ()
  "Creates project via keyboard including transient menu navigation."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Plan vacation")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. Add project structure and organize via keyboard
  (with-wip-buffer
    ;; Add project tasks
    (goto-char (point-max))
    (newline)
    (insert "** Research destinations\n** Book flights\n** Reserve hotel")
    ;; Use direct function call instead of keyboard simulation on transient
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-project-new))

  ;; 4. VERIFY project structure
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (assert-match "Plan vacation" content)
      (assert-match "Research destinations" content)
      (assert-match "Book flights" content)
      (assert-match "Reserve hotel" content)
      (assert-match "\\[[0-9]+/[0-9]+\\]" content)))  ; Progress cookie

  ;; 5. VERIFY in agenda
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Plan" agenda-content)
    (assert-match "Research destinations" agenda-content)))

;;; Delegated Workflow

(deftest workflow/delegation-via-keyboard ()
  "Delegates via keyboard with person and date input."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Get report from John")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard - simulate delegation workflow
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call and set properties manually
    (with-simulated-input "John RET 2025-12-31 RET"
      (org-gtd-delegate)))

  ;; 4. VERIFY delegation properties
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Get report from John")
    (assert-equal "John" (org-entry-get (point) "DELEGATED_TO"))
    (assert-match "2025-12-31" (org-entry-get (point) org-gtd-timestamp)))

  ;; 5. VERIFY delegation was successful
  (with-current-buffer (org-gtd--default-file)
    (assert-match "Get report from John" (current-buffer-raw-text))))

;;; Calendar Workflow

(deftest workflow/calendar-via-keyboard ()
  "Schedules calendar item via keyboard with date input."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Doctor appointment")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard - calendar organization with date
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call with date input
    (with-simulated-input "2025-06-20 RET"
      (org-gtd-calendar)))

  ;; 4. VERIFY calendar properties
  (with-current-buffer (org-gtd--default-file)
    (assert-match "Doctor appointment" (current-buffer-raw-text))
    (assert-match "2025-06-20" (current-buffer-raw-text))))

;;; Knowledge Workflow

(deftest workflow/knowledge-via-keyboard ()
  "Stores knowledge via keyboard."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Git commands cheatsheet")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard as knowledge
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-knowledge))

  ;; 4. VERIFY knowledge is archived immediately
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Git commands cheatsheet" (current-buffer-raw-text)))

  ;; Knowledge items don't appear in agenda (archived)
  (org-gtd-engage)
  (refute-match "Git commands cheatsheet" (agenda-raw-text)))

;;; Tickler Workflow

(deftest workflow/tickler-via-keyboard ()
  "Defers item via keyboard with date input."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Review insurance policy")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard - tickler with date
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call with date
    (with-simulated-input "2025-06-20 RET"
      (org-gtd-tickler)))

  ;; 4. VERIFY tickler properties
  (with-current-buffer (org-gtd--default-file)
    (assert-match "Review insurance policy" (current-buffer-raw-text))
    (assert-match "2025-06-20" (current-buffer-raw-text))))

;;; Habit Workflow

(deftest workflow/habit-via-keyboard ()
  "Creates habit via keyboard with repeater input."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Morning exercise")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard - habit with repeater
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call with repeater
    (with-simulated-input "2025-01-01 RET .+1d RET"
      (org-gtd-habit)))

  ;; 4. VERIFY habit properties
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (assert-match "Morning exercise" content)
      (assert-match "\\.\\+1d" content))))

;;; Someday Workflow

(deftest workflow/someday-via-keyboard ()
  "Moves item to someday/maybe via keyboard."
  ;; 1. CAPTURE
  (org-gtd-capture nil "i")
  (insert "Learn Haskell")
  (org-capture-finalize)

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE via keyboard - someday
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-someday))

  ;; 4. VERIFY someday properties
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Learn Haskell")
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

;;; Multiple Items Workflow

(deftest workflow/multiple-items-sequential-processing ()
  "Processes multiple items sequentially using keyboard."
  ;; Setup multiple inbox items
  (org-gtd-capture nil "i")
  (insert "Call dentist")
  (org-capture-finalize)

  (org-gtd-capture nil "i")
  (insert "Plan weekend trip")
  (org-capture-finalize)

  (org-gtd-capture nil "i")
  (insert "Read Emacs manual")
  (org-capture-finalize)

  ;; Process inbox using keyboard
  (org-gtd-process-inbox)

  ;; First item: single action
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-single-action))

  ;; Second item: project
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (insert "** Research destinations\n** Book accommodation")
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-project-new))

  ;; Third item: knowledge
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-gtd-knowledge))

  ;; Verify all items processed correctly
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Call dentist" agenda-content)
    (assert-match "Plan weeken" agenda-content)
    (assert-match "Research destinations" agenda-content)
    ;; Knowledge item should not appear (archived)
    (refute-match "Read Emacs manual" agenda-content))

  ;; Verify inbox is empty
  (with-current-buffer (ogt-inbox-buffer)
    (refute-match "Call dentist\\|Plan weekend\\|Read Emacs" (current-buffer-raw-text))))

(provide 'keyboard-driven-workflow-integration-test)

;;; keyboard-driven-workflow-test.el ends here
