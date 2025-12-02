;;; full-user-experience-test.el --- Integration tests for complete GTD user experience -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for complete GTD user experience workflows.
;; Tests multi-item processing, multi-session workflows, and task archiving.
;;
;; Migrated from test/integration/full-user-experience-test.el (buttercup).
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

;;; Multiple Item Processing

(deftest user-experience/processes-multiple-gtd-item-types ()
  "Processes multiple GTD item types using org-gtd-process-inbox with keyboard integration."
  ;; Test the complete user workflow: capture → process-inbox → organize → verify
  (let ((items '(("Review quarterly budget" . single-action)
                 ("Plan team meeting" . project)
                 ("Doctor appointment" . calendar))))
    ;; This uses org-gtd-process-inbox (critical command!) with keyboard verification
    (ogt-multiple-items-with-keyboard-verification items)

    ;; Verify all items were processed correctly
    (with-current-buffer (org-gtd--default-file)
      (let ((content (current-buffer-raw-text)))
        (assert-match "Review quarterly budget" content)
        (assert-match "Plan team meeting" content)
        (assert-match "Doctor appointment" content)
        (assert-match "NEXT" content)  ; Single action
        (assert-match "\\[[0-9]+/[0-9]+\\]" content))))  ; Project cookie

  ;; Verify agenda integration
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Review quarterly budget" agenda-content)
    (assert-match "Plan team m" agenda-content)     ; Truncated project name
    (assert-match "First task" agenda-content)))    ; Project component

;;; Multi-Session Workflow

(deftest user-experience/handles-multi-session-workflow ()
  "Handles multi-session workflow with org-gtd-process-inbox and keyboard verification."
  ;; Session 1: Process one item
  (ogt-verify-keyboard-and-organize-as-single-action "Important client call")

  ;; User gets interrupted - captures more without processing
  (org-gtd-capture nil "i")
  (insert "Prepare presentation")
  (org-capture-finalize)

  ;; Session 2: User returns and processes remaining items
  (org-gtd-process-inbox)  ; Critical command in multi-session context!

  ;; Verify keyboard integration in WIP buffer
  (with-wip-buffer
    (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-map (kbd "C-c c")))
    (assert-true org-gtd-clarify-mode)
    (organize-as-single-action))

  ;; Verify both sessions worked
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (assert-match "Important client call" content)
      (assert-match "Prepare presentation" content))))

;;; Task Completion and Archiving

(deftest user-experience/task-completion-and-archiving ()
  "Demonstrates complete workflow with task completion and archiving."
  ;; Create items for completion workflow using org-gtd-process-inbox
  (ogt-verify-keyboard-and-organize-as-single-action "Complete this task")
  (ogt-verify-keyboard-and-organize-as-project "Team project" "** Task 1\n** Task 2")

  ;; Complete some tasks
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Complete this task")
    (org-todo "DONE")

    (goto-char (point-min))
    (re-search-forward "Task 1")
    (org-todo "DONE"))

  ;; Archive completed items
  (org-gtd-archive-completed-items)

  ;; Verify archiving worked
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (refute-match "Complete this task" content)
      (assert-match "Team project" content)  ; Project remains
      (assert-match "Task 2" content))))     ; Incomplete task remains

(provide 'full-user-experience-integration-test)

;;; full-user-experience-test.el ends here
