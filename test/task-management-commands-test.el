;;; task-management-commands-test.el --- Tests for task management commands -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for interactive commands that manage task dependencies (BLOCKS/BLOCKED_BY relationships)
;;

;;; Code:

;;;; Requirements

(require 'org-gtd)
(require 'org-gtd-task-management)
(require 'buttercup)

;; Load test helpers
(load (expand-file-name "helpers/setup.el" 
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))

;;;; Test Setup

(describe "task dependency commands"
  
  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "org-gtd-task-add-blockers command (Story 1)"
    
    (it "creates bidirectional BLOCKS/DEPENDS_ON relationship between tasks"
      ;; Test the new org-gtd-task-add-blockers command with bidirectional properties
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        
        ;; Go to Task B and add Task A as blocker (Task A blocks Task B)
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        
        ;; Mock the task selection to return single task as list
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id"))))
          (org-gtd-task-add-blockers))
        
        ;; Verify Task B has DEPENDS_ON property
        (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
          (expect depends-on :to-equal '("task-a-id")))
        
        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-b-id"))))))

    (it "creates bidirectional BLOCKS/DEPENDS_ON relationship with multiple blockers (Story 2)"
      ;; Test multi-select functionality
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")
        
        ;; Go to Task C and add both Task A and B as blockers
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        
        ;; Mock the task selection to return multiple IDs
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id" "task-b-id"))))
          (org-gtd-task-add-blockers))
        
        ;; Verify Task C has DEPENDS_ON property with both IDs
        (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
          (expect (sort depends-on 'string<) :to-equal '("task-a-id" "task-b-id")))
        
        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-c-id")))
        
        ;; Verify Task B has BLOCKS property  
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-c-id")))))

  ;; Note: org-gtd-task-add-dependent tests will be implemented in Story 4
  ;; For now focusing on Stories 1-2: Add Task Blockers (Single + Multiple Selection)
  )

;;; task-management-commands-test.el ends here