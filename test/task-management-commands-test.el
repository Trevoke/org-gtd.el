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

  (describe "org-gtd-task-add-blocker command"
    
    (it "adds a blocking task to BLOCKED_BY property"
      ;; Test the org-gtd-task-add-blocker command
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        
        ;; Go to Task B and add Task A as blocker
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        
        ;; Mock the task selection to return task-a-id
        (cl-letf (((symbol-function 'org-gtd-task-management--select-task-id)
                   (lambda (_prompt) "task-a-id")))
          (org-gtd-task-add-blocker))
        
        ;; Verify BLOCKED_BY property was added
        (let ((blocked-by (org-entry-get (point) "BLOCKED_BY")))
          (expect blocked-by :to-equal "task-a-id"))))
    
    (it "appends to existing BLOCKED_BY property"
      ;; Test adding multiple blockers
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:BLOCKED_BY: task-a-id\n:END:\n\n")
        
        ;; Go to Task C and add Task B as additional blocker
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        
        ;; Mock the task selection to return task-b-id
        (cl-letf (((symbol-function 'org-gtd-task-management--select-task-id)
                   (lambda (_prompt) "task-b-id")))
          (org-gtd-task-add-blocker))
        
        ;; Verify BLOCKED_BY property contains both IDs
        (let ((blocked-by (org-entry-get (point) "BLOCKED_BY")))
          (expect blocked-by :to-equal "task-a-id task-b-id")))))

  (describe "org-gtd-task-add-dependent command"
    
    (it "adds current task ID to another task's BLOCKED_BY property"
      ;; Test the org-gtd-task-add-dependent command
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        
        ;; Go to Task A and make Task B depend on it
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        
        ;; Mock the task selection to return task-b-id
        (cl-letf (((symbol-function 'org-gtd-task-management--select-task-id)
                   (lambda (_prompt) "task-b-id")))
          (org-gtd-task-add-dependent))
        
        ;; Verify Task B now has Task A in its BLOCKED_BY property
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocked-by (org-entry-get (point) "BLOCKED_BY")))
          (expect blocked-by :to-equal "task-a-id"))))))

;;; task-dependency-commands-test.el ends here