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

  (describe "cross-project dependencies (Story 5)"
    
    (it "shows tasks from different projects with project labels when selecting blockers"
      ;; This test verifies the acceptance criteria: "tasks from all existing agenda files clearly labeled by project"
      ;; Set up Project A in main GTD tasks file  
      (ogt-capture-single-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (ogt-clarify-as-project)
      
      ;; Create a second GTD file with Project B
      (let ((second-file (org-gtd--path "secondary-project")))
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))
        
        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
          
          ;; Capture what task selection interface shows
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)
            
            ;; Mock the completing-read to capture the options presented
            (let ((presented-options nil)
                  (call-count 0))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &rest _args)
                           (when (= call-count 0) ; Only capture on first call
                             (setq presented-options collection))
                           (setq call-count (1+ call-count))
                           ;; Return empty string on second call to exit multi-select loop
                           (if (= call-count 1) "task-b1-id" ""))))
                (org-gtd-task-management--select-multiple-task-ids "Test prompt: "))
              
              ;; Verify that tasks are labeled with project information
              ;; The collection should include project context for cross-project tasks
              (let ((has-project-labels (cl-some (lambda (option)
                                                   ;; option is a cons cell (display . value)
                                                   (string-match-p "Project B" (car option)))
                                                 presented-options)))
                (expect has-project-labels :to-be-truthy)))))))
    
    (it "creates dependencies between tasks in different projects"
      ;; Test cross-project blocking relationships
      ;; Set up Project A in main GTD tasks file  
      (ogt-capture-single-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (ogt-clarify-as-project)
      
      ;; Create a second GTD file manually 
      (let ((second-file (org-gtd--path "secondary-project")))
        ;; Set up Project B in the secondary file
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))
        
        ;; Ensure the ID is registered in the org-id system by visiting the file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode) ; Make sure org-mode is active
          (goto-char (point-min))
          (search-forward "Task B1")
          (org-back-to-heading t)
          ;; Force org-id to recognize this ID
          (org-id-add-location "task-b1-id" second-file))
        
        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
          
          ;; Now create cross-project dependency: Task A1 depends on Task B1
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)
            
            ;; Mock task selection to return Task B1 from different project  
            (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                       (lambda (_prompt) '("task-b1-id"))))
              (org-gtd-task-add-blockers))
            
            ;; Verify Task A1 has DEPENDS_ON pointing to Task B1
            (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
              (expect depends-on :to-equal '("task-b1-id"))))
          
          ;; Verify Task B1 in the other file has BLOCKS pointing to Task A1
          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task B1")
            (org-back-to-heading t)
            (let ((task-a1-id (with-current-buffer (org-gtd--default-file)
                                (goto-char (point-min))
                                (search-forward "Task A1")
                                (org-back-to-heading t)
                                (org-entry-get (point) "ID")))
                  (blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
              (expect blocks :to-equal (list task-a1-id))))))))

  ;; Note: org-gtd-task-add-dependent tests will be implemented in Story 4
  ;; For now focusing on Stories 1-2: Add Task Blockers (Single + Multiple Selection)
  )

;;; task-management-commands-test.el ends here