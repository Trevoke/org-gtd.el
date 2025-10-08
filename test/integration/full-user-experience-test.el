;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'keyboard-integration (file-name-concat default-directory "test/helpers/keyboard-integration.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Full User Experience Integration Tests with Real End-to-End Workflows"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "processes multiple GTD item types using org-gtd-process-inbox with keyboard integration"
     ;; Test the complete user workflow: capture → process-inbox → organize → verify
     (let ((items '(("Review quarterly budget" . single-action)
                    ("Plan team meeting" . project)
                    ("Doctor appointment" . calendar))))
       ;; This uses org-gtd-process-inbox (critical command!) with keyboard verification
       (ogt-multiple-items-with-keyboard-verification items)
       
       ;; Verify all items were processed correctly
       (with-current-buffer (org-gtd--default-file)
         (let ((content (current-buffer-raw-text)))
           (expect content :to-match "Review quarterly budget")
           (expect content :to-match "Plan team meeting") 
           (expect content :to-match "Doctor appointment")
           (expect content :to-match "NEXT")  ; Single action
           (expect content :to-match "\\[[0-9]+/[0-9]+\\]")))) ; Project cookie
     
     ;; Verify agenda integration
     (org-gtd-engage)
     (let ((agenda-content (agenda-raw-text)))
       (expect agenda-content :to-match "Review quarterly budget")
       (expect agenda-content :to-match "Plan team m")     ; Truncated project name
       (expect agenda-content :to-match "First task")))    ; Project component

 (it "handles multi-session workflow with org-gtd-process-inbox and keyboard verification"
     ;; Session 1: Process one item
     (ogt-verify-keyboard-and-organize-as-single-action "Important client call")
     
     ;; User gets interrupted - captures more without processing
     (org-gtd-capture nil "i")
     (insert "Prepare presentation")
     (org-capture-finalize)
     
     ;; Session 2: User returns and processes remaining items
     (org-gtd-process-inbox)  ; Critical command in multi-session context!
     
     ;; Verify keyboard integration in WIP buffer
     (let ((wip-buffers (seq-filter (lambda (buf) 
                                      (string-match-p org-gtd-wip--prefix (buffer-name buf))) 
                                    (buffer-list))))
       (when wip-buffers
         (with-current-buffer (car wip-buffers)
           (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
                   :to-equal #'org-gtd-organize)
           (expect org-gtd-clarify-mode :to-be-truthy)
           (organize-as-single-action))))
     
     ;; Verify both sessions worked
     (with-current-buffer (org-gtd--default-file)
       (let ((content (current-buffer-raw-text)))
         (expect content :to-match "Important client call")
         (expect content :to-match "Prepare presentation"))))

 (it "demonstrates complete workflow with task completion and archiving"
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
         (expect content :not :to-match "Complete this task")
         (expect content :to-match "Team project")  ; Project remains
         (expect content :to-match "Task 2")))))    ; Incomplete task remains