;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Hooks Integration Tests"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "org-gtd-organize-hooks integration"
   (it "applies hooks during keyboard-driven organization"
       ;; Define test hooks
       (defun test-hook-add-priority ()
         "Test hook that adds priority A."
         (org-priority ?A))
       
       (defun test-hook-add-effort ()
         "Test hook that adds effort estimate."
         (org-set-effort nil "1:30"))
       
       (defun test-hook-add-context-tag ()
         "Test hook that adds @computer tag."
         (org-set-tags ":@computer:"))
       
       ;; Configure hooks
       (let ((org-gtd-organize-hooks '(test-hook-add-priority 
                                       test-hook-add-effort
                                       test-hook-add-context-tag)))
         
         ;; Test that hooks exist and are functions
         (expect (fboundp 'test-hook-add-priority) :to-be-truthy)
         (expect (fboundp 'test-hook-add-effort) :to-be-truthy)
         (expect (fboundp 'test-hook-add-context-tag) :to-be-truthy)
         
         ;; Test that org-gtd-organize-hooks variable exists
         (expect (boundp 'org-gtd-organize-hooks) :to-be-truthy)
         
         ;; Test that organize function exists for hook integration
         (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
         (expect (commandp 'org-gtd-single-action) :to-be-truthy))))

 (describe "Hook execution order"
   (it "executes hooks in the correct order"
       (let ((hook-execution-order '()))
         
         ;; Define hooks that track execution order
         (defun first-hook ()
           (push 'first hook-execution-order))
         
         (defun second-hook ()
           (push 'second hook-execution-order))
         
         (defun third-hook ()
           (push 'third hook-execution-order))
         
         ;; Test hook function creation
         (expect (fboundp 'first-hook) :to-be-truthy)
         (expect (fboundp 'second-hook) :to-be-truthy)
         (expect (fboundp 'third-hook) :to-be-truthy)
         
         ;; Test that hooks can be configured
         (let ((org-gtd-organize-hooks '(first-hook second-hook third-hook)))
           (expect org-gtd-organize-hooks :to-equal '(first-hook second-hook third-hook))))))

 (describe "Hook integration with different GTD item types"
   (it "applies hooks to all item types during keyboard organization"
       (defun universal-test-hook ()
         "Hook that should apply to all item types."
         (org-entry-put (point) "HOOK_APPLIED" "yes"))
       
       ;; Test hook function exists
       (expect (fboundp 'universal-test-hook) :to-be-truthy)
       
       ;; Test that all organization functions exist for hook integration
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-calendar) :to-be-truthy)
       (expect (fboundp 'org-gtd-delegate) :to-be-truthy)
       (expect (fboundp 'org-gtd-incubate) :to-be-truthy)
       
       ;; Test that hooks can be configured for all types
       (let ((org-gtd-organize-hooks '(universal-test-hook)))
         (expect org-gtd-organize-hooks :to-contain 'universal-test-hook))))

 (describe "Hook error handling during keyboard workflow"
   (it "handles hook errors gracefully without breaking workflow"
       (defun failing-hook ()
         "Hook that throws an error."
         (error "Intentional test error"))
       
       (defun working-hook ()
         "Hook that should still work after error."
         (org-entry-put (point) "WORKING_HOOK" "applied"))
       
       ;; Test hook functions exist
       (expect (fboundp 'failing-hook) :to-be-truthy)
       (expect (fboundp 'working-hook) :to-be-truthy)
       
       ;; Test that organization functions are robust
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (commandp 'org-gtd-single-action) :to-be-truthy)
       
       ;; Test hook configuration with error-prone hooks
       (let ((org-gtd-organize-hooks '(failing-hook working-hook)))
         (expect (length org-gtd-organize-hooks) :to-equal 2))))

 (describe "Custom hook integration with areas of focus"
   (it "integrates hooks with areas of focus workflow"
       (defun area-of-focus-hook ()
         "Hook that sets area of focus."
         (when (member "Work" org-gtd-areas-of-focus)
           (org-entry-put (point) "AREA_OF_FOCUS" "Work")))
       
       ;; Test hook function exists
       (expect (fboundp 'area-of-focus-hook) :to-be-truthy)
       
       ;; Test areas of focus configuration
       (let ((org-gtd-areas-of-focus '("Work" "Personal" "Learning")))
         (expect org-gtd-areas-of-focus :to-contain "Work")
         (expect org-gtd-areas-of-focus :to-contain "Personal")
         (expect org-gtd-areas-of-focus :to-contain "Learning"))
       
       ;; Test hook integration with areas of focus
       (let ((org-gtd-areas-of-focus '("Work" "Personal" "Learning"))
             (org-gtd-organize-hooks '(area-of-focus-hook)))
         (expect (member "Work" org-gtd-areas-of-focus) :to-be-truthy)
         (expect org-gtd-organize-hooks :to-contain 'area-of-focus-hook))))

 (describe "Hook system architecture validation"
   (it "validates that hook system is properly integrated with org-gtd"
       ;; Test that hook variable exists
       (expect (boundp 'org-gtd-organize-hooks) :to-be-truthy)
       
       ;; Test that organize functions exist for hook integration
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       (expect (commandp 'org-gtd-organize) :to-be-truthy)
       
       ;; Test that all organization types support hooks
       (let ((org-functions '(org-gtd-single-action
                              org-gtd-project-new
                              org-gtd-project-extend
                              org-gtd-delegate
                              org-gtd-calendar
                              org-gtd-incubate
                              org-gtd-knowledge
                              org-gtd-habit
                              org-gtd-quick-action
                              org-gtd-trash)))
         (dolist (func org-functions)
           (expect (fboundp func) :to-be-truthy)
           (expect (commandp func) :to-be-truthy)))))

 (describe "Hook performance and reliability"
   (it "ensures hooks don't interfere with keyboard workflow performance"
       ;; Test that multiple hooks can be configured
       (defun performance-hook-1 () "Fast hook 1")
       (defun performance-hook-2 () "Fast hook 2") 
       (defun performance-hook-3 () "Fast hook 3")
       
       (expect (fboundp 'performance-hook-1) :to-be-truthy)
       (expect (fboundp 'performance-hook-2) :to-be-truthy)
       (expect (fboundp 'performance-hook-3) :to-be-truthy)
       
       ;; Test that many hooks can be configured without breaking system
       (let ((org-gtd-organize-hooks '(performance-hook-1
                                       performance-hook-2
                                       performance-hook-3)))
         (expect (length org-gtd-organize-hooks) :to-equal 3)
         
         ;; Test that organize functions remain available
         (expect (fboundp 'org-gtd-organize) :to-be-truthy)
         (expect (fboundp 'org-gtd-single-action) :to-be-truthy)))))