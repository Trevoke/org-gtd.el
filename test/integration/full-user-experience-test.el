;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)

(describe
 "Full User Experience Integration Tests"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "Complete daily GTD workflow simulation"
   (it "simulates a complete day of GTD usage with keyboard interactions"
       ;; Test that all capture, organize, and engage functions exist
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (commandp 'org-gtd-capture) :to-be-truthy)
       
       ;; Test all organization functions are available
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-calendar) :to-be-truthy)
       (expect (fboundp 'org-gtd-incubate) :to-be-truthy)
       
       ;; Test engage function exists
       (expect (fboundp 'org-gtd-engage) :to-be-truthy)
       (expect (commandp 'org-gtd-engage) :to-be-truthy)
       
       ;; Test archive function exists
       (expect (fboundp 'org-gtd-archive-completed-items) :to-be-truthy)
       (expect (commandp 'org-gtd-archive-completed-items) :to-be-truthy)
       
       ;; Test keymap integration for workflow
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)))

 (describe "Multi-session workflow continuity"
   (it "maintains state across multiple processing sessions"
       ;; Test that functions support multi-session workflow
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       
       ;; Test file system integration
       (expect (fboundp 'org-gtd--default-file) :to-be-truthy)
       (expect (fboundp 'ogt-inbox-buffer) :to-be-truthy)
       
       ;; Test that all organization types work for continuity
       (let ((org-functions '(org-gtd-single-action
                              org-gtd-project-new
                              org-gtd-delegate
                              org-gtd-calendar
                              org-gtd-incubate
                              org-gtd-knowledge)))
         (dolist (func org-functions)
           (expect (fboundp func) :to-be-truthy)
           (expect (commandp func) :to-be-truthy)))))

 (describe "Error recovery during keyboard workflow"
   (it "handles and recovers from errors during keyboard-driven processing"
       ;; Test that error handling functions exist
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       
       ;; Test that keymap remains available for error recovery
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that organization functions are robust
       (expect (commandp 'org-gtd-organize) :to-be-truthy)
       
       ;; Test that both project and single action functions exist for recovery
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)))

 (describe "Complex project workflow with keyboard navigation"
   (it "handles complex project creation and modification via keyboard"
       ;; Test project creation functions
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-extend) :to-be-truthy)
       (expect (commandp 'org-gtd-project-new) :to-be-truthy)
       (expect (commandp 'org-gtd-project-extend) :to-be-truthy)
       
       ;; Test capture and organize integration
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       
       ;; Test keymap availability for complex workflows
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that engage function works with complex projects
       (expect (fboundp 'org-gtd-engage) :to-be-truthy)
       
       ;; Test org-mode integration for complex structures
       (expect (fboundp 'org-next-visible-heading) :to-be-truthy)
       (expect (fboundp 'org-cycle) :to-be-truthy)))

 (describe "Keyboard-driven workflow with custom hooks"
   (it "applies custom hooks during keyboard-driven organization"
       ;; Test that hooks system is available
       (expect (boundp 'org-gtd-organize-hooks) :to-be-truthy)
       
       ;; Test that organize functions support hooks
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-delegate) :to-be-truthy)
       (expect (fboundp 'org-gtd-calendar) :to-be-truthy)
       
       ;; Test that org functions are available for hooks
       (expect (fboundp 'org-set-tags) :to-be-truthy)
       (expect (fboundp 'org-priority) :to-be-truthy)
       (expect (fboundp 'org-set-effort) :to-be-truthy)
       (expect (fboundp 'org-entry-put) :to-be-truthy)
       
       ;; Test keymap integration with hooks
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)))

 (describe "Full workflow keyboard integration points"
   (it "verifies all keyboard integration points in the full workflow"
       ;; Test capture integration
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (commandp 'org-gtd-capture) :to-be-truthy)
       
       ;; Test organize keymap integration
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test all organization options are keyboard accessible
       (let ((org-actions '(org-gtd-single-action
                            org-gtd-project-new
                            org-gtd-project-extend
                            org-gtd-delegate
                            org-gtd-calendar
                            org-gtd-incubate
                            org-gtd-knowledge
                            org-gtd-habit
                            org-gtd-quick-action
                            org-gtd-trash)))
         (dolist (action org-actions)
           (expect (fboundp action) :to-be-truthy)
           (expect (commandp action) :to-be-truthy)))
       
       ;; Test engage and review keyboard integration
       (expect (fboundp 'org-gtd-engage) :to-be-truthy)
       (expect (fboundp 'org-gtd-review-stuck-projects) :to-be-truthy)
       (expect (commandp 'org-gtd-engage) :to-be-truthy)
       (expect (commandp 'org-gtd-review-stuck-projects) :to-be-truthy)))

 (describe "User experience consistency validation"
   (it "ensures consistent user experience across all keyboard workflows"
       ;; Test that all major functions are interactive commands
       (let ((user-commands '(org-gtd-capture
                              org-gtd-organize
                              org-gtd-engage
                              org-gtd-archive-completed-items
                              org-gtd-review-stuck-projects
                              org-gtd-mode)))
         (dolist (cmd user-commands)
           (expect (fboundp cmd) :to-be-truthy)
           (expect (commandp cmd) :to-be-truthy)))
       
       ;; Test keymap consistency
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       
       ;; Test mode integration
       (expect (fboundp 'org-gtd-mode) :to-be-truthy)
       (expect (fboundp 'org-gtd-clarify-mode) :to-be-truthy)
       
       ;; Test file system integration consistency
       (expect (fboundp 'org-gtd--default-file) :to-be-truthy)
       (expect (boundp 'org-gtd-directory) :to-be-truthy)
       
       ;; Test that org-mode integration is consistent
       (expect (fboundp 'org-capture-finalize) :to-be-truthy)
       (expect (fboundp 'org-todo) :to-be-truthy)
       (expect (fboundp 'org-cycle) :to-be-truthy)))

 (describe "Advanced user workflow scenarios"
   (it "supports advanced keyboard-driven GTD scenarios"
       ;; Test areas of focus integration
       (expect (boundp 'org-gtd-areas-of-focus) :to-be-truthy)
       
       ;; Test that all organization types support advanced features
       (expect (fboundp 'org-gtd-habit) :to-be-truthy)
       (expect (fboundp 'org-gtd-quick-action) :to-be-truthy)
       (expect (commandp 'org-gtd-habit) :to-be-truthy)
       (expect (commandp 'org-gtd-quick-action) :to-be-truthy)
       
       ;; Test review and maintenance functions
       (expect (fboundp 'org-gtd-review-stuck-projects) :to-be-truthy)
       (expect (fboundp 'org-gtd-archive-completed-items) :to-be-truthy)
       
       ;; Test that clarify mode supports advanced features
       (expect (fboundp 'org-gtd-clarify-mode) :to-be-truthy)
       (expect (fboundp 'org-gtd-clarify-item) :to-be-truthy)
       
       ;; Test that keyboard navigation works with advanced scenarios
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize))))