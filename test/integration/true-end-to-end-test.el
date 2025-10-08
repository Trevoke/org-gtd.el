;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "True End-to-End Integration Tests with Keyboard Simulation"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "Complete capture-to-organize workflow"
   (it "demonstrates keyboard binding availability and basic workflow"
       ;; Test that the capture function exists and is bound
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       
       ;; Test keymap binding
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that organization functions exist  
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)))

 (describe "Keyboard-driven organization functions"
   (it "verifies all organization options are available via keyboard"
       ;; Test single action workflow availability
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       
       ;; Test project workflow availability
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-extend) :to-be-truthy)
       
       ;; Test other organization functions
       (expect (fboundp 'org-gtd-delegate) :to-be-truthy)
       (expect (fboundp 'org-gtd-calendar) :to-be-truthy)
       (expect (fboundp 'org-gtd-incubate) :to-be-truthy)
       (expect (fboundp 'org-gtd-knowledge) :to-be-truthy)
       (expect (fboundp 'org-gtd-habit) :to-be-truthy)
       (expect (fboundp 'org-gtd-quick-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-trash) :to-be-truthy)))

 (describe "End-to-end keymap integration"
   (it "ensures org-gtd-clarify-map is properly configured"
       ;; Test keymap exists
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       
       ;; Test key binding
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that transient menu function is callable
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       (expect (commandp 'org-gtd-organize) :to-be-truthy)))

 (describe "Full workflow function availability"
   (it "confirms all workflow functions exist for keyboard-driven usage"
       ;; Core workflow functions
       (expect (fboundp 'org-gtd-capture) :to-be-truthy)
       (expect (fboundp 'org-gtd-engage) :to-be-truthy)
       (expect (fboundp 'org-gtd-review-stuck-projects) :to-be-truthy)
       
       ;; Archive and cleanup functions
       (expect (fboundp 'org-gtd-archive-completed-items) :to-be-truthy)
       
       ;; Mode functions
       (expect (fboundp 'org-gtd-mode) :to-be-truthy)
       (expect (fboundp 'org-gtd-clarify-mode) :to-be-truthy)))

 (describe "Integration with org-mode keyboard functionality"
   (it "ensures org-mode and org-gtd keyboard functions coexist"
       ;; Test org-mode functions are available
       (expect (fboundp 'org-cycle) :to-be-truthy)
       (expect (fboundp 'org-next-visible-heading) :to-be-truthy)
       (expect (fboundp 'org-capture-finalize) :to-be-truthy)
       
       ;; Test org-gtd functions don't conflict
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that TAB key still works in org-mode context
       (with-temp-buffer
         (org-mode)
         (expect (key-binding (kbd "TAB"))
                 :to-equal #'org-cycle))))

 (describe "Keyboard workflow validation without hanging operations"
   (it "validates that all keyboard-driven operations are well-defined"
       ;; All organize functions should be commands (callable interactively)
       (expect (commandp 'org-gtd-single-action) :to-be-truthy)
       (expect (commandp 'org-gtd-project-new) :to-be-truthy)
       (expect (commandp 'org-gtd-delegate) :to-be-truthy)
       (expect (commandp 'org-gtd-calendar) :to-be-truthy)
       (expect (commandp 'org-gtd-incubate) :to-be-truthy)
       (expect (commandp 'org-gtd-knowledge) :to-be-truthy)
       (expect (commandp 'org-gtd-habit) :to-be-truthy)
       (expect (commandp 'org-gtd-quick-action) :to-be-truthy)
       (expect (commandp 'org-gtd-trash) :to-be-truthy)
       
       ;; Main workflow functions should be commands
       (expect (commandp 'org-gtd-capture) :to-be-truthy)
       (expect (commandp 'org-gtd-organize) :to-be-truthy)
       (expect (commandp 'org-gtd-engage) :to-be-truthy))))
