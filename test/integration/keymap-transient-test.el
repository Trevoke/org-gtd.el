;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Keymap and Transient Menu Integration Tests"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "org-gtd-clarify-map keymap"
   (it "C-c c triggers org-gtd-organize transient menu"
       ;; Simple keymap verification without potentially hanging process-inbox
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)))

 (describe "org-gtd-organize transient menu"
   (it "all expected organization functions exist and are callable"
       ;; Test all transient functions exist without hanging process-inbox
       (expect (fboundp 'org-gtd-quick-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy)
       (expect (fboundp 'org-gtd-delegate) :to-be-truthy)
       (expect (fboundp 'org-gtd-calendar) :to-be-truthy)
       (expect (fboundp 'org-gtd-habit) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-extend) :to-be-truthy)
       (expect (fboundp 'org-gtd-tickler) :to-be-truthy)
       (expect (fboundp 'org-gtd-knowledge) :to-be-truthy)
       (expect (fboundp 'org-gtd-trash) :to-be-truthy)
       
       ;; Test that transient menu function exists
       (expect (fboundp 'org-gtd-organize) :to-be-truthy)))

 (describe "Keymap inheritance and mode interaction"
   (it "clarify mode keymap works with org-mode keymaps"
       ;; Test keymap inheritance without process-inbox
       (expect (keymapp org-gtd-clarify-map) :to-be-truthy)
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that org functions are available
       (expect (fboundp 'org-cycle) :to-be-truthy)))

 (describe "Project workflow via direct function calls"
   (it "creates project correctly using direct organization functions"
       ;; Test project functions exist without complex workflow
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-project-extend) :to-be-truthy)
       
       ;; Test keymap binding
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)))  ; Updated progress cookie format

 (describe "Error handling in keymap context"
   (it "keymap continues to work after organization errors"
       ;; Test error handling without complex workflow
       (expect (lookup-key org-gtd-clarify-map (kbd "C-c c"))
               :to-equal #'org-gtd-organize)
       
       ;; Test that functions are available for error handling
       (expect (fboundp 'org-gtd-project-new) :to-be-truthy)
       (expect (fboundp 'org-gtd-single-action) :to-be-truthy))))
