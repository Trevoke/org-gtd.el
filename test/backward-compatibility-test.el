;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

(describe
 "Backward compatibility aliases for incubate→tickler rename"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "org-gtd-incubate is an obsolete alias for org-gtd-tickler"
     ;; Verify the function exists
     (expect (fboundp 'org-gtd-incubate) :to-be-truthy)
     ;; Verify it's marked as obsolete with the correct replacement
     (let ((obsolete-info (get 'org-gtd-incubate 'byte-obsolete-info)))
       (expect (car obsolete-info) :to-equal 'org-gtd-tickler)
       (expect (caddr obsolete-info) :to-equal "4.0")))

 (it "org-gtd-incubate-create is an obsolete alias for org-gtd-tickler-create"
     ;; Verify the function exists
     (expect (fboundp 'org-gtd-incubate-create) :to-be-truthy)
     ;; Verify it's marked as obsolete with the correct replacement
     (let ((obsolete-info (get 'org-gtd-incubate-create 'byte-obsolete-info)))
       (expect (car obsolete-info) :to-equal 'org-gtd-tickler-create)
       (expect (caddr obsolete-info) :to-equal "4.0")))

 (it "org-gtd-incubate constant is an obsolete alias for org-gtd-tickler"
     ;; Verify the constant is accessible via the old name
     (expect (boundp 'org-gtd-incubate) :to-be-truthy)
     ;; Verify it has the same value as the new constant
     (expect org-gtd-incubate :to-equal org-gtd-tickler)
     (expect org-gtd-incubate :to-equal "Tickler")
     ;; Verify it's marked as obsolete
     (let ((obsolete-info (get 'org-gtd-incubate 'byte-obsolete-variable)))
       (expect (car obsolete-info) :to-equal 'org-gtd-tickler)
       (expect (caddr obsolete-info) :to-equal "4.0"))))
