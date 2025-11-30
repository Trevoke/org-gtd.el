;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; NOTE: Original test was buggy - it used create-reference-item instead of create-quick-action.
;; Fixed and migrated to test-eunit/acceptance/basic-workflows-test.el (quick-action-moves-to-archive)
;; The e-unit version uses the correct helper and verifies ORG_GTD="Quick".

(describe
 "Processing a Quick Action item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, moves the task to the archive file"
     (create-reference-item "Yowza")
     (expect (archive-contains? "Yowza") :to-be-truthy)))
