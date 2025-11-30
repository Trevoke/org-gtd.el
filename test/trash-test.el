;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; "through the inbox, mark a task as trash" migrated to
;; test-eunit/acceptance/basic-workflows-test.el (trash-item-moves-to-archive)

(describe
 "Processing a trash item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, mark a task as trash"
     (discard-inbox-item "Yowza")
     (expect (archive-contains? "Yowza") :to-be-truthy)))
