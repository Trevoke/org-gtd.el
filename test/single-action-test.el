;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; "appears in daily agenda after creation" migrated to
;; test-eunit/acceptance/basic-workflows-test.el (single-action-programmatic-create)

(describe
 "A single action"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

  (it "appears in daily agenda after creation"
     (org-gtd-single-action-create "Write this test")
     (org-gtd-engage)
     (expect (agenda-contains? "Write this test") :to-be-truthy)))
