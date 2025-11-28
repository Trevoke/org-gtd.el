;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe
 "org-gtd-command-center"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "is defined as a transient prefix"
     (expect (fboundp 'org-gtd-command-center) :to-be t))

 (it "contains engage commands"
     (expect (fboundp 'org-gtd-engage) :to-be t)
     (expect (fboundp 'org-gtd-engage-grouped-by-context) :to-be t)
     (expect (fboundp 'org-gtd-show-all-next) :to-be t))

 (it "contains capture and process commands"
     (expect (fboundp 'org-gtd-capture) :to-be t)
     (expect (fboundp 'org-gtd-process-inbox) :to-be t)
     (expect (fboundp 'org-gtd-clarify-item) :to-be t))

 (it "contains reflect commands"
     (expect (fboundp 'org-gtd-reflect-area-of-focus) :to-be t)
     (expect (fboundp 'org-gtd-reflect-someday-maybe) :to-be t)
     (expect (fboundp 'org-gtd-reflect-upcoming-delegated) :to-be t)
     (expect (fboundp 'org-gtd-reflect-completed-items) :to-be t)
     (expect (fboundp 'org-gtd-reflect-completed-projects) :to-be t))

 (it "has a stuck items sub-menu"
     (expect (fboundp 'org-gtd-command-center--stuck) :to-be t))

 (it "has a missed items sub-menu"
     (expect (fboundp 'org-gtd-command-center--missed) :to-be t)))
