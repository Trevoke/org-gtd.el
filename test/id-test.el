;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; "generates a makeshift id if the heading is empty" migrated to
;; test-eunit/unit/id-test.el (id-generates-makeshift-for-empty-heading)

(describe
 "Generating ID for headings"


 (before-each (setq inhibit-message t))

 (it "generates a makeshift id if the heading is empty"
     (with-temp-buffer
       (org-mode)
       (insert "* \nfoo")
       (org-gtd-id-get-create)

       (expect (org-entry-get nil "ID")
               :to-match
               "org-gtd-makeshift-id"))))
