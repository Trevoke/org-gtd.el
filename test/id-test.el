;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

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
