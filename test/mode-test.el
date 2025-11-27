;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "org-gtd-mode inbox count"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "org-gtd-inbox-count"

   (it "returns 0 when inbox is empty"
       (expect (org-gtd-inbox-count) :to-equal 0))

   (it "counts items in main inbox"
       (capture-inbox-item "First item")
       (capture-inbox-item "Second item")
       (capture-inbox-item "Third item")
       (expect (org-gtd-inbox-count) :to-equal 3))

   (it "includes items from additional inbox files"
       (capture-inbox-item "Main inbox item")

       (let* ((additional-file (f-join org-gtd-directory "extra-inbox.org"))
              (additional-buffer (find-file-noselect additional-file)))
         (with-current-buffer additional-buffer
           (insert "* Extra item 1\n* Extra item 2\n")
           (basic-save-buffer))

         (let ((org-gtd-additional-inbox-files (list additional-file)))
           (expect (org-gtd-inbox-count) :to-equal 3)))))

 (describe "org-gtd-mode-lighter"

   (it "formats count in lighter string"
       (capture-inbox-item "Test item")
       (capture-inbox-item "Another item")
       (expect (org-gtd-mode-lighter) :to-equal " GTD[2]"))

   (it "shows zero when inbox is empty"
       (expect (org-gtd-mode-lighter) :to-equal " GTD[0]"))))
