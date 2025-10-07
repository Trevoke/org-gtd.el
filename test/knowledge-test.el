;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing a knowledge item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, moves the task to the archive file"
     (create-reference-item "Yowza")
     (with-current-buffer (ogt--archive)
       (expect (buffer-string) :to-match "Yowza"))))
