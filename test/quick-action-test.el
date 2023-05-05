;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing a Quick Action item"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, moves the task to the archive file"
     (ogt-capture-and-process-knowledge-item "Yowza")
     (with-current-buffer (ogt--archive)
       (expect (buffer-string) :to-match "Yowza"))))
