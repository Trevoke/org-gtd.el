;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing a trash item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, mark a task as trash"
     (ogt-capture-and-process-trash-item "Yowza")
     (with-current-buffer (ogt--archive)
       (expect (buffer-string) :to-match "Yowza"))))
