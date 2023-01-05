;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management 2"

 (before-each
  (ogt--configure-emacs)
  (ogt--prepare-filesystem))

 (after-each (ogt--close-and-delete-files))

 (describe
  "adds new tasks to an existing project"

  (it
   "as the first NEXT task"
   (ogt--add-and-process-project "project headline")
   (ogt--add-single-item "Task 0")
   (org-gtd-process-inbox)

   (with-simulated-input "project SPC headline TAB RET"
                         (org-gtd--modify-project))
   (org-gtd-engage)
   (with-current-buffer org-agenda-this-buffer-name
     (expect (buffer-string) :to-match "Task 0")
     (expect (buffer-string) :not :to-match "Task 1")))))
