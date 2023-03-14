;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "WIP state for tasks"

 (before-each
  (ogt--configure-emacs)
  (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (it "holds the subtree for the task we want to clarify"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (expect (ogt--buffer-string (car (org-gtd-wip--get-buffers)))
               :to-match
               "This is the heading to clarify")))

 (it "has the org-gtd-processing mode"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
         (with-current-buffer wip-buffer
           (expect local-minor-modes :to-contain 'org-gtd-process-mode))))))
