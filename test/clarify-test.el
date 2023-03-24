;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Flow for clarifying items"
 (it "stores a marker to the original heading as local variable in the WIP buffer"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (let ((task-id (with-current-buffer source-buffer (org-id-get)))
             (wip-buffer (car (org-gtd-wip--get-buffers))))
         (with-current-buffer wip-buffer
           (expect (org-entry-get org-gtd--stuff-marker "ID")
                   :to-equal task-id))))))