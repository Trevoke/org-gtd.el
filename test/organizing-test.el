;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Organizing (in 3.0)"
 (before-each
  (ogt--configure-emacs)
  (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (it "restores the window configuration"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (window-config nil)
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (setq window-config org-gtd--window-config)
       (org-gtd-organize-task-at-point-as-single-action)

       (expect (compare-window-configurations (current-window-configuration) window-config)
               :to-be t)))
 (it "kills the temp buffer"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (org-gtd-organize-task-at-point-as-single-action)
       (expect (org-gtd-wip--get-buffers) :to-be nil)))

 (it "deletes the source heading"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (org-gtd-organize-task-at-point-as-single-action)
       (expect (buffer-size) :to-equal 0))))