;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Organizing (in 3.0)"
 (before-each
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "restores the window configuration"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (window-config nil)
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (setq window-config org-gtd-clarify--window-config)
       (org-gtd-single-action--one-off)

       (expect (compare-window-configurations (current-window-configuration) window-config)
               :to-be t)))

 (it "kills the temp buffer"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (org-gtd-single-action--one-off)
       (expect (org-gtd-clarify--get-buffers) :to-be nil)))

 (it "deletes the source heading"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
           (org-gtd-refile-to-any-target t))
       (set-buffer source-buffer)
       (org-gtd-clarify-item)
       (org-gtd-single-action--one-off)
       (expect (buffer-size) :to-equal 0))
     ;; TODO after I remove the headers, test that I can remove this
     (remove-hook 'post-command-hook 'org-add-log-note)))
