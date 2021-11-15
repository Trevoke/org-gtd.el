;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "delegating a task"

  (before-all (ogt--configure-emacs))

 (before-each (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (it "can be done through the agenda and show on the agenda"
     (ogt--add-and-process-single-action "delegateme")
     (ogt--save-all-buffers)
     (org-gtd-mode)
     (org-agenda nil "g")
     (with-current-buffer org-agenda-buffer-name
       (beginning-of-buffer)
       (search-forward "delegateme")
       (with-simulated-input "That SPC Guy RET RET"
                             (org-gtd-agenda-delegate-task)))
     (kill-buffer org-agenda-buffer-name)

     (ogt--save-all-buffers)
     (org-agenda nil "g")
     (with-current-buffer org-agenda-buffer-name
       (expect (buffer-string) :to-match "WAIT ")
       (expect (buffer-string) :to-match "That Guy"))))
