;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "archiving"

 (before-all (ogt--configure-emacs))

 (before-each (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (describe
  "finished work"
  (it "on a single action"

      (ogt--add-and-process-single-action)
      ;; insert a single action
      ;; insert / complete another single action
      ;; run a new command that archives completed actions
      ;; see that the completed action is archived, and the other one isn't
      )))
