;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Org GTD"

 (before-all (ogt--configure-emacs))
 (before-each (ogt--clean-target-directory org-gtd-directory))
 (after-each (ogt--close-and-delete-files))

 (it "Creates a default file for each action where there isn't a refile target"
     (ogt--add-and-process-project "project headline")
     (with-simulated-input "!" (save-some-buffers))
     (list-directory org-gtd-directory)
     (with-current-buffer "*Directory*"
       (expect (buffer-string)
               :to-match
               ".*projects\\.org.*"))
     (kill-buffer "*Directory*")
     ))
