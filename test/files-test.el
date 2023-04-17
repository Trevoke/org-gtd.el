;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Create a default file"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "with default content"
  (it "for the inbox"
      (with-current-buffer (org-gtd--inbox-file)
        (expect (ogt--current-buffer-raw-text)
                :to-match
                "This is the inbox")
        (expect (ogt--current-buffer-raw-text)
                :to-match
                "This is the inbox.")))

  (describe
   "when there isn't a refile target"
   (it "for a project"
       (ogt-capture-and-process-project "project headline")
       (ogt--save-all-buffers)
       (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

   (it "for a calendar item"
       (ogt-capture-and-process-calendar-item "calendar headline")
       (ogt--save-all-buffers)
       (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

   (it "for a delegated item"
       (ogt-capture-and-process-delegated-item "delegated-headline")
       (ogt--save-all-buffers)
       (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

   (it "for a incubated item"
       (ogt-capture-and-process-incubated-item "incubated headline")
       (ogt--save-all-buffers)
       (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

   (it "for a single action"
       (ogt-capture-and-process-single-action "single action")
       (ogt--save-all-buffers)
       (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org")))))
