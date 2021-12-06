;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe "delegating a task"

  (before-each
    (ogt--configure-emacs)
    (ogt--prepare-filesystem))
  (after-each (ogt--close-and-delete-files))

  (it "can be done through the agenda and show on the agenda"
    (ogt--add-and-process-single-action "delegateme")
    (ogt--save-all-buffers)
    (org-gtd-daily-agenda)
    (with-current-buffer org-agenda-buffer
      (beginning-of-buffer)
      (search-forward "delegateme")
      (with-simulated-input "That SPC Guy RET RET"
        (org-gtd-agenda-delegate)))

    (ogt--save-all-buffers)
    (org-gtd-daily-agenda)
    (with-current-buffer org-agenda-buffer
      (expect (buffer-string) :to-match "WAIT ")
      (expect (buffer-string) :to-match "That Guy"))))
