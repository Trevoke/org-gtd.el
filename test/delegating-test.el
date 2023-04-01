;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe "delegating a task"

  (before-each
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "can be done through the agenda and show on the agenda"
    (ogt--add-and-process-single-action "delegateme")
    (ogt--save-all-buffers)
    (org-gtd-engage)
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (search-forward "delegateme")
      (with-simulated-input "That SPC Guy RET RET"
        (org-gtd-agenda-delegate)))

    (ogt--save-all-buffers)
    (org-gtd-engage)
    (with-current-buffer org-agenda-buffer
      (expect (ogt--current-buffer-raw-text) :to-match "WAIT ")
      (expect (ogt--current-buffer-raw-text) :to-match "That Guy"))))
