;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Reviews"

 :var ((inhibit-message t)
       (org-gtd-areas-of-focus '("Health" "Home" "Career")))

 (before-each (ogt--configure-emacs)
              (add-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus))

 (describe
  "Areas of focus"
  (it "throws an error if called programmatically with an area not in the list"
      (expect
       (org-gtd-review-area-of-focus "Playing")
       :to-throw
       'org-gtd-invalid-area-of-focus))

  (it "shows projects, next actions, habits, incubated items in agenda for a specific area of focus"
      ()
      )))
