;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Reviews"

 (before-each (ogt--configure-emacs)
              (add-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "Areas of focus"
  (xit "prints an error if called programmatically with an area not in the list"
      ;(kill-buffer "*Messages*")
      (org-gtd-review-area-of-focus "Playing")
      (message (current-message))
      (ogt--print-buffer-list)
      (expect (buffer-name) :to-equal "*scratch*")

      (expect                           ;(current-message)
       (ogt--buffer-string "*Messages*")
       :to-match
       (format "`Playing' is not a member of (Health Home Career)"))
      )))
