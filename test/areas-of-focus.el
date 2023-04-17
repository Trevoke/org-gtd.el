;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Areas of focus"

 (before-each (ogt--configure-emacs)
              (add-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-decorate-item-hooks #'org-gtd-areas-of-focus)
             (setq org-gtd-areas-of-focus nil))

 (it "sets the org-mode CATEGORY on clarified item from a customizable list"
     (ogt-capture-single-item "Medical Appointment")
     (org-gtd-process-inbox)
     (execute-kbd-macro (kbd "C-c c s H e a l t h RET"))
     (org-gtd-engage)
     (expect (ogt--buffer-string org-agenda-buffer)
             :to-match
             "Health.*Medical"))

 (it "sets the CATEGORY on item at point from the areas of focus"
     (with-current-buffer (get-buffer-create "temp.org")
       (org-mode)
       (insert "* A heading")
       (with-simulated-input "Health RET"
                             (org-gtd-areas-of-focus))
       (expect (org-entry-get (point) "CATEGORY")
               :to-equal
               "Health")
       (kill-buffer))))
