;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(setq org-gtd-update-ack "3.0.0")
(load "org-gtd-autoloads")
(load "test/helpers/utils.el")
(load "test/helpers/autoload-setup.el")

(describe
 "autoload management (recursive)"
 (before-each (ogt--prepare-gtd-directory))
 (after-each (ogt--clear-gtd-directory))

 (it "org-gtd-clarify-item"
     (ogt--with-temp-org-buffer
      "* This is the heading to clarify"
      (org-gtd-clarify-item))))
