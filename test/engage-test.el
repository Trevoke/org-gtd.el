;;; -*- lexical-binding: t; -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe
 "engage view"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "displays comprehensive daily engagement view with scheduled items, habits, next actions, and delegated tasks"
     ;; TODAY'S THINGS
     ;; (regular agenda?)
     ;;; hourly
     ;;; habits
     ;;; scheduled to start
     ;;; deadlines
     ;; NEXT
     ;; DELEGATED TO CHECK UP ON TODAY
     ))
