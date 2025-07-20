;;; -*- lexical-binding: t; -*-
(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)

(describe
 "engage view"
 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "shows cool things"
     ;; TODAY'S THINGS
     ;; (regular agenda?)
     ;;; hourly
     ;;; habits
     ;;; scheduled to start
     ;;; deadlines
     ;; NEXT
     ;; DELEGATED TO CHECK UP ON TODAY
     ))
