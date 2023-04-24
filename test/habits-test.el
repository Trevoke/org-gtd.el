;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A habit"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "is formatted like org-mode wants"
     (let* ((repeater "++1m"))
       (ogt-capture-and-process-habit "Yowza" repeater)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (org-entry-get (point) "STYLE")
                 :to-equal "habit")
         (expect (org-entry-get (point) "SCHEDULED")
                 :to-match (format "%s" repeater))))))
