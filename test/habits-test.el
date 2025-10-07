;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A habit"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "can be added programmatically"
     (org-gtd-habit-create "Dentist appointment"
                              ".+3m")
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "Dentist appointment")))


 (it "is formatted like org-mode wants"
     (let* ((repeater "++1m"))
       (create-habit "Yowza" repeater)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (org-entry-get (point) "STYLE")
                 :to-equal "habit")
         (expect (org-entry-get (point) "SCHEDULED")
                 :to-match (format "%s" repeater))))))
