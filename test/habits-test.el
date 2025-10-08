;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A habit"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "appears in daily agenda after creation with recurring schedule"
     (org-gtd-habit-create "Dentist appointment"
                              ".+3m")
     (org-gtd-engage)
     (expect (agenda-contains? "Dentist appointment") :to-be-truthy))


 (it "is formatted like org-mode wants"
     (let* ((repeater "++1m"))
       (create-habit "Yowza" repeater)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (task-property (current-task) "STYLE") :to-equal "habit")
         (expect (task-property (current-task) "SCHEDULED") :to-match (format "%s" repeater))))))
