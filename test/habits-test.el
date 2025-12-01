;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; "appears in daily agenda after creation with recurring schedule" migrated to
;; test-eunit/acceptance/basic-workflows-test.el (habit-item-programmatic-create)

;; Remaining tests (not yet migrated):

(describe
 "A habit"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "is formatted like org-mode wants"
     (let* ((repeater "++1m"))
       (create-habit "Yowza" repeater)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (task-property (current-task) "STYLE") :to-equal "habit")
         (expect (task-property (current-task) "SCHEDULED") :to-match (format "%s" repeater))))))
