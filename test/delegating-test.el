;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "delegating a task"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "appears in daily agenda with WAIT state after delegation"
     (org-gtd-delegate-create "Talk to university"
                              "Favorite student"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (expect (agenda-contains? "Talk to university") :to-be-truthy))

 (it "can be done through the agenda and show on the agenda"
     (create-single-action "delegateme")
     (ogt--save-all-buffers)
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (goto-char (point-min))
       (search-forward "delegateme")
       (with-simulated-input "That SPC Guy RET RET"
                             (org-gtd-delegate-agenda-item)))

     (ogt--save-all-buffers)
     (org-gtd-engage)
     (expect (agenda-contains? "WAIT ") :to-be-truthy)
     (expect (agenda-contains? "That Guy") :to-be-truthy)))

(describe
 "A delegated item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "stores check-in date in ORG_GTD_TIMESTAMP property to enable follow-up"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (create-delegated-item "TASK DESC" "Someone" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "TASK DESC")
         (let ((timestamp (task-timestamp (current-task))))
           (expect timestamp :to-match (format "%s-%#02d-%#02d" year month day)))))))

(describe
 "Customizing delegation input"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "allows delegation without refiling via skip-refile"
     (let ((topic "Custom delegate test")
           (checkin-date (format-time-string "%Y-%m-%d")))
       (create-single-action topic)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward topic)
         (org-back-to-heading t)
         (let ((original-pos (point-marker)))
           ;; Clarify with skip-refile
           (let ((current-prefix-arg '(4)))
             (org-gtd-clarify-item))
           ;; Delegate via organize menu
           (with-current-buffer (car (org-gtd-wip--get-buffers))
             (with-simulated-input "Custom SPC Person RET RET"
               (org-gtd-delegate)))
           ;; Verify delegation was set up and item wasn't refiled
           (goto-char (point-min))
           (search-forward topic)
           (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
           (expect (org-entry-get (point) "DELEGATED_TO") :to-equal "Custom Person"))))))
