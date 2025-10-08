;; -*- lexical-binding: t; coding: utf-8 -*-

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
           (expect timestamp :to-match (format "%s-%#02d-%#02d" year month day))))))

 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (create-delegated-item "TASK DESC" "Someone" date)
        (expect (file-contains? (org-gtd--default-file)
                               (format "<%s-%#02d-%#02d>" year month day))
                :to-be-truthy)))))

(describe
 "Customizing delegation input"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "allows users to provide custom input functions for person name"
     (let ((topic "Custom delegate test")
           (checkin-date (format-time-string "%Y-%m-%d"))
           (buffer (generate-new-buffer "Test custom delegate"))
           (org-id-overriding-file-name "org-gtd"))

       (with-current-buffer buffer
         (org-mode)
         (insert (format "* %s" topic))
         (org-gtd-clarify-item)

         ;; Use org-gtd-delegate-item-at-point with parameters to show it works non-interactively
         (org-gtd-delegate-item-at-point "Custom Person" checkin-date)

         ;; Verify the delegation was set up correctly
         (expect (task-delegated-to (current-task)) :to-equal "Custom Person")
         (expect (task-timestamp (current-task)) :to-equal (format "<%s>" checkin-date)))

       (kill-buffer buffer))))
