;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "delegating a task"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "can be done programmatically"
     (org-gtd-delegate-create "Talk to university"
                              "Favorite student"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "Talk to university")))

 (it "can be done through the agenda and show on the agenda"
     (ogt-capture-and-process-single-action "delegateme")
     (ogt--save-all-buffers)
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (goto-char (point-min))
       (search-forward "delegateme")
       (with-simulated-input "That SPC Guy RET RET"
                             (org-gtd-agenda-delegate)))

     (ogt--save-all-buffers)
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text) :to-match "WAIT ")
       (expect (ogt--current-buffer-raw-text) :to-match "That Guy"))))

(describe
 "A delegated item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has a specific property with the active timestamp"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (ogt-capture-and-process-delegated-item "TASK DESC" "Someone" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "TASK DESC")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-match (format "%s-%#02d-%#02d" year month day)))))

 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (ogt-capture-and-process-delegated-item "TASK DESC" "Someone" date)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "TASK DESC")
          (org-end-of-meta-data t)
          (expect (buffer-substring (point) (point-max))
                  :to-match
                  (format "<%s-%#02d-%#02d>" year month day)))))))
