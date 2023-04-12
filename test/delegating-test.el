;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(defun ogt-add-delegated-item (label to-whom year month day)
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd (format "C-c c d %s RET %s-%s-%s RET" to-whom year month day))))

(describe
 "delegating a task"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "can be done through the agenda and show on the agenda"
     (ogt--add-and-process-single-action "delegateme")
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
 "A calendar item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has a specific property with the active timestamp"
     (let* ((timestamp (decode-time))
            (year (nth 5 timestamp))
            (month (nth 4 timestamp))
            (day (nth 3 timestamp)))
       (ogt-add-delegated-item "TASK DESC" "Someone" year month day)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "TASK DESC")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-match (format "%s-%#02d-%#02d" year month day))
         )))
 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((timestamp (decode-time))
            (year (nth 5 timestamp))
            (month (nth 4 timestamp))
            (day (nth 3 timestamp)))
        (ogt-add-delegated-item "TASK DESC" "Someone" year month day)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "TASK DESC")
         (org-end-of-meta-data t)
         (expect (buffer-substring (point) (point-max))
                 :to-match
                 (format "<%s-%#02d-%#02d>" year month day)))))))
