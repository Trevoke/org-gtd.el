;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing items"

 (before-all (ogt--configure-emacs))

 (before-each (ogt--prepare-filesystem)
              (ogt--add-single-item))
 (after-each (ogt--close-and-delete-files))

 (it "processes all the elements"
     (dotimes (x 9)
       (ogt--add-single-item (format "single action %s" x)))

     (with-simulated-input
         ("p" "M-> RET" (insert ogt--project-text) "C-c c TAB RET"
          "c C-c c RET TAB RET"
          "d C-c c RET Someone RET TAB RET"
          "i C-c c RET TAB RET"
          "d C-c c RET Someone RET TAB RET"
          "s C-c c TAB RET"
          "p" "M-> RET" (insert ogt--project-text) "C-c c TAB RET"
          "s C-c c TAB RET"
          "c C-c c RET TAB RET"
          "i C-c c RET TAB RET")
         (org-gtd-process-inbox))

     (ogt--save-all-buffers)

     (dotimes (x 9)
       (ogt--add-single-item (format "single action %s" x)))

     (with-simulated-input
         ("p" "M-> RET" (insert ogt--project-text) "C-c c TAB RET"
          "c C-c c RET TAB RET"
          "d C-c c RET Someone RET TAB RET"
          "d C-c c RET Someone RET TAB RET"
          "s C-c c TAB RET"
          "p" "M-> RET" (insert ogt--project-text) "C-c c TAB RET"
          "s C-c c TAB RET"
          "c C-c c RET TAB RET"
          "i C-c c RET TAB RET")
         (org-gtd-process-inbox))

     (with-current-buffer (org-gtd--inbox-file)
       (expect (buffer-string)
               :not :to-match
               "single action"))
     )

 (it "uses configurable decorations on the processed items"
     (let ((org-gtd-process-item-hooks '(org-set-tags-command org-priority)))
       (with-simulated-input "s C-c c RET A TAB RET" (org-gtd-process-inbox)))

     (org-gtd-show-all-next)
     (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
       (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
               :to-be-truthy)))

 (it "shows item in agenda when done"
     (with-simulated-input "s C-c c TAB RET" (org-gtd-process-inbox))
     (expect (buffer-modified-p (org-gtd--default-action-file)) :to-equal t)

     (org-gtd-show-all-next)

     (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
       (expect (string-match "single action" ogt-agenda-string)
               :to-be-truthy))))
