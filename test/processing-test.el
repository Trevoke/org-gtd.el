;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing items"

  :var ((inhibit-message t))

 (before-each
  (ogt--configure-emacs)
  (ogt-capture-single-item))
 (after-each (ogt--close-and-delete-files))

 (it "processes all the elements"
     (dotimes (x 8)
       (ogt-capture-single-item (format "single action %s" x)))

     (org-gtd-process-inbox)

     (execute-kbd-macro (kbd "M-> RET"))
     (insert ogt--project-text)
     (ogt-clarify-as-project)

     (ogt-clarify-as-calendar-item)
     (ogt-clarify-as-delegated-item "Someone")
     (ogt-clarify-as-incubated-item)
     (ogt-clarify-as-single-action)
     (ogt-clarify-as-knowledge-item)

     (execute-kbd-macro (kbd "M-> RET"))
     (insert ogt--project-text)
     (ogt-clarify-as-project)

     (ogt-clarify-as-calendar-item)

     (dotimes (x 8)
       (ogt-capture-single-item (format "single action %s" x)))

     (org-gtd-process-inbox)

     (execute-kbd-macro (kbd "C-c c i RET"))

     (execute-kbd-macro (kbd "M-> RET"))
     (insert ogt--project-text)
     (ogt-clarify-as-project)

     (ogt-clarify-as-single-action)
     (ogt-clarify-as-single-action)
     (ogt-clarify-as-calendar-item)
     (ogt-clarify-as-delegated-item "Someone")
     (ogt-clarify-as-incubated-item)
     (ogt-clarify-as-single-action)
     (ogt-clarify-as-knowledge-item)

     (with-current-buffer (ogt-inbox-buffer)
       (expect (ogt--current-buffer-raw-text)
               :not :to-match
               "single action")))

 (it "uses configurable decorations on the processed items"
     (let ((org-gtd-organize-hooks '(org-set-tags-command org-priority)))
       (org-gtd-process-inbox)
       (execute-kbd-macro (kbd "C-c c s RET A TAB RET")))

     (org-gtd-engage)
     (let ((ogt-agenda-string (ogt--buffer-string org-agenda-buffer)))
       (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
               :to-be-truthy)))

 (it "shows item in agenda when done"
     (org-gtd-process-inbox)
     (execute-kbd-macro (kbd "C-c c s TAB RET"))
     (expect (buffer-modified-p (org-gtd--default-file)) :to-equal t)

     (org-gtd-engage)
     (let ((ogt-agenda-string (ogt--buffer-string org-agenda-buffer)))
       (expect (string-match "single action" ogt-agenda-string)
               :to-be-truthy)))

 (describe
  "error management"
  (describe
   "when project has incorrect shape"
   (it "tells the user and returns to editing"
       (org-gtd-process-inbox)
       (execute-kbd-macro (kbd "C-c c p RET"))

       (expect (buffer-name) :to-match org-gtd-clarify--prefix)
       (expect (ogt--buffer-string "*Message*") :to-match "** First task")))))
