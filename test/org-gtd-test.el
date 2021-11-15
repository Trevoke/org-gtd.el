;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Org GTD"

 (before-all (ogt--configure-emacs))

 (before-each (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (describe
  "Processing items"

  (before-each (ogt--add-single-item))

  (it "uses configurable decorations on the processed items"
      (let ((org-gtd-process-item-hooks '(org-set-tags-command org-priority)))
        (with-simulated-input "s C-c c RET A TAB RET" (org-gtd-process-inbox)))

      (org-gtd-show-all-next)
      (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
        (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
                :to-be-truthy)))

  (it "shows item in agenda when done"
      (with-simulated-input "s C-c c RET TAB RET" (org-gtd-process-inbox))
      (expect (buffer-modified-p (org-gtd--default-action-file)) :to-equal t)

      (org-gtd-show-all-next)

      (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
        (expect (string-match "single action" ogt-agenda-string)
                :to-be-truthy)))))
