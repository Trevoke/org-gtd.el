;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
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
        (with-simulated-input "s C-c c RET A RET" (org-gtd-process-inbox)))

      (org-gtd-show-all-next)
      (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
        (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
                :to-be-truthy)))


  (it "shows item in agenda when done"
      (with-simulated-input "s C-c c RET" (org-gtd-process-inbox))
      (expect (buffer-modified-p (org-gtd--actionable-file)) :to-equal nil)

      (org-gtd-show-all-next)

      (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
        (expect (string-match "single action" ogt-agenda-string)
                :to-be-truthy)))))
