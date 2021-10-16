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
                :to-be-truthy))))

 (describe
  "Finding a refile target"

  (it "finds the Actions heading in the actionable file"
      (org-gtd-show-all-next)
      (expect (car (org-gtd--refile-target org-gtd-actions))
              :to-equal
              "Actions"))

  (it "finds the Incubate headings in the incubate file"
      (org-gtd-show-all-next)
      (with-current-buffer (org-gtd--incubate-file)
        (goto-char (point-max))
        (insert "* To Read\n* To Eat\n")
        (save-buffer))
      (setq org-refile-targets (org-gtd--refile-incubate-targets))
      (let ((ogt-target-names (mapcar 'car (org-refile-get-targets))))
        (expect ogt-target-names
                :to-have-same-items-as
                '("Auto-generated incubate headline" "To Eat" "To Read"))))))
