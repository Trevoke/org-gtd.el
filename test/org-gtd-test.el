;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)
(require 'f)

(describe
 "Org GTD"
 :var ((org-gtd-directory
        (f-join (f-dirname (f-this-file)) "runtime-file-path")))

 (before-each
  (ogt--clean-target-directory org-gtd-directory)
  (org-gtd-find-or-create-and-save-files)
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
  (setq org-agenda-files `(,org-gtd-directory)
        org-capture-templates `(("i" "GTD item"
                                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t))))

 (after-each
  (mapcar (lambda (buffer)
            (with-current-buffer buffer (kill-buffer)))
          (org-gtd-find-or-create-and-save-files))
  (ogt--reset-variables))

 (describe
  "Processing items"

  (before-each (ogt--add-single-item))

  (it "uses configurable decorations on the processed items"
      (setq org-gtd-process-item-hooks '(org-set-tags-command org-priority))
      (with-simulated-input "s C-c c RET A RET" (org-gtd-process-inbox))

      (org-gtd-show-all-next)
      (setq ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer))

      (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string) :to-be-truthy))


  (it "shows item in agenda when done"
      (with-simulated-input "s C-c c RET" (org-gtd-process-inbox))
      (expect (with-current-buffer (org-gtd--actionable-file) (buffer-modified-p)) :to-equal nil)

      (org-gtd-show-all-next)
      (setq ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer))


      (expect (string-match "single action" ogt-agenda-string) :to-be-truthy)))

 (describe
  "Finding a refile target"

  (it "finds the Actions heading in the actionable file"
      (org-gtd-show-all-next)
      (expect (car (org-gtd--refile-target org-gtd-actions))
              :to-equal "Actions"))

  (it "finds the Incubate headings in the incubate file"
      (org-gtd-show-all-next)
      (with-current-buffer (org-gtd--incubate-file)
        (goto-char (point-max))
        (insert "* To Read\n* To Eat\n")
        (save-buffer))
      (setq org-refile-targets (org-gtd--refile-incubate-targets))
      (setq ogt-target-names (mapcar 'car (org-refile-get-targets)))
      (expect ogt-target-names :to-have-same-items-as '("Auto-generated incubate headline" "To Eat" "To Read")))))
