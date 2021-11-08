;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "When refiling a project"

 (before-all (ogt--configure-emacs))

 (before-each
  (ogt--prepare-filesystem)
  (ogt--add-and-process-project "project headline"))

 (after-each (ogt--close-and-delete-files))

 (describe
  "Finding a refile target"

  (it "finds the Project target"
      (expect (caar (with-org-gtd-refile
                     org-gtd-projects
                     (org-refile-get-targets)))
              :to-equal
              "Projects"))

  (it "finds the Incubate headings in the incubate file"
      (with-current-buffer (org-gtd--default-incubated-file)
        (goto-char (point-max))
        (insert "* To Read
:PROPERTIES:
:ORG_GTD: Incubated
:END:
* To Eat
:PROPERTIES:
:ORG_GTD: Incubated
:END:")
        (save-buffer))
      (with-org-gtd-refile
       org-gtd-incubated
       (let ((ogt-target-names (mapcar 'car (org-refile-get-targets))))
         (expect ogt-target-names
                 :to-have-same-items-as
                 '("Incubate" "To Eat" "To Read"))))))

 (describe
  "And I have multiple files as possible targets"

  (it "offers refiling targets"
      (let ((new-buffer (create-additional-project-target "more-projects"))
            (temp-buffer (find-file "/tmp/test.org")))

        (with-current-buffer temp-buffer
          (insert "* choose-refile-target")
          (point-min)
          (with-simulated-input
           "AdditionalHeading RET"
           (org-gtd--refile org-gtd-projects)))

        (expect (with-current-buffer new-buffer (buffer-string))
                :to-match
                ".*choose-refile-target.*")

        (ogt--clear-file-and-buffer new-buffer)
        (ogt--clear-file-and-buffer temp-buffer)))))
