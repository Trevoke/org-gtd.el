;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "When I refile a project"

 (before-all (ogt--configure-emacs))

 (before-each
  (ogt--prepare-filesystem)
  (ogt--add-and-process-project "project headline"))

 (after-each (ogt--close-and-delete-files))

 (describe
  "And I have multiple files as possible targets"

  (it "offers refiling targets"
      (let ((new-buffer (create-additional-project-target-in-org-gtd-directory "more-projects"))
            (temp-buffer (find-file "/tmp/test.org")))

        (with-current-buffer temp-buffer
          (insert "* choose-refile-target")
          (point-min)
          (with-simulated-input
           "AdditionalHeading RET"
           (org-gtd--refile-project)))

        (expect (with-current-buffer new-buffer (buffer-string))
                :to-match
                ".*choose-refile-target.*")

        (ogt--clear-file-and-buffer new-buffer)
        (ogt--clear-file-and-buffer temp-buffer)))))
