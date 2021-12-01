;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe "When refiling a project"

  (before-each
    (ogt--configure-emacs)
    (ogt--prepare-filesystem)
    (ogt--add-and-process-project "project headline"))

  (after-each (ogt--close-and-delete-files))

  (it "skips refiling choice if option is enabled"
    (let ((org-gtd-refile-to-any-target t)
          (temp-buffer (find-file-noselect (make-temp-file "foo"))))

      (with-current-buffer temp-buffer
        (insert "* foobar")
        (org-gtd-refile--do org-gtd-projects))

      (with-current-buffer (org-gtd--default-projects-file)
        (expect (buffer-string) :to-match "foobar"))))

  (describe "Finding a refile target"

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

  (describe "And I have multiple files as possible targets"

    (it "offers refiling targets"
      (let ((new-buffer (create-additional-project-target "more-projects"))
            (temp-buffer (find-file "/tmp/test.org")))

        (with-current-buffer temp-buffer
          (insert "* choose-refile-target")
          (point-min)
          (with-simulated-input
              "AdditionalHeading RET"
            (org-gtd-refile--do org-gtd-projects)))

        (expect (with-current-buffer new-buffer (buffer-string))
                :to-match
                "choose-refile-target")

        (ogt--clear-file-and-buffer new-buffer)
        (ogt--clear-file-and-buffer temp-buffer)))))
