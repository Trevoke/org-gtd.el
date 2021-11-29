;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)


(describe "Refiling"

  (before-all (ogt--configure-emacs))
  (before-each (ogt--prepare-filesystem))
  (after-each (ogt--close-and-delete-files))

  (describe "a project"

    (it "creates/uses a target automatically if it is configured to do so"
      (let ((temp-buffer (find-file-noselect (make-temp-file "foo")))
            (org-gtd-refile-to-any-target t))
        (with-current-buffer temp-buffer
          (insert "* foobar")
          (org-gtd-refile--do org-gtd-projects))

        (with-current-buffer (org-gtd--default-projects-file)
          (expect (buffer-string)
                  :to-match "foobar"))

        (ogt--clear-file-and-buffer temp-buffer)))

    (describe "with multiple files as possible targets"

      (before-each (ogt--add-and-process-project "project headline"))

      (it "offers refiling targets"
        (let ((new-buffer (create-additional-project-target "more-projects"))
              (temp-buffer (find-file-noselect (make-temp-file "foo"))))

          (message "????")

          (with-current-buffer temp-buffer
            (insert "* choose-refile-target")
            ;; (org-gtd-refile--do org-gtd-projects)
            ;; (execute-kbd-macro (kbd "AdditionalHeading RET"))
            (with-simulated-input
                "Add TAB RET"
                ;"AdditionalHeading RET"
              (org-gtd-refile--do org-gtd-projects))
            )

          (message "bar")

          (with-current-buffer new-buffer
            (message "foo")
            (message (buffer-string))
            (expect (buffer-string)
                    :to-match
                    "choose-refile-target"))

          (ogt--clear-file-and-buffer new-buffer)
          (ogt--clear-file-and-buffer temp-buffer)))))

  (describe "Finding a refile target"

    (it "finds the Project target"
      (org-gtd--default-projects-file)
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
        (basic-save-buffer))
      (with-org-gtd-refile
       org-gtd-incubated
       (let ((ogt-target-names (mapcar 'car (org-refile-get-targets))))
         (expect ogt-target-names
                 :to-have-same-items-as
                 '("Incubate" "To Eat" "To Read")))))))
