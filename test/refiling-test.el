;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "When refiling a project"
 (before-each
  (ogt--configure-emacs)
  (ogt-capture-and-process-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer)))

 (after-each (ogt--close-and-delete-files))

 (it "skips refiling choice if option is enabled"
     (let ((org-gtd-refile-to-any-target t)
           (temp-buffer (get-buffer-create (generate-new-buffer-name "wip"))))

       (with-current-buffer temp-buffer
         (org-mode)
         (insert "* foobar")
         (org-gtd--refile org-gtd-projects))

       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text) :to-match "foobar"))

       (kill-buffer temp-buffer)))

 (describe
  "finding a refile target"
  (before-each (setq org-gtd-refile-to-any-target nil))

  (it "finds the Project target"
      (let ((targets (caar (with-org-gtd-refile
                               org-gtd-projects
                             (org-refile-get-targets)))))
        (expect targets :to-equal "Projects")))

  (it "finds the Incubate headings in the incubate file"
      (with-current-buffer (org-gtd--default-file)
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
                  '("To Eat" "To Read"))))))

 (describe
  "And I have multiple files as possible targets"

  (it "offers refiling targets"
      (let ((org-gtd-refile-to-any-target nil)
            (new-buffer (create-additional-project-target "more-projects"))
            (temp-buffer (get-buffer-create (generate-new-buffer-name "wip"))))

        (with-current-buffer temp-buffer
          (org-mode)
          (insert "* choose-refile-target")
          (point-min)
          (with-simulated-input
           "AdditionalHeading RET"
           (org-gtd--refile org-gtd-projects)))

        (expect (with-current-buffer new-buffer (ogt--current-buffer-raw-text))
                :to-match
                "choose-refile-target")

        (ogt--clear-file-and-buffer new-buffer)
        (kill-buffer temp-buffer)))))
