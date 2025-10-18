;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Refiling"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "A project"

  (before-each
   (create-project "project headline")
   (with-current-buffer (org-gtd--default-file)
     (basic-save-buffer)))

  (it "skips refiling choice if option is enabled"
      (let ((org-gtd-refile-to-any-target t)
            (temp-buffer (get-buffer-create (generate-new-buffer-name "wip"))))

        (with-current-buffer temp-buffer
          (org-mode)
          (insert "* foobar")
          (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))

        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text) :to-match "foobar"))

        (kill-buffer temp-buffer)))

  (describe
   "finding a refile target"
   (before-each (setq org-gtd-refile-to-any-target nil))

   (it "finds targets marked with ORG_GTD_REFILE property"
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-max))
         (insert "\n* Work Projects
:PROPERTIES:
:ORG_GTD_REFILE: Projects
:END:")
         (save-buffer))
       (let ((targets (mapcar 'car (with-org-gtd-refile org-gtd-projects
                                     (org-refile-get-targets)))))
         (expect targets :to-contain "Work Projects")))

   (it "finds the Project target"
       (let ((targets (caar (with-org-gtd-refile org-gtd-projects
                              (org-refile-get-targets)))))
         (expect targets :to-equal "Projects")))

   (it "finds the Incubate headings in the incubate file"
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-max))
         (insert "* To Read
:PROPERTIES:
:ORG_GTD_REFILE: Incubated
:END:
* To Eat
:PROPERTIES:
:ORG_GTD_REFILE: Incubated
:END:")
         (save-buffer))
       (with-org-gtd-refile org-gtd-incubate
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
            (org-gtd-refile--do org-gtd-projects org-gtd-projects-template)))

         (expect (with-current-buffer new-buffer (current-buffer-raw-text))
                 :to-match
                 "choose-refile-target")

         (ogt--clear-file-and-buffer new-buffer)
         (kill-buffer temp-buffer))))))
