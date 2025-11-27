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
  "merging user's refile targets"

  (it "includes user's org-refile-targets in the target list"
      (create-project "project headline")
      (with-current-buffer (org-gtd--default-file)
        (basic-save-buffer))
      ;; Create a user refile target file OUTSIDE the GTD directory
      (let* ((user-file (make-temp-file "user-targets" nil ".org"))
             (user-buffer (find-file-noselect user-file)))
        (with-current-buffer user-buffer
          (erase-buffer)
          (insert "* User Custom Target\n")
          (basic-save-buffer))
        ;; Set user's org-refile-targets to include their file
        (let* ((org-refile-targets `((,user-file :maxlevel . 1)))
               (target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
          ;; Should include both user's targets and org-gtd's
          (expect target-names :to-contain "User Custom Target")
          (expect target-names :to-contain "Projects"))
        (kill-buffer user-buffer)
        (delete-file user-file)))

  (it "places user's targets before org-gtd targets"
      (create-project "project headline")
      (with-current-buffer (org-gtd--default-file)
        (basic-save-buffer))
      ;; Create a user refile target file OUTSIDE the GTD directory
      (let* ((user-file (make-temp-file "user-targets" nil ".org"))
             (user-buffer (find-file-noselect user-file)))
        (with-current-buffer user-buffer
          (erase-buffer)
          (insert "* User Target First\n")
          (basic-save-buffer))
        ;; Set user's org-refile-targets
        (let* ((org-refile-targets `((,user-file :maxlevel . 1)))
               (target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
          ;; User's target should appear before org-gtd's
          (expect (seq-position target-names "User Target First")
                  :to-be-less-than
                  (seq-position target-names "Projects")))
        (kill-buffer user-buffer)
        (delete-file user-file))))

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
       (let ((targets (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
         (expect targets :to-contain "Work Projects")))

   (it "finds the Project target"
       (let ((targets (caar (org-gtd-refile--get-targets org-gtd-projects))))
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
       (let ((ogt-target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-incubate))))
         (expect ogt-target-names
                 :to-have-same-items-as
                 '("To Eat" "To Read")))))

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
