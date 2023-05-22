;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Refiling"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "A project"

  (before-each
   (ogt-capture-and-process-project "project headline")
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
          (expect (ogt--current-buffer-raw-text) :to-match "foobar"))

        (kill-buffer temp-buffer)))

  (describe
   "finding a refile target"
   (before-each (setq org-gtd-refile-to-any-target nil))

   (it "finds the Project target"
       (let ((targets (caar (with-org-gtd-refile org-gtd-projects
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

         (expect (with-current-buffer new-buffer (ogt--current-buffer-raw-text))
                 :to-match
                 "choose-refile-target")

         (ogt--clear-file-and-buffer new-buffer)
         (kill-buffer temp-buffer)))))

 (describe
  "a new task in a project"

  (it "finds targets in and out of org-gtd-directory"
                                        ;(ogt-capture-and-process-addition-to-project "new task" "Project RET")
      (let* ((ogt-agenda-dir (make-temp-file "org-agenda-dir" t))
             (org-agenda-files `(,ogt-agenda-dir))
             (org-gtd-refile-to-any-target nil)
             (inside-file (find-file-noselect (f-join org-gtd-directory "inside.org")))
             (outside-file (find-file-noselect (f-join ogt-agenda-dir "outside.org"))))

        (with-current-buffer inside-file
          (insert org-gtd-projects-template)
          (goto-char (point-max))
          (insert "\n** Project\n*** Task 1\n*** Task 2")
          (basic-save-buffer))
        (with-current-buffer outside-file
          (insert org-gtd-projects-template)
          (goto-char (point-max))
          (insert "\n** Project\n*** Task 1\n*** Task 2")
          (basic-save-buffer))

        (expect (sort (mapcar #'car (org-gtd-refile--project-targets))
                      #'string-lessp)
                :to-equal
                '("inside.org/Project" "outside.org/Project"))

        ))))
