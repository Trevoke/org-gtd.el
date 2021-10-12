;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management"

 (before-each
  (ogt--prepare-filesystem-and-configure-emacs)
  (ogt--add-and-process-project "project headline"))

 (after-each
  (ogt--close-and-delete-files)
  (ogt--reset-variables))

 (it "gets a list of the task states"
     (with-current-buffer "actionable.org"
       (beginning-of-buffer)
       (search-forward "project headline")
       (let ((task-states (org-gtd--current-project-states)))
         (expect task-states :to-equal '("NEXT" "TODO" "TODO")))))

 (it "archives completed and canceled projects"
     (with-current-buffer (org-gtd--actionable-file)
       (beginning-of-buffer)
       (search-forward "project headline")
       (beginning-of-line)
       (newline)
       (previous-line)
       (insert ogt--canceled-project)
       (newline)
       (insert ogt--completed-project)
       (save-buffer))
     (org-gtd-archive-complete-projects)
     (let ((archived-projects (ogt--archived-projects-buffer-string)))
       (expect archived-projects :to-match ".*completed.*")
       (expect archived-projects :to-match ".*canceled.*")))

 (describe
  "processing project"

  (it
   "offers refiling targets"
   (let ((new-buffer (find-file (f-join org-gtd-directory "more-projects.org"))))
     (with-current-buffer new-buffer
       (insert "* ORG_GTD_PROJECTS")
       (save-buffer))
     (with-temp-buffer
       (insert "* choose-refile-target")
       (with-simulated-input "more-projects.org RET"
                             (org-gtd--refile-project)))
     (expect (with-current-buffer new-buffer (buffer-string))
             :to-match
             ".*choose-refile-target.*"))
   (with-current-buffer new-buffer
     (delete-file (buffer-file-name))
     (kill-buffer))
   ))

 (describe
  "canceling projects"
  (it "recognizes a project as canceled if the last task is canceled"
      (let ((last-one '("DONE" "DONE" "CNCL"))
            (last-two '("DONE" "CNCL" "CNCL"))
            (only-random-task '("DONE" "CNCL" "NEXT")))
        (expect (org-gtd--project-canceled-p last-one) :to-equal t)
        (expect (org-gtd--project-canceled-p last-two) :to-equal t)
        (expect (org-gtd--project-canceled-p only-random-task) :to-equal nil)))
  (it "marks all undone tasks of a canceled project as canceled"
      (with-current-buffer (org-gtd--actionable-file)
        (beginning-of-buffer)
        (search-forward "project headline")
        (org-gtd-cancel-project)
        (org-gtd-archive-complete-projects)
        (save-buffer))
      (let ((archived-projects (ogt--archived-projects-buffer-string)))
        (expect archived-projects :to-match ".*project headline.*")))

  (it "marks all undone tasks of a canceled project as canceled through the agenda"
      (org-gtd-show-all-next)
      (with-current-buffer ogt--agenda-buffer
        (beginning-of-buffer)
        (search-forward "Task 1")
        (org-gtd-agenda-cancel-project)
        (org-gtd-archive-complete-projects))
      (let ((archived-projects (ogt--archived-projects-buffer-string)))
        (expect archived-projects :to-match ".*project headline.*")))
  ))
