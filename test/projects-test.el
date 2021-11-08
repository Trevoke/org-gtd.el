;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management"

 (before-all (ogt--configure-emacs))

 (before-each
  (ogt--prepare-filesystem)
  (ogt--add-and-process-project "project headline"))

 (after-each (ogt--close-and-delete-files))

 (it "gets a list of the task states"
     (with-current-buffer (org-gtd--default-projects-file)
       (beginning-of-buffer)
       (search-forward "project headline")
       (let ((task-states (org-gtd--current-project-states)))
         (expect task-states :to-equal '("NEXT" "TODO" "TODO")))))

 (describe
  "canceling projects"

  (it "recognizes a project as canceled if the last task is canceled"
      (let ((last-one '("DONE" "DONE" "CNCL"))
            (last-two '("DONE" "CNCL" "CNCL"))
            (only-random-task '("DONE" "CNCL" "NEXT")))
        (expect (org-gtd--project-canceled-p last-one)
                :to-equal t)
        (expect (org-gtd--project-canceled-p last-two)
                :to-equal t)
        (expect (org-gtd--project-canceled-p only-random-task)
                :to-equal nil)))

  (it "marks all undone tasks of a canceled project as canceled"
      (with-current-buffer (org-gtd--default-projects-file)
        (beginning-of-buffer)
        (search-forward "project headline")
        (org-gtd-cancel-project)
        (org-gtd-archive-completed-items)
        (save-buffer))
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match ".*project headline.*")))

  (it "marks all undone tasks of a canceled project as canceled through the agenda"
      (org-gtd-show-all-next)
      (with-current-buffer ogt--agenda-buffer
        (beginning-of-buffer)
        (search-forward "Task 1")
        (org-gtd-agenda-cancel-project)
        (org-gtd-archive-completed-items))
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match ".*project headline.*")))))
