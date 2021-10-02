;; -*- lexical-binding: t; -*-

(load "test/helpers.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)
(require 'f)

(describe
 "Project management"

 :var ((org-gtd-directory
        (f-join (f-dirname (f-this-file)) "runtime-file-path")))

 (before-each
  (ogt--clean-target-directory org-gtd-directory)
  (org-gtd-find-or-create-and-save-files)
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
  (setq org-agenda-files `(,org-gtd-directory)
        org-capture-templates `(("i" "GTD item"
                                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)))
  (ogt--add-and-process-project "project headline"))

 (after-each
  (mapcar (lambda (buffer)
            (with-current-buffer buffer (kill-buffer)))
          (org-gtd-find-or-create-and-save-files))
  (ogt--reset-variables))

 (it "gets a list of the task states"
     (with-current-buffer "actionable.org"
       (beginning-of-buffer)
       (search-forward "project headline")
       (let ((task-states (org-gtd--current-project-states)))
         (expect task-states :to-equal '("NEXT" "TODO" "TODO")))))

 (it "recognizes a project as canceled if the last task is canceled"
     (let ((last-one '("DONE" "DONE" "CANCELED"))
           (last-two '("DONE" "CANCELED" "CANCELED"))
           (only-random-task '("DONE" "CANCELED" "NEXT")))
       (expect (org-gtd--project-canceled-p last-one) :to-equal t)
       (expect (org-gtd--project-canceled-p last-two) :to-equal t)
       (expect (org-gtd--project-canceled-p only-random-task) :to-equal nil)))

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

 (it "marks all undone tasks of a canceled project as canceled"
     (with-current-buffer (org-gtd--actionable-file)
       (beginning-of-buffer)
       (search-forward "project headline")
       (org-gtd-cancel-project)
       (org-gtd-archive-complete-projects)
       (save-buffer))
     (let ((archived-projects (ogt--archived-projects-buffer-string)))
       (expect archived-projects :to-match ".*project headline.*"))))
