;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe "Project management"

  (before-each
    (ogt--configure-emacs)
    (ogt--prepare-filesystem)
    (ogt--add-and-process-project "project headline"))

  (after-each (ogt--close-and-delete-files))

  (describe "marks all undone tasks of a canceled project as canceled"

    (it "when on the heading"
      (with-current-buffer (org-gtd--default-file)
        (beginning-of-buffer)
        (search-forward "project headline")
        (org-gtd-cancel-project)
        (org-gtd-archive-completed-items)
        (basic-save-buffer))
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "project headline")))

    (it "on a task in the agenda"
      (org-gtd-daily-agenda)
      (with-current-buffer org-agenda-buffer
        (beginning-of-buffer)
        (search-forward "Task 1")
        (org-gtd-agenda-cancel-project)
        (org-gtd-archive-completed-items))
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "project headline")))))
