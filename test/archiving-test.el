;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "archiving"

 (before-all (ogt--configure-emacs))

 (before-each (ogt--prepare-filesystem))
 (after-each (ogt--close-and-delete-files))

 (describe
  "finished work"

  (it "archives completed and canceled projects"
      (ogt--add-and-process-project "project headline")
      (with-current-buffer (org-gtd--default-projects-file)
        (end-of-buffer)
        (newline)
        (insert ogt--canceled-project)
        (end-of-buffer)
        (newline)
        (insert ogt--completed-project)
        (save-buffer))
      (org-gtd-archive-completed-items)
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "completed")
        (expect archived-projects :to-match "canceled"))))

 (it "on a single action"
     (ogt--add-and-process-single-action "one")
     (ogt--save-all-buffers)
     (ogt--add-and-process-single-action "two")
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-action-file)
       (beginning-of-buffer)
       (search-forward "NEXT one")
       (org-todo "DONE"))
     (org-gtd-archive-completed-items)
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-action-file)
       (expect (buffer-string)
               :to-match
               "two")
       (expect (buffer-string)
               :not :to-match
               " DONE one")))

 (it "does not archive repeating scheduled items"
     (let* ((temporary-file-directory org-gtd-directory)
           (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (with-current-buffer (find-file-noselect gtd-file)
         (expect (buffer-string) :to-match "repeating item")
         (expect (buffer-string) :not :to-match "write a nice test"))))

 (it "does not archive undone incubated items"
     (let* ((temporary-file-directory org-gtd-directory)
           (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (with-current-buffer (find-file-noselect gtd-file)
         (expect (buffer-string) :to-match "For later")
         (expect (buffer-string) :not :to-match "not worth thinking about")))))
