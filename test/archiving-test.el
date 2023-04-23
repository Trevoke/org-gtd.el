;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "archiving"

 :var ((inhibit-message t))

 (before-each
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "finished work"

  (it "archives completed and canceled projects"
      (ogt-capture-and-process-project "project headline")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-max))
        (newline)
        (insert ogt--canceled-project)
        (goto-char (point-max))
        (newline)
        (insert ogt--completed-project)
        (basic-save-buffer))
      (org-gtd-archive-completed-items)
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "completed")
        (expect archived-projects :to-match "canceled"))))

 (it "on a single action"
     (ogt-capture-and-process-single-action "one")
     (ogt--save-all-buffers)
     (ogt-capture-and-process-single-action "two")
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "NEXT one")
       (org-todo org-gtd-done))
     (org-gtd-archive-completed-items)
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-file)
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
         (expect (ogt--current-buffer-raw-text) :to-match "repeating item")
         (expect (ogt--current-buffer-raw-text) :not :to-match "write a nice test"))))

 (it "does not archive undone incubated items"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (with-current-buffer (find-file-noselect gtd-file)
         (expect (ogt--current-buffer-raw-text) :to-match "For later")
         (expect (ogt--current-buffer-raw-text) :not :to-match "not worth thinking about")))))
