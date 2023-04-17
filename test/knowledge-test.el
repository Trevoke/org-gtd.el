;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(defun ogt-add-knowledge-item (label)
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "C-c c k")))

(describe
 "Processing a knowledge item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "through the inbox, moves the task to the archive file"
     (let* ((timestamp (decode-time))
            (year (nth 5 timestamp))
            (month (nth 4 timestamp))
            (day (nth 3 timestamp)))
       (ogt-add-knowledge-item "Yowza")
       (with-current-buffer (ogt--archive)
         (expect (buffer-string) :to-match "Yowza")))))
