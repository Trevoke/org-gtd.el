;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Modifying a project"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "when org-reverse-note-order is t"

  (before-each (setq org-reverse-note-order t))
  (after-each (ogt--reset-var 'org-reverse-note-order))

  (it "as the first NEXT task"
      (ogt-capture-and-process-project "project headline")
      (ogt-capture-and-process-addition-to-project "Task 0" "project SPC headline TAB RET")
      (org-gtd-engage)
      (with-current-buffer org-agenda-this-buffer-name
        (expect (ogt--current-buffer-raw-text) :to-match "Task 0")
        (expect (ogt--current-buffer-raw-text) :not :to-match "Task 1")))

  (it "keeps sanity of TODO states in modified project"
      (let* ((temporary-file-directory org-gtd-directory)
             (gtd-file-buffer (ogt--temp-org-file-buffer "foo" (org-file-contents "test/fixtures/gtd-file.org"))))
        (ogt-capture-and-process-addition-to-project "Task 0" "[1/3] SPC addtaskhere RET")
        (with-current-buffer gtd-file-buffer
          (expect (ogt--current-buffer-raw-text) :to-match "NEXT Task 0")
          (expect (ogt--current-buffer-raw-text) :to-match "DONE finished task")
          (expect (ogt--current-buffer-raw-text) :to-match "TODO initial next task")
          (expect (ogt--current-buffer-raw-text) :to-match "TODO initial last task")

          (search-forward "NEXT Task 0")
          (org-todo 'done)
          (expect (ogt--current-buffer-raw-text) :to-match "DONE Task 0")
          (expect (ogt--current-buffer-raw-text) :to-match "DONE finished task")
          (expect (ogt--current-buffer-raw-text) :to-match "NEXT initial next task")
          (expect (ogt--current-buffer-raw-text) :to-match "TODO initial last task")))))

 (describe
  "when org-reverse-note-order is nil"

  (before-each (setq org-reverse-note-order nil))
  (after-each (ogt--reset-var 'org-reverse-note-order))

  (it "as the last NEXT task"
      (ogt-capture-and-process-project "project headline")
      (ogt-capture-and-process-addition-to-project "Task 0" "project SPC headline TAB RET")
      (org-gtd-engage)
      (with-current-buffer org-agenda-this-buffer-name
        (expect (ogt--current-buffer-raw-text) :not :to-match "Task 0")
        (expect (ogt--current-buffer-raw-text) :to-match "Task 1"))))

 (it "keeps sanity of TODO states in modified project"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (ogt-capture-and-process-addition-to-project "Task 0" "[1/3] SPC addtaskhere RET")

       (with-current-buffer (find-file-noselect gtd-file)
         (search-forward "addtaskhere")
         (org-narrow-to-subtree)
         (expect (ogt--current-buffer-raw-text) :to-match "DONE finished task")
         (expect (ogt--current-buffer-raw-text) :to-match "NEXT initial next task")
         (expect (ogt--current-buffer-raw-text) :to-match "TODO initial last task")
         (expect (ogt--current-buffer-raw-text) :to-match "TODO Task 0")

         (search-forward "NEXT")
         (org-entry-put (org-gtd-projects--org-element-pom (org-element-at-point)) "TODO" "DONE")

         (expect (ogt--current-buffer-raw-text) :to-match "DONE finished task")
         (expect (ogt--current-buffer-raw-text) :to-match "DONE initial next task")
         (expect (ogt--current-buffer-raw-text) :to-match "NEXT initial last task")
         (expect (ogt--current-buffer-raw-text) :to-match "TODO Task 0")))))
