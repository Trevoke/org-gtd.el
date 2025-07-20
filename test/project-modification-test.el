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
      (ogt-capture-and-process-addition-to-project
       "Task 0"
       "project SPC headline TAB RET")

      (org-gtd-engage)
      (with-current-buffer org-agenda-this-buffer-name
        (goto-char (point-min))

        (search-forward "Task 0")
        (expect (ogt--current-buffer-raw-text) :not :to-match "Task 1")))

  (xit "keeps sanity of TODO states in modified project"
      (let* ((temporary-file-directory org-gtd-directory)
             (gtd-file-buffer (ogt--temp-org-file-buffer "foo" (org-file-contents "test/fixtures/gtd-file.org"))))
        (org-gtd-core-prepare-buffer gtd-file-buffer)

        (ogt-capture-and-process-addition-to-project
         "Task 0"
         "Projects/addtaskhere TAB RET")

        (with-current-buffer gtd-file-buffer
          (goto-char (point-min))

          (search-forward "NEXT Task 0")
          (search-forward "DONE finished task")
          (search-forward "TODO initial next task")
          (search-forward "TODO initial last task")
          (goto-char (point-min))

          (search-forward "NEXT Task 0")
          (org-todo 'done)
          (goto-char (point-min))
          (search-forward "DONE Task 0")
          (search-forward "DONE finished task")
          (search-forward "NEXT initial next task")
          (search-forward "TODO initial last task")))))

 (describe
  "when org-reverse-note-order is nil"

  (before-each (setq org-reverse-note-order nil))
  (after-each (ogt--reset-var 'org-reverse-note-order))

  (it "as the last NEXT task"
      (ogt-capture-and-process-project "project headline")

      (ogt-capture-and-process-addition-to-project
       "Task 0"
       "project SPC headline TAB RET")

      (org-gtd-engage)
      (with-current-buffer org-agenda-this-buffer-name
        (goto-char (point-min))

        (expect (ogt--current-buffer-raw-text) :not :to-match "Task 0")
        (search-forward "Task 1"))))

 (xit "keeps sanity of TODO states in modified project"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file-buffer (ogt--temp-org-file-buffer "foo" (org-file-contents "test/fixtures/gtd-file.org"))))

       (org-gtd-core-prepare-buffer gtd-file-buffer)

       (ogt-capture-and-process-addition-to-project
        "Task 0"
        "Projects/addtaskhere TAB RET")

       (with-current-buffer gtd-file-buffer
         (search-forward "addtaskhere")
         (org-narrow-to-subtree)
         (search-forward "DONE finished task")
         (search-forward "NEXT initial next task")
         (search-forward "TODO initial last task")
         (search-forward "TODO Task 0")

         (goto-char (point-min))
         (search-forward "NEXT")
         (org-entry-put (org-gtd-projects--org-element-pom (org-element-at-point)) "TODO" "DONE")
         (goto-char (point-min))
         (search-forward "DONE finished task")
         (search-forward "DONE initial next task")
         (search-forward "NEXT initial last task")
         (search-forward "TODO Task 0")))))
