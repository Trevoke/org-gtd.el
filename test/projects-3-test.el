(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management 3"

 (before-each
  (ogt--configure-emacs)
  (ogt--prepare-filesystem))

 (after-each (ogt--close-and-delete-files))

 (it
  "keeps sanity of TODO states in modified project"
  (let* ((temporary-file-directory org-gtd-directory)
         (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
    (ogt--add-single-item "Task 0")
    (org-gtd-process-inbox)
    (with-simulated-input "[1/3] SPC addtaskhere RET"
                          (org-gtd--modify-project))

    (with-current-buffer (find-file-noselect gtd-file)
      (expect (buffer-string) :to-match "NEXT Task 0")
      (expect (buffer-string) :to-match "DONE finished task")
      (expect (buffer-string) :to-match "TODO initial next task")
      (expect (buffer-string) :to-match "TODO initial last task")

      (search-forward "NEXT Task 0")
      (org-todo 'done)
      (expect (buffer-string) :to-match "DONE Task 0")
      (expect (buffer-string) :to-match "DONE finished task")
      (expect (buffer-string) :to-match "NEXT initial next task")
      (expect (buffer-string) :to-match "TODO initial last task")))))
