(defconst ogt--project-text
  "** Task 1
** Task 2
** Task 3")

(defconst ogt--project-to-cancel
  "** cancel me
*** DONE Task 1
*** NEXT Task 2
*** TODO Task 3")

(defconst ogt--completed-project
  "** completed
*** DONE Task 1
*** DONE Task 2
*** DONE Task 3")

(defconst ogt--canceled-project
  "** canceled
*** DONE Task 1
*** CANCELED Task 2
*** CANCELED Task 3")

(defconst ogt--agenda-buffer "*Org Agenda*")

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(defun ogt--get-string-from-buffer (buffer)
  "Return buffer's content."
  (with-current-buffer buffer
    (buffer-string)))

(defun ogt--archived-projects-buffer-string ()
  "return string of items archived from actionable file"
  (let* ((filename (string-join '("actionable.org" "archive") "_"))
         (archive-file (f-join org-gtd-directory filename)))
    (find-file archive-file)
    (with-current-buffer filename
      (save-buffer))
    (ogt--get-string-from-buffer filename)))

(defun ogt--reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun ogt--reset-variables ()
  "Remove all customizations related to org-gtd"
  (dolist (var
           '(org-agenda-files
             org-gtd-process-item-hooks
             org-capture-templates
             org-refile-targets
             target-names))
    (ogt--reset-var var)))

(defun ogt--add-single-item ()
  (org-gtd-capture nil "i")
  (insert "single action")
  (org-capture-finalize))

(defun ogt--add-and-process-project (label)
  "LABEL is the project label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input
      ("p" "M-> RET" (insert ogt--project-text) "C-c c RET")
    (org-gtd-process-inbox))
  (org-gtd-find-or-create-and-save-files))
