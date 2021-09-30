(defconst ogt--project-text
  "** Task 1
** Task 2
** Task 3")

(defconst ogt--agenda-buffer "*Org Agenda*")
(defconst ogt--next-buffer "*Org Agenda*(t)")

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(defun ogt--get-string-from-buffer (buffer)
  "Return buffer's content."
  (with-current-buffer buffer
    (buffer-string)))

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

(defun ogt--add-and-process-project ()
  (org-gtd-capture nil "i")
  (insert "project headline")
  (org-capture-finalize)
  (with-simulated-input
      ("p" "M-> RET" (insert ogt--project-text) "C-c c RET")
    (org-gtd-process-inbox))
  (org-gtd-find-or-create-and-save-files))
