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
*** CNCL Task 2
*** CNCL Task 3")

(defconst ogt--agenda-buffer "*Org Agenda*")

(defun ogt--prepare-filesystem-and-configure-emacs ()
  "run before each test"
  (setq org-gtd-directory (f-join default-directory "test"  "runtime-file-path"))
  (ogt--clean-target-directory org-gtd-directory)
  (org-gtd-find-or-create-and-save-files)
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
  (setq org-agenda-files `(,org-gtd-directory)
        org-capture-templates `(("i" "GTD item"
                                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t))))

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(defun ogt--get-string-from-buffer (buffer)
  "Return buffer's content."
  (with-current-buffer buffer
    (buffer-string)))

(defun ogt--archived-projects-buffer-string ()
  "return string of items archived from actionable file"
  (ogt--get-string-from-buffer (org-gtd--actionable-archive)))

(defun ogt--reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun ogt--reset-variables ()
  "Remove all customizations related to org-gtd"
  (dolist (var
           '(org-agenda-files
             org-gtd-process-item-hooks
             org-capture-templates
             org-refile-targets))
    (ogt--reset-var var)))

(defun ogt--close-and-delete-files ()
  "Run after every test to clear external state"
  (org-gtd-show-all-next)
  (kill-buffer ogt--agenda-buffer)
  (mapcar (lambda (buffer)
            (let ((filename (buffer-file-name buffer)))
              (with-current-buffer buffer (save-buffer))
            (kill-buffer buffer)
            (delete-file filename)))
          `(,(org-gtd--actionable-archive)
            ,(org-gtd--actionable-file)
            ,(org-gtd--inbox-file)
            ,(org-gtd--incubate-file))))

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
      ("p" "M-> RET" (insert ogt--project-text) "C-c c RET ORG_GTD_PROJECTS RET")
    (org-gtd-process-inbox))
  (org-gtd-find-or-create-and-save-files))
