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

(defconst ogt--base-project-heading
  "* AdditionalHeading
:PROPERTIES:
:ORG_GTD: Projects
:END:
")

(defun ogt--configure-emacs ()
  (setq org-gtd-directory (f-join default-directory "test"  "runtime-file-path"))
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
  (setq org-agenda-files `(,org-gtd-directory)))

(defun create-additional-project-target (filename)
  (let* ((file (f-join org-gtd-directory (format "%s.org" filename)))
         (buffer (find-file file)))
    (with-current-buffer buffer
      (insert ogt--base-project-heading)
      (save-buffer))
    buffer))

(defun ogt--prepare-filesystem ()
  "run before each test"
  (ogt--clean-target-directory org-gtd-directory))

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

(defun ogt--close-and-delete-files ()
  "Run after every test to clear external state"
  (org-gtd-show-all-next)
  (kill-buffer ogt--agenda-buffer)
  (mapcar (lambda (buffer)
            (ogt--clear-file-and-buffer buffer))
          `(,(ogt--default-projects-archive)
            ,(org-gtd--default-action-file)
            ,(org-gtd--default-delegated-file)
            ,(org-gtd--default-scheduled-file)
            ,(org-gtd--inbox-file)
            ,(org-gtd--default-incubated-file))))

(defun ogt--clear-file-and-buffer (buffer)
  (if (bufferp buffer)
      (let ((filename (buffer-file-name buffer)))
        (with-current-buffer buffer (save-buffer))
        (kill-buffer buffer)
        (delete-file filename))))

(defun ogt--default-projects-archive ()
  "Create or return the buffer to the archive file for the actionable items."
  (let* ((filename (string-join `(,(buffer-file-name (org-gtd--default-projects-file)) "archive") "_"))
        (archive-file (f-join org-gtd-directory filename)))
    (find-file archive-file)))

(defun ogt--archived-projects-buffer-string ()
  "return string of items archived from actionable file"
  (ogt--get-string-from-buffer (ogt--default-projects-archive)))

(defun ogt--save-all-buffers ()
  (with-simulated-input "!" (save-some-buffers)))

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
   ("p" "M-> RET" (insert ogt--project-text) "C-c c RET TAB RET")
   (org-gtd-process-inbox)))

(defun ogt--add-and-process-scheduled-item (label)
  "LABEL is the scheduled label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "c C-c c RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-delegated-item (label)
  "LABEL is the delegated label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "d C-c c RET Someone RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-incubated-item (label)
  "LABEL is the incubated label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "i C-c c RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-single-action (label)
  "LABEL is the single action label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "s C-c c RET TAB RET"
                        (org-gtd-process-inbox)))
