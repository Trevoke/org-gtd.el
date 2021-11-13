(load "test/helpers/project.el")
(load "test/helpers/processing.el")

(defconst ogt--agenda-buffer "*Org Agenda*")

(defun ogt--configure-emacs ()
  (setq org-gtd-directory (f-join default-directory "test"  "runtime-file-path"))
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize))

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
  (with-current-buffer (org-gtd--inbox-file)
    (find-file
     (car (with-org-gtd-context
              (org-archive--compute-location
               (funcall org-gtd-archive-location)))))))

(defun ogt--archive-string ()
  "return string of items archived from actionable file"
  (ogt--get-string-from-buffer (ogt--default-projects-archive)))

(defun ogt--save-all-buffers ()
  (with-simulated-input "!" (save-some-buffers)))

(defun ogt--add-single-item (&optional label)
  (org-gtd-capture nil "i")
  (insert (or label "single action"))
  (org-capture-finalize))
