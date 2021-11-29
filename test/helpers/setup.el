(load "test/helpers/project.el")
(load "test/helpers/processing.el")
(load "test/helpers/utils.el")

(defconst ogt--agenda-buffer "*Org Agenda*")

(defun ogt--configure-emacs ()
  (setq org-gtd-directory (make-temp-file "org-gtd" t)
        org-gtd-process-item-hooks '()
        org-gtd-refile-to-any-target nil)

  (define-key org-gtd-process-map (kbd "C-c c") #'org-gtd-choose))

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
            ,(org-gtd--default-calendar-file)
            ,(org-gtd--inbox-file)
            ,(org-gtd--default-incubated-file))))

(defun ogt--clear-file-and-buffer (buffer)
  (if (bufferp buffer)
      (let ((filename (buffer-file-name buffer)))
        (with-current-buffer buffer (basic-save-buffer))
        (kill-buffer buffer)
        (delete-file filename))))
