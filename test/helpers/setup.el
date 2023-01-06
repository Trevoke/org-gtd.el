(load "test/helpers/project.el")
(load "test/helpers/processing.el")
(load "test/helpers/utils.el")

(defun ogt--configure-emacs ()
  (setq org-gtd-directory (make-temp-file "org-gtd" t)
        org-gtd-process-item-hooks '()
        org-gtd-refile-to-any-target nil
        org-edna-use-inheritance t)
  (org-edna-mode 1)
  (define-key org-gtd-process-map (kbd "C-c c") #'org-gtd-choose))

(defun ogt--prepare-filesystem ()
  "run before each test"
  ;(ogt--clean-target-directory org-gtd-directory)
  )

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(defun ogt--reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun ogt--close-and-delete-files ()
  "Run after every test to clear open buffers state"
  (kill-matching-buffers ".*\\.org" nil t)
  (kill-matching-buffers ".*Agenda.*" nil t)
  (kill-matching-buffers ".*Calendar.*" nil t)
  )

(defun ogt--clear-file-and-buffer (buffer)
  (if (bufferp buffer)
      (let ((filename (buffer-file-name buffer)))
        (with-current-buffer buffer (basic-save-buffer))
        (kill-buffer buffer)
        (delete-file filename))))
