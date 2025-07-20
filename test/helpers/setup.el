;;; -*- lexical-binding: t; -*-
(setq org-gtd-update-ack "3.0.0")

(load "test/helpers/clarifying.el")
(load "test/helpers/project-fixtures.el")
(load "test/helpers/processing.el")
(load "test/helpers/utils.el")

(defun ogt--configure-emacs ()
  (setq last-command nil
        org-gtd-directory (make-temp-file "org-gtd" t)
        org-gtd-areas-of-focus nil
        org-gtd-organize-hooks '()
        org-gtd-refile-to-any-target t
        org-edna-use-inheritance t)
  (org-edna-mode 1)
  (define-key org-gtd-clarify-map (kbd "C-c c") #'org-gtd-organize))

(defun ogt--reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun ogt--close-and-delete-files ()
  "Run after every test to clear open buffers state"

  (mapc
   #'ogt--kill-buffer
   (-flatten (mapcar
              #'ogt--get-buffers
              `(".*\\.org" ".*Agenda.*" "gtd_archive.*" ".*Calendar.*"
                ,(format ".*%s.*" org-gtd-clarify--prefix))))))

(defun ogt--clear-file-and-buffer (buffer)
  (if (bufferp buffer)
      (let ((filename (buffer-file-name buffer)))
        (with-current-buffer buffer (basic-save-buffer))
        (kill-buffer buffer)
        (delete-file filename))))

(defun ogt--get-buffers (regexp)
  (seq-filter (lambda (buf)
                (string-match-p regexp (buffer-name buf)))
              (buffer-list)))

(defun ogt--kill-buffer (buffer)
  (when (buffer-file-name buffer)
    (with-current-buffer buffer
      (revert-buffer t t)))
  (kill-buffer buffer))

(defun ogt--recursive-eldev-test (file)
  (unless (file-readable-p (file-name-concat "test" file))
    (error "Cannot find or read file %s" file))
  (with-temp-buffer
    (prog1 (call-process
            eldev-shell-command
            nil
            t
            nil
            "test"
            "-f"
            file)
      (princ (buffer-substring 1 (point-max))))))
