(defun create-additional-project-target (filename)
  (ogt--create-org-file-in-org-gtd-dir filename ogt--base-project-heading))

(defun ogt--archive ()
  "Create or return the buffer to the archive file."
  (with-current-buffer (org-gtd--inbox-file)
    (find-file-noselect
     (car (with-org-gtd-context
              (org-archive--compute-location
               (funcall org-gtd-archive-location)))))))

(defun ogt--archive-string ()
  "return string of items archived from actionable file"
  (ogt--buffer-string (ogt--archive)))

(defun ogt--save-all-buffers ()
  (let ((inhibit-message t))
    (with-simulated-input "!" (save-some-buffers))))

(defun ogt--temp-org-file-buffer (basename &optional text)
  "Create a new org-mode file with a unique name.
The name is based on BASENAME, the TEXT is optional content.
Return the buffer visiting that file."
  (let ((filename (make-temp-file basename nil ".org" text)))
    (find-file-noselect filename)))

(defun ogt--create-org-file-in-org-gtd-dir (basename &optional initial-contents)
  (let* ((file (f-join org-gtd-directory (string-join `(,basename ".org"))))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-mode)
      (insert (or initial-contents ""))
      (basic-save-buffer))
    buffer))

(defun ogt--buffer-string (buffer)
  "Return buffer's content."
  (with-current-buffer buffer
    (ogt--current-buffer-raw-text)))

(defun ogt--current-buffer-raw-text ()
  "Returns text without faces"
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ogt--print-buffer-list ()
  (message "*** Start List of active buffers")
  (mapc (lambda (x) (message (buffer-name x))) (buffer-list))
  (message "*** End List of active buffers"))

(defun ogt--org-dir-buffer-string ()
  (let ((ogt-files (progn (list-directory org-gtd-directory)
                          (with-current-buffer "*Directory*"
                            (buffer-string)))))
    (kill-buffer "*Directory*")
    ogt-files))
