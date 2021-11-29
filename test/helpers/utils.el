(defun ogt--org-dir-buffer-string ()
  (let ((ogt-files (progn (list-directory org-gtd-directory)
                          (with-current-buffer "*Directory*"
                            (buffer-string)))))
    (kill-buffer "*Directory*")
    ogt-files))

(defun ogt--default-projects-archive ()
  "Create or return the buffer to the archive file for the actionable items."
  (with-current-buffer (org-gtd--inbox-file)
    (find-file-noselect
     (car (with-org-gtd-context
              (org-archive--compute-location
               (funcall org-gtd-archive-location)))))))

(defun ogt--archive-string ()
  "return string of items archived from actionable file"
  (ogt--get-string-from-buffer (ogt--default-projects-archive)))

(defun ogt--save-all-buffers ()
  (with-simulated-input "!" (save-some-buffers)))
