(defun ogt--org-dir-buffer-string ()
  (let ((ogt-files (progn (list-directory org-gtd-directory)
                          (with-current-buffer "*Directory*"
                            (buffer-string)))))
    (kill-buffer "*Directory*")
    ogt-files))
