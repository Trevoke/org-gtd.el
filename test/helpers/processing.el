(defun ogt--add-single-item (&optional label)
  (org-gtd-capture nil "i")
  (insert (or label "single action"))
  (org-capture-finalize))

(defun ogt--print-buffer-list ()
  (message "Start List of active buffers")
  (mapc (lambda (x) (message (buffer-name x))) (buffer-list))
  (message "End List of active buffers"))

(defun ogt--add-and-process-project (label)
  "LABEL is the project label."
  (ogt--add-single-item label)
  (message "After add-single-item, I am in %s" (buffer-name (current-buffer)))
  (org-gtd-process-inbox)
  ;;(set-buffer (car (org-gtd-clarify--get-buffers)))
  (execute-kbd-macro (kbd "C-n"))
  (message "after C-n I am in %s" (buffer-name (current-buffer)))
  (execute-kbd-macro (kbd "M-> RET"))
  (message "after M-> RET, I am in %s" (buffer-name (current-buffer)))
  (with-current-buffer (current-buffer)
    (insert ogt--project-text))
                                        ;(insert ogt--project-text)
  (message "after insert text, I am in %s" (buffer-name (current-buffer)))
  (execute-kbd-macro (kbd "C-c c p"))
  )

(defun ogt--add-and-process-calendar-item (label)
  "LABEL is the calendared item label."
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "C-c c c RET")))

(defun ogt--add-and-process-delegated-item (label)
  "LABEL is the delegated label."
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "C-c c d RET Someone RET")))

(defun ogt--add-and-process-incubated-item (label)
  "LABEL is the incubated label."
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "C-c c i RET")))

(defun ogt--add-and-process-single-action (label)
  "LABEL is the single action label."
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "C-c c s")))
