(defun ogt--add-single-item (&optional label)
  (org-gtd-capture nil "i")
  (insert (or label "single action"))
  (org-capture-finalize))

(defun ogt--add-and-process-project (label)
  "LABEL is the project label."
  (ogt--add-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "M-> RET"))
  (with-current-buffer (current-buffer)
    (insert ogt--project-text))
  (execute-kbd-macro (kbd "C-c c p")))

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
