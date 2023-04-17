(load "test/helpers/clarifying.el")

(defun ogt-capture-single-item (&optional label)
  (org-gtd-capture nil "i")
  (insert (or label "single action"))
  (org-capture-finalize))

(defun ogt-capture-and-process-project (label)
  "LABEL is the project label."
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (execute-kbd-macro (kbd "M-> RET"))
  (with-current-buffer (current-buffer)
    (insert ogt--project-text))
  (ogt-clarify-as-project))

(defun ogt-capture-and-process-calendar-item (label &optional date)
  "DATE has to be like the output of `calendar-current-date' so (MM DD YYYY)."
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (ogt-clarify-as-calendar-item date))

(defun ogt-capture-and-process-delegated-item (label &optional to-whom date)
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (ogt-clarify-as-delegated-item to-whom date))

(defun ogt-capture-and-process-incubated-item (label &optional date)
  "LABEL is the incubated label."
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (ogt-clarify-as-incubated-item date))

(defun ogt-capture-and-process-single-action (label)
  "LABEL is the single action label."
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (ogt-clarify-as-single-action))

(defun ogt-capture-and-process-knowledge-item (label)
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)
  (ogt-clarify-as-knowledge-item))

(defun ogt-capture-and-process-addition-to-project (label project-heading-simulated-input)
  (ogt-capture-single-item label)
  (org-gtd-process-inbox)

  (with-simulated-input project-heading-simulated-input
                        (org-gtd-organize-inbox-item
                         org-gtd-organize-add-to-project-func)))
