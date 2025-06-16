(load "test/helpers/clarifying.el")

(defun ogt-capture-single-item (&optional label)
  (let ((inhibit-message t))
    (org-gtd-capture nil "i")
    (insert (or label "single action"))
    (org-capture-finalize)))

(defun ogt-capture-and-process-project (label)
  "LABEL is the project label."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (goto-char (point-max))
    (newline)
    (insert ogt--project-text)
    (ogt-clarify-as-project)))

(defun ogt-capture-and-process-calendar-item (label &optional date)
  "DATE has to be like the output of `calendar-current-date' so (MM DD YYYY)."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-calendar-item (or date (calendar-current-date)))))

(defun ogt-capture-and-process-habit (label repeater)
  "REPEATER is an org-mode date repeater, e.g. .+1d or ++1m, etc."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-habit repeater)))

(defun ogt-capture-and-process-delegated-item (label &optional to-whom date)
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-delegated-item (or to-whom "Someone") (or date (calendar-current-date)))))

(defun ogt-capture-and-process-incubated-item (label &optional date)
  "LABEL is the incubated label."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-incubated-item (or date (calendar-current-date)))))

(defun ogt-capture-and-process-single-action (label)
  "LABEL is the single action label."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-single-action)))

(defun ogt-capture-and-process-quick-action (label)
  "LABEL is the quick action label."
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-quick-action)))

(defun ogt-capture-and-process-knowledge-item (label)
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-knowledge-item)))

(defun ogt-capture-and-process-addition-to-project (label project-heading-simulated-input)
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)

    (with-simulated-input project-heading-simulated-input
                          (org-gtd-organize--call
                           org-gtd-add-to-project-func))))

(defun ogt-capture-and-process-trash-item (label)
  (let ((inhibit-message t))
    (ogt-capture-single-item label)
    (org-gtd-process-inbox)
    (ogt-clarify-as-trash-item)))
