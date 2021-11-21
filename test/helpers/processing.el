(defun ogt--add-single-item (&optional label)
  (org-gtd-capture nil "i")
  (insert (or label "single action"))
  (org-capture-finalize))

(defun ogt--add-and-process-project (label)
  "LABEL is the project label."
  (ogt--add-single-item label)
  (with-simulated-input
   ("p" "M-> RET" (insert ogt--project-text) "C-c c TAB RET")
   (org-gtd-process-inbox)))

(defun ogt--add-and-process-calendar-item (label)
  "LABEL is the calendared item label."
  (ogt--add-single-item label)
  (with-simulated-input "c C-c c RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-delegated-item (label)
  "LABEL is the delegated label."
  (ogt--add-single-item label)
  (with-simulated-input "d C-c c RET Someone RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-incubated-item (label)
  "LABEL is the incubated label."
  (ogt--add-single-item label)
  (with-simulated-input "i C-c c RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-single-action (label)
  "LABEL is the single action label."
  (ogt--add-single-item label)

  (with-simulated-input "s C-c c TAB RET"
                        (org-gtd-process-inbox)))
