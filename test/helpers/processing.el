(defun ogt--add-and-process-project (label)
  "LABEL is the project label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input
   ("p" "M-> RET" (insert ogt--project-text) "C-c c RET TAB RET")
   (org-gtd-process-inbox)))

(defun ogt--add-and-process-calendar-item (label)
  "LABEL is the calendared item label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "c C-c c RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-delegated-item (label)
  "LABEL is the delegated label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "d C-c c RET Someone RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-incubated-item (label)
  "LABEL is the incubated label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "i C-c c RET RET TAB RET"
                        (org-gtd-process-inbox)))

(defun ogt--add-and-process-single-action (label)
  "LABEL is the single action label."
  (org-gtd-capture nil "i")
  (insert label)
  (org-capture-finalize)
  (with-simulated-input "s C-c c RET TAB RET"
                        (org-gtd-process-inbox)))
