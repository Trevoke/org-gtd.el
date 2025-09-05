;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-helper-project-fixtures)

(defconst ogt--base-project-heading
  "* AdditionalHeading
:PROPERTIES:
:TRIGGER: org-gtd-next-project-action org-gtd-update-project-task!
:ORG_GTD:  Projects
:END:
")

(defconst ogt--project-text
  "** Task 1
** Task 2
** Task 3")

(defconst ogt--project-to-cancel
  "** [1/3] cancel me
*** DONE Task 1
*** NEXT Task 2
*** TODO Task 3")

(defconst ogt--completed-project
  "** [3/3] completed
*** DONE Task 1
*** CNCL Task 2
*** DONE Task 3")

(defconst ogt--canceled-project
  "** [3/3] canceled
*** DONE Task 1
*** CNCL Task 2
*** CNCL Task 3")

;; End load guard and provide feature
(provide 'org-gtd-test-helper-project-fixtures))
