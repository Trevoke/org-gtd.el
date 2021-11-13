(defconst ogt--project-text
  "** Task 1
** Task 2
** Task 3")

(defconst ogt--project-to-cancel
  "** cancel me
*** DONE Task 1
*** NEXT Task 2
*** TODO Task 3")

(defconst ogt--completed-project
  "** completed
*** DONE Task 1
*** DONE Task 2
*** DONE Task 3")

(defconst ogt--canceled-project
  "** canceled
*** DONE Task 1
*** CNCL Task 2
*** CNCL Task 3")

(defconst ogt--base-project-heading
  "* AdditionalHeading
:PROPERTIES:
:ORG_GTD: Projects
:END:
")
