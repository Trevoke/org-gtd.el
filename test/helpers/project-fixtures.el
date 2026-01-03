;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-helper-project-fixtures)

(defconst ogt--base-project-heading
  "* AdditionalHeading
:PROPERTIES:
:ORG_GTD_REFILE:  Projects
:END:
")

(defconst ogt--project-text
  "** Task 1
** Task 2
** Task 3")

(defconst ogt--project-to-cancel
  "** [1/3] cancel me
:PROPERTIES:
:ID: cancel-me-fixture-id
:ORG_GTD: Projects
:ORG_GTD_FIRST_TASKS: cancel-fixture-task-1-id
:END:
*** DONE Task 1
:PROPERTIES:
:ID: cancel-fixture-task-1-id
:ORG_GTD: Actions
:ORG_GTD_BLOCKS: cancel-fixture-task-2-id
:ORG_GTD_PROJECT: cancel me
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** NEXT Task 2
:PROPERTIES:
:ID: cancel-fixture-task-2-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: cancel-fixture-task-1-id
:ORG_GTD_BLOCKS: cancel-fixture-task-3-id
:ORG_GTD_PROJECT: cancel me
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** TODO Task 3
:PROPERTIES:
:ID: cancel-fixture-task-3-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: cancel-fixture-task-2-id
:ORG_GTD_PROJECT: cancel me
:TRIGGER: org-gtd-update-project-after-task-done!
:END:")

(defconst ogt--completed-project
  "** [3/3] completed
:PROPERTIES:
:ID: completed-fixture-id
:ORG_GTD: Projects
:ORG_GTD_FIRST_TASKS: completed-fixture-task-1-id
:END:
*** DONE Task 1
:PROPERTIES:
:ID: completed-fixture-task-1-id
:ORG_GTD: Actions
:ORG_GTD_BLOCKS: completed-fixture-task-2-id
:ORG_GTD_PROJECT: completed
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** CNCL Task 2
:PROPERTIES:
:ID: completed-fixture-task-2-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: completed-fixture-task-1-id
:ORG_GTD_BLOCKS: completed-fixture-task-3-id
:ORG_GTD_PROJECT: completed
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** DONE Task 3
:PROPERTIES:
:ID: completed-fixture-task-3-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: completed-fixture-task-2-id
:ORG_GTD_PROJECT: completed
:TRIGGER: org-gtd-update-project-after-task-done!
:END:")

(defconst ogt--canceled-project
  "** [3/3] canceled
:PROPERTIES:
:ID: canceled-fixture-id
:ORG_GTD: Projects
:ORG_GTD_FIRST_TASKS: canceled-fixture-task-1-id
:END:
*** DONE Task 1
:PROPERTIES:
:ID: canceled-fixture-task-1-id
:ORG_GTD: Actions
:ORG_GTD_BLOCKS: canceled-fixture-task-2-id
:ORG_GTD_PROJECT: canceled
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** CNCL Task 2
:PROPERTIES:
:ID: canceled-fixture-task-2-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: canceled-fixture-task-1-id
:ORG_GTD_BLOCKS: canceled-fixture-task-3-id
:ORG_GTD_PROJECT: canceled
:TRIGGER: org-gtd-update-project-after-task-done!
:END:
*** CNCL Task 3
:PROPERTIES:
:ID: canceled-fixture-task-3-id
:ORG_GTD: Actions
:ORG_GTD_DEPENDS_ON: canceled-fixture-task-2-id
:ORG_GTD_PROJECT: canceled
:TRIGGER: org-gtd-update-project-after-task-done!
:END:")

;; End load guard and provide feature
(provide 'org-gtd-test-helper-project-fixtures))
