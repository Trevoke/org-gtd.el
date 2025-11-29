;;; reactivate-test.el --- Tests for reactivation -*- lexical-binding: t; -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'org-gtd-reactivate)

(describe "org-gtd-save-state"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "saves delegated item state to PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Delegated
:DELEGATED_TO: John Doe
:ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       (org-todo "WAIT"))
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Delegated")
     (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "WAIT")
     (expect (org-entry-get (point) "PREVIOUS_DELEGATED_TO") :to-equal "John Doe")
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-equal "<2024-06-15>")))

  (it "saves calendar item state to PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Appointment
:PROPERTIES:
:ID: cal-id
:ORG_GTD: Calendar
:ORG_GTD_TIMESTAMP: <2024-07-20>
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-equal "<2024-07-20>")))

  (it "does not save state for someday items"
    (ogt--with-temp-org-buffer
     "* Someday item
:PROPERTIES:
:ID: someday-id
:ORG_GTD: Someday
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)))

  (it "does not save state for tickler items"
    (ogt--with-temp-org-buffer
     "* Tickler item
:PROPERTIES:
:ID: tickler-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-08-01>
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)))

  (it "does not save state for items without ORG_GTD"
    (ogt--with-temp-org-buffer
     "* Inbox item
:PROPERTIES:
:ID: inbox-id
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil))))

;;; reactivate-test.el ends here
