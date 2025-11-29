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

(describe "org-gtd-restore-state"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "restores delegated item from PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: John Doe
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       ;; Mock read-string to return previous values
       (spy-on 'read-string :and-call-fake
               (lambda (prompt &optional initial-input history default-value)
                 default-value))
       (org-gtd-restore-state)
       ;; Verify restored
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated")
       (expect (org-entry-get (point) "TODO") :to-equal "WAIT")
       (expect (org-entry-get (point) "DELEGATED_TO") :to-equal "John Doe")
       (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2024-06-15>")
       ;; Verify PREVIOUS_* cleaned up
       (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_TODO") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_DELEGATED_TO") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-be nil))))

  (it "calls clarify when no PREVIOUS_ORG_GTD exists"
    (ogt--with-temp-org-buffer
     "* Inbox item went to someday
:PROPERTIES:
:ID: direct-someday-id
:ORG_GTD: Someday
:END:"
     (org-back-to-heading t)
     (spy-on 'org-gtd-clarify-item)
     (org-gtd-restore-state)
     ;; Verify ORG_GTD cleared and clarify called
     (expect (org-entry-get (point) "ORG_GTD") :to-be nil)
     (expect 'org-gtd-clarify-item :to-have-been-called))))

(describe "org-gtd-reactivate"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "reactivates a someday item"
    (ogt--with-temp-org-buffer
     "* Delegated then someday'd
:PROPERTIES:
:ID: reactivate-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: Jane
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-09-01>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       (spy-on 'read-string :and-call-fake
               (lambda (prompt &optional initial-input history default-value)
                 default-value))
       (org-gtd-reactivate)
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated"))))

  (it "reactivates a tickler item"
    (ogt--with-temp-org-buffer
     "* Calendar then tickler'd
:PROPERTIES:
:ID: tickler-reactivate-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-12-01>
:PREVIOUS_ORG_GTD: Calendar
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-10-15>
:END:"
     (org-back-to-heading t)
     (spy-on 'read-string :and-call-fake
             (lambda (prompt &optional initial-input history default-value)
               default-value))
     (org-gtd-reactivate)
     (expect (org-entry-get (point) "ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2024-10-15>")))

  (it "errors on non-someday/tickler item"
    (ogt--with-temp-org-buffer
     "* Active item
:PROPERTIES:
:ID: active-id
:ORG_GTD: Delegated
:END:"
     (org-back-to-heading t)
     (expect (org-gtd-reactivate) :to-throw 'user-error))))

;;; reactivate-test.el ends here
