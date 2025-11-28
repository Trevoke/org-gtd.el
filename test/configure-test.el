;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'org-gtd-types)
(require 'org-gtd-configure)
(require 'buttercup)

;; Helper macro for tests
(defmacro ogt--with-temp-org-buffer (contents &rest body)
  "Create temp buffer with CONTENTS in org-mode, point at start.
Execute BODY in this buffer."
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(describe "org-gtd-configure-as-type"

  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "is defined"
    (expect (fboundp 'org-gtd-configure-as-type) :to-be-truthy))

  (describe "sets ORG_GTD property from type definition"

    (it "sets 'Actions' for next-action type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'next-action)
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Actions")))

    (it "sets 'Delegated' for delegated type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "John SPC Doe RET 2025-01-15 RET"
         (org-gtd-configure-as-type 'delegated))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Delegated")))

    (it "sets 'Calendar' for calendar type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "2025-01-15 RET"
         (org-gtd-configure-as-type 'calendar))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Calendar")))

    (it "sets 'Projects' for project type"
      (ogt--with-temp-org-buffer
       "* Test project"
       (org-gtd-configure-as-type 'project)
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Projects")))

    (it "sets 'Reference' for reference type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'reference)
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Reference")))

    (it "sets 'Trash' for trash type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'trash)
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Trash")))

    (it "sets 'Quick' for quick-action type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'quick-action)
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Quick"))))

  (describe "sets TODO state from type's :state semantic"

    (it "sets NEXT keyword for next-action type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'next-action)
       (expect (org-get-todo-state) :to-equal (org-gtd-keywords--next))))

    (it "sets WAIT keyword for delegated type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "John SPC Doe RET 2025-01-15 RET"
         (org-gtd-configure-as-type 'delegated))
       (expect (org-get-todo-state) :to-equal (org-gtd-keywords--wait))))

    (it "sets done keyword for reference type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'reference)
       (expect (member (org-get-todo-state) org-done-keywords) :to-be-truthy)))

    (it "sets canceled keyword for trash type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'trash)
       (expect (org-get-todo-state) :to-equal (org-gtd-keywords--canceled))))

    (it "does not set TODO state when type has nil :state"
      (ogt--with-temp-org-buffer
       "* Test item"
       (org-gtd-configure-as-type 'project)
       (expect (org-get-todo-state) :to-be nil))))

  (describe "prompts for and sets semantic properties"

    (it "sets DELEGATED_TO from :who property for delegated type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "Jane SPC Smith RET 2025-01-15 RET"
         (org-gtd-configure-as-type 'delegated))
       (expect (org-entry-get nil "DELEGATED_TO") :to-equal "Jane Smith")))

    (it "sets ORG_GTD_TIMESTAMP from :when property for delegated type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "John RET 2025-06-15 RET"
         (org-gtd-configure-as-type 'delegated))
       (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
         (expect timestamp :to-be-truthy)
         (expect (string-match "2025-06-15" timestamp) :to-be-truthy))))

    (it "sets ORG_GTD_TIMESTAMP from :when property for calendar type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "2025-03-20 RET"
         (org-gtd-configure-as-type 'calendar))
       (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
         (expect timestamp :to-be-truthy)
         (expect (string-match "2025-03-20" timestamp) :to-be-truthy))))

    (it "sets ORG_GTD_TIMESTAMP from :when property for tickler type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "2025-12-01 RET"
         (org-gtd-configure-as-type 'tickler))
       (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
         (expect timestamp :to-be-truthy)
         (expect (string-match "2025-12-01" timestamp) :to-be-truthy))))

    (it "sets SCHEDULED from :when property for habit type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (with-simulated-input "2025-01-01 RET +1d RET"
         (org-gtd-configure-as-type 'habit))
       (let ((scheduled (org-entry-get nil "SCHEDULED")))
         (expect scheduled :to-be-truthy)
         (expect (string-match "2025-01-01" scheduled) :to-be-truthy)))))

  (describe "errors on invalid type"

    (it "signals error for unknown type"
      (ogt--with-temp-org-buffer
       "* Test item"
       (expect (org-gtd-configure-as-type 'nonexistent-type)
               :to-throw 'user-error))))

  (describe "with values parameter (non-interactive)"

    (it "uses provided values instead of prompting for delegated type"
      (ogt--with-temp-org-buffer
       "* Test task"
       (org-gtd-configure-as-type 'delegated
                                  '((:who . "John Doe")
                                    (:when . "<2025-12-01>")))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Delegated")
       (expect (org-entry-get nil "DELEGATED_TO") :to-equal "John Doe")
       (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>")))

    (it "uses provided values for calendar type"
      (ogt--with-temp-org-buffer
       "* Test task"
       (org-gtd-configure-as-type 'calendar
                                  '((:when . "<2025-06-15>")))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Calendar")
       (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-06-15>")))

    (it "uses provided values for tickler type"
      (ogt--with-temp-org-buffer
       "* Test task"
       (org-gtd-configure-as-type 'tickler
                                  '((:when . "<2025-03-01>")))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Tickler")
       (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-03-01>")))

    (it "uses provided values for habit type"
      (ogt--with-temp-org-buffer
       "* Test task"
       (org-gtd-configure-as-type 'habit
                                  '((:when . "<2025-01-01 +1d>")))
       (expect (org-entry-get nil "ORG_GTD") :to-equal "Habit")
       ;; SCHEDULED is set via org-schedule
       (expect (org-get-scheduled-time (point)) :not :to-be nil))))

  (describe "with :input-fn in user-types"

    (it "calls custom input function instead of default prompt"
      (let ((org-gtd-user-types
             '((delegated
                :properties
                ((:who :org-property "DELEGATED_TO" :type text :required t
                       :prompt "Delegate to"
                       :input-fn (lambda (_prompt) "Custom Person")))))))
        (ogt--with-temp-org-buffer
         "* Test task"
         ;; Only need to provide :when since :who uses input-fn
         (with-simulated-input "2025-01-15 RET"
           (org-gtd-configure-as-type 'delegated))
         (expect (org-entry-get nil "DELEGATED_TO") :to-equal "Custom Person"))))

    (it "input-fn receives the prompt as argument"
      (let* ((received-prompt nil)
             (org-gtd-user-types
              `((delegated
                 :properties
                 ((:who :org-property "DELEGATED_TO" :type text :required t
                        :prompt "Test prompt here"
                        :input-fn ,(lambda (prompt)
                                     (setq received-prompt prompt)
                                     "Result")))))))
        (ogt--with-temp-org-buffer
         "* Test task"
         (with-simulated-input "2025-01-15 RET"
           (org-gtd-configure-as-type 'delegated))
         (expect received-prompt :to-equal "Test prompt here"))))))

;;; configure-test.el ends here
