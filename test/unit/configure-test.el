;;; configure-test.el --- Tests for org-gtd-configure -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-configure functionality.
;;
;; Test Coverage:
;; - Function definition (1 test)
;; - ORG_GTD property setting (7 tests)
;; - TODO state setting (5 tests)
;; - Semantic property prompting (5 tests)
;; - Error handling (1 test)
;; - Non-interactive values parameter (4 tests)
;; - Custom input-fn in user-types (2 tests)
;;
;; Migrated from test/configure-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-types)
(require 'org-gtd-configure)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Function Definition Tests

(deftest configure/is-defined ()
  "Function org-gtd-configure-as-type is defined."
  (assert-true (fboundp 'org-gtd-configure-as-type)))

;;; ORG_GTD Property Setting Tests

(deftest configure/sets-actions-for-next-action-type ()
  "Sets 'Actions' for next-action type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'next-action)
   (assert-equal "Actions" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-delegated-for-delegated-type ()
  "Sets 'Delegated' for delegated type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "John SPC Doe RET 2025-01-15 RET"
     (org-gtd-configure-as-type 'delegated))
   (assert-equal "Delegated" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-calendar-for-calendar-type ()
  "Sets 'Calendar' for calendar type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "2025-01-15 RET"
     (org-gtd-configure-as-type 'calendar))
   (assert-equal "Calendar" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-projects-for-project-type ()
  "Sets 'Projects' for project type."
  (ogt--with-temp-org-buffer
   "* Test project"
   (org-gtd-configure-as-type 'project)
   (assert-equal "Projects" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-reference-for-reference-type ()
  "Sets 'Reference' for reference type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'reference)
   (assert-equal "Reference" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-trash-for-trash-type ()
  "Sets 'Trash' for trash type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'trash)
   (assert-equal "Trash" (org-entry-get nil "ORG_GTD"))))

(deftest configure/sets-quick-for-quick-action-type ()
  "Sets 'Quick' for quick-action type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'quick-action)
   (assert-equal "Quick" (org-entry-get nil "ORG_GTD"))))

;;; TODO State Setting Tests

(deftest configure/sets-next-keyword-for-next-action ()
  "Sets NEXT keyword for next-action type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'next-action)
   (assert-equal (org-gtd-keywords--next) (org-get-todo-state))))

(deftest configure/sets-wait-keyword-for-delegated ()
  "Sets WAIT keyword for delegated type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "John SPC Doe RET 2025-01-15 RET"
     (org-gtd-configure-as-type 'delegated))
   (assert-equal (org-gtd-keywords--wait) (org-get-todo-state))))

(deftest configure/sets-done-keyword-for-reference ()
  "Sets done keyword for reference type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'reference)
   (assert-true (member (org-get-todo-state) org-done-keywords))))

(deftest configure/sets-canceled-keyword-for-trash ()
  "Sets canceled keyword for trash type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'trash)
   (assert-equal (org-gtd-keywords--canceled) (org-get-todo-state))))

(deftest configure/no-todo-state-for-nil-state-type ()
  "Does not set TODO state when type has nil :state."
  (ogt--with-temp-org-buffer
   "* Test item"
   (org-gtd-configure-as-type 'project)
   (assert-nil (org-get-todo-state))))

;;; Semantic Property Prompting Tests

(deftest configure/sets-delegated-to-from-who-property ()
  "Sets DELEGATED_TO from :who property for delegated type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "Jane SPC Smith RET 2025-01-15 RET"
     (org-gtd-configure-as-type 'delegated))
   (assert-equal "Jane Smith" (org-entry-get nil "DELEGATED_TO"))))

(deftest configure/sets-timestamp-from-when-for-delegated ()
  "Sets ORG_GTD_TIMESTAMP from :when property for delegated type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "John RET 2025-06-15 RET"
     (org-gtd-configure-as-type 'delegated))
   (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
     (assert-true timestamp)
     (assert-match "2025-06-15" timestamp))))

(deftest configure/sets-timestamp-from-when-for-calendar ()
  "Sets ORG_GTD_TIMESTAMP from :when property for calendar type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "2025-03-20 RET"
     (org-gtd-configure-as-type 'calendar))
   (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
     (assert-true timestamp)
     (assert-match "2025-03-20" timestamp))))

(deftest configure/sets-timestamp-from-when-for-tickler ()
  "Sets ORG_GTD_TIMESTAMP from :when property for tickler type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "2025-12-01 RET"
     (org-gtd-configure-as-type 'tickler))
   (let ((timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP")))
     (assert-true timestamp)
     (assert-match "2025-12-01" timestamp))))

(deftest configure/sets-scheduled-from-when-for-habit ()
  "Sets SCHEDULED from :when property for habit type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (with-simulated-input "2025-01-01 RET +1d RET"
     (org-gtd-configure-as-type 'habit))
   (let ((scheduled (org-entry-get nil "SCHEDULED")))
     (assert-true scheduled)
     (assert-match "2025-01-01" scheduled))))

;;; Error Handling Tests

(deftest configure/signals-error-for-unknown-type ()
  "Signals error for unknown type."
  (ogt--with-temp-org-buffer
   "* Test item"
   (assert-raises 'user-error
     (org-gtd-configure-as-type 'nonexistent-type))))

;;; Non-interactive Values Parameter Tests

(deftest configure/uses-provided-values-for-delegated ()
  "Uses provided values instead of prompting for delegated type."
  (ogt--with-temp-org-buffer
   "* Test task"
   (org-gtd-configure-as-type 'delegated
                              '((:who . "John Doe")
                                (:when . "<2025-12-01>")))
   (assert-equal "Delegated" (org-entry-get nil "ORG_GTD"))
   (assert-equal "John Doe" (org-entry-get nil "DELEGATED_TO"))
   (assert-equal "<2025-12-01>" (org-entry-get nil "ORG_GTD_TIMESTAMP"))))

(deftest configure/uses-provided-values-for-calendar ()
  "Uses provided values for calendar type."
  (ogt--with-temp-org-buffer
   "* Test task"
   (org-gtd-configure-as-type 'calendar
                              '((:when . "<2025-06-15>")))
   (assert-equal "Calendar" (org-entry-get nil "ORG_GTD"))
   (assert-equal "<2025-06-15>" (org-entry-get nil "ORG_GTD_TIMESTAMP"))))

(deftest configure/uses-provided-values-for-tickler ()
  "Uses provided values for tickler type."
  (ogt--with-temp-org-buffer
   "* Test task"
   (org-gtd-configure-as-type 'tickler
                              '((:when . "<2025-03-01>")))
   (assert-equal "Tickler" (org-entry-get nil "ORG_GTD"))
   (assert-equal "<2025-03-01>" (org-entry-get nil "ORG_GTD_TIMESTAMP"))))

(deftest configure/uses-provided-values-for-habit ()
  "Uses provided values for habit type."
  (ogt--with-temp-org-buffer
   "* Test task"
   (org-gtd-configure-as-type 'habit
                              '((:when . "<2025-01-01 +1d>")))
   (assert-equal "Habit" (org-entry-get nil "ORG_GTD"))
   ;; SCHEDULED is set via org-schedule
   (assert-true (org-get-scheduled-time (point)))))

;;; Custom input-fn in user-types Tests

(deftest configure/calls-custom-input-fn-instead-of-prompt ()
  "Calls custom input function instead of default prompt."
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
     (assert-equal "Custom Person" (org-entry-get nil "DELEGATED_TO")))))

(deftest configure/input-fn-receives-prompt-as-argument ()
  "Input-fn receives the prompt as argument."
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
     (assert-equal "Test prompt here" received-prompt))))

(provide 'configure-test)

;;; configure-test.el ends here
