;; -*- lexical-binding: t; coding: utf-8 -*-

;;; org-gtd-agenda-transient-test.el --- Tests for agenda transient menu -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-agenda-transient menu system.
;;

;;; Code:

(require 'compat)
(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-agenda-transient)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))

;;;; Context Detection Tests

(describe "org-gtd-agenda-transient--task-at-point"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "returns nil when not on an agenda line"
    (with-temp-buffer
      (insert "Not an agenda buffer")
      (expect (org-gtd-agenda-transient--task-at-point) :to-be nil)))

  (it "returns task info when on an agenda line with a task"
    ;; Create a task in a GTD file
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Test task for agenda\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Generate agenda and check task-at-point
    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Test task for agenda")
    (beginning-of-line)

    (let ((info (org-gtd-agenda-transient--task-at-point)))
      (expect info :not :to-be nil)
      (expect (plist-get info :title) :to-match "Test task for agenda")
      (expect (plist-get info :org-gtd-type) :to-equal "Actions")
      (expect (plist-get info :marker) :not :to-be nil))))

;;;; Transient Definition Tests

(describe "org-gtd-agenda-transient"

  (it "is a valid transient prefix"
    (expect (functionp 'org-gtd-agenda-transient) :to-be-truthy)
    (expect (get 'org-gtd-agenda-transient 'transient--prefix) :not :to-be nil))

  (it "can be invoked interactively"
    (expect (commandp 'org-gtd-agenda-transient) :to-be-truthy)))

;;;; Has-Timestamp Detection Tests

(describe "org-gtd-agenda-transient--has-timestamp-p"

  (it "returns truthy for Calendar items"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Calendar task\n")
      (goto-char (point-min))
      (org-entry-put (point) "ORG_GTD" "Calendar")
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be-truthy)))

  (it "returns truthy for Delegated items"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Delegated task\n")
      (goto-char (point-min))
      (org-entry-put (point) "ORG_GTD" "Delegated")
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be-truthy)))

  (it "returns truthy for Tickler items"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Tickler task\n")
      (goto-char (point-min))
      (org-entry-put (point) "ORG_GTD" "Tickler")
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be-truthy)))

  (it "returns nil for Habit items (excluded from Time section)"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Habit task\n")
      (goto-char (point-min))
      (org-entry-put (point) "ORG_GTD" "Habit")
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be nil)))

  (it "returns nil for Actions items"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Action task\n")
      (goto-char (point-min))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be nil)))

  (it "returns nil for items without ORG_GTD property"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Regular task\n")
      (goto-char (point-min))
      (expect (org-gtd-agenda-transient--has-timestamp-p) :to-be nil))))

;;;; State Change Action Tests

(describe "org-gtd-agenda-transient state change actions"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "marks task DONE via --done action"
    ;; Create a task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Task to complete\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Generate agenda and call action
    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Task to complete")
    (beginning-of-line)

    (org-gtd-agenda-transient--done)

    ;; Verify task is DONE
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task to complete")
      (org-back-to-heading t)
      (expect (org-get-todo-state) :to-equal (org-gtd-keywords--done))))

  (it "sets task to WAITING via --waiting action"
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Task to wait on\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Task to wait on")
    (beginning-of-line)

    (org-gtd-agenda-transient--waiting)

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task to wait on")
      (org-back-to-heading t)
      (expect (org-get-todo-state) :to-equal (org-gtd-keywords--wait))))

  (it "sets task to NEXT via --next action"
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Task to make next\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Task to make next")
    (beginning-of-line)

    (org-gtd-agenda-transient--next)

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task to make next")
      (org-back-to-heading t)
      (expect (org-get-todo-state) :to-equal (org-gtd-keywords--next))))

  (it "cancels task via --cancel action"
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Task to cancel\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Task to cancel")
    (beginning-of-line)

    (org-gtd-agenda-transient--cancel)

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task to cancel")
      (org-back-to-heading t)
      (expect (org-get-todo-state) :to-equal (org-gtd-keywords--canceled)))))

;;;; Time Operation Tests

(describe "org-gtd-agenda-transient time operations"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "defers a Calendar item by 1 day via --defer action"
    ;; Create a Calendar task with a timestamp
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Calendar meeting\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Calendar")
      (org-entry-put (point) org-gtd-timestamp "<2025-11-28>")
      (basic-save-buffer))

    ;; Generate agenda and call defer action
    (org-agenda nil "t")
    (goto-char (point-min))
    (search-forward "Calendar meeting")
    (beginning-of-line)

    (org-gtd-agenda-transient--defer)

    ;; Verify timestamp was deferred by 1 day
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Calendar meeting")
      (org-back-to-heading t)
      (let ((new-timestamp (org-entry-get (point) org-gtd-timestamp)))
        (expect new-timestamp :to-equal "<2025-11-29>")))))

;;;; Clarify Action Tests

(describe "org-gtd-agenda-transient clarify actions"

  (it "has a callable clarify-refile function"
    (expect (commandp 'org-gtd-agenda-transient--clarify-refile) :to-be-truthy))

  (it "has a callable clarify-in-place function"
    (expect (commandp 'org-gtd-agenda-transient--clarify-in-place) :to-be-truthy)))

;;;; Clocking and Metadata Action Tests

(describe "org-gtd-agenda-transient clocking and metadata actions"

  (it "has callable clocking functions"
    (expect (commandp 'org-gtd-agenda-transient--clock-in) :to-be-truthy)
    (expect (commandp 'org-gtd-agenda-transient--clock-out) :to-be-truthy))

  (it "has callable metadata functions"
    (expect (commandp 'org-gtd-agenda-transient--effort) :to-be-truthy)
    (expect (commandp 'org-gtd-agenda-transient--priority) :to-be-truthy)
    (expect (commandp 'org-gtd-agenda-transient--tags) :to-be-truthy)
    (expect (commandp 'org-gtd-agenda-transient--note) :to-be-truthy)
    (expect (commandp 'org-gtd-agenda-transient--area-of-focus) :to-be-truthy)))

(provide 'org-gtd-agenda-transient-test)

;;; org-gtd-agenda-transient-test.el ends here
