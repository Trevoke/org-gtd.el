;;; agenda-transient-test.el --- Tests for org-gtd agenda transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd agenda transient functionality.
;;
;; Test Coverage:
;; - Context detection (2 tests)
;; - State change actions (4 tests)
;; - Time operations (1 test)
;;
;; Migrated from test/org-gtd-agenda-transient-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-agenda-transient)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Context Detection Tests

(deftest context/returns-nil-when-not-on-agenda-line ()
  "Returns nil when not on an agenda line."
  (with-temp-buffer
    (insert "Not an agenda buffer")
    (assert-nil (org-gtd-agenda-transient--task-at-point))))

(deftest context/returns-task-info-on-agenda-line ()
  "Returns task info when on an agenda line with a task."
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
    (assert-true info)
    (assert-match "Test task for agenda" (plist-get info :title))
    (assert-equal "Actions" (plist-get info :org-gtd-type))
    (assert-true (plist-get info :marker)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

;;; State Change Action Tests

(deftest state/marks-task-done ()
  "Marks task DONE via --done action."
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
    (assert-equal (org-gtd-keywords--done) (org-get-todo-state)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

(deftest state/sets-task-waiting ()
  "Sets task to WAITING via --waiting action."
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
    (assert-equal (org-gtd-keywords--wait) (org-get-todo-state)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

(deftest state/sets-task-next ()
  "Sets task to NEXT via --next action."
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
    (assert-equal (org-gtd-keywords--next) (org-get-todo-state)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

(deftest state/cancels-task ()
  "Cancels task via --cancel action."
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
    (assert-equal (org-gtd-keywords--canceled) (org-get-todo-state)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

;;; Time Operation Tests

(deftest time/defers-calendar-item-by-one-day ()
  "Defers a Calendar item by 1 day via --defer action."
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
      (assert-equal "<2025-11-29>" new-timestamp)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

(provide 'agenda-transient-test)

;;; agenda-transient-test.el ends here
