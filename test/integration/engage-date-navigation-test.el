;;; engage-date-navigation-test.el --- Tests for date navigation in engage view -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests verifying that delegated and tickler items respond
;; to date navigation in the engage view.
;;
;; User behaviors tested:
;; - Delegated item due Friday appears when jumping to Friday
;; - Delegated item due Friday does NOT appear when viewing today
;; - Tickler item due Friday appears when jumping to Friday
;; - Reflect views with (when . future) still show all future items

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'org-gtd-engage)
(require 'org-gtd-delegate)
(require 'org-gtd-reflect)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (unwind-protect
        (funcall proceed context)
      (test--cleanup-agenda))))

(defun test--create-delegated-item (title person date-string)
  "Create a delegated item with TITLE assigned to PERSON due on DATE-STRING."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* WAIT %s\n" title))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Delegated")
    (org-entry-put (point) "DELEGATED_TO" person)
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date-string))
    (basic-save-buffer)))

(defun test--create-tickler-item (title date-string)
  "Create a tickler item with TITLE due on DATE-STRING."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Tickler")
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date-string))
    (basic-save-buffer)))

(defun test--engage-view-contains-p (text)
  "Return non-nil if engage view buffer contains TEXT."
  (with-current-buffer "*Org Agenda*"
    (goto-char (point-min))
    (search-forward text nil t)))

(defun test--cleanup-agenda ()
  "Kill agenda buffer if it exists."
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

;;; Delegated Item Tests

(deftest engage/delegated-item-appears-on-its-date ()
  "Delegated item appears in engage view when jumping to its due date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-delegated-item "Call Bob about project" "Bob" future-date)
    (org-gtd-engage)
    ;; Jump to the future date
    (org-agenda-goto-date future-date)
    (assert-true (test--engage-view-contains-p "Call Bob about project"))))

(deftest engage/delegated-item-not-shown-on-other-dates ()
  "Delegated item does NOT appear in engage view when viewing a different date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-delegated-item "Call Bob about project" "Bob" future-date)
    (org-gtd-engage)
    ;; Viewing today - item should NOT appear
    (assert-nil (test--engage-view-contains-p "Call Bob about project"))))

;;; Tickler Item Tests

(deftest engage/tickler-item-appears-on-its-date ()
  "Tickler item appears in engage view when jumping to its due date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-tickler-item "Review insurance options" future-date)
    (org-gtd-engage)
    ;; Jump to the future date
    (org-agenda-goto-date future-date)
    (assert-true (test--engage-view-contains-p "Review insurance options"))))

(deftest engage/tickler-item-not-shown-on-other-dates ()
  "Tickler item does NOT appear in engage view when viewing a different date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-tickler-item "Review insurance options" future-date)
    (org-gtd-engage)
    ;; Viewing today - item should NOT appear
    (assert-nil (test--engage-view-contains-p "Review insurance options"))))

;;; Reflect View Tests (when filter preserved)

(deftest reflect/upcoming-delegated-shows-all-future-items ()
  "Reflect upcoming delegated view shows all future delegated items."
  (let ((future-date-1 (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60))))
        (future-date-2 (format-time-string "%Y-%m-%d" (time-add nil (* 10 24 60 60)))))
    (test--create-delegated-item "Call Bob" "Bob" future-date-1)
    (test--create-delegated-item "Call Alice" "Alice" future-date-2)
    (org-gtd-reflect-upcoming-delegated)
    ;; Both items should appear (list view, not date-specific)
    (assert-true (test--engage-view-contains-p "Call Bob"))
    (assert-true (test--engage-view-contains-p "Call Alice"))))

(provide 'engage-date-navigation-test)

;;; engage-date-navigation-test.el ends here
