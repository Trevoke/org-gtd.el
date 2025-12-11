;;; id-test.el --- Unit tests for ID generation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd ID generation functions.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/id-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; ID Generation

(deftest id-generates-makeshift-for-empty-heading ()
  "Generates a makeshift ID if the heading is empty."
  ;; Pure unit test - no filesystem needed
  (with-temp-buffer
    (org-mode)
    (insert "* \nfoo")
    (goto-char (point-min))
    (org-gtd-id-get-create)

    (assert-match "org-gtd-makeshift-id" (org-entry-get nil "ID"))))

(deftest id-truncates-long-headings ()
  "Truncates heading portion of ID to 50 characters."
  (with-temp-buffer
    (org-mode)
    ;; This heading is way over 50 characters when sanitized
    (insert "* Create a comprehensive implementation plan for the new authentication system with OAuth2 support\nfoo")
    (goto-char (point-min))
    (org-gtd-id-get-create)

    (let* ((id (org-entry-get nil "ID"))
           ;; ID format is: <heading-portion>-<timestamp>
           ;; Timestamp is YYYY-MM-DD-HH-MM-SS (19 chars)
           ;; So heading portion is everything before the last 20 chars (including dash)
           (heading-portion (substring id 0 (- (length id) 20))))
      ;; Heading portion should be <= 50 chars
      (assert-true (<= (length heading-portion) 50))
      ;; ID should still contain recognizable text from heading
      (assert-match "Create-a-comprehensive" id))))

(deftest id-preserves-short-headings ()
  "Does not truncate headings under 50 characters."
  (with-temp-buffer
    (org-mode)
    (insert "* Short task name\nfoo")
    (goto-char (point-min))
    (org-gtd-id-get-create)

    (let ((id (org-entry-get nil "ID")))
      ;; Full heading should be preserved
      (assert-match "Short-task-name" id))))

(deftest id-truncates-at-word-boundary ()
  "Truncates at word boundary (dash) when possible."
  ;; The truncation should end cleanly at a word, not mid-word
  (let ((result (org-gtd-id--truncate-to-limit
                 "Create-a-comprehensive-implementation-plan-for-the-new-auth")))
    ;; Should truncate at a dash, not in the middle of a word
    (assert-true (not (string-match-p "-$" result)))
    (assert-true (<= (length result) 50))))

;;; id-test.el ends here
