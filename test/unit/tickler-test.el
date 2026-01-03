;;; tickler-test.el --- Tests for org-gtd tickler functionality -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd tickler and reactivation functionality.
;;
;; Test Coverage:
;; - Tickler item stores review date in ORG_GTD_TIMESTAMP (1 test)
;; - Smart tickler dispatcher detects project heading (1 test)
;; - Smart tickler dispatcher detects single item (1 test)
;; - Smart reactivation dispatcher reactivates tickler'd project (1 test)
;;
;; Note: "appears in daily agenda when review date arrives" migrated to
;; test-eunit/acceptance/basic-workflows-test.el
;;
;; Migrated from test/tickler-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Tickler Item Tests

(deftest tickler/stores-review-date-in-timestamp-property ()
  "Stores review date in ORG_GTD_TIMESTAMP property."
  (let* ((date (calendar-current-date))
         (year (nth 2 date))
         (month (nth 0 date))
         (day (nth 1 date)))
    (create-deferred-item "Yowza" date)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Yowza")
      (assert-equal 'tickler (task-type (current-task)))
      (let ((timestamp (task-timestamp (current-task))))
        (assert-match (format "%s-%#02d-%#02d" year month day) timestamp)))))

;;; Smart Tickler Dispatcher Tests

(deftest tickler/smart-dispatcher-detects-project-heading ()
  "Smart tickler dispatcher detects project heading and calls org-gtd-project-incubate."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)

    ;; Call org-gtd-tickler with review date parameter
    (org-gtd-tickler "2025-12-01")

    ;; Verify project was tickler'd
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))))

(deftest tickler/smart-dispatcher-detects-single-item ()
  "Smart tickler dispatcher detects single item and uses existing tickler logic."
  ;; Verify that calling org-gtd-tickler on a single item
  ;; doesn't error and uses the existing path
  (create-single-action "Test action")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test action")
    (org-back-to-heading t)

    ;; Verify it's not a project (should use existing tickler logic)
    (assert-not-equal "Projects" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))))

;;; Smart Reactivation Dispatcher Tests

(deftest tickler/smart-reactivation-detects-ticklerd-project ()
  "Smart reactivation dispatcher detects tickler'd project and reactivates it."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)

    ;; Tickler it first
    (org-gtd-tickler "2025-12-01")

    ;; Verify it's tickler'd
    (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD"))

    ;; Reactivate it
    (org-gtd-reactivate)

    ;; Verify it's reactivated
    (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))))

(provide 'tickler-test)

;;; tickler-test.el ends here
