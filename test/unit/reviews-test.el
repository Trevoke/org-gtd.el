;;; reviews-test.el --- Tests for org-gtd review functionality -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd review functionality.
;;
;; Test Coverage:
;; - Area of focus review throws error for invalid area (1 test)
;; - Area of focus review shows all item types (1 test)
;; - Missed events review shows past undone items (1 test)
;; - Area of focus review shows tickler projects (1 test)
;;
;; Note: "shows tickler projects in area review" migrated to
;; test-eunit/acceptance/review-flow-test.el
;;
;; Migrated from test/reviews-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    ;; Note: org-gtd-areas-of-focus needs to be set in each test that uses it
    ;; because of an unexplained scope issue where setq here doesn't persist
    (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
    (unwind-protect
        (funcall proceed context)
      (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
      (setq org-gtd-areas-of-focus nil))))

;;; Area of Focus Tests

(deftest reviews/throws-error-for-invalid-area ()
  "Throws error when called with an area not in the list."
  (setq org-gtd-areas-of-focus '("Health" "Home" "Career"))
  (assert-raises 'org-gtd-invalid-area-of-focus
    (org-gtd-review-area-of-focus "Playing")))

(deftest reviews/area-of-focus-shows-all-item-types ()
  "Shows projects, next actions, habits, tickler items in agenda for area."
  ;; Ensure areas-of-focus is set (workaround for around-each scope issue)
  (setq org-gtd-areas-of-focus '("Health" "Home" "Career"))
  (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                      "foo"
                      (org-file-contents
                       "test/fixtures/areas-of-focus.org"))))
    (org-gtd-review-area-of-focus "Home" "2021-11-20")

    (with-current-buffer org-agenda-buffer
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (assert-equal "*Org Agenda: Home*" (buffer-name))
        ;; Check that sections and items are present (not using .* across lines)
        (assert-match "Active projects" content)
        (assert-match "Fix the roof" content)
        (assert-match "Next actions" content)
        (assert-match "Clean gutters" content)
        (assert-match "Reminders" content)
        (assert-match "Meet plumber" content)
        (assert-match "Routines" content)
        (assert-match "Sweep the" content)
        (assert-match "Tickler items" content)
        (assert-match "For later" content)))))

;;; Missed Events Tests

(deftest reviews/missed-events-shows-past-undone-items ()
  "Shows unfinished items that have a timestamp in the past."
  ;; Ensure areas-of-focus is set (workaround for around-each scope issue)
  (setq org-gtd-areas-of-focus '("Health" "Home" "Career"))
  (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                      "foo"
                      (org-file-contents
                       "test/fixtures/gtd-file.org"))))
    (org-gtd-review-missed-items "2021-11-20")
    (let ((agenda-contents (agenda-raw-text)))
      ;; These are DONE OR CANCELED - should NOT appear
      (refute-match "not worth thinking about" agenda-contents)
      (refute-match "write a nice test" agenda-contents)
      ;; These are IN THE FUTURE - should NOT appear
      (refute-match "repeating item" agenda-contents)
      (refute-match "For later" agenda-contents)
      ;; These are UNDONE IN THE PAST - should appear
      (assert-match "probably overdue by now" agenda-contents)
      (assert-match "Time to review this one" agenda-contents)
      ;; This is OVERDUE DELEGATED - should appear
      (assert-match "Overdue delegated" agenda-contents))))

;;; Tickler Projects in Area Review Tests

(deftest reviews/area-review-shows-tickler-projects ()
  "Shows tickler projects in area of focus review."
  (let ((org-gtd-areas-of-focus '("Work" "Personal")))
    ;; Create active and tickler projects in Work area
    ;; Use with-simulated-input to handle area-of-focus prompt during organize
    (with-simulated-input "Work RET"
      (create-project "Active work project"))
    (with-simulated-input "Work RET"
      (create-project "Tickler work project"))

    (with-current-buffer (org-gtd--default-file)
      ;; Set CATEGORY property for both projects to Work area
      (goto-char (point-min))
      (search-forward "Active work project")
      (org-back-to-heading t)
      (org-entry-put (point) "CATEGORY" "Work")

      (goto-char (point-min))
      (search-forward "Tickler work project")
      (org-back-to-heading t)
      (org-entry-put (point) "CATEGORY" "Work")

      ;; Tickler the second project
      (org-gtd-tickler "2025-12-01"))

    ;; Run area of focus review for Work
    (org-gtd-review-area-of-focus "Work")

    (with-current-buffer org-agenda-buffer
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Should show active project in Active projects section
        (assert-match "Active projects" content)
        (assert-match "Active work project" content)
        ;; Should show tickler project in Tickler projects section
        (assert-match "Tickler projects" content)
        (assert-match "Tickler work project" content)))))

(provide 'reviews-test)

;;; reviews-test.el ends here
