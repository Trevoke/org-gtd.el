;;; review-flow-test.el --- Acceptance tests for GTD review workflows -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for GTD review workflows including:
;; - All next actions view
;; - Missed items review (past-dated delegated/calendar/tickler items)
;; - Stuck projects review
;; - Completed projects review
;; - Habits in engage and review views
;; - Area of focus review
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Helper for creating past/future dates

(defun ogt-test--past-calendar-date (days-ago)
  "Return a calendar date DAYS-AGO days in the past."
  (let* ((past-time (time-subtract (current-time) (days-to-time days-ago)))
         (decoded (decode-time past-time)))
    (list (nth 4 decoded)   ; month
          (nth 3 decoded)   ; day
          (nth 5 decoded)))) ; year

(defun ogt-test--future-calendar-date (days-ahead)
  "Return a calendar date DAYS-AHEAD days in the future."
  (let* ((future-time (time-add (current-time) (days-to-time days-ahead)))
         (decoded (decode-time future-time)))
    (list (nth 4 decoded)   ; month
          (nth 3 decoded)   ; day
          (nth 5 decoded)))) ; year

;;; Review of Active Items

(deftest single-action-in-all-next-actions-view ()
  "Verifies single action appears in all next actions view."
  ;; 1. CAPTURE and ORGANIZE single action
  (capture-inbox-item "Review quarterly goals")
  (org-gtd-process-inbox)
  (organize-as-single-action)

  ;; 2. VERIFY appears in all next actions view
  (org-gtd-show-all-next)
  (assert-match "Review quarterly goals" (agenda-raw-text)))

;;; Review of Missed Items

(deftest delegated-item-in-missed-review ()
  "Verifies delegated item with past date shows in missed review."
  (let ((past-date (ogt-test--past-calendar-date 7)))
    ;; 1. CAPTURE and ORGANIZE delegated item with past date
    (capture-inbox-item "Get contract from legal")
    (org-gtd-process-inbox)
    (delegate-item "Legal" past-date)

    ;; 2. VERIFY appears in missed items review
    (org-gtd-review-missed-items)
    (assert-match "Get contract from legal" (agenda-raw-text))))

(deftest calendar-item-in-missed-review ()
  "Verifies calendar item with past date shows in missed review."
  (let ((past-date (ogt-test--past-calendar-date 3)))
    ;; 1. CAPTURE and ORGANIZE calendar item with past date
    (capture-inbox-item "Client presentation")
    (org-gtd-process-inbox)
    (schedule-item past-date)

    ;; 2. VERIFY appears in missed items review
    (org-gtd-review-missed-items)
    (assert-match "Client presentation" (agenda-raw-text))))

(deftest tickler-item-in-missed-review ()
  "Verifies tickler item with past date shows in missed review."
  (let ((past-date (ogt-test--past-calendar-date 5)))
    ;; 1. CAPTURE and ORGANIZE tickler item with past date
    (capture-inbox-item "Review investment portfolio")
    (org-gtd-process-inbox)
    (defer-item past-date)

    ;; 2. VERIFY appears in missed items review
    (org-gtd-review-missed-items)
    (assert-match "Review investment portfolio" (agenda-raw-text))))

;;; Review of Projects

(deftest stuck-project-in-review ()
  "Verifies project shows in stuck projects review when it has no NEXT actions."
  ;; NOTE: A stuck project is one that has TODO state but no NEXT or WAIT tasks.
  ;; Since org-gtd creates projects with NEXT tasks by default via edna triggers,
  ;; we need to transition tasks to TODO (without NEXT) to make the project stuck.

  ;; 1. CAPTURE and ORGANIZE project
  (capture-inbox-item "Launch new website")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Design mockups" :level 2)
    (make-task "Write content" :level 2)
    (make-task "Deploy site" :level 2)
    (organize-as-project))

  ;; 2. Make project stuck by transitioning NEXT tasks back to TODO
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Design mockups")
    (org-todo "TODO")
    (re-search-forward "Write content")
    (org-todo "TODO")
    (re-search-forward "Deploy site")
    (org-todo "TODO"))

  ;; 3. VERIFY project appears in stuck projects review
  (org-gtd-review-stuck-projects)
  (assert-match "Launch new website" (agenda-raw-text)))

(deftest completed-project-not-in-stuck-review ()
  "Verifies completed project does NOT appear in stuck projects review."
  ;; NOTE: A completed project (all tasks DONE/CNCL) should:
  ;; - NOT appear in stuck projects review (has no work remaining)
  ;; - SHOULD appear in completed projects review (ready for archiving)

  ;; 1. CAPTURE and ORGANIZE project
  (capture-inbox-item "Finished Campaign")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Research audience" :level 2)
    (make-task "Create content" :level 2)
    (make-task "Launch campaign" :level 2)
    (organize-as-project))

  ;; 2. Complete all tasks (mark as DONE)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research audience")
    (org-todo "DONE")
    (goto-char (point-min))
    (re-search-forward "Create content")
    (org-todo "DONE")
    (goto-char (point-min))
    (re-search-forward "Launch campaign")
    (org-todo "DONE"))

  ;; 3. VERIFY project does NOT appear in stuck projects review
  (org-gtd-review-stuck-projects)
  (refute-match "Finished Campaign" (agenda-raw-text))

  ;; 4. VERIFY project DOES appear in completed projects review
  (org-gtd-review-completed-projects)
  (assert-match "Finished Campaign" (agenda-raw-text)))

;;; Engage View for Habits

(deftest habit-in-engage-view ()
  "Verifies habit appears in engage view."
  ;; 1. CAPTURE and ORGANIZE habit with daily repeater
  (capture-inbox-item "Daily meditation")
  (org-gtd-process-inbox)
  (organize-as-habit "+1d")

  ;; 2. VERIFY appears in engage view
  (org-gtd-engage)
  (assert-match "Daily meditation" (agenda-raw-text)))

;;; Review of Habits

(deftest habit-in-area-of-focus-review ()
  "Verifies habit appears in area of focus review."
  ;; Habits should appear in review views to ensure they're being maintained
  (let ((org-gtd-areas-of-focus '("Personal" "Work" "Health")))
    ;; 1. CAPTURE and ORGANIZE habit with daily repeater
    (capture-inbox-item "Morning workout")
    (org-gtd-process-inbox)
    (organize-as-habit "+1d")

    ;; 2. Set area of focus (uses CATEGORY property)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (re-search-forward "Morning workout")
      (org-set-property "CATEGORY" "Health"))

    ;; 3. VERIFY appears in area of focus review
    (org-gtd-review-area-of-focus "Health")
    (assert-match "Morning workout" (agenda-raw-text))))

;;; Review of Tickler Items

(deftest tickler-in-area-of-focus-review ()
  "Verifies tickler item appears in area of focus review."
  (let ((future-date (ogt-test--future-calendar-date 7))
        (org-gtd-areas-of-focus '("Personal" "Work" "Health")))
    ;; 1. CAPTURE and ORGANIZE tickler item with future date
    (capture-inbox-item "Learn Italian")
    (org-gtd-process-inbox)
    (defer-item future-date)

    ;; 2. Set area of focus (uses CATEGORY property)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (re-search-forward "Learn Italian")
      (org-set-property "CATEGORY" "Personal"))

    ;; 3. VERIFY appears in area of focus review
    (org-gtd-review-area-of-focus "Personal")
    (assert-match "Learn Italian" (agenda-raw-text))))

(deftest tickler-item-can-be-archived ()
  "Verifies tickler item can be archived after completion."
  ;; 1. CAPTURE and ORGANIZE tickler item
  (capture-inbox-item "Research vacation spots")
  (org-gtd-process-inbox)
  (defer-item (calendar-current-date))

  ;; 2. MARK DONE
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Research vacation spots")
    (org-todo "DONE"))

  ;; 3. ARCHIVE
  (org-gtd-archive-completed-items)

  ;; 4. VERIFY archived
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Research vacation spots" (current-buffer-raw-text))))

;;; Multi-file Project Review
;; NOTE: This test is more complex due to multi-file setup with mock-fs.
;; Skipping for now - will need special handling for secondary files in mock-fs.

;; (deftest multi-file-project-not-stuck-with-next-in-other-file ()
;;   "Verifies multi-file project does NOT appear stuck when it has NEXT task in other file."
;;   ;; TODO: Implement with proper mock-fs multi-file support
;;   )

;;; review-flow-test.el ends here
