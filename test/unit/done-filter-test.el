;;; done-filter-test.el --- Tests for done filter skip function -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for done filter skip function used in the View DSL.
;; Tests the skip function building for various done filter configurations
;; including time-based filtering using CLOSED timestamps.
;;

;;; Code:

(require 'e-unit)
(require 'org)
(require 'org-gtd-view-language)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Done Filter Skip Function Tests

(deftest done-filter/t-includes-any-done-item ()
  "Done filter with t includes any done item."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\nCLOSED: [2020-01-01 Wed 10:00]\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter t))
           (result (funcall skip-fn)))
      (assert-nil result))))  ; nil = include

(deftest done-filter/t-skips-not-done-item ()
  "Done filter with t skips not-done items."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter t))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))  ; number = skip

(deftest done-filter/recent-includes-item-closed-within-7-days ()
  "Done filter with recent includes items closed within 7 days."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 3)))))
      (insert (format "* DONE Recent task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Recent task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'recent))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/past-week-includes-item-closed-within-7-days ()
  "Done filter with past-week includes items closed within 7 days."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 5)))))
      (insert (format "* DONE Week-old task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Week-old task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'past-week))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/today-skips-item-with-zero-day-lookback ()
  "Done filter with today (0 days back) has restrictive time comparison.
Note: This tests current implementation which uses exact time comparison,
not calendar day comparison. Items closed earlier 'today' are skipped."
  (with-temp-buffer
    (org-mode)
    (let ((earlier-today (format-time-string "[%Y-%m-%d %a 09:00]"
                                             (time-subtract (current-time) (days-to-time 0.2)))))
      (insert (format "* DONE Earlier task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" earlier-today))
      (goto-char (point-min))
      (search-forward "DONE Earlier task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'today))
             (result (funcall skip-fn)))
        ;; With 0 days back, cutoff is current-time, so items closed earlier are skipped
        (assert-true (numberp result))))))  ; number = skip

(deftest done-filter/past-day-includes-item-closed-within-1-day ()
  "Done filter with past-day includes items closed within 1 day."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 0.5)))))
      (insert (format "* DONE Yesterday task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Yesterday task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'past-day))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/past-month-includes-item-closed-within-30-days ()
  "Done filter with past-month includes items closed within 30 days."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 20)))))
      (insert (format "* DONE Month-old task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Month-old task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'past-month))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/past-year-includes-item-closed-within-365-days ()
  "Done filter with past-year includes items closed within 365 days."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 200)))))
      (insert (format "* DONE Year-old task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Year-old task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'past-year))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/numeric-value-includes-item-closed-within-days ()
  "Done filter with numeric value includes items closed within that many days."
  (with-temp-buffer
    (org-mode)
    (let ((recent-date (format-time-string "[%Y-%m-%d %a %H:%M]"
                                           (time-subtract (current-time) (days-to-time 10)))))
      (insert (format "* DONE Custom-old task\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" recent-date))
      (goto-char (point-min))
      (search-forward "DONE Custom-old task")
      (org-back-to-heading t)
      (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 14))
             (result (funcall skip-fn)))
        (assert-nil result)))))  ; nil = include

(deftest done-filter/skips-old-item-outside-range ()
  "Done filter skips items closed before the time range."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Old task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\nCLOSED: [2020-01-01 Wed 10:00]\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'past-week))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))  ; number = skip

(deftest done-filter/skips-done-item-without-closed-timestamp ()
  "Done filter with time range skips done items without CLOSED timestamp."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task without closed\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter 'recent))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))  ; number = skip

(deftest done-filter/t-includes-done-item-without-closed-timestamp ()
  "Done filter with t includes done items even without CLOSED timestamp."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task without closed\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter t))
           (result (funcall skip-fn)))
      (assert-nil result))))  ; nil = include

(provide 'done-filter-test)
;;; done-filter-test.el ends here
