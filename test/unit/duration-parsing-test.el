;;; duration-parsing-test.el --- Tests for duration parsing -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd--parse-relative-time duration parsing.
;; These are pure unit tests for the time parsing function.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-skip)

(e-unit-initialize)

;;;; Existing units (sanity checks)

(deftest duration/parse-days ()
  "Parses days correctly (e.g., '2d' = 172800 seconds)."
  (assert-equal 172800 (org-gtd--parse-relative-time "2d")))

(deftest duration/parse-weeks ()
  "Parses weeks correctly (e.g., '1w' = 604800 seconds)."
  (assert-equal 604800 (org-gtd--parse-relative-time "1w")))

(deftest duration/parse-hours ()
  "Parses hours correctly (e.g., '3h' = 10800 seconds)."
  (assert-equal 10800 (org-gtd--parse-relative-time "3h")))

(deftest duration/parse-minutes ()
  "Parses minutes correctly (e.g., '30m' = 1800 seconds)."
  (assert-equal 1800 (org-gtd--parse-relative-time "30m")))

;;;; New units: months and years

(deftest duration/parse-months ()
  "Parses months correctly (e.g., '1M' = ~30 days = 2592000 seconds)."
  (assert-equal 2592000 (org-gtd--parse-relative-time "1M")))

(deftest duration/parse-years ()
  "Parses years correctly (e.g., '1y' = ~365 days = 31536000 seconds)."
  (assert-equal 31536000 (org-gtd--parse-relative-time "1y")))

(deftest duration/parse-two-months ()
  "Parses 2M correctly."
  (assert-equal 5184000 (org-gtd--parse-relative-time "2M")))

(deftest duration/parse-two-years ()
  "Parses 2y correctly."
  (assert-equal 63072000 (org-gtd--parse-relative-time "2y")))

;;;; Signed durations

(deftest duration/parse-positive-explicit ()
  "Parses explicit positive sign (+14d)."
  (assert-equal 1209600 (org-gtd--parse-relative-time "+14d")))

(deftest duration/parse-negative ()
  "Parses negative duration (-7d) as negative seconds."
  (assert-equal -604800 (org-gtd--parse-relative-time "-7d")))

(deftest duration/parse-negative-weeks ()
  "Parses negative weeks (-2w)."
  (assert-equal -1209600 (org-gtd--parse-relative-time "-2w")))

(deftest duration/parse-positive-months ()
  "Parses positive months (+1M)."
  (assert-equal 2592000 (org-gtd--parse-relative-time "+1M")))

(deftest duration/parse-negative-months ()
  "Parses negative months (-1M)."
  (assert-equal -2592000 (org-gtd--parse-relative-time "-1M")))

;;;; Duration to reference time

(deftest duration/reference-time-future ()
  "Converts +14d to a time 14 days from now."
  (let* ((now (current-time))
         (ref (org-gtd--duration-to-reference-time "14d"))
         (diff-days (/ (float-time (time-subtract ref now)) 86400)))
    ;; Should be approximately 14 days in the future
    (assert-true (and (> diff-days 13.9) (< diff-days 14.1)))))

(deftest duration/reference-time-past ()
  "Converts -7d to a time 7 days ago."
  (let* ((now (current-time))
         (ref (org-gtd--duration-to-reference-time "-7d"))
         (diff-days (/ (float-time (time-subtract now ref)) 86400)))
    ;; Should be approximately 7 days in the past
    (assert-true (and (> diff-days 6.9) (< diff-days 7.1)))))

(deftest duration/reference-time-today ()
  "Handles 'today' as a special case."
  (let* ((ref (org-gtd--duration-to-reference-time "today"))
         (today-str (format-time-string "%Y-%m-%d"))
         (ref-str (format-time-string "%Y-%m-%d" ref)))
    (assert-equal today-str ref-str)))

(provide 'duration-parsing-test)

;;; duration-parsing-test.el ends here
