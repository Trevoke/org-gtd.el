;;; org-gtd-skip.el --- various org-agenda-skip-functions -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Skip functions for org-agenda views.  Most filtering in org-gtd v4 is
;; handled by the view-language module using org-ql, but these functions
;; remain for specific use cases.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-agenda)
(require 'org-gtd-types)
(require 'org-gtd-core)

;; Forward declarations to avoid circular dependency
(declare-function org-gtd-projects--has-active-tasks-p "org-gtd-projects")
(declare-function org-gtd-projects--is-stuck-p "org-gtd-projects")

;;;; Functions

(defun org-gtd-skip-unless-in-progress ()
  "Skip-function: only keep if it's not one of the DONE keywords."
  (org-agenda-skip-entry-if 'todo org-done-keywords))

(defun org-gtd-skip-unless-area-of-focus (area)
  "Skip-function: only keep this if it's a specific GTD AREA of focus."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal (downcase area)
                      (downcase (org-entry-get (point) org-gtd-prop-area-of-focus)))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-area-of-focus-func (area)
  "Return a skip-function to only keep if it's a specific GTD AREA of focus."
  (apply-partially #'org-gtd-skip-unless-area-of-focus area))

;;;; Property Predicates

(defun org-gtd-pred--property-equals (property value)
  "Return predicate checking PROPERTY equals VALUE at point.
The predicate returns t if property matches, nil otherwise."
  (lambda ()
    (equal (org-entry-get (point) property) value)))

(defun org-gtd-pred--property-empty-or-missing (property)
  "Return predicate checking PROPERTY is empty or missing at point.
The predicate returns t if property is missing or empty/whitespace-only."
  (lambda ()
    (let ((prop-value (org-entry-get (point) property)))
      (or (not prop-value)
          (string-empty-p (string-trim prop-value))))))

(defun org-gtd-pred--property-invalid-timestamp (property)
  "Return predicate checking PROPERTY is missing or not a valid timestamp.
The predicate returns t if property is missing, empty, or not a valid
org-mode timestamp."
  (lambda ()
    (let ((prop-value (org-entry-get (point) property)))
      (or (not prop-value)
          (string-empty-p (string-trim prop-value))
          (not (string-match org-ts-regexp-both prop-value))))))

;;;; State Predicates

(defun org-gtd-pred--not-done ()
  "Return predicate checking if item at point is not done.
Returns t if the TODO state is not a member of `org-done-keywords'."
  (lambda ()
    (not (org-entry-is-done-p))))

;;;; Project Predicates

(defun org-gtd-pred--project-has-active-tasks ()
  "Return predicate checking if project at point has active tasks.
Only returns t for headings with ORG_GTD=\"Projects\" that have
at least one non-done task."
  (lambda ()
    (when (string-equal (org-entry-get nil "ORG_GTD") org-gtd-projects)
      (org-gtd-projects--has-active-tasks-p (point-marker)))))

(defun org-gtd-pred--project-is-stuck ()
  "Return predicate checking if project at point is stuck.
Only returns t for headings with ORG_GTD=\"Projects\" that have
active tasks but no immediately actionable (NEXT/WAIT) tasks."
  (lambda ()
    (when (string-equal (org-entry-get nil "ORG_GTD") org-gtd-projects)
      (org-gtd-projects--is-stuck-p (point-marker)))))

;;;; Timestamp Comparison Predicates

(defun org-gtd-pred--property-ts< (property reference-date)
  "Return predicate checking PROPERTY timestamp is before REFERENCE-DATE.
REFERENCE-DATE can be \"today\", \"+7d\", or a date string.
Returns nil if property is missing or invalid."
  (lambda ()
    (when-let ((ts-str (org-entry-get (point) property)))
      (let ((ts-time (org-gtd--parse-timestamp ts-str))
            (ref-time (org-gtd--parse-reference-date reference-date)))
        (and ts-time ref-time (time-less-p ts-time ref-time))))))

(defun org-gtd-pred--property-ts> (property reference-date)
  "Return predicate checking PROPERTY timestamp is after REFERENCE-DATE.
REFERENCE-DATE can be \"today\", \"+7d\", or a date string.
Returns nil if property is missing or invalid."
  (lambda ()
    (when-let ((ts-str (org-entry-get (point) property)))
      (let ((ts-time (org-gtd--parse-timestamp ts-str))
            (ref-time (org-gtd--parse-reference-date reference-date)))
        (and ts-time ref-time (time-less-p ref-time ts-time))))))

(defun org-gtd-pred--property-ts= (property reference-date)
  "Return predicate checking PROPERTY timestamp equals REFERENCE-DATE.
REFERENCE-DATE can be \"today\" or a date string.
Compares dates only (ignores time of day).
Returns nil if property is missing or invalid."
  (lambda ()
    (when-let ((ts-str (org-entry-get (point) property)))
      (let ((ts-time (org-gtd--parse-timestamp ts-str))
            (ref-time (org-gtd--parse-reference-date reference-date)))
        (when (and ts-time ref-time)
          ;; Compare year, month, day only
          (let ((ts-decoded (decode-time ts-time))
                (ref-decoded (decode-time ref-time)))
            (and (= (nth 3 ts-decoded) (nth 3 ref-decoded))   ; day
                 (= (nth 4 ts-decoded) (nth 4 ref-decoded))   ; month
                 (= (nth 5 ts-decoded) (nth 5 ref-decoded)))))))))  ; year

;;;; Timestamp Parsing

(defun org-gtd--parse-timestamp (ts-string)
  "Parse org timestamp TS-STRING to internal time representation.
Returns nil if TS-STRING is nil, empty, or not a valid org timestamp."
  (when (and ts-string
             (stringp ts-string)
             (not (string-empty-p (string-trim ts-string)))
             (string-match org-ts-regexp-both ts-string))
    (org-time-string-to-time ts-string)))

(defun org-gtd--parse-reference-date (ref)
  "Parse reference date REF to internal time representation.
REF can be:
  - \"today\" for current date
  - \"+Nd\" for N days from now (e.g., \"+7d\")
  - \"+Nw\" for N weeks from now (e.g., \"+1w\")
  - \"+Nm\" for N months from now (e.g., \"+1m\")
  - \"+Ny\" for N years from now (e.g., \"+1y\")
  - A date string like \"2025-01-15\""
  (cond
   ((string-equal ref "today")
    (org-time-string-to-time (format-time-string "%Y-%m-%d")))
   ((string-match "^\\+\\([0-9]+\\)\\([dwmy]\\)$" ref)
    (let* ((n (string-to-number (match-string 1 ref)))
           (unit (match-string 2 ref))
           (days (* n (pcase unit
                        ("d" 1)
                        ("w" 7)
                        ("m" 30)
                        ("y" 365)))))
      (time-add (current-time) (days-to-time days))))
   (t
    (org-time-string-to-time ref))))

;;;; Skip Function Composition

(defun org-gtd-skip--compose (predicates)
  "Return skip function that includes items matching ALL PREDICATES.
PREDICATES is a list of predicate functions that return t to include an item.
The returned skip function returns nil to include an item (all predicates pass)
or the end of the current entry to skip it (any predicate fails).
Note: Uses `org-entry-end-position' instead of `org-end-of-subtree' to ensure
child headings are still checked even if parent doesn't match all predicates."
  (lambda ()
    (let ((end (org-entry-end-position)))
      (if (cl-every #'funcall predicates)
          nil   ; Include - all predicates passed
        end)))) ; Skip - at least one failed

;;;; Footer

(provide 'org-gtd-skip)

;;; org-gtd-skip.el ends here
