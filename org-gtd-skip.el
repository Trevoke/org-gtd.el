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
;; Skip functions and predicates for org-agenda views.  The view-language
;; module uses these predicates to build skip functions for native org-agenda
;; blocks based on declarative view specifications.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-duration)
(require 'org-gtd-types)
(require 'org-gtd-core)

;; Forward declarations to avoid circular dependency
(declare-function org-gtd-projects--has-active-tasks-p "org-gtd-projects")
(declare-function org-gtd-projects--is-stuck-p "org-gtd-projects")
(declare-function org-gtd-project-last-clock-out-time "org-gtd-projects")

;;;; Functions

(defun org-gtd--compare-values (op actual-value reference-value)
  "Compare ACTUAL-VALUE with REFERENCE-VALUE using OP.
OP should be one of: <, >, <=, >=.
Returns t if comparison passes, nil otherwise."
  (pcase op
    ('< (< actual-value reference-value))
    ('> (> actual-value reference-value))
    ('<= (<= actual-value reference-value))
    ('>= (>= actual-value reference-value))
    (_ nil)))

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

;;;; Priority Predicates

(defun org-gtd-pred--priority-matches (value)
  "Return predicate checking if item priority matches VALUE.
VALUE can be:
  - A symbol/string like A (single priority)
  - A list like (A B) (any of these priorities)
  - A comparison like (>= B)
  - nil (no priority set)"
  (lambda ()
    (let ((item-priority (save-excursion
                           (org-back-to-heading t)
                           (when (looking-at org-complex-heading-regexp)
                             ;; Group 3 is "[#A]", extract just the letter
                             (let ((cookie (match-string-no-properties 3)))
                               (when (and cookie (string-match "\\[#\\(.\\)\\]" cookie))
                                 (match-string 1 cookie)))))))
      (cond
       ;; nil = match missing priority
       ((null value)
        (or (null item-priority)
            (string-empty-p item-priority)))
       ;; Comparison: (>= B)
       ((and (listp value) (memq (car value) '(< > <= >=)))
        (when item-priority
          (let* ((op (car value))
                 (ref (cadr value))
                 (highest (or org-priority-highest ?A))
                 (item-num (1+ (- (aref item-priority 0) highest)))
                 (ref-num (1+ (- (aref (symbol-name ref) 0) highest))))
            (pcase op
              ('< (< item-num ref-num))
              ('> (> item-num ref-num))
              ('<= (<= item-num ref-num))
              ('>= (<= item-num ref-num))))))  ; >= B means numerically <= B
       ;; List: (A B)
       ((listp value)
        (when item-priority
          (member item-priority
                  (mapcar (lambda (p) (if (symbolp p) (symbol-name p) p)) value))))
       ;; Single value
       (t
        (when item-priority
          (equal item-priority
                 (if (symbolp value) (symbol-name value) value))))))))

;;;; Tag Predicates

(defun org-gtd-pred--tags-matches (tags)
  "Return predicate checking if item has any of TAGS.
TAGS is a list of tag strings (e.g., (\"@work\" \"@home\")).
Uses OR semantics: returns t if entry has ANY of the specified tags."
  (lambda ()
    (let ((entry-tags (org-get-tags nil t)))  ; nil=current, t=local only
      (cl-some (lambda (tag) (member tag entry-tags)) tags))))

;;;; Deadline/Scheduled Predicates

(defun org-gtd-pred--deadline-matches (time-spec)
  "Return predicate checking if item's deadline matches TIME-SPEC.
TIME-SPEC can be:
  - \\='past - deadline is before today
  - \\='today - deadline is today
  - \\='future - deadline is after today
Returns nil if item has no deadline."
  (lambda ()
    (when-let ((deadline-time (org-get-deadline-time (point))))
      (let* ((today (org-today))
             (deadline-day (time-to-days deadline-time)))
        (pcase time-spec
          ('past (< deadline-day today))
          ('today (= deadline-day today))
          ('future (> deadline-day today))
          (_ nil))))))

(defun org-gtd-pred--scheduled-matches (time-spec)
  "Return predicate checking if item's scheduled date matches TIME-SPEC.
TIME-SPEC can be:
  - \\='past - scheduled before today
  - \\='today - scheduled for today
  - \\='future - scheduled after today
Returns nil if item has no scheduled date."
  (lambda ()
    (when-let ((scheduled-time (org-get-scheduled-time (point))))
      (let* ((today (org-today))
             (scheduled-day (time-to-days scheduled-time)))
        (pcase time-spec
          ('past (< scheduled-day today))
          ('today (= scheduled-day today))
          ('future (> scheduled-day today))
          (_ nil))))))

;;;; Todo Keyword Predicates

(defun org-gtd-pred--todo-matches (keywords)
  "Return predicate checking if item's TODO state is in KEYWORDS.
KEYWORDS is a list of TODO keyword strings (e.g., (\"TODO\" \"NEXT\")).
Uses OR semantics: returns t if entry has ANY of the specified keywords."
  (lambda ()
    (when-let ((todo-state (org-get-todo-state)))
      (member todo-state keywords))))

;;;; Clocked Time Predicates

(defun org-gtd-pred--clocked-matches (value)
  "Return predicate checking if item clocked time matches VALUE.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes
  - (> \"2:00\") - more than 2 hours
  - (between \"0:30\" \"2:00\") - range (inclusive)
  - nil - zero time clocked"
  (lambda ()
    (let ((clocked-mins (save-excursion
                          (org-clock-sum-current-item))))
      (cond
       ;; nil = match zero clocked time
       ((null value)
        (or (null clocked-mins) (= clocked-mins 0)))
       ;; Have clocked time, compare
       (t
        (let ((mins (or clocked-mins 0)))
          (pcase (car value)
            ('<
             (let ((threshold (org-duration-to-minutes (cadr value))))
               (< mins threshold)))
            ('>
             (let ((threshold (org-duration-to-minutes (cadr value))))
               (> mins threshold)))
            ('between
             (let ((low (org-duration-to-minutes (cadr value)))
                   (high (org-duration-to-minutes (caddr value))))
               (and (>= mins low) (<= mins high))))
            (_ nil))))))))

(defun org-gtd-pred--last-clocked-out-matches (value)
  "Return predicate checking if item's last clock-out matches VALUE.
VALUE can be:
  - (> \"2d\") - last clocked out more than 2 days ago
  - (< \"1w\") - last clocked out within the past week
  - nil - never clocked out

For project headings, checks all tasks in the project."
  (lambda ()
    (let* ((is-project (equal (org-entry-get (point) "ORG_GTD") org-gtd-projects))
           (last-clock (if is-project
                           (org-gtd-project-last-clock-out-time (point-marker))
                         (org-clock-get-last-clock-out-time))))
      (cond
       ;; nil = match items never clocked
       ((null value)
        (null last-clock))
       ;; Have a clock time, compare
       (last-clock
        (condition-case nil
            (let* ((op (car value))
                   (duration-str (cadr value))
                   (threshold-seconds (org-gtd--parse-relative-time duration-str))
                   (age-seconds (float-time (time-subtract (current-time) last-clock))))
              (org-gtd--compare-values op age-seconds threshold-seconds))
          (error nil)))  ; Invalid time format, skip item
       ;; No clock time but value specified - doesn't match
       (t nil)))))

;;;; Effort Predicates

(defun org-gtd-pred--effort-matches (value)
  "Return predicate checking if item effort matches VALUE.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes
  - (> \"1:00\") - more than 1 hour
  - (between \"0:15\" \"1:00\") - range (inclusive)
  - nil - no effort set"
  (lambda ()
    (let ((effort-str (org-entry-get (point) "Effort")))
      (cond
       ;; nil = match missing effort
       ((null value)
        (or (null effort-str)
            (string-empty-p effort-str)))
       ;; Have effort, need to compare
       ((and effort-str (not (string-empty-p effort-str)))
        (condition-case nil
            (let ((effort-mins (org-duration-to-minutes effort-str)))
              (pcase (car value)
                ('<
                 (let ((threshold (org-duration-to-minutes (cadr value))))
                   (< effort-mins threshold)))
                ('>
                 (let ((threshold (org-duration-to-minutes (cadr value))))
                   (> effort-mins threshold)))
                ('between
                 (let ((low (org-duration-to-minutes (cadr value)))
                       (high (org-duration-to-minutes (caddr value))))
                   (and (>= effort-mins low) (<= effort-mins high))))
                (_ nil)))
          (error nil)))  ; Invalid duration format, skip item
       (t nil)))))  ; No effort but filter wants comparison

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

(defun org-gtd--parse-relative-time (duration-str)
  "Parse DURATION-STR like \"2d\", \"+14d\", \"-7d\" to seconds.
Supports: m (minutes), h (hours), d (days), w (weeks), M (months), y (years).
Sign: + or no sign = positive, - = negative (returns negative seconds)."
  (let ((num (string-to-number duration-str))
        (unit (substring duration-str -1)))
    (pcase unit
      ("d" (* num 86400))      ; days
      ("w" (* num 604800))     ; weeks
      ("h" (* num 3600))       ; hours
      ("m" (* num 60))         ; minutes
      ("M" (* num 2592000))    ; months (~30 days)
      ("y" (* num 31536000))   ; years (~365 days)
      (_ (error "Unknown time unit in %s" duration-str)))))

(defun org-gtd--duration-to-reference-time (duration-str)
  "Convert DURATION-STR to a reference time for comparison.
DURATION-STR can be:
  - \"today\" for current date at start of day
  - \"14d\" or \"+14d\" for 14 days from now
  - \"-7d\" for 7 days ago
  - Any format supported by `org-gtd--parse-relative-time'
Returns an Emacs time value."
  (if (string-equal duration-str "today")
      (org-time-string-to-time (format-time-string "%Y-%m-%d"))
    (let ((seconds (org-gtd--parse-relative-time duration-str)))
      (time-add (current-time) seconds))))

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
