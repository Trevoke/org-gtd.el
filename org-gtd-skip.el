;;; org-gtd-skip.el --- various org-agenda-skip-functions -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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
;; Building agenda views is complex, and filtering them effectively can truly
;; require its own language.  This is that language.
;;
;; HERE, COPIED, IS DOCUMENTATION FOR `org-agenda-skip-function',
;; because it reminds us what the API needs to be for all of these helpers.
;;
;; ----
;; Function to be called at each match during agenda construction.
;; If this function returns nil, the current match should not be skipped.
;; Otherwise, the function must return a position from where the search
;; should be continued.

;; This may also be a Lisp form that will be evaluated.  Useful
;; forms include ‘org-agenda-skip-entry-if’ and
;; ‘org-agenda-skip-subtree-if’.  See the Info node ‘(org) Special
;; Agenda Views’ for more details and examples.

;; Never set this variable using ‘setq’ or similar, because then it
;; will apply to all future agenda commands.  If you want a global
;; skipping condition, use the option ‘org-agenda-skip-function-global’
;; instead.

;; The correct way to use ‘org-agenda-skip-function’ is to bind it with ‘let’
;; to scope it dynamically into the agenda-constructing command.
;; A good way to set it is through options in ‘org-agenda-custom-commands’.
;; -----

;;; Code:

;;;; Requirements

(require 'org-gtd-delegate)

;;;; Functions

;;;;; Public

(defun org-gtd-skip-AND (funcs)
  "Ensure all of the functions FUNCS want to skip the current entry."
  (let ((non-nil-funcs (seq-drop-while (lambda (x) (not (funcall x))) funcs)))
    (if non-nil-funcs
        (funcall (car non-nil-funcs)))))

(defun org-gtd-keep-ANY (funcs)
  "Keep if any FUNCS  want to keep this entry."
  (let ((non-nil-funcs (seq-filter (lambda (x) (funcall x)) funcs)))
    (if (= (length funcs) (length non-nil-funcs))
        (funcall (car non-nil-funcs)))))

(defun org-gtd-skip-unless-in-progress ()
  "Skip-function: only keep if it's not one of the DONE keywords"
  (org-agenda-skip-entry-if 'todo org-done-keywords))

(defun org-gtd-skip-if-habit ()
  "Skip-function: only keep this if it's a habit."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal "habit" (org-entry-get (point) "STYLE"))
        subtree-end
      nil)))

(defun org-gtd-skip-unless-action-invalid ()
  "Return non-nil if the action wouldn't show up in the agenda."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (invalidp (or (not (org-entry-is-todo-p))
                      (org-entry-get nil "TODO" org-gtd-todo))))
    (if invalidp
        nil
      subtree-end)))

(defun org-gtd-skip-unless-area-of-focus (area)
  "Skip-function: only keep this if it's a specific GTD AREA of focus."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal (downcase area)
                      (downcase (org-entry-get (point) "CATEGORY")))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-area-of-focus-func (area)
  "Return a skip-function to only keep if it's a specific GTD AREA of focus."
  (apply-partially #'org-gtd-skip-unless-area-of-focus area))

(defun org-gtd-skip-unless-calendar ()
  "Skip-function: only keep this if it's an org-gtd calendar entry."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (and (string-equal (org-entry-get (point) "ORG_GTD" t)
                           org-gtd-calendar)
             (org-entry-get (point) "ORG_GTD_TIMESTAMP"))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-deadline-in-the-past ()
  "Skip entry unless deadline is before now."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (deadline (org-entry-get (point) "DEADLINE"))
        (start-of-day (org-gtd-skip--start-of-day (current-time))))
    (if (and deadline
             (time-less-p (org-time-string-to-time deadline)
                          start-of-day))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-delegated ()
  "Skip entry unless it is delegated."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (org-entry-get (point) org-gtd-delegate-property)
        nil
      subtree-end)))

(defun org-gtd-skip-unless-delegated-to-empty ()
  "Skip-function: only keep this if it's a habit."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (org-entry-get (point) org-gtd-delegate-property)
        nil
      subtree-end)))

(defun org-gtd-skip-unless-habit ()
  "Skip-function: only keep this if it's a habit."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal "habit" (org-entry-get (point) "STYLE"))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-habit-invalid ()
  "Return non-nil if the current headline's ORG_GTD_TIMESTAMP property is not set, null, or not a date."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (style (or (org-entry-get nil "STYLE") ""))
        (timestamp (or (org-entry-get nil "SCHEDULED") "")))
    (if (and (string-equal style "habit")
             (org-string-match-p org-repeat-re timestamp))
        subtree-end
      nil)))

(defun org-gtd-skip-unless-project-heading ()
  "Skip-function: only keep this if it's an org-gtd project heading entry."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (and (equal 2 (org-element-property :level (org-element-at-point)))
             (string-equal (org-entry-get (point) "ORG_GTD" t)
                           org-gtd-projects))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-scheduled-start-in-the-past ()
  "Skip entry unless scheduled time is before now."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (scheduled-start (org-entry-get (point) "SCHEDULED"))
        (start-of-day (org-gtd-skip--start-of-day (current-time))))
    (if (and scheduled-start
             (time-less-p (org-time-string-to-time scheduled-start)
                          start-of-day))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-timestamp-empty-or-invalid ()
  "Return non-nil if the current headline's ORG_GTD_TIMESTAMP property is not set, null, or not a date."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (prop (org-entry-get nil org-gtd-timestamp)))
    (if (and prop
             (org-string-match-p org-ts-regexp-both prop))
        subtree-end
      nil)))

(defun org-gtd-skip-unless-timestamp-in-the-past ()
  "Skip unless ORG_GTD_TIMESTAMP is in the past."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (timestamp (org-entry-get (point) "ORG_GTD_TIMESTAMP"))
        (start-of-day (org-gtd-skip--start-of-day (current-time))))
    (if (and timestamp
             (time-less-p (org-time-string-to-time timestamp)
                          start-of-day))
        nil
      subtree-end)))

;;;;; Private

(defun org-gtd-skip--start-of-day (timestamp)
  "Take TIMESTAMP and return start of day for that day."
  (let ((decoded (decode-time timestamp)))
    (setf (nth 0 decoded) 0)
    (setf (nth 1 decoded) 0)
    (setf (nth 2 decoded) 0)
    (apply #'encode-time decoded)))

;;;; Footer

(provide 'org-gtd-skip)

;;; org-gtd-skip.el ends here
