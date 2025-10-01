;;; org-gtd-review.el --- GTD review logic for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Reviews are a crucial part of GTD.  This code determines how to use
;; the agenda views for review purposes.
;;
;;; Code:

;;;; Requirements
(require 'org)

(require 'org-gtd-core)
(require 'org-gtd-areas-of-focus)
(require 'org-gtd-skip)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-view-language)

;;;; Commands

(defun org-gtd-review--area-of-focus-view-specs (area)
  "Create GTD view specifications for reviewing AREA of focus."
  `(((name . "Active projects")
     (filters . ((category . projects)
                 (area-of-focus . ,area))))

    ((name . "Next actions")
     (filters . ((todo . (,(org-gtd-keywords--next)))
                 (area-of-focus . ,area))))

    ((name . "Reminders")
     (filters . ((category . calendar)
                 (area-of-focus . ,area))))

    ((name . "Routines")
     (filters . ((category . habit)
                 (area-of-focus . ,area))))

    ((name . "Incubated items")
     (filters . ((category . incubate)
                 (timestamp . future)
                 (area-of-focus . ,area))))))

;;;###autoload
(defun org-gtd-review-area-of-focus (&optional area start-date)
  "Generate an overview agenda for a given area of focus.

You can pass an optional AREA (must be a member of `org-gtd-areas-of-focus') to
skip the menu to choose one.
START-DATE tells the code what to use as the first day for the agenda.  It is
mostly of value for testing purposes."
  (interactive (list (completing-read
                      "Which area of focus would you like to review? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
  (when (not (member area org-gtd-areas-of-focus))
    (signal 'org-gtd-invalid-area-of-focus `(,area ,org-gtd-areas-of-focus)))
  (let ((start-date (or start-date (format-time-string "%Y-%m-%d"))))
    (org-gtd-core-prepare-agenda-buffers)
    (with-org-gtd-context
        (let ((org-agenda-custom-commands
               (org-gtd-view-lang--create-custom-commands
                (org-gtd-review--area-of-focus-view-specs area)
                "a"
                (format "Area of Focus: %s" area)))
              (org-agenda-buffer-name (format "*Org Agenda: %s*" area)))
          (org-agenda nil "a")
          (goto-char (point-min))))))

(defconst org-gtd-review-missed-items-view-specs
  '(((name . "Missed calendar events")
     (filters . ((category . calendar)
                 (timestamp . past))))

    ((name . "Incubated events to review")
     (filters . ((category . incubate)
                 (timestamp . past))))

    ((name . "Missed delegated events")
     (filters . ((category . delegated)
                 (timestamp . past)))))
  "GTD view specifications for reviewing missed items.")

(defun org-gtd-review-missed-items (&optional start-date)
  "Agenda view with all incubated, delegated, or calendar items whose dates
are in the past.

You can pass an optional START-DATE to tell the code what to use as the first
day for the agenda.  It is mostly of value for testing purposes."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (let* ((start-date (or start-date (format-time-string "%Y-%m-%d")))
             (org-agenda-custom-commands
              (org-gtd-view-lang--create-custom-commands
               org-gtd-review-missed-items-view-specs
               "g"
               "Missed Items")))
        (org-agenda nil "g"))))

(defun org-gtd-review-stuck-calendar-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "+ORG_GTD=\"Calendar\""
                       ((org-agenda-include-diary nil)
                        (org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

(defun org-gtd-review-stuck-delegated-items ()
  "Agenda view with all invalid delegated actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             `(("g" "foobar"
                ((tags (format "+TODO=\"%s\"" (org-gtd-keywords--wait))
                       ((org-agenda-skip-function
                         '(org-gtd-skip-AND
                           '(org-gtd-skip-unless-timestamp-empty-or-invalid
                             org-gtd-skip-unless-delegated-to-empty)))
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

(defun org-gtd-review-stuck-habit-items ()
  "Agenda view with all invalid habit actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "ORG_GTD=\"Habits\""
                       ((org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

(defun org-gtd-review-stuck-incubated-items ()
  "Agenda view with all invalid incubated actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "ORG_GTD=\"Incubated\""
                       ((org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

;;;###autoload
(defun org-gtd-review-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (with-org-gtd-context
      (org-agenda-list-stuck-projects)))

(defun org-gtd-review-stuck-single-action-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             `(("g" "foobar"
                ((tags (format "+ORG_GTD=\"%s\"" org-gtd-action)
                       ((org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

;;;; Functions

;;;;; Private

(define-error
  'org-gtd-invalid-area-of-focus
  "`%s' is not a member of `%s'"
  'org-gtd-error)

;;;; Footer

(provide 'org-gtd-review)

;;; org-gtd-review.el ends here
