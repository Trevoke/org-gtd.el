;;; org-gtd-reflect.el --- GTD Reflect step for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; The GTD "Reflect" step - reviewing your system to keep it current and trusted.
;; This code provides agenda views for various review purposes.
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

(defun org-gtd-reflect--area-of-focus-view-specs (area)
  "Create GTD view specifications for reflecting on AREA of focus."
  `(((name . "Active projects")
     (filters . ((category . projects)
                 (area-of-focus . ,area))))

    ((name . "Incubated projects")
     (filters . ((category . incubated-projects)
                 (area-of-focus . ,area)))
     (prefix-format . "  Incubated: "))

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
(defun org-gtd-reflect-area-of-focus (&optional area _start-date)
  "Generate an overview agenda for a given area of focus.

You can pass an optional AREA (must be a member of `org-gtd-areas-of-focus') to
skip the menu to choose one.
START-DATE tells the code what to use as the first day for the agenda.  It is
mostly of value for testing purposes."
  (interactive (list (completing-read
                      "Which area of focus would you like to reflect on? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
  (when (not (member area org-gtd-areas-of-focus))
    (signal 'org-gtd-invalid-area-of-focus `(,area ,org-gtd-areas-of-focus)))
  (let ((org-agenda-buffer-name (format "*Org Agenda: %s*" area)))
    (org-gtd-view-show (org-gtd-reflect--area-of-focus-view-specs area))))

(defconst org-gtd-reflect-missed-items-view-specs
  '(((name . "Missed calendar events")
     (filters . ((category . calendar)
                 (timestamp . past))))

    ((name . "Incubated events to review")
     (filters . ((category . incubate)
                 (timestamp . past))))

    ((name . "Missed delegated events")
     (filters . ((category . delegated)
                 (timestamp . past)))))
  "GTD view specifications for reflecting on missed items.")

(defun org-gtd-reflect-missed-items (&optional _start-date)
  "Agenda view with all incubated, delegated, or calendar items whose dates
are in the past.

You can pass an optional START-DATE to tell the code what to use as the first
day for the agenda.  It is mostly of value for testing purposes."
  (interactive)
  (org-gtd-view-show org-gtd-reflect-missed-items-view-specs))

(defun org-gtd-reflect-stuck-calendar-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Calendar Items")
     (filters . ((category . calendar)
                 (invalid-timestamp . t))))))

(defun org-gtd-reflect-stuck-delegated-items ()
  "Agenda view with all invalid delegated actions."
  (interactive)
  (org-gtd-view-show
   `((name . "Stuck Delegated Items")
     (filters . ((todo . (,(org-gtd-keywords--wait)))
                 (category . delegated)
                 (invalid-timestamp . t))))))

(defun org-gtd-reflect-stuck-habit-items ()
  "Agenda view with all invalid habit actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Habit Items")
     (filters . ((category . habit)
                 (invalid-timestamp . t))))))

(defun org-gtd-reflect-stuck-incubated-items ()
  "Agenda view with all invalid incubated actions."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Incubated Items")
     (filters . ((category . incubate)
                 (invalid-timestamp . t))))))

;;;###autoload
(defun org-gtd-reflect-stuck-projects ()
  "Show all projects that do not have a next action.

Stuck projects have TODO tasks (work remaining) but no NEXT or WAIT tasks,
indicating they need attention to identify the next actionable step."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Projects")
     (filters . ((category . stuck-projects))))))

(defun org-gtd-reflect-stuck-single-action-items ()
  "Agenda view with all invalid single action items."
  (interactive)
  (org-gtd-view-show
   `((name . "Stuck Single Action Items")
     (filters . ((property . (("ORG_GTD" . ,org-gtd-action)))
                 (invalid-timestamp . t))))))

;;;###autoload
(defun org-gtd-reflect-completed-items (&optional days-back)
  "Show items completed in the last DAYS-BACK days (default 7).

This view shows all items with done TODO states that were closed within
the specified time period. Useful for weekly reviews to see what was
accomplished."
  (interactive "p")
  (let* ((days (or days-back 7))
         (time-spec (cond
                     ((= days 1) 'past-day)
                     ((= days 7) 'past-week)
                     ((= days 30) 'past-month)
                     ((= days 365) 'past-year)
                     (t 'recent))))
    (org-gtd-view-show
     `((name . ,(format "Completed in Last %d Days" days))
       (filters . ((done . t)
                   (closed . ,time-spec)))))))

;;;###autoload
(defun org-gtd-reflect-completed-projects ()
  "Show all completed projects.

Projects are considered completed when all their tasks are done.
This view helps identify projects ready for archiving."
  (interactive)
  (org-gtd-view-show
   '((name . "Completed Projects")
     (filters . ((category . completed-projects))))))

;;;; Functions

;;;;; Private

(define-error
  'org-gtd-invalid-area-of-focus
  "`%s' is not a member of `%s'"
  'org-gtd-error)

;;;; Missed Engagements Reflection (formerly "oops")

(defconst org-gtd-reflect-missed-engagements-view-specs
  '(((name . "Missed check-ins on delegated items")
     (filters . ((category . delegated)
                 (timestamp . past))))

    ((name . "Missed appointments")
     (filters . ((category . calendar)
                 (timestamp . past))))

    ((name . "Projects that should have finished")
     (filters . ((category . projects)
                 (deadline . past))))

    ((name . "Projects that should have started")
     (filters . ((category . projects)
                 (scheduled . past)
                 (not-habit . t)))))
  "GTD view specifications for missed engagement reflections.")

(defconst org-gtd-reflect-upcoming-delegated-view-spec
  '((name . "Upcoming check-ins on delegated items")
    (filters . ((category . delegated)
                (timestamp . future)
                (not-done . t))))
  "GTD view specification for upcoming delegated item check-ins.")

;;;###autoload
(defun org-gtd-reflect-missed-engagements ()
  "Show all missed engagements using GTD view language.
Shows delegated check-ins, missed appointments, overdue projects."
  (interactive)
  (org-gtd-view-show org-gtd-reflect-missed-engagements-view-specs))

;;;###autoload
(defun org-gtd-reflect-missed-delegated ()
  "Show only missed delegated items needing check-in."
  (interactive)
  (org-gtd-view-show (car org-gtd-reflect-missed-engagements-view-specs)))

;;;###autoload
(defun org-gtd-reflect-missed-calendar ()
  "Show only missed calendar appointments."
  (interactive)
  (org-gtd-view-show (cadr org-gtd-reflect-missed-engagements-view-specs)))

;;;###autoload
(defun org-gtd-reflect-missed-projects ()
  "Show only overdue projects."
  (interactive)
  (org-gtd-view-show
   (list (caddr org-gtd-reflect-missed-engagements-view-specs)
         (cadddr org-gtd-reflect-missed-engagements-view-specs))))

;;;###autoload
(defun org-gtd-reflect-upcoming-delegated ()
  "Show delegated items with upcoming check-in dates.
Displays all delegated items where ORG_GTD_TIMESTAMP is in the future.
Useful for planning follow-ups and catching early completions."
  (interactive)
  (org-gtd-view-show org-gtd-reflect-upcoming-delegated-view-spec))

(defcustom org-gtd-reflect-missed-custom-views nil
  "Additional custom missed engagement views defined by the user.
Each view should be a GTD view specification alist with \\='name
and \\='filters keys.

Example:
\\='(((name . \"My Custom View\")
   (filters . ((category . delegated)
               (area-of-focus . \"Work\")))))"
  :group 'org-gtd
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :package-version '(org-gtd . "4.0"))

;;;###autoload
(defun org-gtd-reflect-missed-with-custom ()
  "Show missed engagement reflections including user-defined custom views."
  (interactive)
  (org-gtd-view-show
   (append org-gtd-reflect-missed-engagements-view-specs
           org-gtd-reflect-missed-custom-views)))

;;;; Backward Compatibility Aliases

;; org-gtd-oops-* -> org-gtd-reflect-*
;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops
  'org-gtd-reflect-missed-engagements "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-delegated
  'org-gtd-reflect-missed-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-calendar
  'org-gtd-reflect-missed-calendar "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-projects
  'org-gtd-reflect-missed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-with-custom
  'org-gtd-reflect-missed-with-custom "4.0")

(define-obsolete-variable-alias 'org-gtd-oops-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-oops-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")

;; org-gtd-review-* -> org-gtd-reflect-*
;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-area-of-focus
  'org-gtd-reflect-area-of-focus "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-items
  'org-gtd-reflect-missed-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-calendar-items
  'org-gtd-reflect-stuck-calendar-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-delegated-items
  'org-gtd-reflect-stuck-delegated-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-habit-items
  'org-gtd-reflect-stuck-habit-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-incubated-items
  'org-gtd-reflect-stuck-incubated-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-projects
  'org-gtd-reflect-stuck-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-single-action-items
  'org-gtd-reflect-stuck-single-action-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-completed-items
  'org-gtd-reflect-completed-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-completed-projects
  'org-gtd-reflect-completed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-engagements
  'org-gtd-reflect-missed-engagements "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-delegated
  'org-gtd-reflect-missed-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-calendar
  'org-gtd-reflect-missed-calendar "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-projects
  'org-gtd-reflect-missed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-upcoming-delegated
  'org-gtd-reflect-upcoming-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-with-custom
  'org-gtd-reflect-missed-with-custom "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-items-view-specs
  'org-gtd-reflect-missed-items-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-engagements-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")

;;;; Footer

(provide 'org-gtd-reflect)

;;; org-gtd-reflect.el ends here
