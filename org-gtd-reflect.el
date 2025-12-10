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
     (type . project)
     (area-of-focus . ,area))

    ((name . "Tickler projects")
     (type . tickler-project)
     (area-of-focus . ,area)
     (prefix . ("Tickler"))
     (prefix-width . 10))

    ((name . "Next actions")
     (type . next-action)
     (area-of-focus . ,area))

    ((name . "Reminders")
     (type . calendar)
     (area-of-focus . ,area))

    ((name . "Routines")
     (type . habit)
     (area-of-focus . ,area))

    ((name . "Tickler items")
     (type . tickler)
     (when . future)
     (area-of-focus . ,area))))

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

;; Backward compatibility alias - must come before referent
(define-obsolete-variable-alias 'org-gtd-review-missed-items-view-specs
  'org-gtd-reflect-missed-items-view-specs "4.0")

(defconst org-gtd-reflect-missed-items-view-specs
  '(((name . "Missed calendar events")
     (type . calendar)
     (when . past))

    ((name . "Tickler events to review")
     (type . tickler)
     (when . past))

    ((name . "Missed delegated events")
     (type . delegated)
     (when . past)))
  "GTD view specifications for reflecting on missed items.")

(defun org-gtd-reflect-missed-items (&optional _start-date)
  "Agenda view with all tickler, delegated, or calendar items whose dates
are in the past.

You can pass an optional START-DATE to tell the code what to use as the first
day for the agenda.  It is mostly of value for testing purposes."
  (interactive)
  (org-gtd-view-show org-gtd-reflect-missed-items-view-specs))

(defun org-gtd-reflect-stuck-calendar-items ()
  "Agenda view with all invalid Calendar actions.
Shows calendar items that are missing a valid timestamp."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Calendar Items")
     (type . stuck-calendar))))

(defun org-gtd-reflect-stuck-delegated-items ()
  "Agenda view with all invalid delegated actions.
Shows delegated items that are missing either:
- A valid timestamp (when to follow up)
- The person delegated to (who)"
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Delegated Items")
     (type . stuck-delegated))))

(defun org-gtd-reflect-stuck-habit-items ()
  "Agenda view with all invalid habit actions.
Shows habit items that are missing a valid timestamp."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Habit Items")
     (type . stuck-habit))))

(defun org-gtd-reflect-stuck-tickler-items ()
  "Agenda view with all invalid tickler actions.
Shows tickler items that are missing a valid timestamp."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Tickler Items")
     (type . stuck-tickler))))

;;;###autoload
(defun org-gtd-reflect-someday-maybe ()
  "Show all someday/maybe items.

These are items you might want to do eventually, but with no specific
timeframe. Use this view during your weekly or monthly reviews to
decide if any items should be activated."
  (interactive)
  (org-gtd-view-show
   '((name . "Someday/Maybe Items")
     (type . someday))))

;;;###autoload
(defun org-gtd-reflect-stuck-projects ()
  "Show all projects that do not have a next action.

Stuck projects have TODO tasks (work remaining) but no NEXT or WAIT tasks,
indicating they need attention to identify the next actionable step."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Projects")
     (type . stuck-project))))

(defun org-gtd-reflect-stuck-single-action-items ()
  "Agenda view with single actions that need attention.
Shows single actions (ORG_GTD=Actions) that are undone but not in NEXT state.
Single actions should always be in NEXT state since they are ready to work on."
  (interactive)
  (org-gtd-view-show
   '((name . "Stuck Single Actions")
     (type . stuck-single-action))))

;;;###autoload
(defun org-gtd-reflect-completed-items (&optional days-back)
  "Show items completed in the last DAYS-BACK days (default 7).

This view shows all items with done TODO states that were closed within
the specified time period. Useful for weekly reviews to see what was
accomplished.

With a numeric prefix argument (e.g., C-u 14), shows items completed
in that many days."
  (interactive "P")
  (let ((days (or (and days-back (prefix-numeric-value days-back)) 7)))
    (org-gtd-view-show
     `((name . ,(format "Completed in Last %d Days" days))
       (done . ,days)))))

;;;###autoload
(defun org-gtd-reflect-completed-projects ()
  "Show all completed projects.

Projects are considered completed when all their tasks are done.
This view helps identify projects ready for archiving."
  (interactive)
  (org-gtd-view-show
   '((name . "Completed Projects")
     (type . completed-project))))

;;;; Functions

;;;;; Private

(define-error
  'org-gtd-invalid-area-of-focus
  "`%s' is not a member of `%s'"
  'org-gtd-error)

;;;; Missed Engagements Reflection (formerly "oops")

;; Backward compatibility aliases - must come before referents
(define-obsolete-variable-alias 'org-gtd-oops-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-engagements-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(defconst org-gtd-reflect-missed-engagements-view-specs
  '(((name . "Missed check-ins on delegated items")
     (type . delegated)
     (when . past))

    ((name . "Missed appointments")
     (type . calendar)
     (when . past))

    ((name . "Projects that should have finished")
     (type . project)
     (deadline . past))

    ((name . "Projects that should have started")
     (type . project)
     (scheduled . past)
     (not-habit . t)))
  "GTD view specifications for missed engagement reflections.")

(defconst org-gtd-reflect-upcoming-delegated-view-spec
  '((name . "Upcoming check-ins on delegated items")
    (type . delegated)
    (when . future))
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

;; Backward compatibility aliases - must come before referents
(define-obsolete-variable-alias 'org-gtd-oops-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")

(defcustom org-gtd-reflect-missed-custom-views nil
  "Additional custom missed engagement views defined by the user.
Each view should be a GTD view specification alist with \\='name
and \\='filters keys.

Example:
\\='(((name . \"My Custom View\")
   (filters . ((type . delegated)
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
  #'org-gtd-reflect-missed-engagements "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-delegated
  #'org-gtd-reflect-missed-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-calendar
  #'org-gtd-reflect-missed-calendar "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-projects
  #'org-gtd-reflect-missed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-oops-with-custom
  #'org-gtd-reflect-missed-with-custom "4.0")

;; org-gtd-review-* -> org-gtd-reflect-*
;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-area-of-focus
  #'org-gtd-reflect-area-of-focus "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-items
  #'org-gtd-reflect-missed-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-calendar-items
  #'org-gtd-reflect-stuck-calendar-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-delegated-items
  #'org-gtd-reflect-stuck-delegated-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-habit-items
  #'org-gtd-reflect-stuck-habit-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-incubated-items
  #'org-gtd-reflect-stuck-tickler-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-projects
  #'org-gtd-reflect-stuck-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-stuck-single-action-items
  #'org-gtd-reflect-stuck-single-action-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-completed-items
  #'org-gtd-reflect-completed-items "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-completed-projects
  #'org-gtd-reflect-completed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-engagements
  #'org-gtd-reflect-missed-engagements "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-delegated
  #'org-gtd-reflect-missed-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-calendar
  #'org-gtd-reflect-missed-calendar "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-projects
  #'org-gtd-reflect-missed-projects "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-upcoming-delegated
  #'org-gtd-reflect-upcoming-delegated "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-review-missed-with-custom
  #'org-gtd-reflect-missed-with-custom "4.0")

;;;; Footer

(provide 'org-gtd-reflect)

;;; org-gtd-reflect.el ends here
