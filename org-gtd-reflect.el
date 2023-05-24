;;; org-gtd-reflect.el --- Reflect on system contents -*- lexical-binding: t; coding: utf-8 -*-
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

;;;; Commands

;;;###autoload
(defun org-gtd-reflect-area-of-focus (&optional area start-date)
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
               `(("a" ,(format "Area of Focus: %s" area)
                  ((tags ,org-gtd-project-headings
                         ((org-agenda-overriding-header "Active projects")))
                   (todo ,org-gtd-next
                         ((org-agenda-overriding-header "Next actions")))
                   (agenda ""
                           ((org-agenda-overriding-header "Reminders")
                            (org-agenda-start-day ,start-date)
                            (org-agenda-show-all-dates nil)
                            (org-agenda-show-future-repeats nil)
                            (org-agenda-span 90)
                            (org-agenda-include-diary nil)
                            (org-agenda-skip-additional-timestamps-same-entry t)
                            (org-agenda-skip-function
                             '(org-gtd-skip-AND '(org-gtd-skip-unless-calendar
                                                  ,(org-gtd-skip-unless-area-of-focus-func area))))))
                   (agenda ""
                           ((org-agenda-overriding-header "Routines")
                            (org-agenda-time-grid '((require-timed) () "" ""))
                            (org-agenda-entry-types '(:scheduled))
                            (org-agenda-start-day ,start-date)
                            (org-agenda-span 'day)
                            (org-habit-show-habits-only-for-today nil)
                            (org-agenda-skip-function
                             '(org-gtd-skip-AND '(org-gtd-skip-unless-habit
                                                  ,(org-gtd-skip-unless-area-of-focus-func area))))))
                   (tags ,(format "+ORG_GTD=\"%s\"+%s>\"<%s>\""
                                  org-gtd-incubate
                                  org-gtd-timestamp
                                  start-date)
                         ((org-agenda-overriding-header "Incubated items"))))
                  ((org-agenda-skip-function '(org-gtd-skip-unless-area-of-focus ,area))
                   (org-agenda-buffer-name ,(format "*Org Agenda: %s*" area)))))))
          (org-agenda nil "a")
          (goto-char (point-min))))))

;;;###autoload
(defalias 'org-gtd-review-area-of-focus 'org-gtd-reflect-area-of-focus)
(make-obsolete 'org-gtd-review-area-of-focus "use `org-gtd-reflect-area-of-focus' instead." "2023-05-24")

(defun org-gtd-reflect-stuck-calendar-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "+ORG_GTD=\"Calendar\"+LEVEL=2"
                       ((org-agenda-include-diary nil)
                        (org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

;;;###autoload
(defalias 'org-gtd-review-stuck-calendar-items 'org-gtd-reflect-stuck-calendar-items)
(make-obsolete 'org-gtd-review-stuck-calendar-items "use `org-gtd-reflect-stuck-calendar-items' instead." "2023-05-24")

(defun org-gtd-reflect-stuck-delegated-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             `(("g" "foobar"
                ((tags (format "+TODO=\"%s\"" org-gtd-wait)
                       ((org-agenda-skip-function
                         '(org-gtd-skip-AND
                           '(org-gtd-skip-unless-timestamp-empty-or-invalid
                             org-gtd-skip-unless-delegated-to-empty)))
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

;;;###autoload
(defalias 'org-gtd-review-stuck-delegated-items 'org-gtd-reflect-stuck-delegated-items)
(make-obsolete 'org-gtd-review-stuck-delegated-items "use `org-gtd-reflect-stuck-delegated-items' instead." "2023-05-24")

(defun org-gtd-reflect-stuck-habit-items ()
  "Agenda view with all invalid Calendar actions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("g" "foobar"
                ((tags "ORG_GTD=\"Habits\""
                       ((org-agenda-skip-function
                         'org-gtd-skip-unless-timestamp-empty-or-invalid)
                        (org-agenda-skip-additional-timestamps-same-entry t))))))))
        (org-agenda nil "g"))))

;;;###autoload
(defalias 'org-gtd-review-stuck-habit-items 'org-gtd-reflect-stuck-habit-items)
(make-obsolete 'org-gtd-review-stuck-habit-items "use `org-gtd-reflect-stuck-habit-items' instead." "2023-05-24")

(defun org-gtd-reflect-stuck-incubated-items ()
  "Agenda view with all invalid Calendar actions."
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
(defalias 'org-gtd-review-stuck-incubated-items 'org-gtd-reflect-stuck-incubated-items)
(make-obsolete 'org-gtd-review-stuck-incubated-items "use `org-gtd-reflect-stuck-incubated-items' instead." "2023-05-24")

;;;###autoload
(defun org-gtd-reflect-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (with-org-gtd-context
      (org-agenda-list-stuck-projects)))

;;;###autoload
(defalias 'org-gtd-review-stuck-projects 'org-gtd-reflect-stuck-projects)
(make-obsolete 'org-gtd-review-stuck-projects "use `org-gtd-reflect-stuck-projects' instead." "2023-05-24")

(defun org-gtd-reflect-stuck-single-action-items ()
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

;;;###autoload
(defalias 'org-gtd-review-stuck-single-action-items 'org-gtd-reflect-stuck-single-action-items)
(make-obsolete 'org-gtd-review-stuck-single-action-items "use `org-gtd-reflect-stuck-single-action-items' instead." "2023-05-24")

;;;; Functions

;;;;; Private

(define-error
  'org-gtd-invalid-area-of-focus
  "`%s' is not a member of `%s'"
  'org-gtd-error)

;;;; Footer

(provide 'org-gtd-reflect)

;;; org-gtd-reflect.el ends here
