;;; org-gtd-calendar.el --- Define calendar items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Calendar items have their own state and logic, defined here.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-refile)
(require 'org-gtd-clarify)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-calendar "Calendar")

(defconst org-gtd-calendar-func #'org-gtd-calendar--apply
  "Function called when item at point is a task that must happen on a given day.

Keep this clean and don't load your calendar with things that aren't
actually appointments or deadlines.")

(defconst org-gtd-calendar-template
  (format "* Calendar
:PROPERTIES:
:ORG_GTD: %s
:END:
" org-gtd-calendar))

;;;; Commands

(defun org-gtd-calendar (&optional appointment-date)
  "Decorate and refile item at point as a calendar item.

You can pass APPOINTMENT-DATE as a YYYY-MM-DD string if you want to use this
non-interactively."
  (interactive)
  (org-gtd-organize--call
   (apply-partially org-gtd-calendar-func
                    appointment-date)))

;;;; Functions

;;;;; Public

(defun org-gtd-calendar-create (topic appointment-date)
  "Automatically create a calendar task in the GTD flow.

Takes TOPIC as the string from which to make the heading to add to `org-gtd' and
APPOINTMENT-DATE as a YYYY-MM-DD string."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-calendar appointment-date))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-calendar--apply (&optional appointment-date)
  "Add a date/time to this item and store in org gtd.

You can pass APPOINTMENT-DATE as a YYYY-MM-DD string if you want to use this
non-interactively."
  (let ((date (or appointment-date
                  (org-read-date t nil nil "When is this going to happen? "))))
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date))))
  (setq-local org-gtd--organize-type 'calendar)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-calendar org-gtd-calendar-template))

;;;; Footer

(provide 'org-gtd-calendar)

;;; org-gtd-calendar.el ends here
