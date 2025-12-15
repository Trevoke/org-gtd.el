;;; org-gtd-calendar.el --- Define calendar items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Calendar items have their own state and logic, defined here.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-refile)
(require 'org-gtd-clarify)
(require 'org-gtd-configure)
(require 'org-gtd-organize-core)

;;;; Constants

(defconst org-gtd-calendar-template
  (format "* Calendar
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-calendar))

;;;; Commands

(defun org-gtd-calendar (&optional appointment-date)
  "Decorate and refile item at point as a calendar item.

You can pass APPOINTMENT-DATE as a YYYY-MM-DD string if you want to use this
non-interactively."
  (interactive)
  (let ((config-override (when appointment-date
                           `(('active-timestamp . ,(lambda (_x) (format "<%s>" appointment-date)))))))
    (org-gtd-organize--call
     (lambda () (org-gtd-calendar--apply config-override)))))

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
      (org-gtd-calendar appointment-date))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-calendar--configure (&optional config-override)
  "Configure item at point as a calendar item.

CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  (org-gtd-configure-as-type 'calendar
                             (when config-override
                               `((:when . ,(funcall (alist-get '(quote active-timestamp) config-override nil nil #'equal) nil))))))

(defun org-gtd-calendar--finalize ()
  "Finalize calendar item organization and refile."
  (setq-local org-gtd--organize-type 'calendar)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-calendar org-gtd-calendar-template)))

(defun org-gtd-calendar--apply (&optional config-override)
  "Process GTD inbox item by transforming it into a calendar item.

Orchestrates the calendar item organization workflow:
1. Configure with calendar settings
2. Finalize and refile to calendar file

CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  (org-gtd-calendar--configure config-override)
  (org-gtd-calendar--finalize))

;;;; Footer

(provide 'org-gtd-calendar)

;;; org-gtd-calendar.el ends here
