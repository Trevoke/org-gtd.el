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

(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)
(declare-function 'org-gtd-clarify-item 'org-gtd-clarify)

;;;; Constants

(defconst org-gtd-calendar "Calendar")


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
  (let ((config-override (when appointment-date
                           `(('active-timestamp . ,(lambda (x) (format "<%s>" appointment-date)))))))
    (org-gtd-organize--call
     (lambda () (org-gtd-calendar--apply config-override)))))

;;;; Functions

;;;;; Public

(defun org-gtd-calendar-create (topic appointment-date)
  "Automatically create a calendar task in the GTD flow.

Takes TOPIC as the string from which to make the heading to add to `org-gtd' and
APPOINTMENT-DATE as a YYYY-MM-DD string."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config-override `(('active-timestamp . ,(lambda (x) (format "<%s>" appointment-date))))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-calendar--apply config-override))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-calendar--apply (&optional config-override)
  "Add a date/time to this item and store in org gtd.

CONFIG-OVERRIDE can provide input configuration to override default prompting behavior."
  ;; Use configure-item with optional config override
  (org-gtd-configure-item (point) :calendar nil config-override)
  ;; Insert timestamp in content (get it from the property that was just set)
  (let ((timestamp (org-entry-get (point) "ORG_GTD_TIMESTAMP")))
    (when timestamp
      (save-excursion
        (org-end-of-meta-data t)
        (open-line 1)
        (insert timestamp))))
  (setq-local org-gtd--organize-type 'calendar)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-calendar org-gtd-calendar-template))

;;;; Footer

(provide 'org-gtd-calendar)

;;; org-gtd-calendar.el ends here
