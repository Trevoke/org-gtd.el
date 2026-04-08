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

;;;; Commands

(defun org-gtd-calendar (&optional appointment-date)
  "DWIM: organize the heading at point as a calendar item.

APPOINTMENT-DATE is an optional YYYY-MM-DD string for non-interactive use.
When invoked from within the clarify/WIP transient flow, keeps the
`org-gtd-organize--call' wrapping so queue/source-cut/window-restore
behavior is preserved.  Otherwise dispatches directly via
`org-gtd--dispatch'."
  (interactive)
  (let ((config (when appointment-date
                  `((:when . ,(format "<%s>" appointment-date))))))
    (if (and (boundp 'org-gtd-clarify--clarify-id) org-gtd-clarify--clarify-id)
        (org-gtd-organize--call
         (lambda () (org-gtd-process-heading (point-marker) 'calendar config)))
      (org-gtd--dispatch 'calendar))))

;;;; Functions

;;;;; Public

(defun org-gtd-calendar-create (topic appointment-date)
  "Automatically create a calendar task in the GTD flow.

Takes TOPIC as the string from which to make the heading to add to `org-gtd' and
APPOINTMENT-DATE as a YYYY-MM-DD string."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config `((:when . ,(format "<%s>" appointment-date)))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (goto-char (point-min))
      (org-gtd-process-heading (point-marker) 'calendar config))
    (kill-buffer buffer)))

;;;; Footer

(provide 'org-gtd-calendar)

;;; org-gtd-calendar.el ends here
