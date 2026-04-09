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
(require 'org-gtd-create)

;;;; Commands

(defun org-gtd-calendar (&optional appointment-date)
  "DWIM: organize the heading at point as a calendar item.
APPOINTMENT-DATE is an optional YYYY-MM-DD string for non-interactive
use."
  (interactive)
  (let ((config (when appointment-date
                  `((:when . ,(format "<%s>" appointment-date))))))
    (org-gtd--dispatch 'calendar config)))

;;;; Functions

;;;;; Public

(defun org-gtd-calendar-create (topic appointment-date)
  "Automatically create a calendar item in the GTD flow.

TOPIC is the string you want to see in the agenda view.
APPOINTMENT-DATE is the YYYY-MM-DD string of the event.

Obsolete: use `org-gtd-create-item' instead.

\(fn TOPIC APPOINTMENT-DATE)"
  (declare (obsolete org-gtd-create-item "4.1.0"))
  (org-gtd-create-item 'calendar topic
                       `((:when . ,(format "<%s>" appointment-date)))))

;;;; Footer

(provide 'org-gtd-calendar)

;;; org-gtd-calendar.el ends here
