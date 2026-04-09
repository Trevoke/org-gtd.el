;;; org-gtd-habit.el --- Define habits in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Habits have org-mode requirements, we satisfy them here.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-organize-core)
(require 'org-gtd-create)

;;;; Commands

(defun org-gtd-habit (&optional repeater)
  "DWIM: organize the heading at point as a habit.
REPEATER is an `org-mode'-style repeater string (e.g. \".+3d\")
for non-interactive use."
  (interactive)
  (let ((config (when repeater
                  `((:when . ,(format "<%s %s>" (format-time-string "%F") repeater))))))
    (org-gtd--dispatch 'habit config)))

;;;; Functions

;;;;; Public

(defun org-gtd-habit-create (topic repeater)
  "Automatically create a habit in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REPEATER is `org-mode'-style repeater string (.e.g \".+3d\") which will
determine how often you'll be reminded of this habit.

Obsolete: use `org-gtd-create-item' instead.

\(fn TOPIC REPEATER)"
  (declare (obsolete org-gtd-create-item "4.1.0"))
  (org-gtd-create-item 'habit topic
                       `((:when . ,(format "<%s %s>"
                                           (format-time-string "%F")
                                           repeater)))))

;;;; Footer

(provide 'org-gtd-habit)

;;; org-gtd-habit.el ends here
