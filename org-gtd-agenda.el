;;; org-gtd-agenda.el --- Agenda utilities for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Shared agenda utilities for org-gtd.
;; For the engage views, see org-gtd-engage.el.
;;
;;; Code:

;;;; Requirements

(require 'winner)
(require 'org-agenda)

(require 'org-gtd-core)

;;;; Functions

;;;;; Public

(defun org-gtd-agenda-replace-link-with-description (text)
  "Replace all `org-mode' links in TEXT with their descriptions."
  (replace-regexp-in-string org-link-bracket-re "\\2" text))

(defun org-gtd-agenda-get-category-for-task ()
  "Get CATEGORY for task at point, looking up project if needed.
In v4, project tasks may not have a direct CATEGORY property.
This function looks up the project heading's CATEGORY via ORG_GTD_PROJECT_IDS."
  (or (org-entry-get (point) "CATEGORY")
      (when-let* ((project-ids (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids))
                  (first-id (car project-ids))
                  (project-marker (org-id-find first-id 'marker)))
        (org-with-point-at project-marker
          (org-entry-get (point) "CATEGORY")))))

(defun org-gtd-agenda--prefix-format (width)
  "Format prefix for items in agenda buffer, truncated to WIDTH.
Uses project name if available, otherwise CATEGORY, otherwise \"no project\"."
  (let* ((proj-name (org-entry-get (point) org-gtd-prop-project))
         (category (org-gtd-agenda-get-category-for-task))
         (tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))
    (truncate-string-to-width
     (string-trim
      (org-gtd-agenda-replace-link-with-description
       (cond
        (proj-name (replace-regexp-in-string tally-cookie-regexp "" proj-name))
        (category     category)
        (t  "no project"))))
     width nil ?\s "…")))

;;;; Footer

(provide 'org-gtd-agenda)

;;; org-gtd-agenda.el ends here
