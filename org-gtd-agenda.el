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

;;;;; Prefix Element Resolvers

(defun org-gtd-agenda--resolve-project ()
  "Return parent project headline for item at point, or nil if none."
  (when-let ((proj-name (org-entry-get (point) org-gtd-prop-project)))
    (let ((tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))
      (string-trim
       (org-gtd-agenda-replace-link-with-description
        (replace-regexp-in-string tally-cookie-regexp "" proj-name))))))

(defun org-gtd-agenda--has-explicit-category-p ()
  "Return non-nil if entry at point has CATEGORY explicitly set in drawer."
  (when-let ((range (org-get-property-block)))
    (save-excursion
      (goto-char (car range))
      (re-search-forward "^:CATEGORY:" (cdr range) t))))

(defun org-gtd-agenda--resolve-area-of-focus ()
  "Return area of focus for item at point, or nil if none.
First checks for explicit CATEGORY on the entry, then looks up the
project's CATEGORY via ORG_GTD_PROJECT_IDS if no explicit value is set."
  (cond
   ;; Entry has explicit CATEGORY in its properties drawer
   ((org-gtd-agenda--has-explicit-category-p)
    (let ((category (org-entry-get (point) "CATEGORY")))
      (unless (string= category "???")
        category)))
   ;; Look up project's CATEGORY via ORG_GTD_PROJECT_IDS
   ((when-let* ((project-ids (org-entry-get-multivalued-property
                              (point) org-gtd-prop-project-ids))
                (first-id (car project-ids))
                (project-marker (org-id-find first-id 'marker)))
      (org-with-point-at project-marker
        (org-entry-get (point) "CATEGORY"))))
   ;; No explicit category and no project - return nil
   (t nil)))

(defun org-gtd-agenda--resolve-file-name ()
  "Return base file name for current buffer, or nil if no file."
  (when buffer-file-name
    (file-name-base buffer-file-name)))

(defun org-gtd-agenda--resolve-prefix-element (element)
  "Resolve ELEMENT to a string value at point, or nil if unavailable.
ELEMENT can be:
- \\='project - parent project headline
- \\='area-of-focus - CATEGORY property
- \\='file-name - buffer file base name
- a string - returned as-is (literal)"
  (pcase element
    ('project (org-gtd-agenda--resolve-project))
    ('area-of-focus (org-gtd-agenda--resolve-area-of-focus))
    ('file-name (org-gtd-agenda--resolve-file-name))
    ((pred stringp) element)
    (_ nil)))

(defun org-gtd-agenda--resolve-prefix-chain (elements width)
  "Try each element in ELEMENTS, return first non-nil truncated to WIDTH.
ELEMENTS is a list of prefix element specifiers (symbols or strings).
The first element that resolves to a non-nil value is used.
Result is truncated with ellipsis if too long, padded with spaces if too short."
  (let ((result (cl-some #'org-gtd-agenda--resolve-prefix-element elements)))
    (truncate-string-to-width (or result "") width nil ?\s org-gtd-agenda-truncate-ellipsis)))

;;;; Footer

(provide 'org-gtd-agenda)

;;; org-gtd-agenda.el ends here
