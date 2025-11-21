;;; org-gtd-agenda.el --- Manage the agenda view -*- lexical-binding: t; coding: utf-8 -*-
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
;; Agenda management for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'winner)
(require 'org-agenda)

(require 'org-gtd-core)
(require 'org-gtd-backward-compatibility)
(require 'org-gtd-view-language)

(defgroup org-gtd-engage nil
  "Customize the engage views in the org-gtd package."
  :group 'org-gtd
  :package-version '(org-gtd . "3.1"))

(defcustom org-gtd-engage-prefix-width 12
  "How many characters to dedicate to the agenda prefix in the engage view.

This is where the project name is displayed, on the left side."
  :group 'org-gtd-engage
  :package-version '(org-gtd . "3.1")
  :type 'integer)

;;;; GTD View Specifications

(defun org-gtd-engage-view-spec ()
  "Return GTD view specification for the engage view."
  (let ((project-format-prefix
         (format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
                 org-gtd-engage-prefix-width)))
    `((name . "GTD Engage View")
      (view-type . agenda)
      (agenda-span . 1)
      (additional-blocks . ((todo . ,(org-gtd-keywords--next))))
      (prefix-format . ,project-format-prefix))))

(defun org-gtd-engage-grouped-by-context-view-spec ()
  "Return GTD view specification for the grouped by context engage view."
  `((name . "Actions by Context")
    (view-type . tags-grouped)
    (group-by . context)
    (filters . ((tags-match . "{^@}")
                (todo . (,(org-gtd-keywords--next)))))))

(defun org-gtd-show-all-next-view-spec ()
  "Return GTD view specification for showing all next actions."
  `((name . "All Next Actions")
    (filters . ((todo . (,(org-gtd-keywords--next)))))))

;;;; Commands

;;;###autoload
(defun org-gtd-engage ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (org-gtd-view-show (org-gtd-engage-view-spec)))

;;;###autoload
(defun org-gtd-engage-grouped-by-context ()
  "Show all `org-gtd-next' actions grouped by context (tag prefixed with @)."
  (interactive)
  (org-gtd-view-show (org-gtd-engage-grouped-by-context-view-spec)))

;;;###autoload
(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-gtd-view-show (org-gtd-show-all-next-view-spec)))

;;;; Functions

;;;;; Private

(defun org-gtd--replace-link-with-description (text)
  "Replace all org-mode links in the given text with their descriptions."
    (replace-regexp-in-string org-link-bracket-re "\\2" text))

  ;; (while (string-match org-link-bracket-re text)
  ;;   (let ((description (match-string 3 text)))
  ;;     (setq text (replace-match description nil t text))))
  ;; text)

(defun org-gtd--truncate-project-to-width (st)
  "Truncates the string to the width indicated by org-gtd-engage-prefix-width."
  (truncate-string-to-width (string-trim st) org-gtd-engage-prefix-width nil ?\s  "…"))

(defun org-gtd-agenda--prefix-format ()
  "Format prefix for items in agenda buffer."
  (let* ((project-name (org-entry-get (point) org-gtd-prop-project))
         (category (org-entry-get (point) org-gtd-prop-area-of-focus t))
         (tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))
    (org-gtd--truncate-project-to-width
     ;; if has ORG_GTD_PROJECT, use it
     ;; otherwise if it has category, use category
     ;; else "no project" so we avoid failing
     (org-gtd--replace-link-with-description
      (cond
       (project-name (replace-regexp-in-string tally-cookie-regexp "" project-name))
       (category     category)
       (t  "no project")
       )))))

;;;; Footer

(provide 'org-gtd-agenda)

;;; org-gtd-agenda.el ends here
