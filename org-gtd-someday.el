;;; org-gtd-someday.el --- Define someday/maybe items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Someday/Maybe items have their own logic, defined here.
;; Unlike tickler items, someday/maybe items have NO timeframe.
;; They are categorized by refile targets with ORG_GTD_REFILE: Someday property.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-reactivate)
(require 'org-gtd-organize-core)
(require 'org-gtd-create)

;;;; Customization

(defcustom org-gtd-someday-lists nil
  "List of someday/maybe list names for review grouping.
When nil, all someday items are reviewed together without prompting.
When populated, user is prompted to select which list to review."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0")
  :type '(repeat string))

;;;; Commands

(defun org-gtd-someday ()
  "DWIM: organize the heading at point as a someday/maybe item.
Dispatches to the project organize-fn when on a project heading or
project task; otherwise processes the plain heading."
  (interactive)
  (org-gtd--dispatch 'someday))

;;;; Functions

;;;;; Public

(defun org-gtd-someday-create (topic)
  "Automatically create a someday/maybe item in the GTD flow.

TOPIC is the string you want to see when reviewing someday/maybe items.

Obsolete: use `org-gtd-create-item' instead."
  (declare (obsolete org-gtd-create-item "4.1.0"))
  (org-gtd-create-item 'someday topic nil))

;;;;; Private

(defun org-gtd-someday--organize (type _config)
  "Configure heading at point as TYPE (someday/maybe).
Saves previous state, configures as TYPE, optionally prompts for a
list when `org-gtd-someday-lists' is populated, clears the TODO
keyword, and removes any timestamp properties."
  (org-gtd-save-state)
  (org-gtd-configure-as-type type)
  (when org-gtd-someday-lists
    (let ((list (completing-read "Someday list: " org-gtd-someday-lists nil t)))
      (org-entry-put nil org-gtd-prop-someday-list list)))
  (org-todo "")
  (org-entry-delete (point) org-gtd-timestamp)
  (org-entry-delete (point) "SCHEDULED")
  (org-entry-delete (point) "DEADLINE"))

(defun org-gtd-someday--organize-project (pom _config)
  "Move project at POM to someday/maybe."
  (require 'org-gtd-projects)
  (org-gtd-project-someday pom))

;;;; Footer

(provide 'org-gtd-someday)

;;; org-gtd-someday.el ends here
