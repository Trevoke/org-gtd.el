;;; org-gtd-checklist.el --- Reusable checklist templates for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

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
;; Reusable checklist templates for org-gtd, stored as plain org
;; headings in a checklists file inside `org-gtd-directory'.  Each
;; top-level heading is a template whose checkbox items can be spawned
;; as a fresh subtree wherever needed.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'f)

(require 'org-gtd-core)
(require 'org-gtd-files)

;;;; Constants

(defconst org-gtd-checklist-file-name "checklists"
  "Base name of the checklist templates file inside `org-gtd-directory'.")

(defconst org-gtd-checklist--starter-contents
  "* Weekly Review triggers
- [ ] Projects started but not completed?
- [ ] Commitments or promises made to others?
- [ ] Communications to make or expecting (calls, emails)?
- [ ] Writing to finish or submit?
- [ ] Meetings that need to be set or requested?
- [ ] Decisions that need to be made?
- [ ] Waiting for someone else's reply or delivery?
- [ ] Financial or administrative loose ends?

* Mind sweep prompts
- [ ] Boss, partners, colleagues?
- [ ] Family and friends?
- [ ] Household — repairs, maintenance, errands?
- [ ] Health — appointments, checkups, exercise?
- [ ] Finances — bills, taxes, banks?
- [ ] Car or transportation?
- [ ] Creative ideas, things to learn?
- [ ] Places to go, people to see?
"
  "Contents used to seed a brand-new checklists file.")

;;;; Functions

;;;;; Private

(defun org-gtd-checklist--file-path ()
  "Return the full path to the checklists file."
  (org-gtd--path org-gtd-checklist-file-name))

(defun org-gtd-checklist--file-buffer ()
  "Return a buffer visiting the checklists file, creating it if needed.
A newly created file is seeded with starter templates."
  (let ((path (org-gtd-checklist--file-path)))
    (org-gtd--ensure-file-exists path org-gtd-checklist--starter-contents)
    (find-file-noselect path)))

;;;;; Commands

;;;###autoload
(defun org-gtd-checklist-visit ()
  "Visit the checklist templates file.
Each top-level heading is a reusable checklist template."
  (interactive)
  (pop-to-buffer (org-gtd-checklist--file-buffer)))

;;;; Footer

(provide 'org-gtd-checklist)

;;; org-gtd-checklist.el ends here
