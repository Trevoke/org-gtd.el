;;; org-gtd-organize.el --- Move tasks where they belong -*- lexical-binding: t; coding: utf-8 -*-
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
;; Move tasks where they need to be in the org-gtd system.
;;
;;; Code:

;;;; Requirements

(require 'transient)

(require 'org-gtd-backward-compatibility)
(require 'org-gtd-organize-core)
(require 'org-gtd-calendar)
(require 'org-gtd-habit)
(require 'org-gtd-knowledge)
(require 'org-gtd-tickler)
(require 'org-gtd-someday)
(require 'org-gtd-quick-action)
(require 'org-gtd-single-action)
(require 'org-gtd-trash)
(require 'org-gtd-delegate)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-process)

;;;; Constants

(defconst org-gtd-organize-action-types
  '(quick-action single-action calendar habit
                 delegated tickler someday knowledge trash
                 project-heading project-task everything)
  "Valid actions types as input for `org-gtd-organize-type-member-p'.")

;;;; Variables

(defvar-local org-gtd--organize-type nil
  "Type of action chosen by the user for this one item.")

;;;; Macros

(transient-define-infix org-gtd-organize--skip-refile-infix ()
  "Transient infix for skip-refile toggle."
  :class 'transient-lisp-variable
  :variable 'org-gtd-clarify--skip-refile
  :reader (lambda (&rest _) (if org-gtd-clarify--skip-refile nil t))
  :description "Update in place (no refile)")

(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item."
  [:if (lambda () (not org-gtd-clarify--inbox-p))
   "Options"
   ("-n" org-gtd-organize--skip-refile-infix)]
  ["Actionable"
   [("q" "Quick action" org-gtd-quick-action)
    ("s" "Single action" org-gtd-single-action)]
   [("d" "Delegate" org-gtd-delegate)
    ("c" "Calendar" org-gtd-calendar)
    ("h" "Habit" org-gtd-habit)]]
  [("p" "Project (multi-step)" org-gtd-project-new)
   ("a" "Add this task to an existing project" org-gtd-project-extend)]
  ["Non-actionable"
   [("i" "Tickler" org-gtd-tickler)
    ("y" "Someday/Maybe" org-gtd-someday)]
   [("k" "Knowledge to be stored" org-gtd-knowledge)
    ("t" "Trash" org-gtd-trash)]])

;;;; Functions

;;;;; Public

(defun org-gtd-organize-type-member-p (list)
  "Return t if the action type chosen by the user is in LIST.

Valid members of LIST include:
- \\='quick-action (done in less than two minutes)
- \\='single-action (do when possible)
- \\='calendar (do at a given time)
- \\='delegated (done by someone else)
- \\='habit (a recurring action)
- \\='tickler (remind me later - has a specific date)
- \\='someday (maybe do someday - no specific timeframe)
- \\='knowledge (stored as reference)
- \\='trash (self-explanatory)
- \\='project-heading (top-level project info, e.g. area of focus)
- \\='project-task (task-specific info, similar in spirit to single-action)
- \\='everything (if this is in the list, always return t)"
  (let ((list (ensure-list list)))
    (unless (seq-every-p
             (lambda (x) (member x org-gtd-organize-action-types))
             list)
      (signal 'org-gtd-invalid-organize-action-type-error
              `(,list ,org-gtd-organize-action-types)))
    (or (member 'everything list)
        (member org-gtd--organize-type list))))

(define-error
  'org-gtd-invalid-organize-action-type-error
  "At least one element of %s is not in %s"
  'org-gtd-error)


;;;; Footer

(provide 'org-gtd-organize)

;;; org-gtd-organize.el ends here
