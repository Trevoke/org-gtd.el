;;; org-gtd-organize.el --- Move tasks where they belong -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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
(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-calendar)
(require 'org-gtd-habit)
(require 'org-gtd-knowledge)
(require 'org-gtd-incubate)
(require 'org-gtd-quick-action)
(require 'org-gtd-single-action)
(require 'org-gtd-trash)
(require 'org-gtd-delegate)
(require 'org-gtd-engage)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-process)

;;;; Customization

(defgroup org-gtd-organize nil
  "Manage the functions for organizing the GTD actions."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element.  The default value has
one function: setting org tags on the item.  Some built-in examples are
provided as options here.  You can create your own functions to further organize
the items once they have been processed and add them to that list.

Once you have your ground items managed, you might like to set the variable
`org-gtd-areas-of-focus' and add `org-gtd-set-area-of-focus' to these hooks."
  :group 'org-gtd-organize
  :options '(org-set-tags-command org-set-effort org-priority)
  :package-version '(org-gtd . "1.0.4")
  :type 'hook)

;;;; Constants

(defconst org-gtd-organize-action-types
  '(quick-action single-action calendar habit
                 delegated incubated knowledge trash
                 project-heading project-task everything)
  "Valid actions types as input for `org-gtd-organize-type-member-p'.")

;;;; Variables

(defvar-local org-gtd--organize-type nil
  "Type of action chosen by the user for this one item.")

;;;; Macros

(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item."
  ["Actionable"
   [("q" "Quick action" org-gtd-quick-action)
    ("s" "Single action" org-gtd-single-action)]
   [("d" "Delegate" org-gtd-delegate)
    ("c" "Calendar" org-gtd-calendar)
    ("h" "Habit" org-gtd-habit)]]
  [("p" "Project (multi-step)" org-gtd-project-new)
   ("a" "Add this task to an existing project" org-gtd-project-extend)]
  ["Non-actionable"
   [("i" "Incubate" org-gtd-incubate)
    ("k" "Knowledge to be stored" org-gtd-knowledge)]
   [("t" "Trash" org-gtd-trash)]])

;;;; Functions

;;;;; Public

(defun org-gtd-organize-apply-hooks ()
  "Apply hooks to add metadata to a given GTD item."
  (dolist (hook org-gtd-organize-hooks)
    (save-excursion
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (save-restriction (funcall hook)))))

(defun org-gtd-organize-type-member-p (list)
  "Return t if the action type chosen by the user is in LIST.

Valid members of LIST include:
- 'quick-action (done in less than two minutes)
- 'single-action (do when possible)
- 'calendar (do at a given time)
- 'delegated (done by someone else)
- 'habit (a recurring action)
- 'incubated (remind me later)
- 'knowledge (stored as reference)
- 'trash (self-explanatory)
- 'project-heading (top-level project info, e.g. area of focus)
- 'project-task (task-specific info, similar in spirit to single-action)
- 'everything (if this is in the list, always return t)"
  (let ((list (ensure-list list)))
    (unless (seq-every-p
             (lambda (x) (member x org-gtd-organize-action-types))
             list)
      (signal 'org-gtd-invalid-organize-action-type-error
              `(,list ,org-gtd-organize-action-types)))
    (or (member 'everything list)
        (member org-gtd--organize-type list))))

;;;;; Private

(define-error
  'org-gtd-invalid-organize-action-type-error
  "At least one element of %s is not in %s"
  'org-gtd-error)

(defun org-gtd-organize--call (func)
  "Wrap FUNC, which does the real work, to keep Emacs clean.
This handles the internal bits of `org-gtd'."
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  (catch 'org-gtd-error
    (with-org-gtd-context
        (save-excursion (funcall func)))
    (let ((loop-p (and (boundp org-gtd-clarify--inbox-p) org-gtd-clarify--inbox-p))
          (task-id org-gtd-clarify--clarify-id)
          (window-config org-gtd-clarify--window-config)
          (buffer (marker-buffer org-gtd-clarify--source-heading-marker))
          (position (marker-position org-gtd-clarify--source-heading-marker)))
      (with-current-buffer buffer
        (goto-char position)
        (org-cut-subtree))
      (set-window-configuration window-config)
      (kill-buffer (org-gtd-clarify--buffer-name task-id))
      (if loop-p (org-gtd-process-inbox)))))

;;;; Footer

(provide 'org-gtd-organize)

;;; org-gtd-organize.el ends here
