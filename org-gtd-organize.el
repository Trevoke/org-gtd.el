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

(require 'transient)
(require 'org-gtd-core)
(require 'org-gtd-calendar)
(require 'org-gtd-habit)
(require 'org-gtd-knowledge)
(require 'org-gtd-incubate)
(require 'org-gtd-quick-action)
(require 'org-gtd-single-action)
(require 'org-gtd-trash)
(require 'org-gtd-delegate)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)

(defgroup org-gtd-organize nil
  "Manage the functions for organizing the GTD actions."
  :package-version '(org-gtd . "3.0.0")
  :group 'org-gtd)

(defcustom org-gtd-decorate-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element.  The default value has
one function: setting org tags on the item.  Some built-in examples are
provided as options here.  You can create your own functions to enhance/decorate
the items once they have been processed and add them to that list."
  :group 'org-gtd
  :package-version '(org-gtd . "1.0.4")
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

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

(defalias 'org-gtd-choose #'org-gtd-organize)
(make-obsolete 'org-gtd-choose #'org-gtd-organize "2.3.0")

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

(defun org-gtd-organize-decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (dolist (hook org-gtd-decorate-item-hooks)
    (save-excursion
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (save-restriction
        (funcall hook)))))

(defun org-gtd-organize--decorate-element (element)
  "Apply `org-gtd--decorate-item' to org-element ELEMENT."
  (org-with-point-at (org-gtd-projects--org-element-pom element)
    (org-narrow-to-element)
    (org-gtd-organize-decorate-item)))

(provide 'org-gtd-organize)
;;; org-gtd-organize.el ends here
