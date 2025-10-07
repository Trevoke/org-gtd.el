;;; org-gtd-accessors.el --- Domain-focused accessor layer for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2025 Aldric Giacomoni
;;
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Accessor layer wrapping org-mode operations with domain-focused functions.
;;
;; This module provides clean, testable interfaces for accessing and manipulating
;; task properties in the org-gtd system. It wraps repetitive org-mode patterns
;; with domain-meaningful function names.
;;
;; Benefits:
;; - Clearer business intent in domain services
;; - Easier testing (can mock accessor layer)
;; - Consistent property access patterns
;; - Single source of truth for property operations
;;
;; Core Functions:
;; - Property Readers: Get task dependencies, blockers, projects, state
;; - Property Writers: Set task state, add/remove from multivalued properties
;; - Task Lookup: Find task markers by ID (current buffer first, then org-id-find)
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)

(declare-function 'org-gtd-keywords--is-done-p 'org-gtd-core)
(declare-function 'org-gtd-keywords--canceled 'org-gtd-core)

;;;; Property Name Constants

(defconst org-gtd-prop-depends-on "ORG_GTD_DEPENDS_ON"
  "Property storing task IDs this task depends on.")

(defconst org-gtd-prop-blocks "ORG_GTD_BLOCKS"
  "Property storing task IDs this task blocks.")

(defconst org-gtd-prop-first-tasks "ORG_GTD_FIRST_TASKS"
  "Property storing root task IDs for a project.")

(defconst org-gtd-prop-project-ids "ORG_GTD_PROJECT_IDS"
  "Property storing project IDs this task belongs to.")

(defconst org-gtd-prop-category "ORG_GTD"
  "Property storing org-gtd category (Actions, Projects, etc.).")

;;;; Property Readers

(defun org-gtd-get-task-dependencies (task-id)
  "Get list of task IDs that TASK-ID depends on.
Returns nil if task not found or has no dependencies."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-get-multivalued-property (point) org-gtd-prop-depends-on))))

(defun org-gtd-get-task-blockers (task-id)
  "Get list of task IDs blocked by TASK-ID.
Returns nil if task not found or blocks no tasks."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-get-multivalued-property (point) org-gtd-prop-blocks))))

(defun org-gtd-get-project-first-tasks (project-marker)
  "Get list of root task IDs for project at PROJECT-MARKER.
PROJECT-MARKER should be a marker pointing to the project heading.
Returns nil if no first tasks defined."
  (org-with-point-at project-marker
    (org-entry-get-multivalued-property (point) org-gtd-prop-first-tasks)))

(defun org-gtd-get-task-state (task-id)
  "Get TODO keyword state for TASK-ID.
Returns the TODO state string (e.g., \"TODO\", \"NEXT\", \"DONE\") or nil if not found."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-get (point) "TODO"))))

(defun org-gtd-get-task-projects (task-id)
  "Get list of project IDs that TASK-ID belongs to.
Returns nil if task not found or not part of any projects."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids))))

(defun org-gtd-get-task-category (task-id)
  "Get org-gtd category for TASK-ID.
Returns category string (e.g., \"Actions\", \"Projects\") or nil if not found."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-get (point) org-gtd-prop-category))))

;;;; Property Writers

(defun org-gtd-set-task-state (task-id state)
  "Set TODO keyword STATE for TASK-ID.
STATE should be a valid TODO keyword string from org-gtd-keywords.
Returns t if successful, nil if task not found."
  (when-let ((marker (org-gtd-find-task-marker task-id)))
    (org-with-point-at marker
      (org-entry-put (point) "TODO" state)
      t)))

(defun org-gtd-add-to-multivalued-property (task-id property value)
  "Add VALUE to multivalued PROPERTY on TASK-ID.
Returns t if successful, signals error if task not found."
  (let ((marker (org-gtd-find-task-marker task-id)))
    (if marker
        (prog1 t
          (org-with-point-at marker
            (org-entry-add-to-multivalued-property (point) property value)))
      (user-error "Could not find task with ID: %s" task-id))))

(defun org-gtd-remove-from-multivalued-property (task-id property value)
  "Remove VALUE from multivalued PROPERTY on TASK-ID.
Returns t if successful, signals error if task not found."
  (let ((marker (org-gtd-find-task-marker task-id)))
    (if marker
        (prog1 t
          (org-with-point-at marker
            (org-entry-remove-from-multivalued-property (point) property value)))
      (user-error "Could not find task with ID: %s" task-id))))

;;;; Task Lookup

(defun org-gtd-find-task-marker (task-id)
  "Find marker for task with TASK-ID.
First tries current buffer (for performance and test support),
then falls back to org-id-find for cross-file lookup.
Returns marker or nil if not found."
  (or
   (org-gtd-find-task-in-current-buffer task-id)
   (org-id-find task-id t)))

(defun org-gtd-find-task-in-current-buffer (task-id)
  "Find marker for task with TASK-ID in current buffer.
Returns marker or nil if not found in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when-let ((pos (org-find-entry-with-id task-id)))
      (goto-char pos)
      (point-marker))))

;;;; Task State Predicates

(defun org-gtd-task-is-done-p (task-id)
  "Check if TASK-ID is marked as DONE or CNCL.
Returns t if task is done/canceled, nil otherwise or if task not found."
  (when-let ((state (org-gtd-get-task-state task-id)))
    (or (org-gtd-keywords--is-done-p state)
        (equal state (org-gtd-keywords--canceled)))))

(defun org-gtd-task-is-active-p (task-id)
  "Check if TASK-ID is in an active state (not DONE, CNCL, or WAIT).
Returns t if task is active, nil otherwise or if task not found."
  (when-let ((state (org-gtd-get-task-state task-id)))
    (not (or (org-gtd-keywords--is-done-p state)
             (equal state (org-gtd-keywords--canceled))
             (equal state (org-gtd-keywords--wait))))))

;;;; Footer

(provide 'org-gtd-accessors)

;;; org-gtd-accessors.el ends here
