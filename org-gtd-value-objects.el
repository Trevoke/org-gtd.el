;;; org-gtd-value-objects.el --- Domain predicates and value objects for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Domain predicates and value objects for org-gtd.
;;
;; This module encapsulates business rules about TODO states and task
;; dependencies, making code self-documenting and centralizing validation logic.
;;
;; TODO State Predicates:
;; - org-gtd-todo-state-is-active-p: Is this an active (not done/canceled/wait) state?
;; - org-gtd-todo-state-is-ready-p: Can this task be worked on?
;; - org-gtd-todo-state-blocks-others-p: Does this state prevent dependent tasks from becoming NEXT?
;; - org-gtd-todo-state-should-reset-p: Should this state be reset to TODO during project updates?
;;
;; Task Dependency Value Object:
;; - org-gtd-task-deps: Struct wrapping dependency and blocker lists
;; - org-gtd-task-deps-from-task: Read dependency data from task
;; - org-gtd-task-deps-is-blocked-p: Does this task have unsatisfied dependencies?
;; - org-gtd-task-deps-is-ready-p: Are all dependencies satisfied?
;;
;; These predicates and value objects:
;; - Encapsulate business rules about TODO states
;; - Make code self-documenting
;; - Centralize validation logic
;; - Work with state from org files (use accessors to read state)
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-accessors)

(declare-function 'org-gtd-keywords--is-done-p 'org-gtd-core)
(declare-function 'org-gtd-keywords--canceled 'org-gtd-core)
(declare-function 'org-gtd-keywords--wait 'org-gtd-core)
(declare-function 'org-gtd-keywords--next 'org-gtd-core)
(declare-function 'org-gtd-keywords--todo 'org-gtd-core)

;;;; TODO State Predicates

(defun org-gtd-todo-state-is-active-p (state)
  "Domain predicate: Is STATE an active (not done/canceled/wait) state?

Active states are those that represent work that can be or is being done,
excluding states that indicate waiting, completion, or cancellation.

STATE should be a TODO keyword string (e.g., \"TODO\", \"NEXT\", \"DONE\").
Returns t if STATE is active, nil otherwise or if STATE is nil."
  (when state
    (not (or (org-gtd-keywords--is-done-p state)
             (equal state (org-gtd-keywords--canceled))
             (equal state (org-gtd-keywords--wait))))))

(defun org-gtd-todo-state-is-ready-p (state)
  "Domain predicate: Can a task in STATE be worked on?

Ready states are NEXT (explicitly ready to work on) or TODO (available to work on).

STATE should be a TODO keyword string.
Returns t if STATE is ready, nil otherwise or if STATE is nil."
  (when state
    (or (equal state (org-gtd-keywords--next))
        (equal state (org-gtd-keywords--todo)))))

(defun org-gtd-todo-state-blocks-others-p (state)
  "Domain rule: Does STATE prevent dependent tasks from becoming NEXT?

A task blocks others unless it's DONE or CNCL. Tasks in TODO, NEXT, or WAIT
states all block their dependents from becoming NEXT.

This encodes the business rule: dependent tasks can only advance when
their blockers are fully completed or canceled.

STATE should be a TODO keyword string.
Returns t if STATE blocks others, nil if it doesn't or if STATE is nil."
  (when state
    (not (or (org-gtd-keywords--is-done-p state)
             (equal state (org-gtd-keywords--canceled))))))

(defun org-gtd-todo-state-should-reset-p (state)
  "Domain rule: Should STATE be reset to TODO during project updates?

States that should NOT be reset:
- WAIT: User explicitly marked task as waiting
- CNCL: Task is canceled
- DONE: Task is completed

All other states (TODO, NEXT) should be reset and recalculated.

This encodes the business rule: preserve explicit user decisions about
waiting and completion, but recalculate everything else.

STATE should be a TODO keyword string.
Returns t if STATE should be reset, nil otherwise or if STATE is nil."
  (when state
    (not (or (equal state (org-gtd-keywords--wait))
             (equal state (org-gtd-keywords--canceled))
             (org-gtd-keywords--is-done-p state)))))

;;;; Task Dependency Value Object

(cl-defstruct (org-gtd-task-deps
               (:constructor org-gtd-task-deps--create))
  "Value object wrapping task dependency relationships.

Dependencies are read from org file properties but wrapped in a struct
to provide domain-focused operations.

Slots:
- depends-on: List of task IDs this task depends on (must complete first)
- blocks: List of task IDs this task blocks (can't start until this completes)"
  depends-on
  blocks)

(defun org-gtd-task-deps-from-task (task-id)
  "Read dependency value object from task in org file.

TASK-ID is the ID of the task to read dependencies from.

Returns org-gtd-task-deps struct with dependency lists, or nil if task not found."
  (when task-id
    (org-gtd-task-deps--create
     :depends-on (org-gtd-get-task-dependencies task-id)
     :blocks (org-gtd-get-task-blockers task-id))))

(defun org-gtd-task-deps-is-blocked-p (deps)
  "Domain query: Does task have unsatisfied dependencies?

DEPS should be an org-gtd-task-deps struct.

Returns t if task has dependencies and any are not done, nil if all
dependencies are satisfied or if task has no dependencies."
  (when-let ((depends-on (org-gtd-task-deps-depends-on deps)))
    (not (cl-every #'org-gtd-task-is-done-p depends-on))))

(defun org-gtd-task-deps-is-ready-p (deps)
  "Domain query: Are all dependencies satisfied?

DEPS should be an org-gtd-task-deps struct.

Returns t if task has no dependencies or all dependencies are done.
Returns nil if any dependencies are not satisfied."
  (not (org-gtd-task-deps-is-blocked-p deps)))

;;;; Footer

(provide 'org-gtd-value-objects)

;;; org-gtd-value-objects.el ends here
