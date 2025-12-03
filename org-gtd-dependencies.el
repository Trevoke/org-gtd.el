;;; org-gtd-dependencies.el --- Dependency graph operations for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Dependency bounded context for org-gtd.
;;
;; This module provides graph algorithms and services for managing task
;; dependencies. It implements breadth-first search (BFS) for finding
;; ready tasks and depth-first search (DFS) for cycle detection.
;;
;; Core Services:
;; - org-gtd-dependencies-find-ready-tasks: BFS to find unblocked tasks
;; - org-gtd-dependencies-has-path-p: DFS to detect dependency paths
;; - org-gtd-dependencies-validate-acyclic: Ensure no cycles before creating dependency
;; - org-gtd-dependencies-create: Create bidirectional dependency relationships
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)

;;;; Public Services

(defun org-gtd-dependencies-find-ready-tasks (project-id first-task-ids)
  "Find tasks ready to work on via BFS traversal.

PROJECT-ID is the ID of the project being analyzed.
FIRST-TASK-IDS is a list of root task IDs to start traversal from.

For each task in breadth-first order:
- If DONE/CNCL: continue traversing to children
- If TODO and all dependencies satisfied: mark ready and stop this branch
- If TODO but blocked: leave as TODO and stop this branch

Returns list of task IDs whose dependencies are all satisfied."
  (let ((ready-tasks '())
        (visited (make-hash-table :test 'equal))
        (queue (copy-sequence first-task-ids)))

    (while queue
      (let ((task-id (pop queue)))
        (when (not (gethash task-id visited))
          (puthash task-id t visited)
          (when (string= (org-gtd-get-task-category task-id) "Actions")
            (let* ((state (org-gtd-get-task-state task-id))
                   (deps (org-gtd-task-deps-from-task task-id)))

              (cond
               ((not (org-gtd-todo-state-blocks-others-p state))
                (when-let ((blocks (org-gtd-task-deps-blocks deps)))
                  (dolist (blocked-id blocks)
                    (let ((blocked-project-ids (org-gtd-get-task-projects blocked-id)))
                      (when (member project-id blocked-project-ids)
                        (setq queue (append queue (list blocked-id))))))))

               ((org-gtd-task-deps-is-ready-p deps)
                (push task-id ready-tasks))))))))

    (nreverse ready-tasks)))

(defun org-gtd-dependencies-has-path-p (from-id to-id)
  "Check if path exists from FROM-ID to TO-ID via dependency graph.

Uses depth-first search to traverse ORG_GTD_BLOCKS relationships.
Returns t if path exists, nil otherwise.

This is used for cycle detection: if adding FROM-ID → TO-ID would
create a cycle, then there must already be a path TO-ID → FROM-ID."
  (unless (equal from-id to-id)
    (let ((visited (make-hash-table :test 'equal)))
      (org-gtd-dependencies--dfs-path from-id to-id visited))))

(defun org-gtd-dependencies-validate-acyclic (blocker-id dependent-id)
  "Validate that creating dependency won't create cycle.

BLOCKER-ID is the task that must complete first.
DEPENDENT-ID is the task that depends on the blocker.

Throws user-error with descriptive path if cycle would be created."
  (when (org-gtd-dependencies-has-path-p dependent-id blocker-id)
    (let ((existing-path (org-gtd-dependencies--find-path dependent-id blocker-id)))
      (user-error "Circular dependency detected: %s"
             (mapconcat #'identity
                        (append (or existing-path (list dependent-id blocker-id))
                                (list dependent-id))
                        " -> ")))))

(defun org-gtd-dependencies-create (blocker-id dependent-id)
  "Create bidirectional dependency relationship.

BLOCKER-ID must complete before DEPENDENT-ID can be worked on.

Creates bidirectional properties:
- Adds DEPENDENT-ID to BLOCKER-ID's ORG_GTD_BLOCKS
- Adds BLOCKER-ID to DEPENDENT-ID's ORG_GTD_DEPENDS_ON

Validates no cycles before creating relationship."
  (org-gtd-dependencies-validate-acyclic blocker-id dependent-id)
  (org-gtd-add-to-multivalued-property blocker-id org-gtd-prop-blocks dependent-id)
  (org-gtd-add-to-multivalued-property dependent-id org-gtd-prop-depends-on blocker-id))

;;;; Private Helpers

(defun org-gtd-dependencies--dfs-path (current-id target-id visited)
  "Depth-first search for path from CURRENT-ID to TARGET-ID.
VISITED is hash table to prevent infinite loops.
Returns t if path exists, nil otherwise."
  (if (gethash current-id visited)
      nil
    (puthash current-id t visited)

    (let ((blocks (org-gtd-get-task-blockers current-id)))
      (catch 'found
        (dolist (blocked-id blocks)
          (when (equal blocked-id target-id)
            (throw 'found t))
          (when (org-gtd-dependencies--dfs-path blocked-id target-id visited)
            (throw 'found t)))
        nil))))

(defun org-gtd-dependencies--find-path (from-id to-id)
  "Find and return dependency path from FROM-ID to TO-ID as list of IDs.
Returns nil if no path exists."
  (let ((visited (make-hash-table :test 'equal)))
    (org-gtd-dependencies--dfs-find-path from-id to-id visited)))

(defun org-gtd-dependencies--dfs-find-path (current-id target-id visited)
  "Depth-first search to find path from CURRENT-ID to TARGET-ID.
Returns path as list of IDs, or nil if no path exists."
  (if (gethash current-id visited)
      nil
    (puthash current-id t visited)

    (if (equal current-id target-id)
        (list current-id)
      (let ((blocks (org-gtd-get-task-blockers current-id)))
        (catch 'found
          (dolist (blocked-id blocks)
            (let ((sub-path (org-gtd-dependencies--dfs-find-path blocked-id target-id visited)))
              (when sub-path
                (throw 'found (cons current-id sub-path)))))
          nil)))))


;;;; Footer

(provide 'org-gtd-dependencies)

;;; org-gtd-dependencies.el ends here
