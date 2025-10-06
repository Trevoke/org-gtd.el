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

(declare-function 'org-gtd-keywords--is-done-p 'org-gtd-keywords)
(declare-function 'org-gtd-keywords--canceled 'org-gtd-keywords)

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
      (let* ((task-id (pop queue))
             (marker (org-gtd-dependencies--find-id-marker task-id)))
        (when (and marker (not (gethash task-id visited)))
          (puthash task-id t visited)
          (with-current-buffer (marker-buffer marker)
            (save-excursion
              (goto-char marker)
              (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
                (let* ((todo-state (org-entry-get (point) "TODO"))
                       (is-done (or (org-gtd-keywords--is-done-p todo-state)
                                    (equal todo-state (org-gtd-keywords--canceled))))
                       (depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
                       (all-deps-satisfied (or (null depends-on)
                                               (cl-every #'org-gtd-dependencies--task-is-done-p
                                                         depends-on)))
                       (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

                  (cond
                   (is-done
                    (when blocks
                      (dolist (blocked-id blocks)
                        (when-let ((blocked-marker (org-gtd-dependencies--find-id-marker blocked-id)))
                          (with-current-buffer (marker-buffer blocked-marker)
                            (save-excursion
                              (goto-char blocked-marker)
                              (let ((blocked-project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
                                (when (member project-id blocked-project-ids)
                                  (setq queue (append queue (list blocked-id)))))))))))

                   (all-deps-satisfied
                    (push task-id ready-tasks))))))))))

    (nreverse ready-tasks)))

(defun org-gtd-dependencies-has-path-p (from-id to-id)
  "Check if path exists from FROM-ID to TO-ID via dependency graph.

Uses depth-first search to traverse ORG_GTD_BLOCKS relationships.
Returns t if path exists, nil otherwise.

This is used for cycle detection: if adding FROM-ID → TO-ID would
create a cycle, then there must already be a path TO-ID → FROM-ID."
  (when (equal from-id to-id)
    nil)
  (let ((visited (make-hash-table :test 'equal)))
    (org-gtd-dependencies--dfs-path from-id to-id visited)))

(defun org-gtd-dependencies-validate-acyclic (blocker-id dependent-id)
  "Validate that creating dependency won't create cycle.

BLOCKER-ID is the task that must complete first.
DEPENDENT-ID is the task that depends on the blocker.

Throws user-error with descriptive path if cycle would be created."
  (when (org-gtd-dependencies-has-path-p dependent-id blocker-id)
    (let ((existing-path (org-gtd-dependencies--find-path dependent-id blocker-id)))
      (error "Circular dependency detected: %s"
             (mapconcat 'identity
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
  (org-gtd-dependencies--add-to-task-property blocker-id "ORG_GTD_BLOCKS" dependent-id)
  (org-gtd-dependencies--add-to-task-property dependent-id "ORG_GTD_DEPENDS_ON" blocker-id))

;;;; Private Helpers

(defun org-gtd-dependencies--find-id-marker (id)
  "Find marker for task with ID.
First tries current buffer, then falls back to org-id-find."
  (or
   (save-excursion
     (goto-char (point-min))
     (when-let ((pos (org-find-entry-with-id id)))
       (goto-char pos)
       (point-marker)))
   (org-id-find id t)))

(defun org-gtd-dependencies--task-is-done-p (task-id)
  "Check if TASK-ID is marked as DONE or CNCL.
Returns t if task is done/canceled, nil otherwise."
  (let ((marker (org-gtd-dependencies--find-id-marker task-id)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((todo-state (org-entry-get (point) "TODO")))
              (and todo-state (org-gtd-keywords--is-done-p todo-state)))))
      nil)))

(defun org-gtd-dependencies--dfs-path (current-id target-id visited)
  "Depth-first search for path from CURRENT-ID to TARGET-ID.
VISITED is hash table to prevent infinite loops.
Returns t if path exists, nil otherwise."
  (if (gethash current-id visited)
      nil
    (puthash current-id t visited)

    (let ((blocks (org-gtd-dependencies--get-blocks current-id)))
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
      (let ((blocks (org-gtd-dependencies--get-blocks current-id)))
        (catch 'found
          (dolist (blocked-id blocks)
            (let ((sub-path (org-gtd-dependencies--dfs-find-path blocked-id target-id visited)))
              (when sub-path
                (throw 'found (cons current-id sub-path)))))
          nil)))))

(defun org-gtd-dependencies--get-blocks (task-id)
  "Get list of task IDs that TASK-ID blocks.
Returns ORG_GTD_BLOCKS property value as list."
  (let ((pos (org-gtd-dependencies--find-id-in-current-buffer task-id)))
    (if pos
        (save-excursion
          (goto-char pos)
          (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
      (let ((marker (org-id-find task-id t)))
        (if marker
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          '())))))

(defun org-gtd-dependencies--find-id-in-current-buffer (id)
  "Find position of heading with ID in current buffer.
Returns position or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
      (beginning-of-line)
      (while (and (not (org-at-heading-p)) (not (bobp)))
        (forward-line -1))
      (when (org-at-heading-p)
        (point)))))

(defun org-gtd-dependencies--add-to-task-property (task-id property value)
  "Add VALUE to multivalued PROPERTY on task with TASK-ID."
  (let ((pos (org-gtd-dependencies--find-id-in-current-buffer task-id)))
    (if pos
        (save-excursion
          (goto-char pos)
          (org-entry-add-to-multivalued-property (point) property value))
      (let ((marker (org-id-find task-id t)))
        (if marker
            (with-current-buffer (marker-buffer marker)
              (goto-char marker)
              (org-entry-add-to-multivalued-property (point) property value))
          (user-error "Could not find task with ID: %s" task-id))))))

;;;; Footer

(provide 'org-gtd-dependencies)

;;; org-gtd-dependencies.el ends here
