;;; org-gtd-graph-data.el --- Graph data structures for DAG visualization -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module defines the data structures used for representing project
;; dependency graphs for visualization as DAGs (Directed Acyclic Graphs).
;; It provides structures for nodes (tasks), edges (dependencies), and
;; complete graphs, along with utility functions for graph manipulation.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-gtd-projects)
(require 'org-gtd-id)

;;;; Data Structures

(cl-defstruct (org-gtd-graph-node
               (:constructor org-gtd-graph-node-create)
               (:copier nil))
  "Represents a task node in the dependency graph.

Fields:
  id           - Unique org-id for the task
  title        - Task heading text
  state        - TODO/NEXT/DONE/etc.
  category     - ORG_GTD category (Projects, Single Actions, etc.)
  x            - X coordinate for rendering (set by layout algorithm)
  y            - Y coordinate for rendering (set by layout algorithm)
  layer        - Layer number in the DAG (0 = root tasks)
  width        - Node width in pixels for rendering
  height       - Node height in pixels for rendering
  project-ids  - List of project IDs this task belongs to
  priority     - Org-mode priority (A, B, C, or nil)
  tags         - List of tags for the task
  scheduled    - Scheduled date string (if any)
  deadline     - Deadline date string (if any)"
  id title state category
  (x 0) (y 0) (layer 0)
  (width 120) (height 40)
  project-ids
  priority tags scheduled deadline)

(cl-defstruct (org-gtd-graph-edge
               (:constructor org-gtd-graph-edge-create)
               (:copier nil))
  "Represents a dependency edge in the graph.

Fields:
  from-id - ID of the blocking task (dependency)
  to-id   - ID of the blocked task (dependent)
  type    - Edge type (:blocks or :depends-on, redundant but useful)
  points  - List of (x y) coordinates for edge path (set by layout)"
  from-id to-id type
  (points nil))

(cl-defstruct (org-gtd-graph
               (:constructor org-gtd-graph-create)
               (:copier nil))
  "Represents a complete project dependency graph.

Fields:
  project-id   - ID of the project heading
  project-name - Title of the project
  nodes        - Hash table mapping task IDs to org-gtd-graph-node structs
  edges        - List of org-gtd-graph-edge structs
  root-ids     - List of task IDs with no dependencies (entry points)"
  project-id project-name
  (nodes (make-hash-table :test 'equal))
  (edges nil)
  (root-ids nil))

;;;; Graph Construction Functions

(defun org-gtd-graph-data--extract-from-project (project-marker)
  "Extract graph data from project at PROJECT-MARKER.

Returns an org-gtd-graph structure populated with nodes and edges
extracted from the project's task tree and dependency relationships."
  (let ((graph (org-gtd-graph-create))
        (project-id nil)
        (project-name nil)
        (project-state nil)
        (project-category nil)
        (project-priority nil)
        (project-tags nil)
        (project-scheduled nil)
        (project-deadline nil)
        (first-task-ids nil)
        (task-markers nil)
        (tasks nil))

    ;; Get project metadata and task markers
    (with-current-buffer (marker-buffer project-marker)
      (goto-char (marker-position project-marker))
      (setq project-id (org-gtd-id-get-create)
            project-name (org-get-heading t t t t)
            project-state (org-entry-get (point) org-gtd-prop-todo)
            project-category (org-entry-get (point) "ORG_GTD")
            project-priority (org-entry-get (point) "PRIORITY")
            project-tags (org-get-tags)
            project-scheduled (org-entry-get (point) "SCHEDULED")
            project-deadline (org-entry-get (point) "DEADLINE")
            first-task-ids (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")
            task-markers (org-gtd-projects--collect-tasks-by-graph project-marker)))

    ;; Convert markers to task plists
    (dolist (marker task-markers)
      (org-with-point-at marker
        (let ((task (list :id (org-entry-get (point) "ID")
                          :title (org-get-heading t t t t)
                          :state (org-entry-get (point) org-gtd-prop-todo)
                          :category (org-entry-get (point) "ORG_GTD")
                          :depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")
                          :blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")
                          :priority (org-entry-get (point) "PRIORITY")
                          :tags (org-get-tags)
                          :scheduled (org-entry-get (point) "SCHEDULED")
                          :deadline (org-entry-get (point) "DEADLINE"))))
          (push task tasks))))
    (setq tasks (nreverse tasks))

    ;; Set project metadata
    (setf (org-gtd-graph-project-id graph) project-id
          (org-gtd-graph-project-name graph) project-name)

    ;; Add project heading itself as a node (layer will be set after finding leaves)
    (let ((project-node (org-gtd-graph-node-create
                         :id project-id
                         :title project-name
                         :state project-state
                         :category project-category
                         :project-ids (list project-id)
                         :priority project-priority
                         :tags project-tags
                         :scheduled project-scheduled
                         :deadline project-deadline)))
      (puthash project-id project-node (org-gtd-graph-nodes graph)))

    ;; Extract nodes and edges from tasks
    (org-gtd-graph-data--process-tasks graph tasks project-id)

    ;; Find leaf tasks (tasks with no outgoing edges to other tasks in graph)
    ;; and connect them TO the project node (project is the "finish line")
    (let ((nodes (org-gtd-graph-nodes graph))
          (edges (org-gtd-graph-edges graph))
          (tasks-with-outgoing (make-hash-table :test 'equal))
          (leaf-edges nil))
      ;; Mark all tasks that have outgoing edges to other tasks (not project)
      (dolist (edge edges)
        (let ((from-id (org-gtd-graph-edge-from-id edge))
              (to-id (org-gtd-graph-edge-to-id edge)))
          (when (and (not (equal from-id project-id))
                     (not (equal to-id project-id))
                     (gethash to-id nodes))
            (puthash from-id t tasks-with-outgoing))))
      ;; Find leaf tasks and create edges to project
      (maphash (lambda (task-id _node)
                 (when (and (not (equal task-id project-id))
                            (not (gethash task-id tasks-with-outgoing)))
                   (push (org-gtd-graph-edge-create
                          :from-id task-id
                          :to-id project-id
                          :type :blocks)
                         leaf-edges)))
               nodes)
      (setf (org-gtd-graph-edges graph)
            (append (org-gtd-graph-edges graph) leaf-edges)))

    ;; Set first tasks as root nodes (they are the starting points)
    (let ((valid-roots (seq-filter (lambda (id)
                                     (gethash id (org-gtd-graph-nodes graph)))
                                   first-task-ids)))
      (setf (org-gtd-graph-root-ids graph)
            (or valid-roots
                ;; Fallback: find tasks with no incoming edges
                (org-gtd-graph-data--find-root-tasks graph project-id))))

    ;; Deduplicate edges (BLOCKS and DEPENDS_ON create the same edge)
    (org-gtd-graph-data--deduplicate-edges graph)

    graph))

(defun org-gtd-graph-data--process-tasks (graph tasks project-id)
  "Process TASKS list and populate GRAPH with nodes and edges.
PROJECT-ID is the ID of the parent project."
  (let ((nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph)))

    (dolist (task tasks)
      (let* ((task-id (plist-get task :id))
             (title (plist-get task :title))
             (state (plist-get task :state))
             (category (plist-get task :category))
             (blocks (plist-get task :blocks))
             (depends-on (plist-get task :depends-on))
             (priority (plist-get task :priority))
             (tags (plist-get task :tags))
             (scheduled (plist-get task :scheduled))
             (deadline (plist-get task :deadline))
             (node (org-gtd-graph-node-create
                    :id task-id
                    :title title
                    :state state
                    :category category
                    :project-ids (list project-id)
                    :priority priority
                    :tags tags
                    :scheduled scheduled
                    :deadline deadline)))

        ;; Add node to graph
        (puthash task-id node nodes)

        ;; Create edges from BLOCKS relationships (preserve order)
        ;; Skip edges that point to the project (handled separately as leaf->project)
        (let ((task-edges nil))
          (dolist (blocked-id blocks)
            (unless (equal blocked-id project-id)
              (push (org-gtd-graph-edge-create
                     :from-id task-id
                     :to-id blocked-id
                     :type :blocks)
                    task-edges)))
          ;; Reverse to preserve BLOCKS property order, then append
          (setq edges (append edges (nreverse task-edges))))

        ;; Create edges from DEPENDS_ON relationships (preserve order)
        ;; These create edges FROM the dependency TO this task
        ;; Skip edges from the project (project->task would create cycles)
        (let ((depends-edges nil))
          (dolist (dependency-id depends-on)
            (unless (equal dependency-id project-id)
              (push (org-gtd-graph-edge-create
                     :from-id dependency-id
                     :to-id task-id
                     :type :depends-on)
                    depends-edges)))
          ;; Reverse to preserve DEPENDS_ON property order, then append
          (setq edges (append edges (nreverse depends-edges))))))

    ;; Update edges in graph
    (setf (org-gtd-graph-edges graph) edges)))

(defun org-gtd-graph-data--find-root-tasks (graph project-id)
  "Find root tasks in GRAPH, excluding PROJECT-ID.
Root tasks are those with no incoming edges from other tasks."
  (let ((nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph))
        (has-dependencies (make-hash-table :test 'equal))
        (roots nil))
    ;; Mark all tasks that have incoming edges (dependencies)
    (dolist (edge edges)
      (let ((to-id (org-gtd-graph-edge-to-id edge)))
        (unless (equal to-id project-id)
          (puthash to-id t has-dependencies))))
    ;; Find tasks without incoming edges, excluding project
    (maphash (lambda (task-id _node)
               (when (and (not (equal task-id project-id))
                          (not (gethash task-id has-dependencies)))
                 (push task-id roots)))
             nodes)
    (nreverse roots)))

(defun org-gtd-graph-data--deduplicate-edges (graph)
  "Remove duplicate edges from GRAPH.
An edge is duplicate if another edge exists with the same from-id and to-id.
Keeps the first occurrence of each unique edge."
  (let ((edges (org-gtd-graph-edges graph))
        (seen (make-hash-table :test 'equal))
        (unique-edges nil))

    (dolist (edge edges)
      (let* ((from-id (org-gtd-graph-edge-from-id edge))
             (to-id (org-gtd-graph-edge-to-id edge))
             (edge-key (cons from-id to-id)))
        (unless (gethash edge-key seen)
          (puthash edge-key t seen)
          (push edge unique-edges))))

    ;; Reverse to preserve original order
    (setf (org-gtd-graph-edges graph) (nreverse unique-edges))))

(defun org-gtd-graph-data--calculate-roots (graph)
  "Calculate and set root task IDs for GRAPH.
Root tasks are those that have no incoming edges (no dependencies)."
  (let ((nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph))
        (has-dependencies (make-hash-table :test 'equal))
        (roots nil))

    ;; Mark all tasks that have dependencies
    (dolist (edge edges)
      (puthash (org-gtd-graph-edge-to-id edge) t has-dependencies))

    ;; Find tasks without dependencies
    (maphash (lambda (task-id _node)
               (unless (gethash task-id has-dependencies)
                 (push task-id roots)))
             nodes)

    (setf (org-gtd-graph-root-ids graph) roots)))

;;;; Graph Query Functions

(defun org-gtd-graph-data-get-node (graph node-id)
  "Get node with NODE-ID from GRAPH, or nil if not found."
  (gethash node-id (org-gtd-graph-nodes graph)))

(defun org-gtd-graph-data-get-outgoing-edges (graph node-id)
  "Get list of edges from NODE-ID in GRAPH."
  (cl-remove-if-not
   (lambda (edge) (string= (org-gtd-graph-edge-from-id edge) node-id))
   (org-gtd-graph-edges graph)))

(defun org-gtd-graph-data-get-incoming-edges (graph node-id)
  "Get list of edges to NODE-ID in GRAPH."
  (cl-remove-if-not
   (lambda (edge) (string= (org-gtd-graph-edge-to-id edge) node-id))
   (org-gtd-graph-edges graph)))

(defun org-gtd-graph-data-get-successors (graph node-id)
  "Get list of successor node IDs for NODE-ID in GRAPH."
  (mapcar #'org-gtd-graph-edge-to-id
          (org-gtd-graph-data-get-outgoing-edges graph node-id)))

(defun org-gtd-graph-data-get-predecessors (graph node-id)
  "Get list of predecessor node IDs for NODE-ID in GRAPH."
  (mapcar #'org-gtd-graph-edge-from-id
          (org-gtd-graph-data-get-incoming-edges graph node-id)))

;;;; Graph Validation Functions

(defun org-gtd-graph-data-validate (graph)
  "Validate GRAPH structure and return list of issues.
Returns nil if graph is valid, or a list of issue descriptions."
  (let ((issues nil))

    ;; Check for orphaned nodes (nodes with no edges and not root)
    (setq issues (append issues (org-gtd-graph-data--find-orphans graph)))

    ;; Check for invalid edge references
    (setq issues (append issues (org-gtd-graph-data--find-invalid-edges graph)))

    ;; Check for cycles
    (when-let ((cycle (org-gtd-graph-data--find-cycle graph)))
      (push (format "Cycle detected: %s" (mapconcat #'identity cycle " -> "))
            issues))

    issues))

(defun org-gtd-graph-data--find-orphans (graph)
  "Find orphaned nodes in GRAPH (nodes with no edges and not root).
Returns list of issue descriptions."
  (let ((orphans nil)
        (nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph))
        (roots (org-gtd-graph-root-ids graph)))

    (maphash
     (lambda (node-id node)
       (let ((has-edges (cl-some
                         (lambda (edge)
                           (or (string= (org-gtd-graph-edge-from-id edge) node-id)
                               (string= (org-gtd-graph-edge-to-id edge) node-id)))
                         edges))
             (is-root (member node-id roots)))
         (when (and (not has-edges) (not is-root))
           (push (format "Orphaned task: %s (%s)"
                         (org-gtd-graph-node-title node)
                         node-id)
                 orphans))))
     nodes)

    orphans))

(defun org-gtd-graph-data--find-invalid-edges (graph)
  "Find edges in GRAPH referencing non-existent nodes.
Returns list of issue descriptions."
  (let ((invalid nil)
        (nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph)))

    (dolist (edge edges)
      (let ((from-id (org-gtd-graph-edge-from-id edge))
            (to-id (org-gtd-graph-edge-to-id edge)))
        (unless (gethash from-id nodes)
          (push (format "Edge references non-existent source node: %s" from-id)
                invalid))
        (unless (gethash to-id nodes)
          (push (format "Edge references non-existent target node: %s" to-id)
                invalid))))

    invalid))

(defun org-gtd-graph-data--find-cycle (graph)
  "Find a cycle in GRAPH using DFS. Returns list of node IDs forming cycle, or nil."
  (let ((visited (make-hash-table :test 'equal))
        (rec-stack (make-hash-table :test 'equal))
        (cycle nil))

    ;; Try DFS from each node
    (catch 'cycle-found
      (maphash
       (lambda (node-id _node)
         (unless (gethash node-id visited)
           (when-let ((found-cycle
                       (org-gtd-graph-data--dfs-find-cycle
                        graph node-id visited rec-stack nil)))
             (setq cycle found-cycle)
             (throw 'cycle-found t))))
       (org-gtd-graph-nodes graph)))

    cycle))

(defun org-gtd-graph-data--dfs-find-cycle (graph node-id visited rec-stack path)
  "DFS helper to find cycle from NODE-ID.
GRAPH is the graph structure.
VISITED tracks all visited nodes.
REC-STACK tracks nodes in current recursion path.
PATH is the current path of node IDs.
Returns path forming cycle if found, nil otherwise."
  (puthash node-id t visited)
  (puthash node-id t rec-stack)
  (let ((current-path (append path (list node-id))))

    (catch 'cycle-found
      (dolist (successor-id (org-gtd-graph-data-get-successors graph node-id))
        (cond
         ;; Found cycle - node in current recursion stack
         ((gethash successor-id rec-stack)
          (throw 'cycle-found (append current-path (list successor-id))))
         ;; Not visited yet - recurse
         ((not (gethash successor-id visited))
          (when-let ((cycle (org-gtd-graph-data--dfs-find-cycle
                             graph successor-id visited rec-stack current-path)))
            (throw 'cycle-found cycle)))))

      ;; Remove from recursion stack
      (remhash node-id rec-stack)
      nil)))

;;;; First Actionable Task

(defun org-gtd-graph-data-find-first-actionable (graph)
  "Find the first actionable task in GRAPH using breadth-first search.

An actionable task has a TODO state that is NOT:
- DONE (completed)
- CNCL (cancelled)
- TODO (not yet activated)
- nil (no state)

This means NEXT, WAIT, or other active workflow states.

Searches from root tasks (ORG_GTD_FIRST_TASKS) in BFS order.

Returns the node ID of the first actionable task, or nil if none found."
  (let* ((nodes (org-gtd-graph-nodes graph))
         (root-ids (org-gtd-graph-root-ids graph))
         (project-id (org-gtd-graph-project-id graph))
         (non-actionable-states '("DONE" "CNCL" "TODO"))
         (visited (make-hash-table :test 'equal))
         (queue (copy-sequence root-ids)))

    (catch 'found
      (while queue
        (let ((node-id (pop queue)))
          (unless (or (gethash node-id visited)
                      (equal node-id project-id))
            (puthash node-id t visited)
            (when-let ((node (gethash node-id nodes)))
              (let ((state (org-gtd-graph-node-state node)))
                ;; Actionable = has a state that's not in non-actionable list
                (when (and state
                           (not (member state non-actionable-states)))
                  (throw 'found node-id)))
              ;; Enqueue successors for BFS
              (dolist (succ-id (org-gtd-graph-data-get-successors graph node-id))
                (unless (gethash succ-id visited)
                  (setq queue (append queue (list succ-id)))))))))
      ;; Return nil if no actionable task found
      nil)))

;;;; Footer

(provide 'org-gtd-graph-data)

;;; org-gtd-graph-data.el ends here
