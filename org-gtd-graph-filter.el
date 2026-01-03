;;; org-gtd-graph-filter.el --- Filter functionality for graph views -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides filter functionality for graph views, allowing
;; users to filter by TODO states, priorities, tags, and scheduling.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-gtd-graph-data)

;;;; External Variables and Functions

(defvar org-gtd-graph-view--filter)
(defvar org-gtd-graph-view--graph)
(defvar org-gtd-graph-ui--selected-node-id)

(declare-function org-gtd-graph-view-refresh "org-gtd-graph-view")

;;;; Data Structures

(cl-defstruct (org-gtd-graph-filter
               (:constructor org-gtd-graph-filter-create)
               (:copier copy-org-gtd-graph-filter))
  "Active filters for graph view.

Fields:
  todo-states  - List of TODO states to show (nil = all)
  priorities   - List of priorities to show (nil = all)
  tags         - List of tags to show (nil = all)
  scheduled    - Scheduled filter: 'overdue, 'today, 'week, 'unscheduled, or nil"
  (todo-states nil)
  (priorities nil)
  (tags nil)
  (scheduled nil))

;;;; Filter Application

(defun org-gtd-graph-filter-apply (graph filter)
  "Return list of node IDs that pass FILTER in GRAPH.

Applies all active filters using AND logic. If a filter field is nil,
that filter is not applied (all nodes pass that criterion)."
  (let ((all-node-ids nil))
    ;; Collect all node IDs
    (maphash (lambda (node-id _node)
               (push node-id all-node-ids))
             (org-gtd-graph-nodes graph))

    ;; Apply each filter criterion
    (let ((visible-ids all-node-ids))
      ;; TODO state filter
      (when (org-gtd-graph-filter-todo-states filter)
        (setq visible-ids
              (org-gtd-graph-filter--filter-by-todo-states
               graph visible-ids (org-gtd-graph-filter-todo-states filter))))

      ;; Priority filter
      (when (org-gtd-graph-filter-priorities filter)
        (setq visible-ids
              (org-gtd-graph-filter--filter-by-priorities
               graph visible-ids (org-gtd-graph-filter-priorities filter))))

      ;; Tags filter
      (when (org-gtd-graph-filter-tags filter)
        (setq visible-ids
              (org-gtd-graph-filter--filter-by-tags
               graph visible-ids (org-gtd-graph-filter-tags filter))))

      ;; Scheduled filter
      (when (org-gtd-graph-filter-scheduled filter)
        (setq visible-ids
              (org-gtd-graph-filter--filter-by-scheduled
               graph visible-ids (org-gtd-graph-filter-scheduled filter))))

      visible-ids)))

;;;; Filter Helper Functions

(defun org-gtd-graph-filter--filter-by-todo-states (graph node-ids todo-states)
  "Filter NODE-IDS in GRAPH to only those with TODO state in TODO-STATES."
  (cl-remove-if-not
   (lambda (node-id)
     (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
       (member (org-gtd-graph-node-state node) todo-states)))
   node-ids))

(defun org-gtd-graph-filter--filter-by-priorities (graph node-ids priorities)
  "Filter NODE-IDS in GRAPH to only those with priority in PRIORITIES."
  (cl-remove-if-not
   (lambda (node-id)
     (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
       (member (org-gtd-graph-node-priority node) priorities)))
   node-ids))

(defun org-gtd-graph-filter--filter-by-tags (graph node-ids tags)
  "Filter NODE-IDS in GRAPH to only those with at least one tag in TAGS."
  (cl-remove-if-not
   (lambda (node-id)
     (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
       (let ((node-tags (org-gtd-graph-node-tags node)))
         (and node-tags
              (cl-some (lambda (tag) (member tag tags)) node-tags)))))
   node-ids))

(defun org-gtd-graph-filter--filter-by-scheduled (graph node-ids scheduled-filter)
  "Filter NODE-IDS in GRAPH by SCHEDULED-FILTER criterion.

SCHEDULED-FILTER can be:
  \\='overdue     - scheduled before today
  \\='today       - scheduled for today
  \\='week        - scheduled within 7 days from today
  \\='unscheduled - no scheduled date"
  (let* ((today (format-time-string "%F"))
         (today-time (org-time-string-to-time today))
         (week-from-now (time-add today-time (days-to-time 7))))
    (cl-remove-if-not
     (lambda (node-id)
       (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
         (let ((scheduled (org-gtd-graph-node-scheduled node)))
           (pcase scheduled-filter
             ('overdue
              (and scheduled
                   (time-less-p (org-time-string-to-time scheduled) today-time)))
             ('today
              (and scheduled
                   (string= scheduled today)))
             ('week
              (and scheduled
                   (let ((sched-time (org-time-string-to-time scheduled)))
                     (and (not (time-less-p sched-time today-time))
                          (time-less-p sched-time week-from-now)))))
             ('unscheduled
              (not scheduled))))))
     node-ids)))

(defun org-gtd-graph-filter--get-subtree-ids (graph root-id)
  "Get list of ROOT-ID and all its descendant node IDs in GRAPH.

Uses depth-first search to traverse the dependency graph starting from
ROOT-ID, following all outgoing edges to collect descendants."
  (let ((visited (make-hash-table :test 'equal)))
    (org-gtd-graph-filter--dfs-collect graph root-id visited)))

(defun org-gtd-graph-filter--dfs-collect (graph node-id visited)
  "DFS helper to collect NODE-ID and descendants in GRAPH.

VISITED tracks visited nodes to handle potential cycles.
Returns list of node IDs in the subtree."
  (if (gethash node-id visited)
      nil
    (puthash node-id t visited)
    (cons node-id
          (apply #'append
                 (mapcar (lambda (successor-id)
                           (org-gtd-graph-filter--dfs-collect graph successor-id visited))
                         (org-gtd-graph-data-get-successors graph node-id))))))

;;;; Graph Filtering

(defun org-gtd-graph-filter-create-filtered-graph (graph filter)
  "Create a new GRAPH containing only nodes that pass FILTER.

Returns a new org-gtd-graph structure with only the visible nodes
and edges between visible nodes."
  (let* ((visible-ids (org-gtd-graph-filter-apply graph filter))
         (visible-id-set (make-hash-table :test 'equal))
         (filtered-graph (org-gtd-graph-create
                          :project-id (org-gtd-graph-project-id graph)
                          :project-name (org-gtd-graph-project-name graph)))
         (filtered-nodes (org-gtd-graph-nodes filtered-graph))
         (filtered-edges nil)
         (filtered-roots nil))

    ;; Build set of visible IDs for fast lookup
    (dolist (id visible-ids)
      (puthash id t visible-id-set))

    ;; Copy visible nodes
    (dolist (node-id visible-ids)
      (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
        (puthash node-id node filtered-nodes)))

    ;; Copy edges where both nodes are visible
    (dolist (edge (org-gtd-graph-edges graph))
      (when (and (gethash (org-gtd-graph-edge-from-id edge) visible-id-set)
                 (gethash (org-gtd-graph-edge-to-id edge) visible-id-set))
        (push edge filtered-edges)))

    (setf (org-gtd-graph-edges filtered-graph) filtered-edges)

    ;; Calculate filtered roots
    (dolist (root-id (org-gtd-graph-root-ids graph))
      (when (gethash root-id visible-id-set)
        (push root-id filtered-roots)))

    (setf (org-gtd-graph-root-ids filtered-graph) filtered-roots)

    filtered-graph))

;;;; Helper Functions

(defun org-gtd-graph-filter--get-all-tags (graph)
  "Get all unique tags from nodes in GRAPH."
  (let ((tags-set (make-hash-table :test 'equal))
        (nodes (org-gtd-graph-nodes graph)))
    (maphash (lambda (_id node)
               (when-let ((node-tags (org-gtd-graph-node-tags node)))
                 (dolist (tag node-tags)
                   (puthash tag t tags-set))))
             nodes)
    (let (tags-list)
      (maphash (lambda (tag _val) (push tag tags-list)) tags-set)
      (sort tags-list #'string<))))

;;;; Footer

(provide 'org-gtd-graph-filter)

;;; org-gtd-graph-filter.el ends here
