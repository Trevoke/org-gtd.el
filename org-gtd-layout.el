;;; org-gtd-layout.el --- Sugiyama layout algorithm for DAG visualization -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module implements the Sugiyama layout algorithm for drawing
;; hierarchical directed acyclic graphs (DAGs). The algorithm consists
;; of four phases:
;;
;; 1. Cycle removal (skipped - we validate graphs are acyclic)
;; 2. Layer assignment - assign nodes to horizontal layers
;; 3. Crossing minimization - reduce edge crossings between layers
;; 4. Coordinate assignment - compute actual (x,y) positions
;;
;; The algorithm produces a hierarchical layout suitable for visualizing
;; project task dependencies, with root tasks at the top and dependent
;; tasks flowing downward.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-gtd-graph-data)

;;;; Customization

(defgroup org-gtd-layout nil
  "Customize the layout algorithm for graph visualization."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-layout-node-spacing 20
  "Horizontal spacing between nodes in pixels."
  :type 'integer
  :group 'org-gtd-layout)

(defcustom org-gtd-layout-layer-spacing 80
  "Vertical spacing between layers in pixels."
  :type 'integer
  :group 'org-gtd-layout)

(defcustom org-gtd-layout-margin 20
  "Margin around the graph in pixels."
  :type 'integer
  :group 'org-gtd-layout)

;;;; Main Layout Function

(defun org-gtd-layout-apply (graph)
  "Apply Sugiyama layout algorithm to GRAPH.
Modifies node positions (x, y, layer) in place.
Returns the graph with updated node positions."
  ;; Phase 1: Cycle removal (skipped - graphs are already validated as acyclic)

  ;; Phase 2: Layer assignment
  (org-gtd-layout--assign-layers graph)

  ;; Phase 3: Crossing minimization
  (org-gtd-layout--minimize-crossings graph)

  ;; Phase 4: Coordinate assignment
  (org-gtd-layout--assign-coordinates graph)

  graph)

;;;; Phase 2: Layer Assignment

(defun org-gtd-layout--assign-layers (graph)
  "Assign each node in GRAPH to a layer based on longest path from roots.
Root nodes are assigned to layer 0, and each subsequent layer
contains nodes whose longest path from any root is that layer number."
  (let ((nodes (org-gtd-graph-nodes graph))
        (root-ids (org-gtd-graph-root-ids graph))
        (visited (make-hash-table :test 'equal)))

    ;; Initialize all nodes to layer 0
    (maphash (lambda (_id node)
               (setf (org-gtd-graph-node-layer node) 0))
             nodes)

    ;; BFS from each root to assign layers
    (dolist (root-id root-ids)
      (org-gtd-layout--bfs-assign-layers graph root-id visited))))

(defun org-gtd-layout--bfs-assign-layers (graph start-id visited)
  "Breadth-first traversal from START-ID to assign layers.
GRAPH is the graph structure.
VISITED tracks nodes already processed to avoid redundant work."
  (let ((queue (list (cons start-id 0))))

    (while queue
      (let* ((item (pop queue))
             (node-id (car item))
             (layer (cdr item))
             (node (org-gtd-graph-data-get-node graph node-id)))

        (when node
          ;; Update layer if this path is longer
          (when (> layer (org-gtd-graph-node-layer node))
            (setf (org-gtd-graph-node-layer node) layer))

          ;; Mark as visited at this layer
          (puthash node-id t visited)

          ;; Queue successors for next layer
          (dolist (successor-id (org-gtd-graph-data-get-successors graph node-id))
            (push (cons successor-id (1+ layer)) queue)))))))

;;;; Phase 3: Crossing Minimization

(defun org-gtd-layout--minimize-crossings (graph)
  "Minimize edge crossings between layers in GRAPH.
Uses the barycenter heuristic with bidirectional sweeps."
  (let* ((layers (org-gtd-layout--get-layers-by-number graph))
         (max-layer (1- (length layers)))
         (iterations 4))  ; Number of improvement passes

    ;; Multiple passes for better results
    (dotimes (_ iterations)
      ;; Forward pass (top to bottom)
      (dotimes (layer-num (1- max-layer))
        (org-gtd-layout--reorder-layer-by-barycenter
         graph layers layer-num (1+ layer-num) :forward))

      ;; Backward pass (bottom to top)
      (cl-loop for layer-num from (1- max-layer) downto 1 do
               (org-gtd-layout--reorder-layer-by-barycenter
                graph layers layer-num (1- layer-num) :backward)))))

(defun org-gtd-layout--get-layers-by-number (graph)
  "Get list of node lists, grouped by layer number.
Returns vector where index N contains list of node IDs in layer N."
  (let ((nodes (org-gtd-graph-nodes graph))
        (max-layer 0)
        (layers-hash (make-hash-table :test 'equal)))

    ;; Find max layer and group nodes
    (maphash (lambda (node-id node)
               (let ((layer (org-gtd-graph-node-layer node)))
                 (setq max-layer (max max-layer layer))
                 (push node-id (gethash layer layers-hash))))
             nodes)

    ;; Convert to vector
    (let ((layers (make-vector (1+ max-layer) nil)))
      (dotimes (i (1+ max-layer))
        (aset layers i (nreverse (gethash i layers-hash))))
      layers)))

(defun org-gtd-layout--reorder-layer-by-barycenter (graph layers current-layer-num
                                                           reference-layer-num direction)
  "Reorder nodes in CURRENT-LAYER-NUM based on barycenter positions.
GRAPH is the graph structure.
LAYERS is vector of node lists by layer.
REFERENCE-LAYER-NUM is the layer to use for barycenter calculation.
DIRECTION is :forward or :backward."
  (let* ((current-layer (aref layers current-layer-num))
         (reference-layer (aref layers reference-layer-num))
         (reference-positions (make-hash-table :test 'equal)))

    ;; Build position map for reference layer
    (cl-loop for node-id in reference-layer
             for pos from 0
             do (puthash node-id pos reference-positions))

    ;; Calculate barycenter for each node in current layer
    (let ((node-barycenters
           (cl-loop for node-id in current-layer
                    collect (cons node-id
                                  (org-gtd-layout--calculate-barycenter
                                   graph node-id reference-positions direction)))))

      ;; Sort by barycenter value
      (setq node-barycenters
            (sort node-barycenters (lambda (a b) (< (cdr a) (cdr b)))))

      ;; Update layer with sorted nodes
      (aset layers current-layer-num (mapcar #'car node-barycenters)))))

(defun org-gtd-layout--calculate-barycenter (graph node-id reference-positions direction)
  "Calculate barycenter (average position) of connected nodes.
GRAPH is the graph structure.
NODE-ID is the node to calculate barycenter for.
REFERENCE-POSITIONS maps node IDs to their positions in reference layer.
DIRECTION is :forward (use predecessors) or :backward (use successors)."
  (let* ((connected-ids (if (eq direction :forward)
                            (org-gtd-graph-data-get-predecessors graph node-id)
                          (org-gtd-graph-data-get-successors graph node-id)))
         (positions (cl-remove-if #'null
                                  (mapcar (lambda (id) (gethash id reference-positions))
                                          connected-ids))))

    (if positions
        (/ (cl-reduce #'+ positions) (float (length positions)))
      0.0)))

;;;; Phase 4: Coordinate Assignment

(defun org-gtd-layout--assign-coordinates (graph)
  "Assign final (x,y) coordinates to all nodes in GRAPH.
Uses layer assignments and node ordering to compute pixel positions."
  (let* ((layers (org-gtd-layout--get-layers-by-number graph))
         (num-layers (length layers)))

    ;; Assign Y coordinates based on layer
    (dotimes (layer-num num-layers)
      (let ((y (+ org-gtd-layout-margin
                  (* layer-num org-gtd-layout-layer-spacing))))
        (dolist (node-id (aref layers layer-num))
          (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
            (setf (org-gtd-graph-node-y node) y)))))

    ;; Assign X coordinates based on position in layer
    (dotimes (layer-num num-layers)
      (let ((layer-nodes (aref layers layer-num)))

        (cl-loop for node-id in layer-nodes
                 for pos from 0
                 do (when-let ((node (org-gtd-graph-data-get-node graph node-id)))
                      (let ((x (+ org-gtd-layout-margin
                                  (* pos (+ (org-gtd-graph-node-width node)
                                            org-gtd-layout-node-spacing)))))
                        (setf (org-gtd-graph-node-x node) x))))))

    ;; Calculate edge paths (polylines)
    (org-gtd-layout--calculate-edge-paths graph)))

(defun org-gtd-layout--calculate-layer-width (node-ids nodes)
  "Calculate total width needed for layer containing NODE-IDS.
NODES is the hash table of all nodes."
  (let ((total-width 0))
    (dolist (node-id node-ids)
      (when-let ((node (gethash node-id nodes)))
        (setq total-width (+ total-width
                             (org-gtd-graph-node-width node)
                             org-gtd-layout-node-spacing))))
    (- total-width org-gtd-layout-node-spacing)))  ; Remove trailing spacing

(defcustom org-gtd-layout-waypoint-offset 15
  "Vertical offset in pixels for routing waypoints above/below nodes."
  :type 'integer
  :group 'org-gtd-layout)

(defun org-gtd-layout--calculate-edge-waypoints (from-node to-node)
  "Calculate waypoints for edge from FROM-NODE to TO-NODE.
Returns list of (x y) coordinate pairs forming a polyline path.
For adjacent-layer edges, returns 2 points (straight line).
For multi-layer edges, returns 4+ points with routing waypoints.
For same-column multi-layer edges, adds horizontal detour to make edge visible."
  (let* ((from-x (+ (org-gtd-graph-node-x from-node)
                    (/ (org-gtd-graph-node-width from-node) 2)))
         (from-y (+ (org-gtd-graph-node-y from-node)
                    (org-gtd-graph-node-height from-node)))
         (to-x (+ (org-gtd-graph-node-x to-node)
                  (/ (org-gtd-graph-node-width to-node) 2)))
         (to-y (org-gtd-graph-node-y to-node))
         (from-layer (org-gtd-graph-node-layer from-node))
         (to-layer (org-gtd-graph-node-layer to-node))
         (layer-span (- to-layer from-layer)))

    (if (<= layer-span 1)
        ;; Adjacent layers: straight line
        (list (list from-x from-y)
              (list to-x to-y))

      ;; Multi-layer: check if same column (vertically aligned)
      (if (= from-x to-x)
          ;; Same column: add horizontal detour to make edge visible
          (let* ((node-width (org-gtd-graph-node-width from-node))
                 ;; Route to the right side of the column
                 (detour-x (+ from-x (/ node-width 2) 30)))
            (list
             ;; Start: bottom-center of source
             (list from-x from-y)

             ;; Waypoint 1: drop down from source
             (list from-x (+ from-y org-gtd-layout-waypoint-offset))

             ;; Waypoint 2: route right to detour
             (list detour-x (+ from-y org-gtd-layout-waypoint-offset))

             ;; Waypoint 3: drop parallel to column
             (list detour-x (- to-y org-gtd-layout-waypoint-offset))

             ;; Waypoint 4: route back to target column
             (list to-x (- to-y org-gtd-layout-waypoint-offset))

             ;; End: top-center of target
             (list to-x to-y)))

        ;; Different columns: standard horizontal routing
        (list
         ;; Start: bottom-center of source
         (list from-x from-y)

         ;; Waypoint 1: drop down from source
         (list from-x (+ from-y org-gtd-layout-waypoint-offset))

         ;; Waypoint 2: route horizontally to target column
         (list to-x (+ from-y org-gtd-layout-waypoint-offset))

         ;; Waypoint 3: drop to just above target
         (list to-x (- to-y org-gtd-layout-waypoint-offset))

         ;; End: top-center of target
         (list to-x to-y))))))

(defun org-gtd-layout--calculate-edge-paths (graph)
  "Calculate polyline paths for all edges in GRAPH.
Updates the points field of each edge with (x y) coordinate pairs.
Uses waypoints for multi-layer edges to route around intermediate nodes."
  (let ((edges (org-gtd-graph-edges graph)))
    (dolist (edge edges)
      (let* ((from-node (org-gtd-graph-data-get-node
                         graph (org-gtd-graph-edge-from-id edge)))
             (to-node (org-gtd-graph-data-get-node
                       graph (org-gtd-graph-edge-to-id edge))))

        (when (and from-node to-node)
          ;; Calculate waypoints (2 points for adjacent, 5 for multi-layer)
          (setf (org-gtd-graph-edge-points edge)
                (org-gtd-layout--calculate-edge-waypoints from-node to-node)))))))

;;;; Utility Functions

(defun org-gtd-layout-get-graph-bounds (graph)
  "Calculate bounding box of GRAPH after layout.
Returns (min-x min-y max-x max-y).
Considers both node positions and edge waypoints."
  (let ((min-x most-positive-fixnum)
        (min-y most-positive-fixnum)
        (max-x 0)
        (max-y 0)
        (nodes (org-gtd-graph-nodes graph))
        (edges (org-gtd-graph-edges graph)))

    ;; Consider node positions
    (maphash (lambda (_id node)
               (let ((x (org-gtd-graph-node-x node))
                     (y (org-gtd-graph-node-y node))
                     (w (org-gtd-graph-node-width node))
                     (h (org-gtd-graph-node-height node)))
                 (setq min-x (min min-x x)
                       min-y (min min-y y)
                       max-x (max max-x (+ x w))
                       max-y (max max-y (+ y h)))))
             nodes)

    ;; Also consider edge waypoints
    (dolist (edge edges)
      (when-let ((points (org-gtd-graph-edge-points edge)))
        (dolist (point points)
          (let ((px (car point))
                (py (cadr point)))
            (setq min-x (min min-x px)
                  min-y (min min-y py)
                  max-x (max max-x px)
                  max-y (max max-y py))))))

    (list min-x min-y max-x max-y)))

;;;; Footer

(provide 'org-gtd-layout)

;;; org-gtd-layout.el ends here
