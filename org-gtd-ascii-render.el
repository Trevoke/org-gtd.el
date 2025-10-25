;;; org-gtd-ascii-render.el --- ASCII rendering for dependency graphs -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides ASCII rendering functionality for project dependency
;; graphs. It uses Unicode box-drawing characters to draw nodes (tasks)
;; and edges (dependencies) in a hierarchical layout.
;;
;; The rendering process:
;; 1. Scale pixel coordinates to character grid coordinates
;; 2. Allocate 2D character grid
;; 3. Draw edges first (as background)
;; 4. Draw nodes on top (with boxes)
;; 5. Enhance junction characters (T-junctions, crossings)
;; 6. Place arrows at node boundaries
;;
;; Uses Unicode box-drawing characters:
;;   Nodes: ┌─┐ │ └─┘
;;   Lines: ─ │
;;   Corners: └ ┘ ┌ ┐
;;   T-junctions: ├ ┤ ┬ ┴
;;   Crossings: ┼
;;   Arrows: → ↓ ← ↑
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-gtd-graph-data)
(require 'org-gtd-layout)

;;;; Customization

(defgroup org-gtd-ascii-render nil
  "Customize ASCII rendering for graph visualization."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-ascii-target-width 80
  "Target width in columns for ASCII graph rendering.
The graph will be scaled to fit within this width.
Typical range: 60 (narrow) to 120 (wide)."
  :type 'integer
  :group 'org-gtd-ascii-render)

(defcustom org-gtd-ascii-target-height 40
  "Target height in rows for ASCII graph rendering.
The graph will be scaled to fit within this height.
Typical range: 30 (compact) to 60 (spacious)."
  :type 'integer
  :group 'org-gtd-ascii-render)

(defcustom org-gtd-ascii-margin 2
  "Margin around graph in character cells."
  :type 'integer
  :group 'org-gtd-ascii-render)

(defcustom org-gtd-ascii-min-node-width 12
  "Minimum width for node boxes in characters."
  :type 'integer
  :group 'org-gtd-ascii-render)

(defcustom org-gtd-ascii-node-padding 1
  "Padding inside node boxes in characters."
  :type 'integer
  :group 'org-gtd-ascii-render)

;;;; Grid Management

(cl-defstruct org-gtd-ascii-grid
  "2D character grid for ASCII rendering."
  (width 0 :type integer)
  (height 0 :type integer)
  (cells nil :type (vector (vector string))))

(defun org-gtd-ascii-grid-create (width height)
  "Create empty ASCII grid with WIDTH and HEIGHT."
  (let ((cells (make-vector height nil)))
    (dotimes (row height)
      (aset cells row (make-vector width " ")))
    (make-org-gtd-ascii-grid :width width :height height :cells cells)))

(defun org-gtd-ascii-grid-get (grid x y)
  "Get character at position (X, Y) in GRID.
Returns nil if out of bounds."
  (when (and (>= x 0) (< x (org-gtd-ascii-grid-width grid))
             (>= y 0) (< y (org-gtd-ascii-grid-height grid)))
    (aref (aref (org-gtd-ascii-grid-cells grid) y) x)))

(defun org-gtd-ascii-grid-set (grid x y char)
  "Set character at position (X, Y) in GRID to CHAR.
Returns nil if out of bounds."
  (when (and (>= x 0) (< x (org-gtd-ascii-grid-width grid))
             (>= y 0) (< y (org-gtd-ascii-grid-height grid)))
    (aset (aref (org-gtd-ascii-grid-cells grid) y) x char)))

(defun org-gtd-ascii-grid-to-string (grid)
  "Convert GRID to multi-line string."
  (let ((lines nil))
    (dotimes (row (org-gtd-ascii-grid-height grid))
      (push (mapconcat #'identity
                       (append (aref (org-gtd-ascii-grid-cells grid) row) nil)
                       "")
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; Coordinate Transformation

(defun org-gtd-ascii--create-transform (graph target-width target-height margin)
  "Create coordinate transformation context for GRAPH.
TARGET-WIDTH and TARGET-HEIGHT are the desired grid dimensions.
MARGIN is added around the graph.
Returns plist with :min-x :min-y :scale-x :scale-y for transforming coordinates."
  (let* ((bounds (org-gtd-layout-get-graph-bounds graph))
         (min-x (nth 0 bounds))
         (min-y (nth 1 bounds))
         (max-x (nth 2 bounds))
         (max-y (nth 3 bounds))
         (pixel-width (- max-x min-x))
         (pixel-height (- max-y min-y))
         ;; Calculate scale to fit graph into target dimensions
         ;; Scale = pixels per grid cell
         (scale-x (/ pixel-width (float (- target-width (* 2 margin)))))
         (scale-y (/ pixel-height (float (- target-height (* 2 margin))))))
    (list :min-x min-x :min-y min-y :scale-x scale-x :scale-y scale-y)))

(defun org-gtd-ascii--transform-point (pixel-x pixel-y transform margin)
  "Transform pixel coordinates (PIXEL-X PIXEL-Y) to grid coordinates.
TRANSFORM is the transformation context from `org-gtd-ascii--create-transform'.
MARGIN is added to the result.
Returns (grid-x grid-y) list."
  (let ((min-x (plist-get transform :min-x))
        (min-y (plist-get transform :min-y))
        (scale-x (plist-get transform :scale-x))
        (scale-y (plist-get transform :scale-y)))
    (list (+ margin (round (/ (- pixel-x min-x) scale-x)))
          (+ margin (round (/ (- pixel-y min-y) scale-y))))))

(defun org-gtd-ascii--calculate-grid-size (_graph)
  "Calculate grid dimensions for GRAPH.
Returns (width height) using target dimensions.
GRAPH parameter reserved for future auto-sizing based on complexity."
  (list org-gtd-ascii-target-width org-gtd-ascii-target-height))

;;;; Node Geometry Helpers

(defun org-gtd-ascii--calculate-node-width (node)
  "Calculate ASCII width for NODE based on title length.
Returns width in grid cells."
  (max org-gtd-ascii-min-node-width
       (+ 2 (* 2 org-gtd-ascii-node-padding)
          (length (org-gtd-graph-node-title node)))))

(defun org-gtd-ascii--calculate-node-center (node transform)
  "Calculate center position of NODE in grid space.
TRANSFORM is the transformation context.
Returns (grid-x grid-y) list representing the center of the ASCII node box."
  (let* ((pos (org-gtd-ascii--transform-point
               (org-gtd-graph-node-x node)
               (org-gtd-graph-node-y node)
               transform
               org-gtd-ascii-margin))
         (grid-x (car pos))
         (grid-y (cadr pos))
         (node-width (org-gtd-ascii--calculate-node-width node))
         (node-height 3))
    ;; Center is: top-left + half dimensions
    (list (+ grid-x (/ node-width 2))
          (+ grid-y (/ node-height 2)))))

(defun org-gtd-ascii--calculate-node-top-center (node transform)
  "Calculate top-center position of NODE in grid space.
TRANSFORM is the transformation context.
Returns (grid-x grid-y) for edge attachment point at top of node."
  (let* ((pos (org-gtd-ascii--transform-point
               (org-gtd-graph-node-x node)
               (org-gtd-graph-node-y node)
               transform
               org-gtd-ascii-margin))
         (grid-x (car pos))
         (grid-y (cadr pos))
         (node-width (org-gtd-ascii--calculate-node-width node)))
    (list (+ grid-x (/ node-width 2))
          grid-y)))  ; Top row of node

(defun org-gtd-ascii--calculate-node-bottom-center (node transform)
  "Calculate bottom-center position of NODE in grid space.
TRANSFORM is the transformation context.
Returns (grid-x grid-y) for edge attachment point at bottom of node."
  (let* ((pos (org-gtd-ascii--transform-point
               (org-gtd-graph-node-x node)
               (org-gtd-graph-node-y node)
               transform
               org-gtd-ascii-margin))
         (grid-x (car pos))
         (grid-y (cadr pos))
         (node-width (org-gtd-ascii--calculate-node-width node))
         (node-height 3))
    (list (+ grid-x (/ node-width 2))
          (+ grid-y node-height -1))))  ; Bottom row of node

;;;; Node Rendering

(defun org-gtd-ascii-draw-node (grid node selected-node-id transform)
  "Draw NODE on GRID using TRANSFORM for coordinate conversion.
If NODE's ID matches SELECTED-NODE-ID, use bold box characters.
TRANSFORM is the transformation context from `org-gtd-ascii--create-transform'."
  (let* ((pos (org-gtd-ascii--transform-point
               (org-gtd-graph-node-x node)
               (org-gtd-graph-node-y node)
               transform
               org-gtd-ascii-margin))
         (x (car pos))
         (y (cadr pos))
         (node-width (org-gtd-ascii--calculate-node-width node))
         (node-height 3)  ; Fixed height: top border, text, bottom border
         (is-selected (and selected-node-id
                          (string= (org-gtd-graph-node-id node) selected-node-id)))
         (title (org-gtd-ascii--truncate-title
                 (org-gtd-graph-node-title node)
                 (- node-width (* 2 org-gtd-ascii-node-padding) 2)))
         ;; Box drawing characters
         (h-line (if is-selected "═" "─"))
         (v-line (if is-selected "║" "│"))
         (tl-corner (if is-selected "╔" "┌"))
         (tr-corner (if is-selected "╗" "┐"))
         (bl-corner (if is-selected "╚" "└"))
         (br-corner (if is-selected "╝" "┘")))

    ;; Top border: ┌─────┐
    (org-gtd-ascii-grid-set grid x y tl-corner)
    (dotimes (i (- node-width 2))
      (org-gtd-ascii-grid-set grid (+ x i 1) y h-line))
    (org-gtd-ascii-grid-set grid (+ x node-width -1) y tr-corner)

    ;; Middle line: │ text │
    (org-gtd-ascii-grid-set grid x (+ y 1) v-line)
    (let ((text-x (+ x 1 org-gtd-ascii-node-padding))
          (text-y (+ y 1)))
      (dotimes (i (length title))
        (org-gtd-ascii-grid-set grid (+ text-x i) text-y
                                (substring title i (+ i 1))))
      ;; Fill remaining space with spaces
      (dotimes (i (- node-width 2))
        (when (>= i (+ org-gtd-ascii-node-padding (length title)))
          (org-gtd-ascii-grid-set grid (+ x i 1) text-y " "))))
    (org-gtd-ascii-grid-set grid (+ x node-width -1) (+ y 1) v-line)

    ;; Bottom border: └─────┘
    (org-gtd-ascii-grid-set grid x (+ y 2) bl-corner)
    (dotimes (i (- node-width 2))
      (org-gtd-ascii-grid-set grid (+ x i 1) (+ y 2) h-line))
    (org-gtd-ascii-grid-set grid (+ x node-width -1) (+ y 2) br-corner)

    ;; Store node bounds for collision detection
    (list x y (+ x node-width -1) (+ y node-height -1))))

(defun org-gtd-ascii--truncate-title (title max-length)
  "Truncate TITLE to MAX-LENGTH, adding ... if needed."
  (if (<= (length title) max-length)
      title
    (concat (substring title 0 (- max-length 3)) "...")))

;;;; Edge Rendering

(defun org-gtd-ascii-draw-edge (grid edge graph transform node-bounds)
  "Draw EDGE on GRID using orthogonal routing (ASCII-friendly).
GRAPH is used to look up from/to nodes.
TRANSFORM is the transformation context from `org-gtd-ascii--create-transform'.
NODE-BOUNDS is hash table mapping node-id to (x1 y1 x2 y2) bounds.

Uses simple orthogonal routing: vertical-horizontal-vertical paths."
  (let* ((points (org-gtd-graph-edge-points edge))
         (from-id (org-gtd-graph-edge-from-id edge))
         (to-id (org-gtd-graph-edge-to-id edge))
         (from-node (org-gtd-graph-data-get-node graph from-id))
         (to-node (org-gtd-graph-data-get-node graph to-id)))

    (when (and points from-node to-node)
      ;; CRITICAL: Use Sugiyama X coordinates to preserve column structure
      ;; Nodes in the same Sugiyama column must have the same routing X,
      ;; even though ASCII node widths (and thus centers) vary by title length.
      (let* ((from-pos (org-gtd-ascii--transform-point
                       (org-gtd-graph-node-x from-node)
                       (org-gtd-graph-node-y from-node)
                       transform org-gtd-ascii-margin))
             (to-pos (org-gtd-ascii--transform-point
                     (org-gtd-graph-node-x to-node)
                     (org-gtd-graph-node-y to-node)
                     transform org-gtd-ascii-margin))
             (from-x-grid (car from-pos))  ; Use Sugiyama column X
             (to-x-grid (car to-pos))      ; Use Sugiyama column X
             (from-y (+ (cadr from-pos) 2))  ; Bottom of from-node (height=3)
             (to-y (cadr to-pos)))           ; Top of to-node

        ;; Create orthogonal path
        ;; CRITICAL: Arrow must be placed ABOVE node (at to-y - 1) not AT node top (to-y)
        ;; Otherwise node box will draw over the arrow
        (let* ((arrow-y (- to-y 1))  ; One row above node top
               (orthogonal-path
                (if (= from-x-grid to-x-grid)
                    ;; Same column - check if we need to route around intermediate nodes
                    ;; If vertical span is large, offset to side so edge is visible
                    (let ((vertical-span (- to-y from-y)))
                      (if (> vertical-span 6)
                          ;; Long vertical edge - route to the side to avoid occlusion
                          (let ((offset-x (- from-x-grid 2)))  ; 2 chars left of column
                            (list (list from-x-grid from-y)     ; Start at source
                                  (list offset-x from-y)        ; Horizontal to offset
                                  (list offset-x arrow-y)       ; Down along offset
                                  (list to-x-grid arrow-y)))    ; Horizontal to dest
                        ;; Short vertical edge - straight line is fine
                        (list (list from-x-grid from-y)
                              (list to-x-grid arrow-y))))
                  ;; Different columns - L-shaped path
                  (let ((horiz-y (max (+ from-y 1)      ; At least 1 row below source
                                      (- to-y 2))))     ; At least 2 rows above node (room for arrow)
                    (list (list from-x-grid from-y)     ; Start at bottom of source
                          (list from-x-grid horiz-y)    ; Go down
                          (list to-x-grid horiz-y)      ; Route horizontally
                          (list to-x-grid arrow-y)))))) ; End above node

          ;; Draw line segments between consecutive points
          (cl-loop for (p1 p2) on orthogonal-path
                   while p2
                   do (org-gtd-ascii-draw-line grid
                                               (car p1) (cadr p1)
                                               (car p2) (cadr p2)
                                               node-bounds))

          ;; Place arrow at final point
          (let ((second-last (car (last orthogonal-path 2)))
                (last-point (car (last orthogonal-path))))
            (org-gtd-ascii-place-arrow grid
                                       (car second-last) (cadr second-last)
                                       (car last-point) (cadr last-point))))))))

(defun org-gtd-ascii-draw-line (grid x1 y1 x2 y2 node-bounds)
  "Draw line from (X1,Y1) to (X2,Y2) on GRID.
Edges are drawn behind nodes (nodes will overwrite them when rendered).
NODE-BOUNDS is kept for API compatibility but not used."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (cond
     ;; Horizontal line
     ((= dy 0)
      (let ((start-x (min x1 x2))
            (end-x (max x1 x2)))
        (cl-loop for x from start-x to end-x
                 do (org-gtd-ascii-grid-set grid x y1 "─"))))

     ;; Vertical line
     ((= dx 0)
      (let ((start-y (min y1 y2))
            (end-y (max y1 y2)))
        (cl-loop for y from start-y to end-y
                 do (org-gtd-ascii-grid-set grid x1 y "│"))))

     ;; Diagonal - break into horizontal + vertical
     (t
      (org-gtd-ascii-draw-line grid x1 y1 x2 y1 node-bounds)
      (org-gtd-ascii-draw-line grid x2 y1 x2 y2 node-bounds)))))

(defun org-gtd-ascii--inside-node-p (x y node-bounds)
  "Check if position (X,Y) is inside any node bounds.
NODE-BOUNDS is hash table mapping node-id to (x1 y1 x2 y2)."
  (catch 'found
    (maphash (lambda (_id bounds)
               (when (and (>= x (nth 0 bounds))
                         (<= x (nth 2 bounds))
                         (>= y (nth 1 bounds))
                         (<= y (nth 3 bounds)))
                 (throw 'found t)))
             node-bounds)
    nil))

(defun org-gtd-ascii-place-arrow (grid x1 y1 x2 y2)
  "Place arrow character at (X2,Y2) based on direction from (X1,Y1).
Arrow is placed at edge termination, pointing toward destination node."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (arrow (cond
                 ((> dx 0) "→")  ; Rightward
                 ((< dx 0) "←")  ; Leftward
                 ((> dy 0) "↓")  ; Downward
                 ((< dy 0) "↑")  ; Upward
                 (t "•"))))      ; Fallback (same point)
    ;; Place arrow at destination, but not if it's inside a node
    ;; (arrows should be at node boundaries)
    (org-gtd-ascii-grid-set grid x2 y2 arrow)))

;;;; Junction Enhancement

(defun org-gtd-ascii-enhance-junctions (grid node-bounds)
  "Enhance junction characters in GRID based on connectivity.
Analyzes 4-directional connectivity to choose correct box-drawing character.
NODE-BOUNDS is used to avoid modifying node interiors."
  (dotimes (y (org-gtd-ascii-grid-height grid))
    (dotimes (x (org-gtd-ascii-grid-width grid))
      (unless (org-gtd-ascii--inside-node-p x y node-bounds)
        (let ((char (org-gtd-ascii-grid-get grid x y)))
          (when (member char '("─" "│" "+" "*"))
            ;; Analyze connectivity in 4 directions
            (let* ((up (org-gtd-ascii--connects-p grid x (- y 1) node-bounds))
                   (down (org-gtd-ascii--connects-p grid x (+ y 1) node-bounds))
                   (left (org-gtd-ascii--connects-p grid (- x 1) y node-bounds))
                   (right (org-gtd-ascii--connects-p grid (+ x 1) y node-bounds))
                   (enhanced (org-gtd-ascii--select-junction-char
                              up down left right)))
              (org-gtd-ascii-grid-set grid x y enhanced))))))))

(defun org-gtd-ascii--connects-p (grid x y node-bounds)
  "Check if position (X,Y) has a connecting edge character.
Returns t if position contains edge character, nil otherwise."
  (when-let ((char (org-gtd-ascii-grid-get grid x y)))
    (and (not (org-gtd-ascii--inside-node-p x y node-bounds))
         (member char '("─" "│" "└" "┘" "┌" "┐" "├" "┤" "┬" "┴" "┼"
                       "→" "←" "↑" "↓" "+" "*")))))

(defun org-gtd-ascii--select-junction-char (up down left right)
  "Select appropriate junction character based on connectivity.
UP, DOWN, LEFT, RIGHT are booleans indicating connectivity in each direction."
  (let ((connections (+ (if up 1 0) (if down 1 0)
                       (if left 1 0) (if right 1 0))))
    (cond
     ;; 4-way crossing
     ((= connections 4) "┼")

     ;; 3-way T-junctions
     ((and up down left right) "┼")
     ((and up down left (not right)) "┤")
     ((and up down (not left) right) "├")
     ((and up (not down) left right) "┴")
     ((and (not up) down left right) "┬")

     ;; 2-way corners
     ((and up right) "└")
     ((and up left) "┘")
     ((and down right) "┌")
     ((and down left) "┐")

     ;; 2-way straight lines
     ((and left right) "─")
     ((and up down) "│")

     ;; 1-way (endpoints - shouldn't happen, but handle gracefully)
     ((or up down) "│")
     ((or left right) "─")

     ;; No connections
     (t "+"))))

;;;; Main Rendering Function

(defun org-gtd-ascii-render-graph (graph &optional selected-node-id)
  "Render GRAPH as ASCII art and return as string.
GRAPH must have layout already applied (nodes have x,y coordinates).
If SELECTED-NODE-ID is provided, that node will be highlighted."
  (let* ((grid-size (org-gtd-ascii--calculate-grid-size graph))
         (grid-width (car grid-size))
         (grid-height (cadr grid-size))
         (transform (org-gtd-ascii--create-transform
                     graph grid-width grid-height org-gtd-ascii-margin))
         (grid (org-gtd-ascii-grid-create grid-width grid-height))
         (node-bounds (make-hash-table :test 'equal)))

    ;; FIRST: Calculate node bounds in grid space (without drawing yet)
    (maphash (lambda (_id node)
               (let* ((pos (org-gtd-ascii--transform-point
                           (org-gtd-graph-node-x node)
                           (org-gtd-graph-node-y node)
                           transform org-gtd-ascii-margin))
                      (x (car pos))
                      (y (cadr pos))
                      (node-width (org-gtd-ascii--calculate-node-width node))
                      (node-height 3))
                 (puthash (org-gtd-graph-node-id node)
                          (list x y (+ x node-width -1) (+ y node-height -1))
                          node-bounds)))
             (org-gtd-graph-nodes graph))

    ;; SECOND: Draw edges using transform (avoiding node interiors)
    (dolist (edge (org-gtd-graph-edges graph))
      (let* ((from-id (org-gtd-graph-edge-from-id edge))
             (to-id (org-gtd-graph-edge-to-id edge))
             (from-node (org-gtd-graph-data-get-node graph from-id))
             (to-node (org-gtd-graph-data-get-node graph to-id)))
        (when (and from-node to-node)
          (message "Drawing edge: %s → %s"
                   (org-gtd-graph-node-title from-node)
                   (org-gtd-graph-node-title to-node))))
      (org-gtd-ascii-draw-edge grid edge graph transform node-bounds))

    ;; THIRD: Enhance junction characters
    (org-gtd-ascii-enhance-junctions grid node-bounds)

    ;; FOURTH: Draw nodes on top using transform
    (maphash (lambda (_id node)
               (org-gtd-ascii-draw-node grid node selected-node-id transform))
             (org-gtd-graph-nodes graph))

    ;; Convert grid to string
    (org-gtd-ascii-grid-to-string grid)))

;;;; Footer

(provide 'org-gtd-ascii-render)

;;; org-gtd-ascii-render.el ends here
