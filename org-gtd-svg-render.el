;;; org-gtd-svg-render.el --- SVG rendering for dependency graphs -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides SVG rendering functionality for project dependency
;; graphs. It uses Emacs' built-in svg.el library to draw nodes (tasks)
;; and edges (dependencies) in a hierarchical layout.
;;
;;; Code:

;;;; Requirements

(require 'svg)
(require 'dom)
(require 'color)
(require 'org-gtd-graph-data)
(require 'org-gtd-layout)

;;;; Customization

(defgroup org-gtd-svg-render nil
  "Customize SVG rendering for graph visualization."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-svg-node-corner-radius 5
  "Corner radius for node rectangles in pixels."
  :type 'integer
  :group 'org-gtd-svg-render)

(defcustom org-gtd-svg-font-family "sans-serif"
  "Font family for node text."
  :type 'string
  :group 'org-gtd-svg-render)

(defcustom org-gtd-svg-font-size 12
  "Font size for node text in pixels."
  :type 'integer
  :group 'org-gtd-svg-render)

(defcustom org-gtd-svg-edge-width 2
  "Width of dependency edges in pixels."
  :type 'integer
  :group 'org-gtd-svg-render)

(defcustom org-gtd-svg-arrow-size 8
  "Size of arrowheads in pixels."
  :type 'integer
  :group 'org-gtd-svg-render)

;;;; Color Schemes

(defun org-gtd-svg--state-colors ()
  "Get color mapping for task states using keyword mapping."
  `((,(org-gtd-keywords--todo) . "#ff6b6b")
    (,(org-gtd-keywords--next) . "#4ecdc4")
    (,(org-gtd-keywords--done) . "#95e1d3")
    (,(org-gtd-keywords--wait) . "#feca57")
    ("HOLD" . "#ee5a6f")
    (,(org-gtd-keywords--canceled) . "#ee5a6f")
    (nil . "#b8c5d0")))  ; Default for tasks with no state

(defconst org-gtd-svg--priority-colors
  '(("A" . "#e74c3c")
    ("B" . "#f39c12")
    ("C" . "#3498db"))
  "Color mapping for org-mode priorities.")

(defconst org-gtd-svg--tag-colors
  '(("urgent" . "#e74c3c")
    ("important" . "#f39c12")
    ("waiting" . "#f1c40f")
    ("someday" . "#3498db"))
  "Color mapping for specific tags.")

(defconst org-gtd-svg--default-color "#ecf0f1"
  "Default color for nodes without priority or recognized tags.")

(defconst org-gtd-svg--default-tag-color "#bdc3c7"
  "Default color for nodes with unrecognized tags.")

(defun org-gtd-svg--get-state-color (state)
  "Get color for STATE, or default if not found."
  (let ((colors (org-gtd-svg--state-colors)))
    (or (cdr (assoc state colors))
        (cdr (assoc nil colors)))))

(defun org-gtd-svg--get-node-color (priority tags)
  "Get fill color for node based on PRIORITY and TAGS.
Priority takes precedence over tags. Returns hex color string."
  (cond
   ;; Priority takes precedence
   ((and priority (assoc priority org-gtd-svg--priority-colors))
    (cdr (assoc priority org-gtd-svg--priority-colors)))
   ;; Check first tag
   ((and tags (listp tags) (car tags))
    (let ((first-tag (car tags)))
      (or (cdr (assoc first-tag org-gtd-svg--tag-colors))
          org-gtd-svg--default-tag-color)))
   ;; Default
   (t org-gtd-svg--default-color)))

(defun org-gtd-svg--get-node-opacity (state)
  "Get opacity for node based on TODO STATE.
Completed states (done or canceled) return \"0.5\", others return \"1.0\"."
  (if (or (string= state (org-gtd-keywords--done))
          (string= state (org-gtd-keywords--canceled)))
      "0.5"
    "1.0"))

(defun org-gtd-svg--format-tooltip (node)
  "Format tooltip text for NODE including all metadata."
  (let ((parts nil))
    ;; Task title
    (when (org-gtd-graph-node-title node)
      (push (org-gtd-graph-node-title node) parts))

    ;; TODO state
    (when (org-gtd-graph-node-state node)
      (push (format "State: %s" (org-gtd-graph-node-state node)) parts))

    ;; Priority
    (when (org-gtd-graph-node-priority node)
      (push (format "Priority: %s" (org-gtd-graph-node-priority node)) parts))

    ;; Tags
    (when-let ((tags (org-gtd-graph-node-tags node)))
      (when (and (listp tags) tags)
        (push (format "Tags: %s" (mapconcat #'identity tags ", ")) parts)))

    ;; Scheduled
    (when (org-gtd-graph-node-scheduled node)
      (push (format "Scheduled: %s" (org-gtd-graph-node-scheduled node)) parts))

    ;; Deadline
    (when (org-gtd-graph-node-deadline node)
      (push (format "Deadline: %s" (org-gtd-graph-node-deadline node)) parts))

    (mapconcat #'identity (nreverse parts) "\n")))

;;;; Main Rendering Functions

(defun org-gtd-svg-render-graph (graph &optional selected-node-id)
  "Render GRAPH as SVG and return the SVG object.
GRAPH must have layout already applied (nodes have x,y coordinates).
If SELECTED-NODE-ID is provided, that node will be highlighted."
  (let* ((bounds (org-gtd-layout-get-graph-bounds graph))
         (min-x (nth 0 bounds))
         (min-y (nth 1 bounds))
         (max-x (nth 2 bounds))
         (max-y (nth 3 bounds))
         (width (+ (- max-x min-x) (* 2 org-gtd-layout-margin)))
         (height (+ (- max-y min-y) (* 2 org-gtd-layout-margin)))
         (svg (svg-create width height)))

    ;; Add background
    (svg-rectangle svg 0 0 width height
                   :fill "#f8f9fa"
                   :stroke-width 0)

    ;; Add arrowhead marker definition
    (org-gtd-svg--add-arrow-marker svg)

    ;; Draw edges first (so they appear behind nodes)
    (dolist (edge (org-gtd-graph-edges graph))
      (org-gtd-svg-draw-edge svg edge graph))

    ;; Draw nodes on top
    (maphash (lambda (_id node)
               (org-gtd-svg-draw-node svg node selected-node-id))
             (org-gtd-graph-nodes graph))

    svg))

(defun org-gtd-svg-draw-node (svg node &optional selected-node-id)
  "Draw a task NODE on SVG canvas.
NODE is an org-gtd-graph-node structure.
If NODE's ID matches SELECTED-NODE-ID, draw with bold border."
  (let* ((x (org-gtd-graph-node-x node))
         (y (org-gtd-graph-node-y node))
         (width (org-gtd-graph-node-width node))
         (height (org-gtd-graph-node-height node))
         (title (or (org-gtd-graph-node-title node) "[Untitled]"))
         (state (org-gtd-graph-node-state node))
         (priority (org-gtd-graph-node-priority node))
         (tags (org-gtd-graph-node-tags node))
         (color (org-gtd-svg--get-node-color priority tags))
         (opacity (org-gtd-svg--get-node-opacity state))
         (text-color (org-gtd-svg--get-text-color color))
         (tooltip (org-gtd-svg--format-tooltip node))
         (is-selected (and selected-node-id
                           (string= (org-gtd-graph-node-id node) selected-node-id)))
         (stroke-width (if is-selected 4 2))
         (stroke-color (if is-selected "#e74c3c" "#2d3436")))

    ;; Create group for node with tooltip
    (let ((g (dom-node 'g)))
      ;; Add tooltip as title element
      (dom-append-child g (dom-node 'title nil tooltip))

      ;; Add node rectangle with opacity and color
      (dom-append-child g
                        (dom-node 'rect
                                  `((x . ,x)
                                    (y . ,y)
                                    (width . ,width)
                                    (height . ,height)
                                    (fill . ,color)
                                    (fill-opacity . ,opacity)
                                    (stroke . ,stroke-color)
                                    (stroke-width . ,stroke-width)
                                    (rx . ,org-gtd-svg-node-corner-radius)
                                    (ry . ,org-gtd-svg-node-corner-radius)
                                    (id . ,(org-gtd-graph-node-id node))
                                    (class . "node"))))

      ;; Add state badge if present
      (when state
        (let* ((badge-width 40)
               (badge-height 16)
               (badge-x (+ x 5))
               (badge-y (+ y 5)))
          (dom-append-child g
                            (dom-node 'rect
                                      `((x . ,badge-x)
                                        (y . ,badge-y)
                                        (width . ,badge-width)
                                        (height . ,badge-height)
                                        (fill . "#2d3436")
                                        (opacity . 0.7)
                                        (rx . 3)
                                        (ry . 3))))
          (dom-append-child g
                            (dom-node 'text
                                      `((x . ,(+ badge-x (/ badge-width 2)))
                                        (y . ,(+ badge-y (/ badge-height 2) 1))
                                        (font-family . ,org-gtd-svg-font-family)
                                        (font-size . "9")
                                        (fill . "#ffffff")
                                        (text-anchor . "middle")
                                        (dominant-baseline . "middle")
                                        (font-weight . "bold"))
                                      state))))

      ;; Add task title
      (let* ((text-x (+ x (/ width 2)))
             (text-y (+ y (/ height 2)))
             (max-title-length 15)
             (truncated-title (if (> (length title) max-title-length)
                                  (concat (substring title 0 max-title-length) "...")
                                title)))
        (dom-append-child g
                          (dom-node 'text
                                    `((x . ,text-x)
                                      (y . ,text-y)
                                      (font-family . ,org-gtd-svg-font-family)
                                      (font-size . ,(number-to-string org-gtd-svg-font-size))
                                      (fill . ,text-color)
                                      (text-anchor . "middle")
                                      (dominant-baseline . "middle")
                                      (class . "node-text"))
                                    truncated-title)))

      ;; Append group to svg
      (dom-append-child svg g))))

(defun org-gtd-svg-draw-edge (svg edge graph)
  "Draw a dependency EDGE on SVG canvas with waypoint routing.
EDGE is an org-gtd-graph-edge structure.
GRAPH is used to look up node information.
Uses polylines for multi-layer edges, straight lines for adjacent layers."
  (let* ((points (org-gtd-graph-edge-points edge))
         (from-node (org-gtd-graph-data-get-node
                     graph (org-gtd-graph-edge-from-id edge)))
         (to-node (org-gtd-graph-data-get-node
                   graph (org-gtd-graph-edge-to-id edge))))

    (when (and points from-node to-node)
      ;; Debug output
      (when (> (length points) 2)
        (message "Drawing polyline %s -> %s with %d points:"
                 (org-gtd-graph-node-title from-node)
                 (org-gtd-graph-node-title to-node)
                 (length points))
        (dolist (point points)
          (message "  (%d, %d)" (car point) (cadr point))))

      (if (= (length points) 2)
          ;; Straight line for adjacent layers (2 points)
          (let ((from-point (car points))
                (to-point (cadr points)))
            (svg-line svg
                      (car from-point) (cadr from-point)
                      (car to-point) (cadr to-point)
                      :stroke "#636e72"
                      :stroke-width org-gtd-svg-edge-width
                      :marker-end "url(#arrowhead)"
                      :class "edge"))

        ;; Polyline for multi-layer edges (3+ points)
        ;; NOTE: svg-polyline has a bug - it formats points incorrectly
        ;; We manually construct the points string in correct SVG format
        (let ((points-string
               (mapconcat (lambda (point)
                            (format "%d,%d" (car point) (cadr point)))
                          points
                          " ")))
          (dom-append-child
           svg
           (dom-node 'polyline
                     `((points . ,points-string)
                       (stroke . "#636e72")
                       (stroke-width . ,org-gtd-svg-edge-width)
                       (fill . "none")
                       (marker-end . "url(#arrowhead)")
                       (class . "edge")))))))))

;;;; Helper Functions

(defun org-gtd-svg--add-arrow-marker (svg)
  "Add arrowhead marker definition to SVG."
  (let ((defs (dom-node 'defs))
        (marker (dom-node 'marker
                          `((id . "arrowhead")
                            (markerWidth . ,(* 2 org-gtd-svg-arrow-size))
                            (markerHeight . ,(* 2 org-gtd-svg-arrow-size))
                            (refX . ,(1- org-gtd-svg-arrow-size))
                            (refY . ,org-gtd-svg-arrow-size)
                            (orient . "auto"))))
        (path (dom-node 'path
                        `((d . ,(format "M 0 0 L %d %d L 0 %d z"
                                        org-gtd-svg-arrow-size
                                        org-gtd-svg-arrow-size
                                        (* 2 org-gtd-svg-arrow-size)))
                          (fill . "#636e72")))))
    (dom-append-child marker path)
    (dom-append-child defs marker)
    (dom-append-child svg defs)))

(defun org-gtd-svg--get-text-color (bg-color)
  "Determine appropriate text color for BG-COLOR.
Returns white or black based on background luminance."
  (let* ((rgb (color-name-to-rgb bg-color))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         ;; Calculate relative luminance
         (luminance (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
    (if (> luminance 0.5) "#2d3436" "#ffffff")))

;;;; Export Functions

(defun org-gtd-svg-to-string (svg)
  "Convert SVG object to string representation."
  (with-temp-buffer
    (svg-print svg)
    (buffer-string)))

(defun org-gtd-svg-to-image (svg)
  "Convert SVG object to Emacs image object for display."
  (create-image (org-gtd-svg-to-string svg) 'svg t))

;;;; Footer

(provide 'org-gtd-svg-render)

;;; org-gtd-svg-render.el ends here
