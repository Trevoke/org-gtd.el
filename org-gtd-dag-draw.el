;;; org-gtd-dag-draw.el --- dag-draw integration for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Translation layer between org-gtd graph data structures and dag-draw.
;; Converts org-gtd's domain-specific graph representation into dag-draw's
;; format, computing visual properties (colors, opacity, markers, tooltips)
;; from GTD metadata.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-graph-data)
(require 'org-gtd-svg-render)  ; For color/opacity/tooltip functions
(require 'org-gtd-core)  ; For org-gtd-keywords--* functions

;;;; Visual Property Computation

(defun org-gtd-dag-draw--compute-ascii-marker (node)
  "Compute ASCII marker prefix for NODE based on TODO state.
Returns string like \"✓ \" for DONE, \"→ \" for NEXT, etc."
  (let ((state (org-gtd-graph-node-state node)))
    (cond
     ((string= state (org-gtd-keywords--done)) "✓ ")
     ((string= state (org-gtd-keywords--canceled)) "✗ ")
     ((string= state (org-gtd-keywords--next)) "→ ")
     ((string= state (org-gtd-keywords--wait)) "⏸ ")
     ((string= state (org-gtd-keywords--todo)) "○ ")
     (t ""))))

(defun org-gtd-dag-draw--compute-stroke-color (node selected-node-id)
  "Compute SVG stroke color for NODE.
SELECTED-NODE-ID determines if this node should be highlighted."
  (if (string= (org-gtd-graph-node-id node) selected-node-id)
      "#e74c3c"  ; Highlight color for selected node
    "#2d3436"))  ; Default stroke color

(defun org-gtd-dag-draw--compute-stroke-width (node selected-node-id)
  "Compute SVG stroke width for NODE.
SELECTED-NODE-ID determines if this node should be highlighted."
  (if (string= (org-gtd-graph-node-id node) selected-node-id)
      4  ; Thicker border for selected
    2))  ; Normal border

;;;; Main Translation Function

(defun org-gtd-dag-draw-translate (org-gtd-graph &optional selected-node-id)
  "Translate ORG-GTD-GRAPH to dag-draw spec format.
Returns plist with :nodes and :edges suitable for dag-draw.

SELECTED-NODE-ID, if provided, determines node selection."
  (let (nodes edges)
    ;; Translate nodes
    (maphash
     (lambda (id node)
       (push (list (intern id)  ; dag-draw expects symbol IDs
                   :label (org-gtd-graph-node-title node)
                   ;; SVG visual properties
                   :svg-fill (org-gtd-svg--get-node-color
                             (org-gtd-graph-node-priority node)
                             (org-gtd-graph-node-tags node))
                   :svg-fill-opacity (org-gtd-svg--get-node-opacity
                                     (org-gtd-graph-node-state node))
                   :svg-stroke (org-gtd-dag-draw--compute-stroke-color
                               node selected-node-id)
                   :svg-stroke-width (org-gtd-dag-draw--compute-stroke-width
                                     node selected-node-id)
                   :svg-tooltip (org-gtd-svg--format-tooltip node)
                   ;; ASCII visual properties
                   :ascii-marker (org-gtd-dag-draw--compute-ascii-marker node)
                   :ascii-highlight (and selected-node-id
                                        (string= id selected-node-id)))
             nodes))
     (org-gtd-graph-nodes org-gtd-graph))

    ;; Translate edges
    (dolist (edge (org-gtd-graph-edges org-gtd-graph))
      (push (list (intern (org-gtd-graph-edge-from-id edge))
                  (intern (org-gtd-graph-edge-to-id edge)))
            edges))

    (list :nodes (nreverse nodes)
          :edges (nreverse edges))))

;;;; High-level Rendering Functions

(defun org-gtd-dag-draw-render (org-gtd-graph format &optional selected-node-id)
  "Render ORG-GTD-GRAPH using dag-draw in FORMAT.
FORMAT should be \\='svg or \\='ascii.
SELECTED-NODE-ID, if provided, highlights that node.

Returns rendered string (SVG XML or ASCII art)."
  (require 'dag-draw)
  (declare-function dag-draw-create-from-spec "dag-draw")
  (declare-function dag-draw-layout-graph "dag-draw")
  (declare-function dag-draw-render-graph "dag-draw")
  (let* ((spec (org-gtd-dag-draw-translate org-gtd-graph selected-node-id))
         (dd-graph (apply #'dag-draw-create-from-spec spec)))
    (dag-draw-layout-graph dd-graph)
    (dag-draw-render-graph dd-graph format)))

;;;; Footer

(provide 'org-gtd-dag-draw)

;;; org-gtd-dag-draw.el ends here
