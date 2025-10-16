;;; org-gtd-graph-navigation.el --- Keyboard navigation for graph views -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides keyboard-based navigation commands for graph
;; visualization buffers. Users can navigate between nodes using various
;; strategies: sequential traversal, layer-based movement, dependency
;; chains, and completion-based selection.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)

;;;; Traversal Order Calculation

(defun org-gtd-graph-nav--get-traversal-order ()
  "Get list of node IDs in breadth-first traversal order.
Caches result in buffer-local variable `org-gtd-graph-ui--traversal-order'."
  (or org-gtd-graph-ui--traversal-order
      (setq org-gtd-graph-ui--traversal-order
            (let ((result '())
                  (visited (make-hash-table :test 'equal))
                  (queue (copy-sequence (org-gtd-graph-root-ids org-gtd-graph-view--graph))))
              (while queue
                (let ((node-id (pop queue)))
                  (unless (gethash node-id visited)
                    (puthash node-id t visited)
                    (push node-id result)
                    (dolist (successor (org-gtd-graph-data-get-successors
                                        org-gtd-graph-view--graph node-id))
                      (setq queue (append queue (list successor)))))))
              (nreverse result)))))

;;;; Sequential Navigation

(defun org-gtd-graph-nav-next ()
  "Move to first child (successor) in DAG structure."
  (interactive)
  (when org-gtd-graph-ui--selected-node-id
    (let ((successors (org-gtd-graph-data-get-successors
                       org-gtd-graph-view--graph
                       org-gtd-graph-ui--selected-node-id)))
      (if successors
          (progn
            (setq org-gtd-graph-ui--selected-node-id (car successors))
            (org-gtd-graph-ui-update-details)
            (org-gtd-graph-view-refresh))
        (message "No child nodes")))))

(defun org-gtd-graph-nav-previous ()
  "Move to parent (predecessor) in DAG structure.
Prefers non-project parents over the project heading."
  (interactive)
  (when org-gtd-graph-ui--selected-node-id
    (let* ((predecessors (org-gtd-graph-data-get-predecessors
                          org-gtd-graph-view--graph
                          org-gtd-graph-ui--selected-node-id))
           (project-id (org-gtd-graph-project-id org-gtd-graph-view--graph))
           ;; Prefer non-project predecessors
           (non-project-preds (cl-remove project-id predecessors :test 'equal))
           (chosen-pred (or (car non-project-preds) (car predecessors))))
      (if chosen-pred
          (progn
            (setq org-gtd-graph-ui--selected-node-id chosen-pred)
            (org-gtd-graph-ui-update-details)
            (org-gtd-graph-view-refresh))
        (message "No parent node")))))

;;;; Layer-Based Navigation

(defun org-gtd-graph-nav--get-current-layer ()
  "Get layer number of currently selected node."
  (when org-gtd-graph-ui--selected-node-id
    (when-let ((node (org-gtd-graph-data-get-node
                      org-gtd-graph-view--graph
                      org-gtd-graph-ui--selected-node-id)))
      (org-gtd-graph-node-layer node))))

(defun org-gtd-graph-nav--get-nodes-in-layer (layer-num)
  "Get list of node IDs in layer LAYER-NUM in traversal order."
  (let ((order (org-gtd-graph-nav--get-traversal-order))
        (result '()))
    (dolist (node-id order)
      (when-let ((node (org-gtd-graph-data-get-node
                        org-gtd-graph-view--graph node-id)))
        (when (= (org-gtd-graph-node-layer node) layer-num)
          (push node-id result))))
    (nreverse result)))

(defun org-gtd-graph-nav-next-sibling ()
  "Move to next sibling node (same layer)."
  (interactive)
  (when-let* ((current-layer (org-gtd-graph-nav--get-current-layer))
              (layer-nodes (org-gtd-graph-nav--get-nodes-in-layer current-layer))
              ((> (length layer-nodes) 1)))
    (let* ((current-pos (cl-position org-gtd-graph-ui--selected-node-id layer-nodes :test 'equal))
           (next-pos (1+ current-pos)))
      (if (< next-pos (length layer-nodes))
          (let ((next-node-id (nth next-pos layer-nodes)))
            (setq org-gtd-graph-ui--selected-node-id next-node-id)
            (org-gtd-graph-ui-update-details)
            (org-gtd-graph-view-refresh))
        (message "No next sibling")))))

(defun org-gtd-graph-nav-previous-sibling ()
  "Move to previous sibling node (same layer)."
  (interactive)
  (when-let* ((current-layer (org-gtd-graph-nav--get-current-layer))
              (layer-nodes (org-gtd-graph-nav--get-nodes-in-layer current-layer))
              ((> (length layer-nodes) 1)))
    (let* ((current-pos (cl-position org-gtd-graph-ui--selected-node-id layer-nodes :test 'equal))
           (prev-pos (1- current-pos)))
      (if (>= prev-pos 0)
          (let ((prev-node-id (nth prev-pos layer-nodes)))
            (setq org-gtd-graph-ui--selected-node-id prev-node-id)
            (org-gtd-graph-ui-update-details)
            (org-gtd-graph-view-refresh))
        (message "No previous sibling")))))

(defun org-gtd-graph-nav-first-in-layer ()
  "Jump to first node in current layer."
  (interactive)
  (when-let* ((current-layer (org-gtd-graph-nav--get-current-layer))
              (layer-nodes (org-gtd-graph-nav--get-nodes-in-layer current-layer))
              (first-node-id (car layer-nodes)))
    (setq org-gtd-graph-ui--selected-node-id first-node-id)
    (org-gtd-graph-ui-update-details)
    (org-gtd-graph-view-refresh)))

(defun org-gtd-graph-nav-last-in-layer ()
  "Jump to last node in current layer."
  (interactive)
  (when-let* ((current-layer (org-gtd-graph-nav--get-current-layer))
              (layer-nodes (org-gtd-graph-nav--get-nodes-in-layer current-layer))
              (last-node-id (car (last layer-nodes))))
    (setq org-gtd-graph-ui--selected-node-id last-node-id)
    (org-gtd-graph-ui-update-details)
    (org-gtd-graph-view-refresh)))

;;;; Dependency-Based Navigation

(defun org-gtd-graph-nav-down-dependency ()
  "Navigate down the dependency chain (to blocked tasks)."
  (interactive)
  (when org-gtd-graph-ui--selected-node-id
    (let ((successors (org-gtd-graph-data-get-successors
                       org-gtd-graph-view--graph
                       org-gtd-graph-ui--selected-node-id)))
      (when successors
        (setq org-gtd-graph-ui--selected-node-id (car successors))
        (org-gtd-graph-ui-update-details)
        (org-gtd-graph-view-refresh)))))

(defun org-gtd-graph-nav-up-dependency ()
  "Navigate up the dependency chain (to blocking tasks).
Prefers non-project parents over the project heading."
  (interactive)
  (when org-gtd-graph-ui--selected-node-id
    (let* ((predecessors (org-gtd-graph-data-get-predecessors
                          org-gtd-graph-view--graph
                          org-gtd-graph-ui--selected-node-id))
           (project-id (org-gtd-graph-project-id org-gtd-graph-view--graph))
           ;; Prefer non-project predecessors
           (non-project-preds (cl-remove project-id predecessors :test 'equal))
           (chosen-pred (or (car non-project-preds) (car predecessors))))
      (when chosen-pred
        (setq org-gtd-graph-ui--selected-node-id chosen-pred)
        (org-gtd-graph-ui-update-details)
        (org-gtd-graph-view-refresh)))))

;;;; Goto Navigation

(defun org-gtd-graph-nav-goto ()
  "Go to node by name with completion."
  (interactive)
  (when org-gtd-graph-view--graph
    (let* ((nodes (org-gtd-graph-nodes org-gtd-graph-view--graph))
           (node-alist (let (result)
                         (maphash (lambda (_id node)
                                    (push (cons (org-gtd-graph-node-title node)
                                                (org-gtd-graph-node-id node))
                                          result))
                                  nodes)
                         result))
           (title (completing-read "Go to node: " node-alist nil t))
           (node-id (cdr (assoc title node-alist))))
      (when node-id
        (setq org-gtd-graph-ui--selected-node-id node-id)
        (org-gtd-graph-ui-update-details)
        (org-gtd-graph-view-refresh)))))

;;;; Footer

(provide 'org-gtd-graph-navigation)

;;; org-gtd-graph-navigation.el ends here
