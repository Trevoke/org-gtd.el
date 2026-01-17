;;; org-gtd-svg-render.el --- Visual styling helpers for org-gtd graphs -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni
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
;; This module provides helper functions for computing visual properties
;; of graph nodes based on GTD metadata (state, priority, tags).
;; These helpers are used by org-gtd-dag-draw to translate GTD data
;; into rendering attributes.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-graph-data)
(require 'org-gtd-core)  ; For org-gtd-keywords--* functions

;;;; Color Schemes

(defun org-gtd-svg--state-colors ()
  "Get color mapping for task states using keyword mapping.
Colors are from the Okabe-Ito palette, designed to be accessible
to people with color vision deficiency (color blindness)."
  `((,(org-gtd-keywords--todo) . "#E69F00")   ; Orange - blocked/waiting
    (,(org-gtd-keywords--next) . "#0072B2")   ; Blue - ready/actionable
    (,(org-gtd-keywords--done) . "#999999")   ; Grey - completed
    (,(org-gtd-keywords--wait) . "#F0E442")   ; Yellow - waiting for input
    ("HOLD" . "#CC79A7")                       ; Reddish purple - on hold
    (,(org-gtd-keywords--canceled) . "#999999") ; Grey - canceled
    (nil . "#56B4E9")))  ; Light blue - default for tasks with no state

(defconst org-gtd-svg--priority-colors
  '(("A" . "#e74c3c")
    ("B" . "#f39c12")
    ("C" . "#3498db"))
  "Color mapping for `org-mode' priorities.")

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

;;;; SVG String to Image Conversion

(defun org-gtd-svg-to-image (svg-string)
  "Convert SVG-STRING to Emacs image object for display.
SVG-STRING should be a complete SVG XML string."
  (create-image svg-string 'svg t))

;;;; Footer

(provide 'org-gtd-svg-render)

;;; org-gtd-svg-render.el ends here
