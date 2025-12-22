;;; org-gtd-graph-ui.el --- Split-window layout for graph visualization -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides a split-window layout for graph visualization
;; with a dedicated task details panel. It creates a magit-style interface
;; where the main window shows the SVG graph and a side window displays
;; detailed information about the selected node.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-task-management)
(require 'org-gtd-graph-data)

;; Forward declaration to avoid circular dependency with org-gtd-graph-view
(declare-function org-gtd-graph-view-refresh "org-gtd-graph-view")

;;;; Customization

(defcustom org-gtd-graph-ui-split-ratio 0.7
  "Width ratio for graph window (0.0 to 1.0).
0.7 means graph takes 70% of width, details take 30%."
  :type 'float
  :group 'org-gtd)

;;;; Variables

(defvar-local org-gtd-graph-ui--selected-node-id nil
  "ID of currently selected node in graph.")

(defvar-local org-gtd-graph-ui--details-buffer nil
  "Buffer displaying task details.")

(defvar-local org-gtd-graph-ui--traversal-order nil
  "Cached list of node IDs in breadth-first traversal order.")

;;;; Window Management

(defun org-gtd-graph-ui-setup-windows (graph-buffer)
  "Setup split-window layout for GRAPH-BUFFER.
Creates side window for task details panel.
Returns (graph-window . details-window) cons cell."
  (let* ((graph-window (get-buffer-window graph-buffer))
         (total-width (window-body-width graph-window))
         (graph-width (floor (* total-width org-gtd-graph-ui-split-ratio)))
         (details-width (- total-width graph-width))
         ;; Use split-window instead of split-window-right for Emacs 28 compatibility
         ;; (split-window-right WINDOW arg was added in Emacs 29)
         (details-window (split-window graph-window (- details-width) 'right))
         (details-buffer (get-buffer-create
                          (format "*GTD Graph Details: %s*"
                                  (buffer-name graph-buffer)))))

    ;; Set up details window
    (set-window-buffer details-window details-buffer)
    (set-window-dedicated-p details-window t)
    (set-window-parameter details-window 'no-delete-other-windows t)

    ;; Make windows atomic so they behave as a single unit
    ;; Only if parent is not the root window (which would cause errors)
    (when-let ((parent-window (window-parent graph-window)))
      (unless (eq parent-window (frame-root-window))
        (window-make-atom parent-window)))

    ;; Store reference to details buffer in graph buffer
    (with-current-buffer graph-buffer
      (setq org-gtd-graph-ui--details-buffer details-buffer))

    ;; Return window pair
    (cons graph-window details-window)))

(defun org-gtd-graph-ui-get-windows ()
  "Get the graph and details windows for current buffer.
Returns (graph-window . details-window) or nil if not in split layout."
  (when org-gtd-graph-ui--details-buffer
    (let ((graph-window (get-buffer-window (current-buffer)))
          (details-window (get-buffer-window org-gtd-graph-ui--details-buffer)))
      (when (and graph-window details-window)
        (cons graph-window details-window)))))

(defun org-gtd-graph-ui-cleanup-windows ()
  "Clean up the split-window layout.
Closes details window and restores single-window state."
  (when org-gtd-graph-ui--details-buffer
    (let ((details-window (get-buffer-window org-gtd-graph-ui--details-buffer)))
      (when (window-live-p details-window)
        (delete-window details-window)))
    (when (buffer-live-p org-gtd-graph-ui--details-buffer)
      (kill-buffer org-gtd-graph-ui--details-buffer))
    (setq org-gtd-graph-ui--details-buffer nil
          org-gtd-graph-ui--selected-node-id nil)))

;;;; Task Details Panel

(defun org-gtd-graph-ui-select-node (node-id)
  "Select NODE-ID and update details panel.
Updates the details buffer to show task information."
  (setq org-gtd-graph-ui--selected-node-id node-id)
  (org-gtd-graph-ui-update-details)
  ;; Refresh graph to show selection highlight
  (when (fboundp 'org-gtd-graph-view-refresh)
    (org-gtd-graph-view-refresh)))

(defun org-gtd-graph-ui-update-details ()
  "Update details panel with info from selected node.
Shows: heading, TODO state, scheduled/deadline, properties, body text.
When viewing in context of a project graph, separates same-project
and cross-project dependencies."
  (when (and org-gtd-graph-ui--selected-node-id
             org-gtd-graph-ui--details-buffer
             (buffer-live-p org-gtd-graph-ui--details-buffer))
    (let ((details-text (org-gtd-graph-ui--format-task-details
                         org-gtd-graph-ui--selected-node-id
                         (when (boundp 'org-gtd-graph-view--graph)
                           org-gtd-graph-view--graph))))
      (with-current-buffer org-gtd-graph-ui--details-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert details-text)
          (goto-char (point-min))
          (org-mode)
          ;; Set up local keymap for RET to jump to task
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (current-local-map))
            (define-key map (kbd "RET") #'org-gtd-graph-ui-jump-to-task)
            (use-local-map map))
          (setq buffer-read-only t))))))

(defun org-gtd-graph-ui--format-task-details (node-id &optional graph)
  "Format task details for NODE-ID as a string.
When GRAPH is provided, separates dependencies into same-project
and cross-project sections based on whether IDs exist in the graph nodes.
Returns formatted text suitable for display in details buffer."
  (let ((marker (org-id-find node-id t)))
    (if (not marker)
        (format "* Node: %s\n\nNo org entry found for this node.\n\n[This may be an external task]" node-id)
      (org-with-point-at marker
        (let* ((heading (org-get-heading t t t t))
               (todo-state (org-get-todo-state))
               (scheduled (org-entry-get nil "SCHEDULED"))
               (deadline (org-entry-get nil "DEADLINE"))
               (file (buffer-file-name))
               (depends-on (org-entry-get-multivalued-property nil "ORG_GTD_DEPENDS_ON"))
               (blocks (org-entry-get-multivalued-property nil "ORG_GTD_BLOCKS"))
               (first-tasks (org-entry-get-multivalued-property nil "ORG_GTD_FIRST_TASKS"))
               (body (org-gtd-graph-ui--get-body-text))
               ;; Partition dependencies into same-project and cross-project
               (nodes (when graph (org-gtd-graph-nodes graph)))
               (depends-same-project nil)
               (depends-other-projects nil)
               (blocks-same-project nil)
               (blocks-other-projects nil))
          ;; Partition depends-on
          (if nodes
              (dolist (id depends-on)
                (if (gethash id nodes)
                    (push id depends-same-project)
                  (push id depends-other-projects)))
            (setq depends-same-project depends-on))
          (setq depends-same-project (nreverse depends-same-project))
          (setq depends-other-projects (nreverse depends-other-projects))
          ;; Partition blocks
          (if nodes
              (dolist (id blocks)
                (if (gethash id nodes)
                    (push id blocks-same-project)
                  (push id blocks-other-projects)))
            (setq blocks-same-project blocks))
          (setq blocks-same-project (nreverse blocks-same-project))
          (setq blocks-other-projects (nreverse blocks-other-projects))

          (concat
           (format "* %s" heading)
           (when todo-state (format " [%s]" todo-state))
           "\n\n"
           (when file (format "File: %s\n\n" file))
           (when scheduled (format "SCHEDULED: %s\n" scheduled))
           (when deadline (format "DEADLINE: %s\n" deadline))
           (when (or scheduled deadline) "\n")
           (when first-tasks
             (concat "First tasks:\n"
                     (mapconcat (lambda (id)
                                  (format "  - %s"
                                          (org-gtd-task-management--get-heading-for-id id)))
                                first-tasks
                                "\n")
                     "\n\n"))
           (when depends-same-project
             (concat "Blocked by:\n"
                     (mapconcat (lambda (id)
                                  (format "  - %s"
                                          (org-gtd-task-management--get-heading-for-id id)))
                                depends-same-project
                                "\n")
                     "\n\n"))
           (when depends-other-projects
             (concat "Blocked by (other projects):\n"
                     (mapconcat (lambda (id)
                                  (format "  - %s"
                                          (org-gtd-task-management--get-heading-for-id id)))
                                depends-other-projects
                                "\n")
                     "\n\n"))
           (when blocks-same-project
             (concat "Blocks:\n"
                     (mapconcat (lambda (id)
                                  (format "  - %s"
                                          (org-gtd-task-management--get-heading-for-id id)))
                                blocks-same-project
                                "\n")
                     "\n\n"))
           (when blocks-other-projects
             (concat "Blocks (other projects):\n"
                     (mapconcat (lambda (id)
                                  (format "  - %s"
                                          (org-gtd-task-management--get-heading-for-id id)))
                                blocks-other-projects
                                "\n")
                     "\n\n"))
           (when (and body (not (string-empty-p (string-trim body))))
             (concat body "\n\n"))
           "[Press RET to jump to task in org file]"))))))

(defun org-gtd-graph-ui--get-body-text ()
  "Get body text of current entry, excluding property drawer and child headings."
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (content-begin (org-element-property :contents-begin element))
           (content-end (org-element-property :contents-end element)))
      (when (and content-begin content-end)
        (let ((content (buffer-substring-no-properties content-begin content-end)))
          ;; Remove property drawer (handle multiline with [\s\S] which matches any char including newlines)
          (setq content (replace-regexp-in-string
                         "^[ \t]*:PROPERTIES:[[:ascii:][:nonascii:]]*?:END:[ \t]*\n"
                         ""
                         content))
          ;; Remove child headings and everything after them
          (when (string-match "^\\*+" content)
            (setq content (substring content 0 (match-beginning 0))))
          (string-trim content))))))

(defun org-gtd-graph-ui-jump-to-task ()
  "Jump to the currently selected task in its org file."
  (interactive)
  (if (not org-gtd-graph-ui--selected-node-id)
      (message "No task selected")
    (let ((marker (org-id-find org-gtd-graph-ui--selected-node-id t)))
      (if (not marker)
          (message "Cannot find task with ID: %s" org-gtd-graph-ui--selected-node-id)
        (pop-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        (org-fold-show-context)
        (message "Jumped to task: %s"
                 (org-get-heading t t t t))))))

;;;; Footer

(provide 'org-gtd-graph-ui)

;;; org-gtd-graph-ui.el ends here
