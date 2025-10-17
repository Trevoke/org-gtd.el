;;; org-gtd-graph-view.el --- Interactive graph visualization buffer -*- lexical-binding: t; coding: utf-8 -*-
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
;; This module provides an interactive buffer for visualizing and
;; manipulating project dependency graphs. It displays the graph
;; as an SVG image with clickable regions for interaction.
;;
;;; Code:

;;;; Requirements

(require 'filenotify)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-filter)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-undo)
(require 'org-gtd-layout)
(require 'org-gtd-svg-render)
(require 'org-gtd-projects)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-id)
(require 'org-gtd-configure)
(require 'org-gtd-files)

;;;; Variables

(defvar-local org-gtd-graph-view--graph nil
  "The graph structure displayed in this buffer.")

(defvar-local org-gtd-graph-view--project-marker nil
  "Marker to the project heading for this graph.")

(defvar-local org-gtd-graph-view--filter nil
  "Currently active filter for this graph buffer.")

(defvar-local org-gtd-graph-view--image-map nil
  "Image map for clickable regions in the graph.")

(defvar-local org-gtd-graph-view--file-watch-descriptor nil
  "File watch descriptor for auto-refresh on file changes.")

(defvar-local org-gtd-graph-view--refresh-timer nil
  "Timer for debounced refresh after file changes.")

;;;; Buffer Management

(defun org-gtd-graph-view--buffer-name (project-id)
  "Generate buffer name for PROJECT-ID graph view."
  (format "*Org GTD Graph: %s*" project-id))

(defun org-gtd-graph-view-create (project-marker)
  "Create or update graph view for project at PROJECT-MARKER."
  (let* ((project-id (org-with-point-at project-marker
                       (org-gtd-id-get-create)))
         (buffer-name (org-gtd-graph-view--buffer-name project-id))
         (buffer (get-buffer-create buffer-name)))

    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (org-gtd-graph-view-refresh)

      ;; Auto-select project heading as initial node
      (when-let* ((graph org-gtd-graph-view--graph)
                  (project-id (org-gtd-graph-project-id graph)))
        (org-gtd-graph-ui-select-node project-id t))  ; t = no history

      (org-gtd-graph-view--setup-file-watch)
      (add-hook 'kill-buffer-hook #'org-gtd-graph-view--cleanup-file-watch nil t)
      (add-hook 'kill-buffer-hook #'org-gtd-graph-ui-cleanup-windows nil t))

    (switch-to-buffer buffer)
    ;; Setup split-window layout after buffer is displayed
    (org-gtd-graph-ui-setup-windows buffer)
    buffer))

(defun org-gtd-graph-view--setup-file-watch ()
  "Setup file watching for auto-refresh on changes."
  (when org-gtd-graph-view--project-marker
    (let ((file-path (buffer-file-name
                      (marker-buffer org-gtd-graph-view--project-marker))))
      (when (and file-path (file-exists-p file-path))
        ;; Remove existing watch if any
        (org-gtd-graph-view--cleanup-file-watch)

        ;; Setup new watch
        (setq org-gtd-graph-view--file-watch-descriptor
              (file-notify-add-watch
               file-path
               '(change)
               #'org-gtd-graph-view--file-changed-callback))))))

(defun org-gtd-graph-view--cleanup-file-watch ()
  "Remove file watch descriptor."
  (when org-gtd-graph-view--file-watch-descriptor
    (file-notify-rm-watch org-gtd-graph-view--file-watch-descriptor)
    (setq org-gtd-graph-view--file-watch-descriptor nil)))

(defun org-gtd-graph-view--file-changed-callback (_event)
  "Handle file change with debounced refresh.
_EVENT is the file-notify event (unused)."
  (when (buffer-live-p (current-buffer))
    ;; Cancel existing timer
    (when org-gtd-graph-view--refresh-timer
      (cancel-timer org-gtd-graph-view--refresh-timer))

    ;; Setup new debounced refresh (300ms delay)
    (setq org-gtd-graph-view--refresh-timer
          (run-with-timer 0.3 nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (org-gtd-graph-view-refresh))))
                          (current-buffer)))))

(defun org-gtd-graph-view-refresh ()
  "Refresh the graph display in the current buffer."
  (interactive)
  (when org-gtd-graph-view--project-marker
    (let* ((full-graph (org-gtd-graph-data--extract-from-project
                        org-gtd-graph-view--project-marker))
           (issues (org-gtd-graph-data-validate full-graph))
           ;; Apply filter if active, otherwise use full graph
           (graph (if org-gtd-graph-view--filter
                      (org-gtd-graph-filter-create-filtered-graph
                       full-graph org-gtd-graph-view--filter)
                    full-graph)))

      ;; Warn about validation issues
      (when issues
        (message "Graph validation issues: %s"
                 (mapconcat #'identity issues "; ")))

      ;; Apply layout and render
      (org-gtd-layout-apply graph)
      (let ((svg (org-gtd-svg-render-graph graph org-gtd-graph-ui--selected-node-id)))
        ;; Store both the full graph and the displayed (possibly filtered) graph
        (setq org-gtd-graph-view--graph full-graph)
        (org-gtd-graph-view--display-svg svg graph)))))

(defun org-gtd-graph-view--display-svg (svg displayed-graph)
  "Display SVG in the current buffer showing DISPLAYED-GRAPH."
  (let ((inhibit-read-only t)
        (image (org-gtd-svg-to-image svg)))
    (erase-buffer)

    ;; Insert centered image
    (let ((window-width (window-body-width nil t))
          (image-width (car (image-size image t))))
      (when (> window-width image-width)
        (insert (make-string (/ (- window-width image-width) 2) ?\s))))

    (insert-image image)
    (insert "\n\n")

    ;; Insert legend
    (insert (propertize "Graph View Controls:\n" 'face 'bold))
    (insert "  Quick Keys:      ? - Show all commands (transient menu)\n")
    (insert "  Navigation:      n/p/j/k - Down/up dependency | TAB - Next sibling | g - Go to node\n")
    (insert "  View:            z - Zoom\n")
    (insert "  Actions:         r - Refresh | u - Undo | C-r - Redo | q - Quit\n")
    (insert "\n")
    (insert "  Press ? to see all commands including: add child/root/blocker tasks\n")

    (goto-char (point-min))))

;;;; Dependency Management

(defun org-gtd-graph-view-add-dependency ()
  "Add a dependency between two tasks.
Prompts for blocker task from ALL tasks in org-agenda-files.
Prompts for blocked task from current project tasks.
If blocker is external to project, adds project ID and TRIGGER property."
  (interactive)
  (when org-gtd-graph-view--graph
    (let* ((project-id (org-with-point-at org-gtd-graph-view--project-marker
                         (org-entry-get (point) "ID")))
           ;; Get all tasks from agenda files for blocker selection
           (all-tasks (org-gtd-graph-view--get-all-tasks))
           (blocker-title (completing-read "Blocking task: " all-tasks nil t))
           (blocker-id (cdr (assoc blocker-title all-tasks)))
           ;; Get project tasks for blocked task selection
           (nodes (org-gtd-graph-nodes org-gtd-graph-view--graph))
           (node-list (org-gtd-graph-view--nodes-to-completion-list nodes))
           (blocked-title (completing-read "Blocked task: " node-list nil t))
           (blocked-id (cdr (assoc blocked-title node-list))))

      (when (and blocker-id blocked-id)
        (condition-case err
            (progn
              ;; Create the bidirectional dependency
              (org-gtd-dependencies-create blocker-id blocked-id)

              ;; Check if blocker is external (not in current project's task nodes)
              (let ((blocker-in-project (gethash blocker-id nodes)))
                (unless blocker-in-project
                  ;; Blocker is external - add project ID and TRIGGER
                  (org-gtd-add-to-multivalued-property blocker-id "ORG_GTD_PROJECT_IDS" project-id)
                  (org-with-point-at (org-id-find blocker-id t)
                    (org-entry-put (point) "TRIGGER" "org-gtd-projects-trigger")
                    (save-buffer))))

              (message "Added dependency: %s blocks %s" blocker-title blocked-title)
              ;; TODO: Record for undo (currently disabled - causes test failures)
              ;; (org-gtd-graph-undo-record-add-dependency blocker-id blocked-id)
              (org-gtd-graph-view-refresh))
          (error (user-error "Cannot add dependency: %s" (error-message-string err))))))))

(defun org-gtd-graph-view-add-blocker ()
  "Add a blocker task and make it a root task of the project.
Prompts for blocker task from ALL tasks in org-agenda-files.
Prompts for blocked task from current project tasks.
Adds blocker to project's ORG_GTD_FIRST_TASKS.
If blocker is external to project, adds project ID and TRIGGER property."
  (interactive)
  (when org-gtd-graph-view--graph
    (let* ((project-id (org-with-point-at org-gtd-graph-view--project-marker
                         (org-entry-get (point) "ID")))
           ;; Get all tasks from agenda files for blocker selection
           (all-tasks (org-gtd-graph-view--get-all-tasks))
           (blocker-title (completing-read "Blocker task: " all-tasks nil t))
           (blocker-id (cdr (assoc blocker-title all-tasks)))
           ;; Get project tasks for blocked task selection
           (nodes (org-gtd-graph-nodes org-gtd-graph-view--graph))
           (node-list (org-gtd-graph-view--nodes-to-completion-list nodes))
           (blocked-title (completing-read "Task to block: " node-list nil t))
           (blocked-id (cdr (assoc blocked-title node-list))))

      (when (and blocker-id blocked-id)
        (condition-case err
            (progn
              ;; Create the bidirectional dependency
              (org-gtd-dependencies-create blocker-id blocked-id)

              ;; Add blocker to project's ORG_GTD_FIRST_TASKS
              (org-with-point-at org-gtd-graph-view--project-marker
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" blocker-id)
                (save-buffer))

              ;; Check if blocker is external (not in current project's task nodes)
              (let ((blocker-in-project (gethash blocker-id nodes)))
                (unless blocker-in-project
                  ;; Blocker is external - add project ID and TRIGGER
                  (org-gtd-add-to-multivalued-property blocker-id "ORG_GTD_PROJECT_IDS" project-id)
                  (org-with-point-at (org-id-find blocker-id t)
                    (org-entry-put (point) "TRIGGER" "org-gtd-projects-trigger")
                    (save-buffer))))

              (message "Added blocker: %s blocks %s and is now a root task" blocker-title blocked-title)
              ;; TODO: Record for undo (currently disabled - causes test failures)
              ;; (org-gtd-graph-undo-record-add-dependency blocker-id blocked-id)
              (org-gtd-graph-view-refresh))
          (error (user-error "Cannot add blocker: %s" (error-message-string err))))))))

(defun org-gtd-graph-view-remove-dependency ()
  "Remove a dependency between two tasks interactively."
  (interactive)
  (when org-gtd-graph-view--graph
    (let* ((edges (org-gtd-graph-edges org-gtd-graph-view--graph))
           (edge-list (org-gtd-graph-view--edges-to-completion-list edges))
           (edge-desc (completing-read "Remove dependency: " edge-list nil t))
           (edge-ids (cdr (assoc edge-desc edge-list)))
           (blocker-id (car edge-ids))
           (blocked-id (cdr edge-ids)))

      (when (and blocker-id blocked-id)
        ;; Remove bidirectional dependency
        (org-gtd-remove-from-multivalued-property blocker-id org-gtd-prop-blocks blocked-id)
        (org-gtd-remove-from-multivalued-property blocked-id org-gtd-prop-depends-on blocker-id)
        (message "Removed dependency")
        ;; TODO: Record for undo (currently disabled - causes test failures)
        ;; (org-gtd-graph-undo-record-remove-dependency blocker-id blocked-id)
        (org-gtd-graph-view-refresh)))))

;;;; Helper Functions

(defun org-gtd-graph-view--get-all-tasks ()
  "Get all tasks from org-agenda-files as completion list.
Returns list of (title . id) cons cells for tasks with IDs."
  (let (tasks)
    (org-map-entries
     (lambda ()
       (when-let* ((id (org-entry-get (point) "ID"))
                   (todo-state (org-get-todo-state))
                   (title (org-get-heading t t t t)))
         (when todo-state  ; Only include headings with TODO keywords
           (push (cons title id) tasks))))
     nil
     'agenda)
    (nreverse tasks)))

(defun org-gtd-graph-view--nodes-to-completion-list (nodes)
  "Convert NODES hash table to completion list of (title . id)."
  (let (result)
    (maphash (lambda (_id node)
               (push (cons (org-gtd-graph-node-title node)
                           (org-gtd-graph-node-id node))
                     result))
             nodes)
    (nreverse result)))

(defun org-gtd-graph-view--edges-to-completion-list (edges)
  "Convert EDGES list to completion list of (description . (from-id . to-id))."
  (mapcar (lambda (edge)
            (let* ((from-id (org-gtd-graph-edge-from-id edge))
                   (to-id (org-gtd-graph-edge-to-id edge))
                   (from-node (org-gtd-graph-data-get-node
                               org-gtd-graph-view--graph from-id))
                   (to-node (org-gtd-graph-data-get-node
                             org-gtd-graph-view--graph to-id))
                   (from-title (or (and from-node (org-gtd-graph-node-title from-node))
                                   "[Unknown]"))
                   (to-title (or (and to-node (org-gtd-graph-node-title to-node))
                                 "[Unknown]"))
                   (desc (format "%s blocks %s" from-title to-title)))
              (cons desc (cons from-id to-id))))
          edges))

;;;; Footer

(provide 'org-gtd-graph-view)

;;; org-gtd-graph-view.el ends here
