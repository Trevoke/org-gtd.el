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
(require 'org-gtd-svg-render)  ; For color helpers and svg-to-image
(require 'org-gtd-dag-draw)
(require 'org-gtd-projects)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-id)
(require 'org-gtd-configure)
(require 'org-gtd-files)

(declare-function org-gtd-graph-view-mode "org-gtd-graph-view")

;;;; Customization

(defcustom org-gtd-graph-render-mode 'svg
  "Default rendering mode for project graphs.
Can be \\='svg (default) or \\='ascii.
Users can toggle per-buffer using `org-gtd-graph-toggle-render-mode'."
  :type '(choice (const :tag "SVG (graphical)" svg)
                 (const :tag "ASCII (text-based)" ascii))
  :group 'org-gtd)

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

(defvar-local org-gtd-graph-view--render-mode nil
  "Current render mode for this buffer (\\='svg or \\='ascii).
Initialized from `org-gtd-graph-render-mode' on buffer creation.
Can be toggled with `org-gtd-graph-toggle-render-mode'.")

;;;; Buffer Management

(defun org-gtd-graph-view--buffer-name (proj-name)
  "Generate buffer name for PROJ-NAME graph view."
  (format "*Org GTD Graph: %s*" proj-name))

(defun org-gtd-graph-view-create (project-marker)
  "Create or update graph view for project at PROJECT-MARKER."
  (let* ((proj-name (org-with-point-at project-marker
                         (org-get-heading t t t t)))
         (buffer-name (org-gtd-graph-view--buffer-name proj-name))
         (buffer (get-buffer-create buffer-name)))

    ;; Initialize buffer settings (but don't render yet)
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      ;; Initialize render mode from customization variable
      (unless org-gtd-graph-view--render-mode
        (setq org-gtd-graph-view--render-mode org-gtd-graph-render-mode))

      (org-gtd-graph-view--setup-file-watch)
      (add-hook 'kill-buffer-hook #'org-gtd-graph-view--cleanup-file-watch nil t)
      (add-hook 'kill-buffer-hook #'org-gtd-graph-ui-cleanup-windows nil t))

    ;; Display buffer and setup split BEFORE rendering
    (switch-to-buffer buffer)
    (org-gtd-graph-ui-setup-windows buffer)

    ;; Ensure graph window is selected, then render with correct dimensions
    (when-let ((graph-window (get-buffer-window buffer)))
      (select-window graph-window)
      (org-gtd-graph-view-refresh))

    ;; Auto-select first actionable task (or project heading if none)
    (when org-gtd-graph-view--graph
      (let ((initial-node (or (org-gtd-graph-data-find-first-actionable
                               org-gtd-graph-view--graph)
                              (org-gtd-graph-project-id org-gtd-graph-view--graph))))
        (org-gtd-graph-ui-select-node initial-node)))

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

      ;; Store the graph before rendering
      (setq org-gtd-graph-view--graph full-graph)

      ;; Render using dag-draw
      (if (eq org-gtd-graph-view--render-mode 'ascii)
          (let ((ascii-text (org-gtd-dag-draw-render graph 'ascii org-gtd-graph-ui--selected-node-id)))
            (org-gtd-graph-view--display-ascii ascii-text graph))
        ;; Default to SVG
        (let ((svg (org-gtd-dag-draw-render graph 'svg org-gtd-graph-ui--selected-node-id)))
          (org-gtd-graph-view--display-svg svg graph))))))

(defun org-gtd-graph-view--display-svg (svg _displayed-graph)
  "Display SVG in the current buffer showing DISPLAYED-GRAPH."
  (let* ((inhibit-read-only t)
         (image (org-gtd-svg-to-image svg)))
    (erase-buffer)
    (insert-image image)
    (insert "\n\n")

    ;; Insert legend
    (insert (propertize "Graph View Controls:\n" 'face 'bold))
    (insert "  ? - All commands    v - Toggle ASCII/SVG    r - Refresh    q - Quit\n")
    (insert "\n")
    (insert "  Navigation:\n")
    (insert "    n - A task this blocks       p - A task that blocks this\n")
    (insert "    TAB - Next sibling task      S-TAB - Previous sibling task\n")
    (insert "    g - Go to task by name\n")
    (insert "\n")
    (insert "  Press ? to see all commands including: add/remove tasks, modify relationships\n")
    (insert (format "\n  Render mode: SVG (press 'v' to toggle)\n"))

    (goto-char (point-min))))

(defun org-gtd-graph-view--display-ascii (ascii-text _displayed-graph)
  "Display ASCII-TEXT in the current buffer showing DISPLAYED-GRAPH."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Insert ASCII graph
    (insert ascii-text)
    (insert "\n\n")

    ;; Insert legend
    (insert (propertize "Graph View Controls:\n" 'face 'bold))
    (insert "  ? - All commands    v - Toggle ASCII/SVG    r - Refresh    q - Quit\n")
    (insert "\n")
    (insert "  Navigation:\n")
    (insert "    n - A task this blocks       p - A task that blocks this\n")
    (insert "    TAB - Next sibling task      S-TAB - Previous sibling task\n")
    (insert "    g - Go to task by name\n")
    (insert "\n")
    (insert "  Press ? to see all commands including: add/remove tasks, modify relationships\n")
    (insert (format "\n  Render mode: ASCII (press 'v' to toggle)\n"))

    (goto-char (point-min))))

;;;; Render Mode Toggle

(defun org-gtd-graph-toggle-render-mode ()
  "Toggle between SVG and ASCII rendering for current graph view."
  (interactive)
  (setq org-gtd-graph-view--render-mode
        (if (eq org-gtd-graph-view--render-mode 'ascii)
            'svg
          'ascii))
  (message "Graph render mode: %s" (upcase (symbol-name org-gtd-graph-view--render-mode)))
  (org-gtd-graph-view-refresh))

;;;; Dependency Management

(defun org-gtd-graph-view-add-dependency ()
  "Add a dependency between two tasks.
Prompts for blocker task from ALL tasks in agenda files.
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
                  ;; Blocker is external - add project ID
                  ;; TRIGGER is already set by org-gtd-dependencies-create
                  (org-gtd-add-to-multivalued-property blocker-id "ORG_GTD_PROJECT_IDS" project-id)
                  (org-with-point-at (org-id-find blocker-id t)
                    (save-buffer))))

              (message "Added dependency: %s blocks %s" blocker-title blocked-title)
              (org-gtd-graph-view-refresh))
          (error (user-error "Cannot add dependency: %s" (error-message-string err))))))))

(defun org-gtd-graph-view-add-blocker ()
  "Add a blocker task and make it a root task of the project.
Prompts for blocker task from ALL tasks in agenda files.
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
                  ;; Blocker is external - add project ID
                  ;; TRIGGER is already set by org-gtd-dependencies-create
                  (org-gtd-add-to-multivalued-property blocker-id "ORG_GTD_PROJECT_IDS" project-id)
                  (org-with-point-at (org-id-find blocker-id t)
                    (save-buffer))))

              (message "Added blocker: %s blocks %s and is now a root task" blocker-title blocked-title)
              (org-gtd-graph-view-refresh))
          (error (user-error "Cannot add blocker: %s" (error-message-string err))))))))

(defun org-gtd-graph-view-show-relationships ()
  "Show relationships for the currently selected node.
Displays which tasks block this task and which tasks this task blocks."
  (interactive)
  (if (not org-gtd-graph-ui--selected-node-id)
      (user-error "No node selected. Click on a node first")
    (let* ((node-id org-gtd-graph-ui--selected-node-id)
           (nodes (org-gtd-graph-nodes org-gtd-graph-view--graph))
           (node (gethash node-id nodes))
           (title (org-gtd-graph-node-title node))
           (marker (org-id-find node-id t))
           (depends-on (when marker
                        (org-with-point-at marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))))
           (blocks (when marker
                    (org-with-point-at marker
                      (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))))

      (with-output-to-temp-buffer "*Org GTD Relationships*"
        (princ (format "Relationships for: %s\n" title))
        (princ (make-string 60 ?=))
        (princ "\n\n")

        (if depends-on
            (progn
              (princ "This task depends on (blockers):\n")
              (dolist (blocker-id depends-on)
                (when-let ((blocker-node (gethash blocker-id nodes)))
                  (princ (format "  • %s\n" (org-gtd-graph-node-title blocker-node))))))
          (princ "This task has no blockers.\n"))

        (princ "\n")

        (if blocks
            (progn
              (princ "This task blocks (dependents):\n")
              (dolist (dependent-id blocks)
                (when-let ((dependent-node (gethash dependent-id nodes)))
                  (princ (format "  • %s\n" (org-gtd-graph-node-title dependent-node))))))
          (princ "This task blocks no other tasks.\n"))))))

(defun org-gtd-graph-view-clear-relationships ()
  "Remove all dependency relationships from selected node.
Removes both blockers (depends on) and dependents (blocks)."
  (interactive)
  (if (not org-gtd-graph-ui--selected-node-id)
      (user-error "No node selected. Click on a node first")
    (let* ((node-id org-gtd-graph-ui--selected-node-id)
           (nodes (org-gtd-graph-nodes org-gtd-graph-view--graph))
           (node (gethash node-id nodes))
           (title (org-gtd-graph-node-title node))
           (marker (org-id-find node-id t))
           (depends-on (when marker
                        (org-with-point-at marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))))
           (blocks (when marker
                    (org-with-point-at marker
                      (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))))
           (total-relationships (+ (length depends-on) (length blocks))))

      (when (zerop total-relationships)
        (user-error "Task '%s' has no relationships to clear" title))

      (when (yes-or-no-p
             (format "Clear all %d relationship%s from '%s'? "
                     total-relationships
                     (if (= total-relationships 1) "" "s")
                     title))

        ;; Remove all blockers (tasks this depends on)
        (dolist (blocker-id depends-on)
          (org-gtd-remove-from-multivalued-property blocker-id org-gtd-prop-blocks node-id)
          (org-gtd-remove-from-multivalued-property node-id org-gtd-prop-depends-on blocker-id))

        ;; Remove all dependents (tasks that depend on this)
        (dolist (dependent-id blocks)
          (org-gtd-remove-from-multivalued-property node-id org-gtd-prop-blocks dependent-id)
          (org-gtd-remove-from-multivalued-property dependent-id org-gtd-prop-depends-on node-id))

        (message "Cleared %d relationship%s from '%s'"
                 total-relationships
                 (if (= total-relationships 1) "" "s")
                 title)
        (org-gtd-graph-view-refresh)))))

;;;; Export Functions

(defun org-gtd-graph-export-svg (filename)
  "Export current graph to FILENAME as SVG."
  (interactive "FSave SVG to: ")
  (unless org-gtd-graph-view--graph
    (user-error "No graph to export"))
  (let ((svg (org-gtd-dag-draw-render
              org-gtd-graph-view--graph
              'svg
              org-gtd-graph-ui--selected-node-id)))
    (with-temp-file filename
      (insert svg))
    (message "Graph exported to %s" filename)))

(defun org-gtd-graph-export-dot (filename)
  "Export current graph to FILENAME as Graphviz DOT format."
  (interactive "FSave DOT to: ")
  (unless org-gtd-graph-view--graph
    (user-error "No graph to export"))
  (let ((dot (org-gtd-dag-draw-render
              org-gtd-graph-view--graph
              'dot
              org-gtd-graph-ui--selected-node-id)))
    (with-temp-file filename
      (insert dot))
    (message "Graph exported to %s" filename)))

(defun org-gtd-graph-export-ascii (filename)
  "Export current graph to FILENAME as ASCII art."
  (interactive "FSave ASCII to: ")
  (unless org-gtd-graph-view--graph
    (user-error "No graph to export"))
  (let ((ascii (org-gtd-dag-draw-render
                org-gtd-graph-view--graph
                'ascii
                org-gtd-graph-ui--selected-node-id)))
    (with-temp-file filename
      (insert ascii))
    (message "Graph exported to %s" filename)))

;;;; Helper Functions

(defun org-gtd-graph-view--get-all-tasks ()
  "Get all tasks from agenda files as completion list.
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

;;;; Footer

(provide 'org-gtd-graph-view)

;;; org-gtd-graph-view.el ends here
