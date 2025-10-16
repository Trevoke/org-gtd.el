;;; org-gtd-graph-transient.el --- Transient menu for graph view -*- lexical-binding: t; coding: utf-8 -*-
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
;; Provides a magit-style transient menu system for the graph visualization UI.
;; Users can press ? to see a discoverable menu of all available commands,
;; organized into logical groups.
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-filter)
(require 'org-gtd-graph-ui)

;;;; Main Transient Menu

;;;###autoload (autoload 'org-gtd-graph-transient-main "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-main ()
  "Main command menu for GTD project graph view."
  ["Org GTD Graph Commands"
   ["Add"
    ("a c" "child task" org-gtd-graph-transient-add-child)
    ("a s" "sibling task" org-gtd-graph-transient-add-sibling)
    ("a r" "root task" org-gtd-graph-transient-add-root)
    ("a l" "link dependency" org-gtd-graph-view-add-dependency)
    ("a b" "add blocker" org-gtd-graph-view-add-blocker)]
   ["Edit"
    ("e p" "properties" org-gtd-graph-transient-edit-properties)
    ("e t" "todo state" org-gtd-graph-transient-edit-todo)
    ("e s" "schedule" org-gtd-graph-transient-schedule)
    ("e d" "deadline" org-gtd-graph-transient-deadline)]
   ["Navigate"
    ("n" "next node" org-gtd-graph-nav-next :transient t)
    ("j" "next node" org-gtd-graph-nav-next :transient t)
    ("p" "previous node" org-gtd-graph-nav-previous :transient t)
    ("k" "previous node" org-gtd-graph-nav-previous :transient t)
    ("TAB" "next sibling" org-gtd-graph-nav-next-sibling :transient t)
    ("g" "goto node" org-gtd-graph-nav-goto)]
   ["View"
    ("f" "filter" org-gtd-graph-transient-filter)
    ("z" "zoom" org-gtd-graph-transient-zoom)]
   ["Actions"
    ("r" "refresh" org-gtd-graph-view-refresh :transient t)
    ("u" "undo" org-gtd-graph-undo)
    ("C-r" "redo" org-gtd-graph-redo)]
   ["Quit"
    ("q" "quit" transient-quit-one)
    ("Q" "quit and kill" org-gtd-graph-quit-and-kill)]])

;;;; Add Commands

(defun org-gtd-graph-transient-add-child ()
  "Add a child task to the selected node.
Creates a new task as a child of the currently selected node and
creates a dependency where the parent blocks the child."
  (interactive)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected. Click on a node first"))

  (let* ((parent-id org-gtd-graph-ui--selected-node-id)
         (title (read-string "Child task title: ")))
    (when (and title (not (string-empty-p title)))
      (let ((parent-marker (org-id-find parent-id t)))
        (unless parent-marker
          (user-error "Cannot find parent node with ID: %s" parent-id))

        (org-with-point-at parent-marker
          (let ((parent-level (org-current-level)))
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert (make-string (1+ parent-level) ?\*) " " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (let ((child-id (org-gtd-id-get-create)))
              (org-todo "TODO")
              (org-entry-put (point) "ORG_GTD" "Actions")
              (let ((project-id (org-entry-get parent-marker "ORG_GTD_PROJECT_IDS")))
                (when project-id
                  (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)))
              (require 'org-gtd-dependencies)
              (org-gtd-dependencies-create parent-id child-id)
              (save-buffer)
              (message "Created child task: %s" title)
              (org-gtd-graph-view-refresh))))))))

(defun org-gtd-graph-transient-add-sibling ()
  "Add a sibling task to the selected node.
Creates a new task at the same level as the currently selected node."
  (interactive)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected. Click on a node first"))

  (let* ((sibling-id org-gtd-graph-ui--selected-node-id)
         (title (read-string "Sibling task title: ")))
    (when (and title (not (string-empty-p title)))
      (let ((sibling-marker (org-id-find sibling-id t)))
        (unless sibling-marker
          (user-error "Cannot find sibling node with ID: %s" sibling-id))

        (org-with-point-at sibling-marker
          (let ((level (org-current-level)))
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert (make-string level ?\*) " " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (let ((new-id (org-gtd-id-get-create)))
              (org-todo "TODO")
              (org-entry-put (point) "ORG_GTD" "Actions")
              (let ((project-id (org-entry-get sibling-marker "ORG_GTD_PROJECT_IDS")))
                (when project-id
                  (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)))
              (save-buffer)
              (message "Created sibling task: %s" title)
              (org-gtd-graph-view-refresh))))))))

(defun org-gtd-graph-transient-add-root ()
  "Add a new root task to the project.
Creates a task that has no dependencies and adds it to the project's
ORG_GTD_FIRST_TASKS property."
  (interactive)
  (unless org-gtd-graph-view--project-marker
    (user-error "No project marker set"))

  (let* ((title (read-string "Root task title: "))
         (task-id nil))
    (when (and title (not (string-empty-p title)))
      ;; First: create the task
      (org-with-point-at org-gtd-graph-view--project-marker
        (let ((project-id (org-entry-get (point) "ID")))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert "** " title "\n")
          (forward-line -1)
          (org-back-to-heading t)
          (setq task-id (org-gtd-id-get-create))
          (org-todo "TODO")
          (org-entry-put (point) "ORG_GTD" "Actions")
          (when project-id
            (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id))
          (save-buffer)))

      ;; Second: add task to project's FIRST_TASKS (separate org-with-point-at)
      (org-with-point-at org-gtd-graph-view--project-marker
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))

      (message "Created root task: %s" title)
      (org-gtd-graph-view-refresh))))

;;;; Edit Commands

(defun org-gtd-graph-transient-edit-properties ()
  "Edit properties of selected node."
  (interactive)
  (message "Edit properties (Phase 2)"))

(defun org-gtd-graph-transient-edit-todo ()
  "Change TODO state of selected node."
  (interactive)
  (message "Edit TODO state (Phase 2)"))

(defun org-gtd-graph-transient-schedule ()
  "Schedule selected node."
  (interactive)
  (message "Schedule node (Phase 2)"))

(defun org-gtd-graph-transient-deadline ()
  "Set deadline for selected node."
  (interactive)
  (message "Set deadline (Phase 2)"))

;;;; Navigation Commands (Phase 3 placeholders)

(defun org-gtd-graph-nav-next ()
  "Move to next node in graph."
  (interactive)
  (message "Navigation: next node (Phase 3)"))

(defun org-gtd-graph-nav-previous ()
  "Move to previous node in graph."
  (interactive)
  (message "Navigation: previous node (Phase 3)"))

(defun org-gtd-graph-nav-next-sibling ()
  "Move to next sibling node."
  (interactive)
  (message "Navigation: next sibling (Phase 3)"))

(defun org-gtd-graph-nav-goto ()
  "Go to specific node by selection."
  (interactive)
  (message "Navigation: goto node (Phase 3)"))

;;;; View Commands

;;;###autoload (autoload 'org-gtd-graph-transient-filter "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-filter ()
  "Filter graph view by various criteria."
  ["Filter Graph View"
   ["By Status"
    ("t" "TODO state" org-gtd-graph-filter-by-todo)
    ("p" "priority" org-gtd-graph-filter-by-priority)]
   ["By Metadata"
    ("g" "tags" org-gtd-graph-filter-by-tag)
    ("s" "scheduled date" org-gtd-graph-filter-by-scheduled)]
   ["Clear"
    ("c" "clear all filters" org-gtd-graph-filter-clear-all)]])

;;;###autoload (autoload 'org-gtd-graph-transient-zoom "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-zoom ()
  "Zoom graph view focus."
  ["Zoom Graph View"
   ["Zoom"
    ("z" "zoom to selected subtree" org-gtd-graph-zoom-to-subtree)
    ("o" "zoom out to full view" org-gtd-graph-zoom-out-full)]])

;;;; Undo/Redo Commands (Phase 6 placeholders)

(defun org-gtd-graph-undo ()
  "Undo last graph change."
  (interactive)
  (message "Undo (Phase 6)"))

(defun org-gtd-graph-redo ()
  "Redo last undone change."
  (interactive)
  (message "Redo (Phase 6)"))

;;;; Quit Commands

(defun org-gtd-graph-quit-and-kill ()
  "Quit graph view and kill buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;; Footer

(provide 'org-gtd-graph-transient)

;;; org-gtd-graph-transient.el ends here
