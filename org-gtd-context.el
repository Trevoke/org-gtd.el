;;; org-gtd-context.el --- Context resolution for multi-context operations -*- lexical-binding: t; coding: utf-8 -*-
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
;; Provides context resolution for operations that work across
;; org buffers, agenda, and graph view.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-macs)

;;;; External Variable Declarations

(defvar org-gtd-graph-view--project-marker)
(defvar org-gtd-graph-ui--selected-node-id)

;;;; Context Struct

(cl-defstruct (org-gtd-context
               (:constructor org-gtd-context-create)
               (:copier nil))
  "Context for multi-context project operations.

Fields:
  mode           - Symbol: \\='graph-view, \\='agenda, or \\='org
  project-marker - Marker to project heading
  project-id     - ID string of project
  task-id        - ID string of task at point (nil if on project heading)
  task-marker    - Marker to task (nil if on project heading)"
  mode
  project-marker
  project-id
  task-id
  task-marker)

;;;; Context Resolution

(declare-function org-gtd-project--get-marker-at-point "org-gtd-projects")

(defun org-gtd-context-at-point ()
  "Resolve current context for multi-context operations.
Returns `org-gtd-context' struct with mode, markers, and IDs.
Signals `user-error' if not in a supported context."
  (cond
   ((derived-mode-p 'org-gtd-graph-view-mode)
    (org-gtd-context--from-graph-view))
   ((derived-mode-p 'org-agenda-mode)
    (org-gtd-context--from-agenda))
   ((derived-mode-p 'org-mode)
    (org-gtd-context--from-org-buffer))
   (t
    (user-error "Must be in org buffer, agenda, or graph view"))))

(defun org-gtd-context--from-graph-view ()
  "Build context from graph view buffer-locals."
  (unless (bound-and-true-p org-gtd-graph-view--project-marker)
    (user-error "No project in graph view"))
  (let* ((project-marker org-gtd-graph-view--project-marker)
         (project-id (org-with-point-at project-marker (org-id-get)))
         (selected-id (bound-and-true-p org-gtd-graph-ui--selected-node-id))
         ;; Task ID is nil if selected node is the project itself
         (task-id (unless (equal selected-id project-id) selected-id))
         (task-marker (when task-id (org-id-find task-id t))))
    (org-gtd-context-create
     :mode 'graph-view
     :project-marker project-marker
     :project-id project-id
     :task-id task-id
     :task-marker task-marker)))

(defun org-gtd-context--from-agenda ()
  "Build context from agenda item at point."
  (let ((item-marker (org-get-at-bol 'org-marker)))
    (unless item-marker
      (user-error "No item at agenda point"))
    (let* ((task-id (org-with-point-at item-marker (org-id-get)))
           (project-marker (org-with-point-at item-marker
                             (require 'org-gtd-projects)
                             (org-gtd-project--get-marker-at-point)))
           (project-id (org-with-point-at project-marker (org-id-get))))
      (org-gtd-context-create
       :mode 'agenda
       :project-marker project-marker
       :project-id project-id
       :task-id task-id
       :task-marker item-marker))))

(defun org-gtd-context--from-org-buffer ()
  "Build context from org heading at point."
  (org-back-to-heading t)
  (let* ((here-id (org-id-get))
         (org-gtd-val (org-entry-get (point) "ORG_GTD"))
         (on-project-heading (equal org-gtd-val "Projects"))
         (project-marker (if on-project-heading
                             (point-marker)
                           (require 'org-gtd-projects)
                           (org-gtd-project--get-marker-at-point)))
         (project-id (org-with-point-at project-marker (org-id-get))))
    (org-gtd-context-create
     :mode 'org
     :project-marker project-marker
     :project-id project-id
     :task-id (unless on-project-heading here-id)
     :task-marker (unless on-project-heading (point-marker)))))

;;;; Footer

(provide 'org-gtd-context)

;;; org-gtd-context.el ends here
