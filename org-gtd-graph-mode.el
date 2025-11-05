;;; org-gtd-graph-mode.el --- Major mode for graph visualization -*- lexical-binding: t; coding: utf-8 -*-
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
;; Major mode for org-gtd graph visualization buffers.
;; Provides keybindings and commands for interacting with project graphs.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-graph-view)
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-navigation)
(require 'org-gtd-graph-ui)

;;;; Keymap

(defvar org-gtd-graph-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Help/Transient menu
    (define-key map (kbd "?") #'org-gtd-graph-transient-main)

    ;; Graph operations
    (define-key map (kbd "r") #'org-gtd-graph-view-refresh)
    (define-key map (kbd "v") #'org-gtd-graph-toggle-render-mode)
    (define-key map (kbd "q") #'quit-window)

    ;; Dependency-based navigation
    (define-key map (kbd "n") #'org-gtd-graph-nav-down-dependency)
    (define-key map (kbd "p") #'org-gtd-graph-nav-up-dependency)

    ;; Layer-based navigation
    (define-key map (kbd "TAB") #'org-gtd-graph-nav-next-sibling)
    (define-key map (kbd "<backtab>") #'org-gtd-graph-nav-previous-sibling)
    (define-key map (kbd "<") #'org-gtd-graph-nav-first-in-layer)
    (define-key map (kbd ">") #'org-gtd-graph-nav-last-in-layer)

    ;; Goto navigation
    (define-key map (kbd "g") #'org-gtd-graph-nav-goto)

    ;; Navigation history
    (define-key map (kbd "[") #'org-gtd-graph-ui-back)
    (define-key map (kbd "]") #'org-gtd-graph-ui-forward)

    map)
  "Keymap for `org-gtd-graph-view-mode'.")

;;;; Mode Definition

;;;###autoload
(define-derived-mode org-gtd-graph-view-mode special-mode "GTD-Graph"
  "Major mode for visualizing and editing org-gtd project dependency graphs.

This mode displays a project's task dependency structure as an interactive
DAG (Directed Acyclic Graph) using SVG rendering. Tasks are shown as nodes
and dependencies as directed edges.

\\<org-gtd-graph-view-mode-map>

Key bindings:
\\{org-gtd-graph-view-mode-map}

The graph is interactive:
- Click on nodes to view task details
- Use keyboard commands to add/remove dependencies
- Create and delete tasks
- All changes are immediately saved to the org file

The graph auto-updates when you refresh (\\[org-gtd-graph-view-refresh])."
  :group 'org-gtd
  (setq-local buffer-read-only t
              truncate-lines nil)
  (buffer-disable-undo))

;;;; Autoload Entry Point

;;;###autoload
(defun org-gtd-show-project-graph (&optional project-marker)
  "Show interactive dependency graph for project.
If PROJECT-MARKER is nil, use the project at point or prompt for one."
  (interactive)
  (let ((marker (or project-marker
                    (org-gtd-graph-mode--find-project-at-point)
                    (org-gtd-graph-mode--prompt-for-project))))
    (when marker
      (org-gtd-graph-view-create marker))))

(defun org-gtd-graph-mode--find-project-at-point ()
  "Find project heading at point, if any."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (when (string= (org-entry-get nil "ORG_GTD") "Projects")
        (point-marker)))))

(defun org-gtd-graph-mode--prompt-for-project ()
  "Prompt user to select a project."
  (let* ((projects (org-gtd-graph-mode--get-all-projects))
         (project-titles (mapcar #'car projects))
         (choice (completing-read "Select project: " project-titles nil t)))
    (when choice
      (cdr (assoc choice projects)))))

(defun org-gtd-graph-mode--get-all-projects ()
  "Get all projects as alist of (title . marker)."
  (let (projects)
    (with-current-buffer (org-gtd--default-file)
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get nil "ORG_GTD") "Projects")
           (push (cons (org-get-heading t t t t) (point-marker))
                 projects)))
       nil
       'file))
    (nreverse projects)))

;;;; Footer

(provide 'org-gtd-graph-mode)

;;; org-gtd-graph-mode.el ends here
