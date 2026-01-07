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

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Commands

(defun org-gtd-graph-quit ()
  "Quit graph view and close details pane."
  (interactive)
  ;; Clean up the split-window layout first
  (org-gtd-graph-ui-cleanup-windows)
  ;; Then quit the graph window
  (quit-window))

;;;; Keymap

(defvar org-gtd-graph-view-mode-map
  (let ((map (make-sparse-keymap)))
    ;; === Tier 1: Single keys ===

    ;; Help/menu
    (define-key map (kbd "?") #'org-gtd-graph-transient-main)

    ;; Navigation
    (define-key map (kbd "n") #'org-gtd-graph-nav-down-dependency)
    (define-key map (kbd "p") #'org-gtd-graph-nav-up-dependency)
    (define-key map (kbd "TAB") #'org-gtd-graph-nav-next-sibling)
    (define-key map (kbd "<backtab>") #'org-gtd-graph-nav-previous-sibling)
    (define-key map (kbd "G") #'org-gtd-graph-nav-goto)

    ;; Mouse selection
    (define-key map [mouse-1] #'org-gtd-graph-view-click-select)

    ;; Add tasks
    (define-key map (kbd "r") #'org-gtd-graph-transient-add-root)
    (define-key map (kbd "s") #'org-gtd-graph-add-successor)
    (define-key map (kbd "b") #'org-gtd-graph-add-blocker)

    ;; Modify relationships (bulk)
    (define-key map (kbd "B") #'org-gtd-graph-modify-blockers)
    (define-key map (kbd "S") #'org-gtd-graph-modify-successors)

    ;; View
    (define-key map (kbd "v") #'org-gtd-graph-toggle-render-mode)
    (define-key map (kbd "g") #'org-gtd-graph-view-refresh)

    ;; Quit
    (define-key map (kbd "q") #'org-gtd-graph-quit)

    ;; === Tier 2: Prefixed or capital keys ===

    ;; Task operations (t prefix)
    (define-key map (kbd "t t") #'org-gtd-graph-change-state)
    (define-key map (kbd "t r") #'org-gtd-graph-remove-task)
    (define-key map (kbd "t d") #'org-gtd-graph-trash-task)
    (define-key map (kbd "t e") #'org-gtd-graph-ui-jump-to-task)
    (define-key map (kbd "t i") #'org-gtd-graph-view-show-relationships)

    ;; Project operations
    (define-key map (kbd "I") #'org-gtd-graph-incubate-project)
    (define-key map (kbd "C") #'org-gtd-graph-cancel-project)

    ;; Graph view operations
    (define-key map (kbd "Q") #'org-gtd-graph-quit-and-kill)
    (define-key map (kbd "x s") #'org-gtd-graph-export-svg)
    (define-key map (kbd "x d") #'org-gtd-graph-export-dot)
    (define-key map (kbd "x a") #'org-gtd-graph-export-ascii)

    map)
  "Keymap for `org-gtd-graph-view-mode'.")

;;;; Evil-mode Integration

;; When evil-mode is loaded, start graph-view-mode in emacs state.
;; This avoids C-z conflicts between evil-toggle-key and transient's C-z,
;; and provides better UX since graph navigation uses emacs-style bindings.
;;
;; We use both evil-set-initial-state AND a mode hook for robustness:
;; - evil-set-initial-state: handles new buffers entering this mode
;; - mode hook: forces emacs state even if evil-collection or user config
;;   has set a different state for special-mode (our parent)
(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-graph-view-mode 'emacs)
  (add-hook 'org-gtd-graph-view-mode-hook #'evil-emacs-state))

;;;; Mode Definition

;;;###autoload
(define-derived-mode org-gtd-graph-view-mode special-mode "GTD-Graph"
  "Major mode for visualizing and editing org-gtd project dependency graphs.

This mode displays a project's task dependency structure as an interactive
DAG (Directed Acyclic Graph) using SVG rendering.  Tasks are shown as nodes
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
  "Display an interactive visual graph of project task dependencies.
Shows tasks as nodes and dependencies as edges in an SVG visualization.
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
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (when (string= (org-entry-get nil "ORG_GTD") "Projects")
               (push (cons (org-get-heading t t t t) (point-marker))
                     projects)))
           nil
           'file))))
    (nreverse projects)))

;;;; Footer

(provide 'org-gtd-graph-mode)

;;; org-gtd-graph-mode.el ends here
