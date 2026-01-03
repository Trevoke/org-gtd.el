;;; org-gtd-project-operations.el --- Simple project task operations for org/agenda -*- lexical-binding: t; coding: utf-8 -*-
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
;; Simple prompt-based operations for adding tasks to projects from org/agenda.
;;
;; Unlike the graph view transient which allows multi-select of predecessors/successors,
;; these functions use the task at point as the single predecessor or blocked task.
;; This provides a streamlined UX when you're already looking at the task you want
;; to relate to.
;;
;; Example usage:
;;   - In agenda on "Buy groceries" task, call `org-gtd-add-successor--simple'
;;   - Enter "Cook dinner" as successor title
;;   - Result: "Cook dinner" is created and blocked by "Buy groceries"
;;
;;; Code:

;;;; Requirements

(require 'org-macs)
(require 'org-gtd-context)
(require 'org-gtd-projects)
(require 'org-gtd-graph-transient)
(require 'org-gtd-configure)

;;;; Simple UX Functions

(defun org-gtd-add-successor--simple ()
  "Simple prompt-based UX for adding successor from org/agenda.
Uses task at point as the predecessor."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-add-successor--simple-with-context ctx)))

(defun org-gtd-add-successor--simple-with-context (ctx)
  "Add successor using context CTX.
Uses task at point as the predecessor.
Offers completion on existing tasks or allows creating new ones."
  (let* ((predecessor-id (org-gtd-context-task-id ctx))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx)))
    (unless predecessor-id
      (user-error "No task at point to add successor to"))
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select successor task: " project-marker))
           (selected (completing-read "Select or create successor task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected))
           task-id)
      (when (string-empty-p title)
        (user-error "Task title required"))
      ;; Use existing task or create new one
      (if existing-id
          (setq task-id existing-id)
        ;; Create new task using proper GTD configuration
        (org-with-point-at project-marker
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert "** " title "\n")
          (forward-line -1)
          (org-back-to-heading t)
          (setq task-id (org-id-get-create))
          ;; Use existing type configuration
          (org-gtd-configure-as-type 'next-action)
          (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
          (save-buffer)))
      ;; Create dependency: predecessor blocks this task
      (org-gtd-dependencies-create predecessor-id task-id)
      ;; Link existing external task to project if needed
      (when existing-id
        (org-gtd-add-to-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id)
        (org-with-point-at (org-id-find task-id t)
          (save-buffer)))
      ;; Fix TODO keywords
      (org-gtd-projects-fix-todo-keywords project-marker)
      (message "Added successor: %s" title))))

(defun org-gtd-add-blocker--simple ()
  "Simple prompt-based UX for adding blocker from org/agenda.
Creates or selects task that blocks the task at point."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-add-blocker--simple-with-context ctx)))

(defun org-gtd-add-blocker--simple-with-context (ctx)
  "Add blocker using context CTX.
Creates or selects task that blocks the task at point."
  (let* ((blocked-id (org-gtd-context-task-id ctx))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx)))
    (unless blocked-id
      (user-error "No task at point to add blocker for"))
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select blocker task: " project-marker))
           (selected (completing-read "Select or create blocker task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected))
           task-id)
      (when (string-empty-p title)
        (user-error "Task title required"))
      ;; Use existing task or create new one
      (if existing-id
          (setq task-id existing-id)
        ;; Create new blocker using proper GTD configuration
        (org-with-point-at project-marker
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert "** " title "\n")
          (forward-line -1)
          (org-back-to-heading t)
          (setq task-id (org-id-get-create))
          ;; Use existing type configuration
          (org-gtd-configure-as-type 'next-action)
          (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
          ;; Add to FIRST_TASKS since new blocker has no blockers
          (org-entry-add-to-multivalued-property project-marker "ORG_GTD_FIRST_TASKS" task-id)
          (save-buffer)))
      ;; Create dependency: this task blocks the task at point
      (org-gtd-dependencies-create task-id blocked-id)
      ;; Link existing external task to project if needed
      (when existing-id
        (org-gtd-add-to-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id)
        ;; Add to FIRST_TASKS if the blocker has no blockers itself
        (let ((blocker-blockers (org-gtd-get-task-dependencies task-id)))
          (unless blocker-blockers
            (org-with-point-at project-marker
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
              (save-buffer))))
        (org-with-point-at (org-id-find task-id t)
          (save-buffer)))
      ;; Fix TODO keywords
      (org-gtd-projects-fix-todo-keywords project-marker)
      (message "Added blocker: %s" title))))

(defun org-gtd-add-root--simple ()
  "Simple prompt-based UX for adding root task from org/agenda.
Creates or selects task to add as root (no dependencies)."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-add-root--simple-with-context ctx)))

(defun org-gtd-add-root--simple-with-context (ctx)
  "Add root task using context CTX.
Creates or selects task to add as root (no dependencies)."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx)))
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select root task: " project-marker))
           (selected (completing-read "Select or create root task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected))
           task-id)
      (when (string-empty-p title)
        (user-error "Task title required"))
      ;; Use existing task or create new one
      (if existing-id
          (setq task-id existing-id)
        ;; Create new root task using proper GTD configuration
        (org-with-point-at project-marker
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert "** " title "\n")
          (forward-line -1)
          (org-back-to-heading t)
          (setq task-id (org-id-get-create))
          ;; Use existing type configuration
          (org-gtd-configure-as-type 'next-action)
          (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
          (save-buffer)))
      ;; Add to FIRST_TASKS (root tasks have no blockers)
      (org-with-point-at project-marker
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))
      ;; Link existing external task to project if needed
      (when existing-id
        (org-gtd-add-to-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id)
        (org-with-point-at (org-id-find task-id t)
          (save-buffer)))
      ;; Fix TODO keywords
      (org-gtd-projects-fix-todo-keywords project-marker)
      (message "Added root task: %s" title))))

;;;; Remove Task Operations

(defun org-gtd-remove-task--simple ()
  "Simple prompt-based UX for removing task at point from its project."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-remove-task--simple-with-context ctx)))

(defun org-gtd-remove-task--simple-with-context (ctx)
  "Remove task at point from project using context CTX."
  (let ((task-id (org-gtd-context-task-id ctx))
        (project-id (org-gtd-context-project-id ctx)))
    (unless task-id
      (user-error "No task at point"))
    (let ((task-title (org-with-point-at (org-id-find task-id t)
                        (org-get-heading t t t t))))
      (when (yes-or-no-p (format "Remove '%s' from this project? " task-title))
        (org-gtd-graph--remove-from-project task-id project-id)
        (message "Removed '%s' from project" task-title)))))

;;;; Trash Task Operations

(defun org-gtd-trash-task--simple ()
  "Simple prompt-based UX for trashing task at point."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-trash-task--simple-with-context ctx)))

(defun org-gtd-trash-task--simple-with-context (ctx)
  "Trash task at point using context CTX."
  (let ((task-id (org-gtd-context-task-id ctx)))
    (unless task-id
      (user-error "No task at point"))
    (let ((task-title (org-with-point-at (org-id-find task-id t)
                        (org-get-heading t t t t))))
      (when (yes-or-no-p (format "Trash task '%s'? This removes it from all projects and marks it as canceled. " task-title))
        (org-gtd-graph--trash-task task-id)
        (message "Trashed task '%s'" task-title)))))

;;;; Change TODO State Operations

(defun org-gtd-change-state--simple ()
  "Change TODO state of task at point."
  (interactive)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-change-state--simple-with-context ctx)))

(defun org-gtd-change-state--simple-with-context (ctx)
  "Change TODO state of task at point using context CTX."
  (let ((task-id (org-gtd-context-task-id ctx)))
    (unless task-id
      (user-error "No task at point"))
    (let ((marker (org-id-find task-id t)))
      (unless marker
        (user-error "Cannot find task with ID: %s" task-id))
      (org-with-point-at marker
        (call-interactively #'org-todo)
        (save-buffer))
      (message "Changed TODO state"))))

;;;; Footer

(provide 'org-gtd-project-operations)

;;; org-gtd-project-operations.el ends here
