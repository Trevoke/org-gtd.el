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
(require 'org-gtd-archive)
(require 'org-gtd-accessors)

(declare-function org-gtd-tickler "org-gtd-tickler" (&optional reminder-date))

;;;; Main Transient Menu

;;;###autoload (autoload 'org-gtd-graph-transient-main "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-main ()
  "Main command menu for GTD project graph view."
  [:description
   (lambda () (org-gtd-graph-transient--show-selected-context))
   :class transient-row]
  ["Insert Tasks"
   ("a b" "Insert BEFORE this" org-gtd-graph-insert-before)
   ("a a" "Insert AFTER this" org-gtd-graph-insert-after)]
  ["Modify Relationships"
   ("m b" "Modify blockers" org-gtd-graph-modify-blockers)
   ("m d" "Modify dependents" org-gtd-graph-modify-successors)]
  ["Task Operations"
   ("r" "Remove from project" org-gtd-graph-remove-task)
   ("T" "Trash task" org-gtd-graph-trash-task)
   ("e" "Edit in org file" org-gtd-graph-ui-jump-to-task)
   ("t" "Change TODO state" org-gtd-graph-change-state)
   ("i" "Show relationships" org-gtd-graph-view-show-relationships)
   ("I" "Incubate this project" org-gtd-graph-incubate-project)]
  ["Navigation"
   ("n" "Next successor" org-gtd-graph-nav-down-dependency :transient t)
   ("p" "Previous blocker" org-gtd-graph-nav-up-dependency :transient t)
   ("TAB" "Next sibling" org-gtd-graph-nav-next-sibling :transient t)
   ("g" "Goto task" org-gtd-graph-nav-goto)]
  ["View"
   ("v" "Toggle ASCII/SVG" org-gtd-graph-toggle-render-mode :transient t)
   ("z" "Zoom" org-gtd-graph-transient-zoom)
   ("R" "Refresh" org-gtd-graph-view-refresh :transient t)]
  ["Export"
   ("E s" "Export as SVG" org-gtd-graph-export-svg)
   ("E d" "Export as DOT" org-gtd-graph-export-dot)
   ("E a" "Export as ASCII" org-gtd-graph-export-ascii)]
  ["Quit"
   ("q" "Quit" transient-quit-one)
   ("Q" "Quit and kill" org-gtd-graph-quit-and-kill)])

;;;; Context Display

(defun org-gtd-graph-transient--show-selected-context ()
  "Show context about currently selected node.
Displays selected task title and state for user orientation."
  (if (not org-gtd-graph-ui--selected-node-id)
      "No task selected"
    (when-let* ((graph org-gtd-graph-view--graph)
                (node (org-gtd-graph-data-get-node graph org-gtd-graph-ui--selected-node-id)))
      (format "Selected: %s [%s]"
              (org-gtd-graph-node-title node)
              (or (org-gtd-graph-node-state node) "NO STATE")))))

;;;; Helper Functions

(defun org-gtd-graph--select-or-create-task (prompt)
  "Prompt user to select existing task or create new one.
PROMPT is displayed to the user.
Returns (TASK-ID . TITLE) cons cell.
If task is newly created, TASK-ID will be nil and only TITLE is set."
  (let* ((all-tasks (org-gtd-graph--get-all-gtd-tasks))
         (choices (mapcar (lambda (task)
                           (cons (format "%s (%s)"
                                       (plist-get task :title)
                                       (or (plist-get task :category) "Unknown"))
                                 (plist-get task :id)))
                         all-tasks))
         (selected (completing-read prompt choices nil nil)))

    (let ((match (assoc-string selected choices)))
      (if match
          ;; Existing task - return ID and display string
          (cons (cdr match) selected)
        ;; New task - return nil ID and user's input as title
        (cons nil selected)))))

(defun org-gtd-graph--get-all-gtd-tasks ()
  "Get all tasks with ORG_GTD property from agenda files.
Returns list of plists with :id, :title, :category.
Excludes project headings (ORG_GTD=Projects)."
  ;; v4: Users configure org-agenda-files directly, no need for with-org-gtd-context
  (let ((tasks '()))
    (condition-case nil
        (org-map-entries
         (lambda ()
           (when-let ((category (org-entry-get (point) "ORG_GTD"))
                      (id (org-entry-get (point) "ID"))
                      (title (org-get-heading t t t t)))
             ;; Exclude project headings - we only want tasks
             (unless (string= category "Projects")
               (push (list :id id
                          :title title
                          :category category)
                     tasks))))
         nil
         'agenda)
      (error nil))
    (nreverse tasks)))

(defun org-gtd-graph--select-or-create-task-prioritizing-current (_prompt project-marker)
  "Select task or create new one, with current project tasks prioritized.
PROMPT is displayed to the user.
PROJECT-MARKER identifies the current project.
Returns list of (display . id) cons cells with current project tasks first."
  (let* ((project-id (org-with-point-at project-marker
                       (org-entry-get (point) "ID")))
         (all-tasks (org-gtd-graph--get-all-gtd-tasks))
         (in-project '())
         (not-in-project '()))

    ;; Partition tasks by project membership
    (dolist (task all-tasks)
      (let* ((task-id (plist-get task :id))
             (project-ids (org-gtd-get-task-projects task-id)))
        (if (and project-ids (member project-id project-ids))
            (push task in-project)
          (push task not-in-project))))

    ;; Build choices: in-project first, then others
    (let ((prioritized-tasks (append (nreverse in-project) (nreverse not-in-project))))
      (mapcar (lambda (task)
                (cons (format "%s (%s)"
                             (plist-get task :title)
                             (or (plist-get task :category) "Unknown"))
                      (plist-get task :id)))
              prioritized-tasks))))

(defun org-gtd-graph--select-or-create-task-excluding-current (_prompt project-marker)
  "Select task or create new one, excluding tasks already in current project.
PROMPT is displayed to the user.
PROJECT-MARKER identifies the current project.
Returns list of (display . id) cons cells with tasks NOT in current project."
  (let* ((project-id (org-with-point-at project-marker
                       (org-entry-get (point) "ID")))
         (all-tasks (org-gtd-graph--get-all-gtd-tasks))
         (not-in-project '()))

    ;; Filter to only tasks NOT in current project
    (dolist (task all-tasks)
      (let* ((task-id (plist-get task :id))
             (project-ids (org-gtd-get-task-projects task-id)))
        (unless (and project-ids (member project-id project-ids))
          (push task not-in-project))))

    ;; Build choices from excluded tasks only
    (mapcar (lambda (task)
              (cons (format "%s (%s)"
                           (plist-get task :title)
                           (or (plist-get task :category) "Unknown"))
                    (plist-get task :id)))
            (nreverse not-in-project))))

;;;; Add Commands

(defun org-gtd-graph-transient-add-child ()
  "Add a child task to the selected node.
User can select an existing task or create a new one.
Creates a dependency where the parent blocks the child."
  (interactive)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected. Click on a node first"))

  (let* ((parent-id org-gtd-graph-ui--selected-node-id)
         (parent-marker (org-id-find parent-id t))
         (project-ids (org-entry-get parent-marker "ORG_GTD_PROJECT_IDS"))
         (choices (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select or create child task: "
                   org-gtd-graph-view--project-marker))
         (selected (completing-read "Select or create child task: " choices nil nil))
         (match (assoc-string selected choices))
         (existing-id (if match (cdr match) nil))
         (title (if match selected selected)))

    (unless parent-marker
      (user-error "Cannot find parent node with ID: %s" parent-id))

    (let ((child-id existing-id))
      (if existing-id
          ;; Link existing task
          (let ((task-marker (org-id-find existing-id t)))
            (unless task-marker
              (user-error "Cannot find task with ID: %s" existing-id))
            (org-with-point-at task-marker
              ;; Add project IDs to existing task
              (when project-ids
                (dolist (pid (split-string project-ids))
                  (org-gtd-add-to-multivalued-property existing-id org-gtd-prop-project-ids pid)))
              (save-buffer)))

        ;; Create new task
        (org-with-point-at parent-marker
          (let ((parent-level (org-current-level)))
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert (make-string (1+ parent-level) ?\*) " " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (setq child-id (org-id-get-create))
            (org-todo "TODO")
            (org-entry-put (point) "ORG_GTD" "Actions")
            (when project-ids
              (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-ids))
            (save-buffer))))

      ;; Create dependency for both new and existing
      (require 'org-gtd-dependencies)
      (org-gtd-dependencies-create parent-id child-id)
      (message "Added child task: %s" title)
      (org-gtd-graph-view-refresh))))

(defun org-gtd-graph-transient-add-root ()
  "Add a new root task to the project.
User can select an existing task or create a new one.
Creates a task that has no dependencies and adds it to the project's
ORG_GTD_FIRST_TASKS property."
  (interactive)
  (unless org-gtd-graph-view--project-marker
    (user-error "No project marker set"))

  (let* ((choices (org-gtd-graph--select-or-create-task-excluding-current
                   "Select or create root task: "
                   org-gtd-graph-view--project-marker))
         (selected (completing-read "Select or create root task: " choices nil nil))
         (match (assoc-string selected choices))
         (existing-id (if match (cdr match) nil))
         (title (if match selected selected))
         (task-id existing-id))
    (when (and title (not (string-empty-p title)))
      ;; Get or create task
      (if existing-id
          ;; Link existing task to project
          (org-with-point-at org-gtd-graph-view--project-marker
            (let ((project-id (org-entry-get (point) "ID")))
              (org-with-point-at (org-id-find existing-id t)
                (org-gtd-add-to-multivalued-property existing-id org-gtd-prop-project-ids project-id)
                (save-buffer))))

        ;; Create new task
        (org-with-point-at org-gtd-graph-view--project-marker
          (let ((project-id (org-entry-get (point) "ID")))
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert "** " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (setq task-id (org-id-get-create))
            (org-todo "TODO")
            (org-entry-put (point) "ORG_GTD" "Actions")
            (when project-id
              (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id))
            (save-buffer))))

      ;; Add task to project's FIRST_TASKS
      (org-with-point-at org-gtd-graph-view--project-marker
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (save-buffer))

      (message "Added root task: %s" title)
      (org-gtd-graph-view-refresh))))

;;;; Navigation Commands - using implementations from org-gtd-graph-navigation.el

;;;; Relationship Display

(defun org-gtd-graph-view-show-relationships ()
  "Display relationships for selected task."
  (interactive)
  (require 'org-gtd-graph-view)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((task-id org-gtd-graph-ui--selected-node-id)
         (graph org-gtd-graph-view--graph)
         (node (org-gtd-graph-data-get-node graph task-id))
         (title (org-gtd-graph-node-title node))
         (blockers (org-gtd-graph-data-get-predecessors graph task-id))
         (successors (org-gtd-graph-data-get-successors graph task-id)))

    (with-output-to-temp-buffer "*Task Relationships*"
      (princ (format "Relationships for: %s\n\n" title))

      (princ "BLOCKED BY (must complete before this task):\n")
      (if blockers
          (dolist (blocker-id blockers)
            (let ((blocker-node (org-gtd-graph-data-get-node graph blocker-id)))
              (princ (format "  - %s\n" (org-gtd-graph-node-title blocker-node)))))
        (princ "  (none)\n"))

      (princ "\nBLOCKS (this task blocks these tasks):\n")
      (if successors
          (dolist (succ-id successors)
            (let ((succ-node (org-gtd-graph-data-get-node graph succ-id)))
              (princ (format "  - %s\n" (org-gtd-graph-node-title succ-node)))))
        (princ "  (none)\n")))))

;;;; View Commands

;;;###autoload (autoload 'org-gtd-graph-transient-zoom "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-zoom ()
  "Zoom graph view focus."
  ["Zoom Graph View"
   ["Zoom"
    ("z" "zoom to selected subtree" org-gtd-graph-zoom-to-subtree)
    ("o" "zoom out to full view" org-gtd-graph-zoom-out-full)]])

;;;; Quit Commands

(defun org-gtd-graph-quit-and-kill ()
  "Quit graph view and kill buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;; New Node-Centric Operations

;;; Core Remove Operation

(defun org-gtd-graph--remove-from-project (task-id project-id)
  "Remove TASK-ID from PROJECT-ID and rewire dependencies.

Steps:
1. Get predecessors and successors from project graph
2. Rewire: create dependencies pred → succ for all combinations
3. Check if successors should become root tasks
4. Remove task from project's FIRST_TASKS
5. Remove project-id from task's PROJECT_IDS
6. Clean up dependencies if task has no more common projects"
  (require 'org-gtd-graph-data)
  (require 'org-gtd-graph-view)

  ;; Get project marker
  (when-let ((project-marker (org-id-find project-id t)))
    ;; Extract graph to get predecessors and successors
    (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
           (predecessors (org-gtd-graph-data-get-predecessors graph task-id))
           (successors (org-gtd-graph-data-get-successors graph task-id)))

      ;; 1. Rewire: connect all predecessors to all successors
      (dolist (pred predecessors)
        (dolist (succ successors)
          (unless (member succ (org-gtd-get-task-blockers pred))
            (org-gtd-dependencies-create pred succ))))

      ;; 2. Check if successors should become root tasks
      ;;    A successor becomes root if it has no other predecessors after rewiring
      (dolist (succ successors)
        (let* ((succ-preds (org-gtd-graph-data-get-predecessors graph succ))
               ;; Filter out task-id since we're removing it
               (other-preds (cl-remove task-id succ-preds :test 'equal)))
          (when (null other-preds)
            ;; Successor has no other predecessors, make it a root
            (org-with-point-at project-marker
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" succ)
              (save-buffer)))))

      ;; 3. Remove task from project's FIRST_TASKS
      (org-gtd-remove-from-multivalued-property project-id "ORG_GTD_FIRST_TASKS" task-id)

      ;; 4. Remove project-id from task's PROJECT_IDS
      (org-gtd-remove-from-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id)

      ;; 5. Clean up dependencies if tasks no longer share projects
      (dolist (pred predecessors)
        (let* ((pred-projects (org-gtd-get-task-projects pred))
               (task-projects (org-gtd-get-task-projects task-id))
               (common-projects (cl-intersection pred-projects task-projects :test 'equal)))
          (when (null common-projects)
            ;; No common projects, remove dependency
            (org-gtd-remove-from-multivalued-property pred org-gtd-prop-blocks task-id)
            (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-depends-on pred))))

      (dolist (succ successors)
        (let* ((succ-projects (org-gtd-get-task-projects succ))
               (task-projects (org-gtd-get-task-projects task-id))
               (common-projects (cl-intersection succ-projects task-projects :test 'equal)))
          (when (null common-projects)
            ;; No common projects, remove dependency
            (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-blocks succ)
            (org-gtd-remove-from-multivalued-property succ org-gtd-prop-depends-on task-id)))))))

(defun org-gtd-graph--keep-as-independent (task-id)
  "Remove TASK-ID from all projects but keep it as independent task.
Dependencies are preserved (orphaned) for potential future project membership.

Unlike remove-from-project, this does NOT clean up dependencies."
  (let ((projects (org-gtd-get-task-projects task-id)))
    (dolist (project-id projects)
      ;; Get project marker
      (when-let ((project-marker (org-id-find project-id t)))
        ;; Extract graph to get predecessors and successors
        (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
               (predecessors (org-gtd-graph-data-get-predecessors graph task-id))
               (successors (org-gtd-graph-data-get-successors graph task-id)))

          ;; Rewire: connect all predecessors to all successors
          (dolist (pred predecessors)
            (dolist (succ successors)
              (unless (member succ (org-gtd-get-task-blockers pred))
                (org-gtd-dependencies-create pred succ))))

          ;; Check if successors should become root tasks
          (dolist (succ successors)
            (let* ((succ-preds (org-gtd-graph-data-get-predecessors graph succ))
                   (other-preds (cl-remove task-id succ-preds :test 'equal)))
              (when (null other-preds)
                (org-with-point-at project-marker
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" succ)
                  (save-buffer)))))

          ;; Remove task from project's FIRST_TASKS
          (org-gtd-remove-from-multivalued-property project-id "ORG_GTD_FIRST_TASKS" task-id)

          ;; Remove project-id from task's PROJECT_IDS
          (org-gtd-remove-from-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id)

          ;; DO NOT clean up dependencies - preserve them for future use
          )))))

(defun org-gtd-graph--trash-task (task-id)
  "Remove TASK-ID from all projects, clean ALL dependencies, and archive it.

Steps:
1. For each project: rewire dependencies and remove from project
2. Get ALL global predecessors and successors
3. Clean up ALL global dependencies bidirectionally
4. Mark task as canceled and archive it"
  (require 'org-gtd-trash)

  ;; First, collect ALL global dependencies before we start modifying
  (let ((all-predecessors (org-gtd-get-task-dependencies task-id))
        (all-successors (org-gtd-get-task-blockers task-id))
        (projects (org-gtd-get-task-projects task-id)))

    ;; Step 1: For each project, rewire and remove
    (dolist (project-id projects)
      (when-let ((project-marker (org-id-find project-id t)))
        (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
               (predecessors (org-gtd-graph-data-get-predecessors graph task-id))
               (successors (org-gtd-graph-data-get-successors graph task-id)))

          ;; Rewire: connect all predecessors to all successors
          (dolist (pred predecessors)
            (dolist (succ successors)
              (unless (member succ (org-gtd-get-task-blockers pred))
                (org-gtd-dependencies-create pred succ))))

          ;; Check if successors should become root tasks
          ;; IMPORTANT: Check CURRENT state after rewiring, not stale graph
          (dolist (succ successors)
            (let ((current-preds (org-gtd-get-task-dependencies succ)))
              (when (null current-preds)
                (org-with-point-at project-marker
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" succ)
                  (save-buffer)))))

          ;; Remove task from project's FIRST_TASKS
          (org-gtd-remove-from-multivalued-property project-id "ORG_GTD_FIRST_TASKS" task-id)

          ;; Remove project-id from task's PROJECT_IDS
          (org-gtd-remove-from-multivalued-property task-id "ORG_GTD_PROJECT_IDS" project-id))))

    ;; Step 2: Clean up ALL global dependencies
    ;; Remove task-id from all predecessors' BLOCKS
    (dolist (pred all-predecessors)
      (org-gtd-remove-from-multivalued-property pred org-gtd-prop-blocks task-id))

    ;; Remove task-id from all successors' DEPENDS_ON
    (dolist (succ all-successors)
      (org-gtd-remove-from-multivalued-property succ org-gtd-prop-depends-on task-id))

    ;; Clear task's own dependencies
    (dolist (dep all-predecessors)
      (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-depends-on dep))
    (dolist (succ all-successors)
      (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-blocks succ))

    ;; Mark as canceled
    (when-let ((marker (org-id-find task-id t)))
      (org-with-point-at marker
        (org-todo (org-gtd-keywords--canceled))
        (save-buffer)))))

(defun org-gtd-graph-insert-before ()
  "Insert new task as prerequisite to selected task.
Prompts for where to insert if selected task has blockers."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         ;; Filter out project ID from predecessors (virtual edge for visualization)
         (predecessors (seq-remove (lambda (id) (string= id project-id))
                                   (org-gtd-graph-data-get-predecessors graph selected-id)))
         (choices (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select or create predecessor task: "
                   org-gtd-graph-view--project-marker))
         (selected (completing-read "Select or create predecessor task: " choices nil nil))
         (match (assoc-string selected choices))
         (existing-id (if match (cdr match) nil))
         (title (if match selected selected)))

    (when title
      ;; Get or create task
      (let ((new-task-id existing-id))
        (if existing-id
            ;; Link existing task
            (org-with-point-at (org-id-find existing-id t)
              (org-gtd-add-to-multivalued-property existing-id org-gtd-prop-project-ids project-id)
              (save-buffer))

          ;; Create new task
          (org-with-point-at org-gtd-graph-view--project-marker
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert "** " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (setq new-task-id (org-id-get-create))
            (org-todo "TODO")
            (org-entry-put (point) "ORG_GTD" "Actions")
            (when project-id
              (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id))
            (save-buffer)))

        ;; Handle different cases based on predecessors
        (cond
         ;; Case 1: No predecessors (selected is root)
         ((null predecessors)
          ;; Remove selected from FIRST_TASKS and add new task
          (org-with-point-at org-gtd-graph-view--project-marker
            (org-entry-remove-from-multivalued-property (point) "ORG_GTD_FIRST_TASKS" selected-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
            (basic-save-buffer))
          ;; Create dependency: new → selected
          (org-gtd-dependencies-create new-task-id selected-id))

         ;; Case 2: One predecessor - straightforward rewiring
         ((= 1 (length predecessors))
          (let ((predecessor-id (car predecessors)))
            ;; Remove: predecessor → selected
            (org-gtd-remove-from-multivalued-property predecessor-id org-gtd-prop-blocks selected-id)
            (org-gtd-remove-from-multivalued-property selected-id org-gtd-prop-depends-on predecessor-id)
            ;; Add: predecessor → new
            (org-gtd-dependencies-create predecessor-id new-task-id)
            ;; Add: new → selected
            (org-gtd-dependencies-create new-task-id selected-id)))

         ;; Case 3: Multiple predecessors - prompt user
         (t
          (let* ((predecessor-choices
                  (mapcar (lambda (pred-id)
                            (let* ((marker (org-id-find pred-id t))
                                   (title (org-with-point-at marker
                                            (org-get-heading t t t t))))
                              (cons title pred-id)))
                          predecessors))
                 (chosen-title (completing-read
                                "Insert between which blocker and selected task? "
                                predecessor-choices
                                nil t))
                 (chosen-predecessor-id (cdr (assoc chosen-title predecessor-choices))))
            ;; Remove: chosen-predecessor → selected
            (org-gtd-remove-from-multivalued-property chosen-predecessor-id org-gtd-prop-blocks selected-id)
            (org-gtd-remove-from-multivalued-property selected-id org-gtd-prop-depends-on chosen-predecessor-id)
            ;; Add: chosen-predecessor → new
            (org-gtd-dependencies-create chosen-predecessor-id new-task-id)
            ;; Add: new → selected
            (org-gtd-dependencies-create new-task-id selected-id))))

        (message "Created predecessor task: %s" title)
        (org-gtd-graph-view-refresh)))))

(defun org-gtd-graph-insert-after ()
  "Insert new task as successor to selected task.
Prompts for which successor to rewire if selected task has multiple successors."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         ;; Filter out project ID from successors (virtual edge for visualization)
         (successors (seq-remove (lambda (id) (string= id project-id))
                                 (org-gtd-graph-data-get-successors graph selected-id)))
         (choices (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select or create successor task: "
                   org-gtd-graph-view--project-marker))
         (selected (completing-read "Select or create successor task: " choices nil nil))
         (match (assoc-string selected choices))
         (existing-id (if match (cdr match) nil))
         (title (if match selected selected)))

    (when title
      ;; Get or create task
      (let ((new-task-id existing-id))
        (if existing-id
            ;; Link existing task
            (org-with-point-at (org-id-find existing-id t)
              (org-gtd-add-to-multivalued-property existing-id org-gtd-prop-project-ids project-id)
              (save-buffer))

          ;; Create new task
          (org-with-point-at org-gtd-graph-view--project-marker
            (org-end-of-subtree t t)
            (unless (bolp) (insert "\n"))
            (insert "** " title "\n")
            (forward-line -1)
            (org-back-to-heading t)
            (setq new-task-id (org-id-get-create))
            (org-todo "TODO")
            (org-entry-put (point) "ORG_GTD" "Actions")
            (when project-id
              (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id))
            (save-buffer)))

        ;; Handle different cases based on successors
        (cond
         ;; Case 1: No successors (leaf task) - simple add
         ((null successors)
          ;; Create dependency: selected → new
          (org-gtd-dependencies-create selected-id new-task-id))

         ;; Case 2: One successor - straightforward rewiring
         ((= 1 (length successors))
          (let ((successor-id (car successors)))
            ;; Remove: selected → successor
            (org-gtd-remove-from-multivalued-property selected-id org-gtd-prop-blocks successor-id)
            (org-gtd-remove-from-multivalued-property successor-id org-gtd-prop-depends-on selected-id)
            ;; Add: selected → new
            (org-gtd-dependencies-create selected-id new-task-id)
            ;; Add: new → successor
            (org-gtd-dependencies-create new-task-id successor-id)))

         ;; Case 3: Multiple successors - prompt user
         (t
          (let* ((successor-choices
                  (mapcar (lambda (succ-id)
                            (let* ((marker (org-id-find succ-id t))
                                   (title (org-with-point-at marker
                                            (org-get-heading t t t t))))
                              (cons title succ-id)))
                          successors))
                 (chosen-title (completing-read
                                "Insert between selected and which successor task? "
                                successor-choices
                                nil t))
                 (chosen-successor-id (cdr (assoc chosen-title successor-choices))))
            ;; Remove: selected → chosen-successor
            (org-gtd-remove-from-multivalued-property selected-id org-gtd-prop-blocks chosen-successor-id)
            (org-gtd-remove-from-multivalued-property chosen-successor-id org-gtd-prop-depends-on selected-id)
            ;; Add: selected → new
            (org-gtd-dependencies-create selected-id new-task-id)
            ;; Add: new → chosen-successor
            (org-gtd-dependencies-create new-task-id chosen-successor-id))))

        (message "Created successor task: %s" title)
        (org-gtd-graph-view-refresh)))))

(defun org-gtd-graph--modify-blockers-internal (task-id new-blocker-ids project-marker)
  "Set TASK-ID's blockers to exactly NEW-BLOCKER-IDS in PROJECT-MARKER's context.
Adds new blockers and removes old ones to match the new list.
If NEW-BLOCKER-IDS is empty, adds task to project's FIRST_TASKS."
  (let* ((current-blockers (org-gtd-get-task-dependencies task-id))
         (to-add (cl-set-difference new-blocker-ids current-blockers :test 'equal))
         (to-remove (cl-set-difference current-blockers new-blocker-ids :test 'equal)))

    ;; Add new blockers
    (dolist (blocker-id to-add)
      (org-gtd-dependencies-create blocker-id task-id))

    ;; Remove old blockers
    (dolist (blocker-id to-remove)
      (org-gtd-remove-from-multivalued-property blocker-id org-gtd-prop-blocks task-id)
      (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-depends-on blocker-id))

    ;; Update FIRST_TASKS for the current project based on new blocker state
    (org-with-point-at project-marker
      (if (null new-blocker-ids)
          ;; No blockers: add to FIRST_TASKS
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        ;; Has blockers: remove from FIRST_TASKS
        (org-entry-remove-from-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id))
      (save-buffer))))

;; State for transient toggle menus
(defvar org-gtd-graph--modify-state nil
  "Alist of (TASK-ID . ENABLED) for transient modify menus.")

(transient-define-prefix org-gtd-graph-modify-blockers-menu ()
  "Toggle which tasks block the selected task."
  :refresh-suffixes t  ;; Recreate suffixes after every toggle to update checkboxes
  [:description org-gtd-graph--modify-blockers-description
   :class transient-columns
   :setup-children org-gtd-graph--modify-blockers-setup]
  ["Actions"
   ("RET" "Apply changes" org-gtd-graph--modify-blockers-apply)
   ("q" "Quit without saving" transient-quit-one)])

(defun org-gtd-graph--modify-blockers-description ()
  "Generate description for modify-blockers menu."
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (node (org-gtd-graph-data-get-node graph selected-id)))
    (format "Modify blockers for: %s" (org-gtd-graph-node-title node))))

(defun org-gtd-graph--modify-blockers-setup (_)
  "Setup children for modify-blockers transient."
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         (all-task-ids (cl-remove selected-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (key-char ?a))

    ;; Create toggle suffixes - return list of group vectors for transient-columns
    ;; Note: State is initialized in org-gtd-graph-modify-blockers, not here
    ;; Pass lambda function objects directly - transient will handle creating gensyms
    (transient-parse-suffixes
     'org-gtd-graph-modify-blockers-menu
     (list
      (vconcat
       (mapcar (lambda (task-id)
                (let* ((node (org-gtd-graph-data-get-node graph task-id))
                       (title (org-gtd-graph-node-title node))
                       (key (char-to-string key-char))
                       (enabled (cdr (assoc task-id org-gtd-graph--modify-state)))
                       (display (if enabled
                                   (format "[X] %s" title)
                                 (format "[ ] %s" title)))
                       ;; Create closure that captures task-id
                       ;; :transient t and :refresh-suffixes t will handle staying open and refreshing
                       (toggle-fn (let ((id task-id))
                                    (lambda ()
                                      (interactive)
                                      (let ((current (cdr (assoc id org-gtd-graph--modify-state))))
                                        (setf (alist-get id org-gtd-graph--modify-state nil nil #'equal)
                                              (not current)))))))
                  (setq key-char (1+ key-char))
                  ;; Pass the lambda function object directly, with :transient t to stay open
                  (list key display toggle-fn :transient t)))
              valid-candidates))))))


(defun org-gtd-graph--modify-blockers-apply (&rest _)
  "Apply the blocker changes from transient state."
  (interactive)
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (new-blockers (mapcar #'car
                              (seq-filter #'cdr org-gtd-graph--modify-state))))
    (org-gtd-graph--modify-blockers-internal selected-id new-blockers org-gtd-graph-view--project-marker)
    (message "Updated blockers")
    (org-gtd-graph-view-refresh)
    (transient-quit-one)))

(defun org-gtd-graph-modify-blockers ()
  "Modify tasks that block the selected task (transient menu)."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))
  ;; Initialize state before opening transient
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         (current-blockers (seq-remove (lambda (id) (string= id project-id))
                                       (org-gtd-graph-data-get-predecessors graph selected-id)))
         (all-task-ids (cl-remove selected-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids)))
    (setq org-gtd-graph--modify-state
          (mapcar (lambda (task-id)
                   (cons task-id (and (member task-id current-blockers) t)))
                 valid-candidates)))
  (with-no-warnings
    (org-gtd-graph-modify-blockers-menu)))

(defun org-gtd-graph--modify-successors-internal (task-id new-successor-ids project-marker)
  "Set TASK-ID's successors in PROJECT-MARKER's context to NEW-SUCCESSOR-IDS.
Adds new successors and removes old ones to match the new list.
Updates FIRST_TASKS for affected successors based on their blocker status."
  (let* ((current-successors (org-gtd-get-task-blockers task-id))
         (to-add (cl-set-difference new-successor-ids current-successors :test 'equal))
         (to-remove (cl-set-difference current-successors new-successor-ids :test 'equal)))

    ;; Add new successors (task-id blocks successor)
    (dolist (successor-id to-add)
      (org-gtd-dependencies-create task-id successor-id)
      ;; Successor now has a blocker, remove from FIRST_TASKS in current project
      (org-with-point-at project-marker
        (org-entry-remove-from-multivalued-property (point) "ORG_GTD_FIRST_TASKS" successor-id)
        (save-buffer)))

    ;; Remove old successors
    (dolist (successor-id to-remove)
      (org-gtd-remove-from-multivalued-property task-id org-gtd-prop-blocks successor-id)
      (org-gtd-remove-from-multivalued-property successor-id org-gtd-prop-depends-on task-id)
      ;; Check if successor now has no blockers in current project
      (let ((successor-blockers (org-gtd-get-task-dependencies successor-id)))
        (when (null successor-blockers)
          ;; Successor has no blockers, add to FIRST_TASKS in current project
          (org-with-point-at project-marker
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" successor-id)
            (save-buffer)))))))

(transient-define-prefix org-gtd-graph-modify-successors-menu ()
  "Toggle which tasks are blocked by the selected task."
  :refresh-suffixes t  ;; Recreate suffixes after every toggle to update checkboxes
  [:description org-gtd-graph--modify-successors-description
   :class transient-columns
   :setup-children org-gtd-graph--modify-successors-setup]
  ["Actions"
   ("RET" "Apply changes" org-gtd-graph--modify-successors-apply)
   ("q" "Quit without saving" transient-quit-one)])

(defun org-gtd-graph--modify-successors-description ()
  "Generate description for modify-successors menu."
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (node (org-gtd-graph-data-get-node graph selected-id)))
    (format "Modify tasks blocked by: %s" (org-gtd-graph-node-title node))))

(defun org-gtd-graph--modify-successors-setup (_)
  "Setup children for modify-successors transient."
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         (all-task-ids (cl-remove selected-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (key-char ?a))

    ;; Create toggle suffixes - return list of group vectors for transient-columns
    ;; Note: State is initialized in org-gtd-graph-modify-successors, not here
    ;; Pass lambda function objects directly - transient will handle creating gensyms
    (transient-parse-suffixes
     'org-gtd-graph-modify-successors-menu
     (list
      (vconcat
       (mapcar (lambda (task-id)
                (let* ((node (org-gtd-graph-data-get-node graph task-id))
                       (title (org-gtd-graph-node-title node))
                       (key (char-to-string key-char))
                       (enabled (cdr (assoc task-id org-gtd-graph--modify-state)))
                       (display (if enabled
                                   (format "[X] %s" title)
                                 (format "[ ] %s" title)))
                       ;; Create closure that captures task-id
                       ;; :transient t and :refresh-suffixes t will handle staying open and refreshing
                       (toggle-fn (let ((id task-id))
                                    (lambda ()
                                      (interactive)
                                      (let ((current (cdr (assoc id org-gtd-graph--modify-state))))
                                        (setf (alist-get id org-gtd-graph--modify-state nil nil #'equal)
                                              (not current)))))))
                  (setq key-char (1+ key-char))
                  ;; Pass the lambda function object directly, with :transient t to stay open
                  (list key display toggle-fn :transient t)))
              valid-candidates))))))

(defun org-gtd-graph--modify-successors-apply (&rest _)
  "Apply the successor changes from transient state."
  (interactive)
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (new-successors (mapcar #'car
                                (seq-filter #'cdr org-gtd-graph--modify-state))))
    (org-gtd-graph--modify-successors-internal selected-id new-successors org-gtd-graph-view--project-marker)
    (message "Updated successors")
    (org-gtd-graph-view-refresh)
    (transient-quit-one)))

(defun org-gtd-graph-modify-successors ()
  "Modify tasks that the selected task blocks (transient menu)."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))
  ;; Initialize state before opening transient
  (let* ((selected-id org-gtd-graph-ui--selected-node-id)
         (graph (org-gtd-graph-data--extract-from-project org-gtd-graph-view--project-marker))
         (project-id (org-with-point-at org-gtd-graph-view--project-marker
                       (org-entry-get (point) "ID")))
         (current-successors (org-gtd-graph-data-get-successors graph selected-id))
         (all-task-ids (cl-remove selected-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids)))
    (setq org-gtd-graph--modify-state
          (mapcar (lambda (task-id)
                   (cons task-id (and (member task-id current-successors) t)))
                 valid-candidates)))
  (with-no-warnings
    (org-gtd-graph-modify-successors-menu)))

(defun org-gtd-graph-remove-task ()
  "Remove task from project with intelligent rewiring.
Connects predecessors to successors before removal.

Offers context-appropriate choices:
- If task only in current project: remove and keep independent
- If task in multiple projects: remove from current only, or all projects"
  (interactive)
  (require 'org-gtd-graph-view)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((task-id org-gtd-graph-ui--selected-node-id)
         (task-title (when-let ((graph org-gtd-graph-view--graph)
                                (node (org-gtd-graph-data-get-node graph task-id)))
                       (org-gtd-graph-node-title node)))
         (projects (org-gtd-get-task-projects task-id))
         (project-count (length projects))
         (current-project-id (when org-gtd-graph-view--project-marker
                               (org-with-point-at org-gtd-graph-view--project-marker
                                 (org-entry-get (point) "ID"))))
         choice)

    (cond
     ;; Task only in one project (current one)
     ((= project-count 1)
      (setq choice (completing-read
                    (format "Task '%s' only in this project: " task-title)
                    '("Remove from this project and keep as independent item")
                    nil t)))

     ;; Task in multiple projects
     ((> project-count 1)
      (setq choice (completing-read
                    (format "Task '%s' is in %d projects: " task-title project-count)
                    '("Remove from this project only"
                      "Remove from all projects and keep as independent item")
                    nil t)))

     ;; Task not in any project (shouldn't happen, but handle gracefully)
     (t
      (user-error "Task '%s' is not part of any project" task-title)))

    ;; Execute chosen operation
    (pcase choice
      ("Remove from this project only"
       (org-gtd-graph--remove-from-project task-id current-project-id)
       (message "Removed '%s' from current project" task-title))

      ((or "Remove from this project and keep as independent item"
           "Remove from all projects and keep as independent item")
       (org-gtd-graph--keep-as-independent task-id)
       (message "Removed '%s' from all projects (kept as independent)" task-title)))

    ;; Refresh the graph view
    (org-gtd-graph-view-refresh)))

(defun org-gtd-graph-trash-task ()
  "Trash selected task: remove from all projects and mark as canceled.
Prompts for confirmation before trashing."
  (interactive)
  (require 'org-gtd-graph-view)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((task-id org-gtd-graph-ui--selected-node-id)
         (task-title (when-let ((graph org-gtd-graph-view--graph)
                                (node (org-gtd-graph-data-get-node graph task-id)))
                       (org-gtd-graph-node-title node))))

    (when (yes-or-no-p (format "Trash task '%s'?  This will remove it from all projects and mark it as canceled.  Are you sure?" task-title))
      (org-gtd-graph--trash-task task-id)
      (message "Trashed task '%s'" task-title)
      (org-gtd-graph-view-refresh))))

(defun org-gtd-graph-change-state ()
  "Change TODO state of selected task."
  (interactive)
  (unless org-gtd-graph-ui--selected-node-id
    (user-error "No node selected"))

  (let* ((task-id org-gtd-graph-ui--selected-node-id)
         (marker (org-id-find task-id t)))
    (unless marker
      (user-error "Cannot find task with ID: %s" task-id))

    (org-with-point-at marker
      (call-interactively 'org-todo)
      (save-buffer))

    (message "Changed TODO state")
    (org-gtd-graph-view-refresh)))

(defun org-gtd-graph-incubate-project ()
  "Incubate the current project being viewed in graph mode.

Calls org-gtd-tickler which will detect it's on a project heading
and move the entire project with all its tasks to the tickler."
  (interactive)
  (org-with-point-at org-gtd-graph-view--project-marker
    (with-no-warnings
      (call-interactively #'org-gtd-tickler))))

;;;; Footer

(provide 'org-gtd-graph-transient)

;;; org-gtd-graph-transient.el ends here
