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
(require 'org-gtd-tickler)
(require 'org-gtd-projects)
(require 'org-gtd-context)

;;;; Sticky Mode

(defvar-local org-gtd-graph-transient-sticky nil
  "When non-nil, keep transient menu open after commands.")

(transient-define-infix org-gtd-graph-transient--sticky ()
  "Toggle sticky mode for graph transient menu."
  :class 'transient-lisp-variable
  :variable 'org-gtd-graph-transient-sticky
  :reader (lambda (&rest _) (not org-gtd-graph-transient-sticky))
  :description (lambda () (if org-gtd-graph-transient-sticky
                              "[X] Sticky mode"
                            "[ ] Sticky mode"))
  :key "=")

(defun org-gtd-graph-transient--resume-if-sticky ()
  "Re-open main transient if sticky mode is enabled.
Call this at the end of sub-transient apply functions."
  (when org-gtd-graph-transient-sticky
    (run-at-time 0 nil #'org-gtd-graph-transient-main)))

(defun org-gtd-graph-transient--do-sticky ()
  "Pre-command that stays in transient if sticky mode is enabled.
Returns `transient--stay' when sticky mode is on, exits properly otherwise.
Named with \"--do-\" so transient.el recognizes it as a pre-command function."
  (if org-gtd-graph-transient-sticky
      transient--stay
    ;; Must call cleanup functions like transient--do-exit does
    ;; (only when actually in a transient context)
    (when transient--prefix
      (transient--export)
      (transient--stack-zap))
    transient--exit))

;;;; Main Transient Menu

;;;###autoload (autoload 'org-gtd-graph-transient-main "org-gtd-graph-transient" nil t)
(transient-define-prefix org-gtd-graph-transient-main ()
  "Main command menu for GTD project graph view."
  :transient-suffix 'org-gtd-graph-transient--do-sticky
  [:description
   (lambda () (org-gtd-graph-transient--show-selected-context))
   :class transient-row]
  [["Add Tasks"
    ("r" "Add root task" org-gtd-graph-transient-add-root)
    ("s" "Add successor" org-gtd-graph-add-successor)
    ("b" "Add blocker" org-gtd-graph-add-blocker)]
   ["Modify Relationships"
    ("B" "Modify blockers" org-gtd-graph-modify-blockers)
    ("S" "Modify successors" org-gtd-graph-modify-successors)]
   ["Task Operations (t prefix)"
    ("t t" "Change TODO state" org-gtd-graph-change-state)
    ("t r" "Remove from project" org-gtd-graph-remove-task)
    ("t d" "Trash task" org-gtd-graph-trash-task)
    ("t e" "Edit in org file" org-gtd-graph-ui-jump-to-task :transient nil)
    ("t i" "Show relationships" org-gtd-graph-view-show-relationships)]]
  [["Navigation"
    ("n" "Next successor" org-gtd-graph-nav-down-dependency :transient t)
    ("p" "Previous blocker" org-gtd-graph-nav-up-dependency :transient t)
    ("TAB" "Next sibling" org-gtd-graph-nav-next-sibling :transient t)
    ("G" "Goto task" org-gtd-graph-nav-goto)]
   ["View"
    ("v" "Toggle ASCII/SVG" org-gtd-graph-toggle-render-mode :transient t)
    ("g" "Refresh" org-gtd-graph-view-refresh :transient t)]
   ["Export (x prefix)"
    ("x s" "Export as SVG" org-gtd-graph-export-svg)
    ("x d" "Export as DOT" org-gtd-graph-export-dot)
    ("x a" "Export as ASCII" org-gtd-graph-export-ascii)]
   ["Session"
    ("I" "Incubate project" org-gtd-graph-incubate-project :transient nil)
    ("C" "Cancel project" org-gtd-graph-cancel-project :transient nil)
    ("q" "Quit menu" transient-quit-one :transient nil)
    ("Q" "Quit and kill buffer" org-gtd-graph-quit-and-kill :transient nil)
    (org-gtd-graph-transient--sticky)]])

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
      (org-gtd-projects-fix-todo-keywords org-gtd-graph-view--project-marker)
      ;; Only refresh if we're actually in graph view mode
      (when (derived-mode-p 'org-gtd-graph-view-mode)
        (org-gtd-graph-view-refresh)))))

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
            (org-gtd-remove-from-multivalued-property succ org-gtd-prop-depends-on task-id))))

      ;; Fix TODO keywords
      (org-gtd-projects-fix-todo-keywords project-marker))))

(defun org-gtd-graph--keep-as-independent (task-id)
  "Remove TASK-ID from all projects and clear all project-related properties.
The task becomes a standalone item with no project associations or dependencies."
  (let ((projects (org-gtd-get-task-projects task-id))
        (all-blockers (org-gtd-get-task-blockers task-id))
        (all-dependencies (org-gtd-get-task-dependencies task-id)))

    ;; For each project: rewire graph and update FIRST_TASKS
    (dolist (project-id projects)
      (when-let ((project-marker (org-id-find project-id t)))
        (let* ((graph (org-gtd-graph-data--extract-from-project project-marker))
               (predecessors (org-gtd-graph-data-get-predecessors graph task-id))
               (successors (org-gtd-graph-data-get-successors graph task-id)))

          ;; Rewire: connect predecessors to successors
          (dolist (pred predecessors)
            (dolist (succ successors)
              (unless (member succ (org-gtd-get-task-blockers pred))
                (org-gtd-dependencies-create pred succ))))

          ;; Promote successors to root if they have no other predecessors
          (dolist (succ successors)
            (let* ((succ-preds (org-gtd-graph-data-get-predecessors graph succ))
                   (other-preds (cl-remove task-id succ-preds :test 'equal)))
              (when (null other-preds)
                (org-with-point-at project-marker
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" succ)
                  (save-buffer)))))

          ;; Remove task from project's FIRST_TASKS
          (org-gtd-remove-from-multivalued-property project-id "ORG_GTD_FIRST_TASKS" task-id)

          ;; Fix TODO keywords for this project
          (org-gtd-projects-fix-todo-keywords project-marker))))

    ;; Clear task from all its blockers' DEPENDS_ON
    (dolist (blocker-id all-blockers)
      (org-gtd-remove-from-multivalued-property blocker-id org-gtd-prop-depends-on task-id))

    ;; Clear task from all its dependencies' BLOCKS
    (dolist (dep-id all-dependencies)
      (org-gtd-remove-from-multivalued-property dep-id org-gtd-prop-blocks task-id))

    ;; Clear all project-related properties from the task itself
    (when-let ((marker (org-id-find task-id t)))
      (org-with-point-at marker
        (org-entry-delete (point) org-gtd-prop-project-ids)
        (org-entry-delete (point) org-gtd-prop-blocks)
        (org-entry-delete (point) org-gtd-prop-depends-on)
        (save-buffer)))))

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

    ;; Add unblocked successors to FIRST_TASKS for each project
    ;; This is needed because after dependency cleanup, some successors may have no blockers
    (dolist (succ all-successors)
      (let ((succ-projects (org-gtd-get-task-projects succ))
            (succ-deps (org-gtd-get-task-dependencies succ)))
        (when (null succ-deps)
          ;; Successor has no blockers, add to FIRST_TASKS for all its projects
          (dolist (proj-id succ-projects)
            (when-let ((proj-marker (org-id-find proj-id t)))
              (org-with-point-at proj-marker
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" succ)
                (save-buffer)))))))

    ;; Step 3: Fix TODO keywords for all affected projects
    ;; Must be done AFTER dependency cleanup so tasks see correct blocker state
    (dolist (project-id projects)
      (when-let ((project-marker (org-id-find project-id t)))
        (org-gtd-projects-fix-todo-keywords project-marker)))

    ;; Mark as canceled
    (when-let ((marker (org-id-find task-id t)))
      (org-with-point-at marker
        (org-todo (org-gtd-keywords--canceled))
        (save-buffer)))))

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
      (save-buffer))

    ;; Fix TODO keywords
    (org-gtd-projects-fix-todo-keywords project-marker)))

;;;; Custom Transient Prefix Class
;;
;; Use a custom transient prefix class to store mutable state (edge-selection)
;; properly on the transient object rather than using global variables.
;; Access via (oref transient--prefix edge-selection) in :setup-children,
;; or (oset (transient-prefix-object) edge-selection ...) in suffix commands.

(defclass org-gtd-graph-transient-prefix (transient-prefix)
  ((edge-selection :initarg :edge-selection :initform nil
                   :documentation "Alist of (TASK-ID . SELECTED-P) for multi-select operations."))
  "Custom transient prefix for graph operations with edge selection state.")

(defun org-gtd-graph-modify-blockers ()
  "Modify tasks that block the selected task (transient menu)."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (unless (eq (org-gtd-context-mode ctx) 'graph-view)
      (user-error "This command requires graph view"))
    (unless (org-gtd-context-task-id ctx)
      (user-error "No task selected"))

    (let* ((task-id (org-gtd-context-task-id ctx))
           (edge-selection (org-gtd-graph--build-modify-blockers-selection ctx task-id)))
      ;; Invoke transient with scope and edge-selection on custom prefix object
      (transient-setup 'org-gtd-graph-modify-blockers-menu nil nil
                       :scope (list :ctx ctx
                                    :task-id task-id)
                       :edge-selection edge-selection))))

(transient-define-prefix org-gtd-graph-modify-blockers-menu ()
  "Toggle which tasks block the selected task."
  :class 'org-gtd-graph-transient-prefix
  :refresh-suffixes t
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (ctx (plist-get scope :ctx))
            (task-id (plist-get scope :task-id))
            (project-marker (org-gtd-context-project-marker ctx))
            (graph (org-gtd-graph-data--extract-from-project project-marker))
            (node (org-gtd-graph-data-get-node graph task-id)))
       (format "Modify blockers for: %s"
               (propertize (org-gtd-graph-node-title node) 'face 'bold))))
   :class transient-columns
   :setup-children org-gtd-graph--modify-blockers-setup]
  [["Actions"
    ("RET" "Apply changes" org-gtd-graph--modify-blockers-apply)
    ("q" "Quit without saving" transient-quit-one)]])

(defun org-gtd-graph--modify-blockers-setup (_)
  "Setup children for modify-blockers transient.
Reads from transient scope and prefix object's edge-selection slot."
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (cl-remove task-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (edge-selection (oref transient--prefix edge-selection))
         (key-char ?a))

    (transient-parse-suffixes
     'org-gtd-graph-modify-blockers-menu
     (list
      (vconcat
       (mapcar (lambda (candidate-id)
                 (let* ((node (org-gtd-graph-data-get-node graph candidate-id))
                        (title (org-gtd-graph-node-title node))
                        (key (char-to-string key-char))
                        (enabled (cdr (assoc candidate-id edge-selection)))
                        (display (if enabled
                                     (format "[X] %s" title)
                                   (format "[ ] %s" title)))
                        (toggle-fn (let ((id candidate-id))
                                     (lambda ()
                                       (interactive)
                                       (let* ((prefix (transient-prefix-object))
                                              (sel (oref prefix edge-selection))
                                              (current (cdr (assoc id sel))))
                                         (setf (alist-get id sel nil nil #'equal) (not current))
                                         (oset prefix edge-selection sel))))))
                   (setq key-char (1+ key-char))
                   (list key display toggle-fn :transient t)))
               valid-candidates))))))

(defun org-gtd-graph--modify-blockers-apply ()
  "Apply the blocker changes using transient scope and prefix object."
  (interactive)
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (project-marker (org-gtd-context-project-marker ctx))
         (edge-selection (oref (transient-prefix-object) edge-selection))
         (new-blockers (mapcar #'car (seq-filter #'cdr edge-selection))))
    (org-gtd-graph--modify-blockers-internal task-id new-blockers project-marker)
    (message "Updated blockers")
    (org-gtd-projects-fix-todo-keywords project-marker)
    (org-gtd-graph-view-refresh)
    (transient-quit-one)
    (org-gtd-graph-transient--resume-if-sticky)))

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
            (save-buffer)))))

    ;; Fix TODO keywords
    (org-gtd-projects-fix-todo-keywords project-marker)))

(defun org-gtd-graph-modify-successors ()
  "Modify tasks that the selected task blocks (transient menu)."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (unless (eq (org-gtd-context-mode ctx) 'graph-view)
      (user-error "This command requires graph view"))
    (unless (org-gtd-context-task-id ctx)
      (user-error "No task selected"))

    (let* ((task-id (org-gtd-context-task-id ctx))
           (edge-selection (org-gtd-graph--build-modify-successors-selection ctx task-id)))
      ;; Invoke transient with scope and edge-selection on custom prefix object
      (transient-setup 'org-gtd-graph-modify-successors-menu nil nil
                       :scope (list :ctx ctx
                                    :task-id task-id)
                       :edge-selection edge-selection))))

(transient-define-prefix org-gtd-graph-modify-successors-menu ()
  "Toggle which tasks are blocked by the selected task."
  :class 'org-gtd-graph-transient-prefix
  :refresh-suffixes t
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (ctx (plist-get scope :ctx))
            (task-id (plist-get scope :task-id))
            (project-marker (org-gtd-context-project-marker ctx))
            (graph (org-gtd-graph-data--extract-from-project project-marker))
            (node (org-gtd-graph-data-get-node graph task-id)))
       (format "Modify tasks blocked by: %s"
               (propertize (org-gtd-graph-node-title node) 'face 'bold))))
   :class transient-columns
   :setup-children org-gtd-graph--modify-successors-setup]
  [["Actions"
    ("RET" "Apply changes" org-gtd-graph--modify-successors-apply)
    ("q" "Quit without saving" transient-quit-one)]])

(defun org-gtd-graph--modify-successors-setup (_)
  "Setup children for modify-successors transient.
Reads from transient scope and prefix object's edge-selection slot."
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (cl-remove task-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (edge-selection (oref transient--prefix edge-selection))
         (key-char ?a))

    (transient-parse-suffixes
     'org-gtd-graph-modify-successors-menu
     (list
      (vconcat
       (mapcar (lambda (candidate-id)
                 (let* ((node (org-gtd-graph-data-get-node graph candidate-id))
                        (title (org-gtd-graph-node-title node))
                        (key (char-to-string key-char))
                        (enabled (cdr (assoc candidate-id edge-selection)))
                        (display (if enabled
                                     (format "[X] %s" title)
                                   (format "[ ] %s" title)))
                        (toggle-fn (let ((id candidate-id))
                                     (lambda ()
                                       (interactive)
                                       (let* ((prefix (transient-prefix-object))
                                              (sel (oref prefix edge-selection))
                                              (current (cdr (assoc id sel))))
                                         (setf (alist-get id sel nil nil #'equal) (not current))
                                         (oset prefix edge-selection sel))))))
                   (setq key-char (1+ key-char))
                   (list key display toggle-fn :transient t)))
               valid-candidates))))))

(defun org-gtd-graph--modify-successors-apply ()
  "Apply the successor changes using transient scope and prefix object."
  (interactive)
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (project-marker (org-gtd-context-project-marker ctx))
         (edge-selection (oref (transient-prefix-object) edge-selection))
         (new-successors (mapcar #'car (seq-filter #'cdr edge-selection))))
    (org-gtd-graph--modify-successors-internal task-id new-successors project-marker)
    (message "Updated successors")
    (org-gtd-projects-fix-todo-keywords project-marker)
    (org-gtd-graph-view-refresh)
    (transient-quit-one)
    (org-gtd-graph-transient--resume-if-sticky)))

(defun org-gtd-graph-remove-task ()
  "Remove task from current project with intelligent rewiring.
Connects predecessors to successors before removal.
Prompts for confirmation before removing."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-graph-remove-task-from-context ctx)))

(defun org-gtd-graph-remove-task-from-context (ctx)
  "Remove task from project using context CTX.
Graph-view specific: refreshes view after removal."
  (require 'org-gtd-graph-view)
  (let ((task-id (org-gtd-context-task-id ctx))
        (project-id (org-gtd-context-project-id ctx))
        (project-marker (org-gtd-context-project-marker ctx)))
    (unless task-id
      (user-error "No task selected"))
    (let ((task-title (when-let ((graph org-gtd-graph-view--graph)
                                 (node (org-gtd-graph-data-get-node graph task-id)))
                        (org-gtd-graph-node-title node))))
      (when (yes-or-no-p (format "Remove '%s' from this project? " task-title))
        (org-gtd-graph--remove-from-project task-id project-id)
        (message "Removed '%s' from project" task-title)
        ;; Refresh the graph view
        (org-gtd-projects-fix-todo-keywords project-marker)
        (when (derived-mode-p 'org-gtd-graph-view-mode)
          (org-gtd-graph-view-refresh))))))

(defun org-gtd-graph-trash-task ()
  "Trash selected task: remove from all projects and mark as canceled.
Prompts for confirmation before trashing."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-graph-trash-task-from-context ctx)))

(defun org-gtd-graph-trash-task-from-context (ctx)
  "Trash task using context CTX.
Graph-view specific: refreshes view after trashing."
  (require 'org-gtd-graph-view)
  (let ((task-id (org-gtd-context-task-id ctx))
        (project-marker (org-gtd-context-project-marker ctx)))
    (unless task-id
      (user-error "No task selected"))
    (let ((task-title (when-let ((graph org-gtd-graph-view--graph)
                                 (node (org-gtd-graph-data-get-node graph task-id)))
                        (org-gtd-graph-node-title node))))
      (when (yes-or-no-p (format "Trash task '%s'? This removes it from all projects and marks it as canceled. " task-title))
        (org-gtd-graph--trash-task task-id)
        (message "Trashed task '%s'" task-title)
        (org-gtd-projects-fix-todo-keywords project-marker)
        (when (derived-mode-p 'org-gtd-graph-view-mode)
          (org-gtd-graph-view-refresh))))))

(defun org-gtd-graph-change-state ()
  "Change TODO state of selected task."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (org-gtd-graph-change-state-from-context ctx)))

(defun org-gtd-graph-change-state-from-context (ctx)
  "Change TODO state of task using context CTX.
Graph-view specific: refreshes view after state change."
  (let ((task-id (org-gtd-context-task-id ctx)))
    (unless task-id
      (user-error "No task selected"))
    (let ((marker (org-id-find task-id t)))
      (unless marker
        (user-error "Cannot find task with ID: %s" task-id))
      (org-with-point-at marker
        (call-interactively #'org-todo)
        (save-buffer))
      (message "Changed TODO state")
      (when (derived-mode-p 'org-gtd-graph-view-mode)
        (org-gtd-graph-view-refresh)))))

(defun org-gtd-graph-incubate-project ()
  "Incubate the current project being viewed in graph mode.
Prompts for a review date, then closes the graph view."
  (interactive)
  (org-gtd-project-incubate-from-context)
  ;; Clean up details pane before quitting
  (org-gtd-graph-ui-cleanup-windows)
  (quit-window))

(defun org-gtd-graph-cancel-project ()
  "Cancel the current project being viewed in graph mode.
Prompts for confirmation, then closes the graph view."
  (interactive)
  (org-gtd-project-cancel-from-context)
  ;; Clean up details pane before quitting
  (org-gtd-graph-ui-cleanup-windows)
  (quit-window))

;;;; Unified Add Commands

;; Note: add-successor and add-blocker both use :scope pattern now

(defun org-gtd-graph-add-successor ()
  "Add a successor task that blocks project task(s).
Step 1: Select existing task or create new one.
Step 2: Select which project tasks the new task should block."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (unless (eq (org-gtd-context-mode ctx) 'graph-view)
      (user-error "This command requires graph view for multi-select"))

    ;; Step 1: Select or create task
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select successor task: "
                     (org-gtd-context-project-marker ctx)))
           (selected (completing-read "Select or create successor task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected))
           (edge-selection (org-gtd-graph--build-predecessor-selection ctx)))

      ;; Invoke transient with scope and edge-selection on custom prefix object
      (transient-setup 'org-gtd-graph-add-successor-menu nil nil
                       :scope (list :ctx ctx
                                    :task-id existing-id
                                    :task-title title)
                       :edge-selection edge-selection))))

(defun org-gtd-graph--build-predecessor-selection (ctx)
  "Build edge selection alist for predecessor selection.
CTX is the context struct.
Pre-selects the currently selected task if any."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (hash-table-keys (org-gtd-graph-nodes graph)))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (selected-id (org-gtd-context-task-id ctx)))
    (mapcar (lambda (task-id)
              (cons task-id (and selected-id (string= task-id selected-id))))
            valid-candidates)))

(defun org-gtd-graph--build-blocked-task-selection (ctx)
  "Build edge selection alist for blocked task selection.
CTX is the context struct.
Pre-selects the currently selected task if any."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (hash-table-keys (org-gtd-graph-nodes graph)))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (selected-id (org-gtd-context-task-id ctx)))
    (mapcar (lambda (task-id)
              (cons task-id (and selected-id (string= task-id selected-id))))
            valid-candidates)))

(defun org-gtd-graph--build-modify-blockers-selection (ctx task-id)
  "Build edge selection alist for modifying blockers of TASK-ID.
CTX is the context struct.
Pre-selects current blockers of the task."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (cl-remove task-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (current-blockers (seq-remove (lambda (id) (string= id project-id))
                                       (org-gtd-graph-data-get-predecessors graph task-id))))
    (mapcar (lambda (candidate-id)
              (cons candidate-id (and (member candidate-id current-blockers) t)))
            valid-candidates)))

(defun org-gtd-graph--build-modify-successors-selection (ctx task-id)
  "Build edge selection alist for modifying successors of TASK-ID.
CTX is the context struct.
Pre-selects current successors of the task."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (cl-remove task-id
                                  (hash-table-keys (org-gtd-graph-nodes graph))
                                  :test 'string=))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (current-successors (org-gtd-graph-data-get-successors graph task-id)))
    (mapcar (lambda (candidate-id)
              (cons candidate-id (and (member candidate-id current-successors) t)))
            valid-candidates)))

(transient-define-prefix org-gtd-graph-add-successor-menu ()
  "Select which tasks should block the new successor (predecessors)."
  :class 'org-gtd-graph-transient-prefix
  :refresh-suffixes t
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (title (plist-get scope :task-title)))
       (format "Adding successor: %s\nSelect predecessors (tasks that must complete first):"
               (propertize title 'face 'bold))))
   :class transient-columns
   :setup-children org-gtd-graph--add-successor-setup]
  [["Actions"
    ("RET" "Confirm" org-gtd-graph--add-successor-apply)
    ("q" "Cancel" transient-quit-one)]])

(defun org-gtd-graph--add-successor-setup (_)
  "Setup children for add-successor transient.
Reads from transient scope and prefix object's edge-selection slot."
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (hash-table-keys (org-gtd-graph-nodes graph)))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (edge-selection (oref transient--prefix edge-selection))
         (key-char ?a))

    (transient-parse-suffixes
     'org-gtd-graph-add-successor-menu
     (list
      (vconcat
       (mapcar (lambda (task-id)
                 (let* ((node (org-gtd-graph-data-get-node graph task-id))
                        (title (org-gtd-graph-node-title node))
                        (key (char-to-string key-char))
                        (enabled (cdr (assoc task-id edge-selection)))
                        (display (if enabled
                                     (format "[X] %s" title)
                                   (format "[ ] %s" title)))
                        (toggle-fn (let ((id task-id))
                                     (lambda ()
                                       (interactive)
                                       (let* ((prefix (transient-prefix-object))
                                              (sel (oref prefix edge-selection))
                                              (current (cdr (assoc id sel))))
                                         (setf (alist-get id sel nil nil #'equal) (not current))
                                         (oset prefix edge-selection sel))))))
                   (setq key-char (1+ key-char))
                   (list key display toggle-fn :transient t)))
               valid-candidates))))))

(defun org-gtd-graph--add-successor-apply ()
  "Apply add-successor using transient scope and prefix object."
  (interactive)
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (task-title (plist-get scope :task-title))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (edge-selection (oref (transient-prefix-object) edge-selection))
         (predecessor-ids (mapcar #'car (seq-filter #'cdr edge-selection)))
         (new-task-id task-id))

    ;; Create new task if needed
    (unless new-task-id
      (org-with-point-at project-marker
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "** " task-title "\n")
        (forward-line -1)
        (org-back-to-heading t)
        (setq new-task-id (org-id-get-create))
        (org-todo "TODO")
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (save-buffer)))

    ;; Create dependencies: each selected task blocks new-task (new task is successor)
    (dolist (predecessor-id predecessor-ids)
      (org-gtd-dependencies-create predecessor-id new-task-id))

    ;; Link existing external task to project
    (when task-id
      (org-gtd-add-to-multivalued-property new-task-id "ORG_GTD_PROJECT_IDS" project-id)
      (org-with-point-at (org-id-find new-task-id t)
        (save-buffer)))

    ;; Note: new successor is NOT added to FIRST_TASKS because it has blockers

    (message "Added successor '%s' after %d task(s)" task-title (length predecessor-ids))
    (org-gtd-projects-fix-todo-keywords project-marker)
    (org-gtd-graph-view-refresh)
    (transient-quit-one)
    (org-gtd-graph-transient--resume-if-sticky)))

(defun org-gtd-graph-add-blocker ()
  "Add a blocker task that blocks project task(s).
Step 1: Select existing task or create new one.
Step 2: Select which project tasks the new task should block."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (unless (eq (org-gtd-context-mode ctx) 'graph-view)
      (user-error "This command requires graph view for multi-select"))

    ;; Step 1: Select or create task
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select blocker task: "
                     (org-gtd-context-project-marker ctx)))
           (selected (completing-read "Select or create blocker task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected))
           (edge-selection (org-gtd-graph--build-blocked-task-selection ctx)))

      ;; Invoke transient with scope and edge-selection on custom prefix object
      (transient-setup 'org-gtd-graph-add-blocker-menu nil nil
                       :scope (list :ctx ctx
                                    :task-id existing-id
                                    :task-title title)
                       :edge-selection edge-selection))))

(transient-define-prefix org-gtd-graph-add-blocker-menu ()
  "Select which tasks the new blocker should block."
  :class 'org-gtd-graph-transient-prefix
  :refresh-suffixes t
  [:description
   (lambda ()
     (let* ((scope (transient-scope))
            (title (plist-get scope :task-title)))
       (format "Adding blocker: %s\nSelect tasks that this blocker will block:"
               (propertize title 'face 'bold))))
   :class transient-columns
   :setup-children org-gtd-graph--add-blocker-setup]
  [["Actions"
    ("RET" "Confirm" org-gtd-graph--add-blocker-apply)
    ("q" "Cancel" transient-quit-one)]])

(defun org-gtd-graph--add-blocker-setup (_)
  "Setup children for add-blocker transient.
Reads from transient scope and prefix object's edge-selection slot."
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (hash-table-keys (org-gtd-graph-nodes graph)))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (edge-selection (oref transient--prefix edge-selection))
         (key-char ?a))

    (transient-parse-suffixes
     'org-gtd-graph-add-blocker-menu
     (list
      (vconcat
       (mapcar (lambda (task-id)
                 (let* ((node (org-gtd-graph-data-get-node graph task-id))
                        (title (org-gtd-graph-node-title node))
                        (key (char-to-string key-char))
                        (enabled (cdr (assoc task-id edge-selection)))
                        (display (if enabled
                                     (format "[X] %s" title)
                                   (format "[ ] %s" title)))
                        (toggle-fn (let ((id task-id))
                                     (lambda ()
                                       (interactive)
                                       (let* ((prefix (transient-prefix-object))
                                              (sel (oref prefix edge-selection))
                                              (current (cdr (assoc id sel))))
                                         (setf (alist-get id sel nil nil #'equal) (not current))
                                         (oset prefix edge-selection sel))))))
                   (setq key-char (1+ key-char))
                   (list key display toggle-fn :transient t)))
               valid-candidates))))))

(defun org-gtd-graph--add-blocker-apply ()
  "Apply add-blocker using transient scope and prefix object."
  (interactive)
  (let* ((scope (transient-scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (task-title (plist-get scope :task-title))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (edge-selection (oref (transient-prefix-object) edge-selection))
         (blocked-task-ids (mapcar #'car (seq-filter #'cdr edge-selection)))
         (new-task-id task-id))

    ;; Create new task if needed
    (unless new-task-id
      (org-with-point-at project-marker
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "** " task-title "\n")
        (forward-line -1)
        (org-back-to-heading t)
        (setq new-task-id (org-id-get-create))
        (org-todo "TODO")
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (save-buffer)))

    ;; Create dependencies: new-task blocks each specified task
    (dolist (blocked-id blocked-task-ids)
      (org-gtd-dependencies-create new-task-id blocked-id))

    ;; Link existing external task to project
    (when task-id
      (org-gtd-add-to-multivalued-property new-task-id "ORG_GTD_PROJECT_IDS" project-id)
      (org-with-point-at (org-id-find new-task-id t)
        (save-buffer)))

    ;; Add new task to FIRST_TASKS since it has no blockers
    (org-with-point-at project-marker
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
      (save-buffer))

    (message "Added blocker '%s' blocking %d task(s)" task-title (length blocked-task-ids))
    (org-gtd-projects-fix-todo-keywords project-marker)
    (org-gtd-graph-view-refresh)
    (transient-quit-one)
    (org-gtd-graph-transient--resume-if-sticky)))

;;;; Internal Functions for Testing

(defun org-gtd-graph--add-successor-internal (task-title predecessor-ids project-marker)
  "Internal function to add a successor task.
TASK-TITLE is the title for the new task.
PREDECESSOR-IDS is list of task IDs that should block the new task.
PROJECT-MARKER is the project marker.
Returns the ID of the newly created task."
  (let ((project-id (org-with-point-at project-marker
                      (org-entry-get (point) "ID")))
        new-task-id)
    ;; Create new task
    (org-with-point-at project-marker
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert "** " task-title "\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq new-task-id (org-id-get-create))
      (org-todo "TODO")
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (save-buffer))

    ;; Create dependencies: each predecessor blocks new-task (new task is successor)
    (dolist (predecessor-id predecessor-ids)
      (org-gtd-dependencies-create predecessor-id new-task-id))

    ;; Note: new successor is NOT added to FIRST_TASKS because it has blockers

    ;; Fix TODO keywords
    (org-gtd-projects-fix-todo-keywords project-marker)

    new-task-id))

(defun org-gtd-graph--add-blocker-internal (task-title blocked-task-ids project-marker)
  "Internal function to add a blocker task.
TASK-TITLE is the title for the new task.
BLOCKED-TASK-IDS is list of task IDs that the new task should block.
PROJECT-MARKER is the project marker.
Returns the ID of the newly created task."
  (let ((project-id (org-with-point-at project-marker
                      (org-entry-get (point) "ID")))
        new-task-id)
    ;; Create new task
    (org-with-point-at project-marker
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert "** " task-title "\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq new-task-id (org-id-get-create))
      (org-todo "TODO")
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (save-buffer))

    ;; Create dependencies: new-task blocks each specified task
    (dolist (blocked-id blocked-task-ids)
      (org-gtd-dependencies-create new-task-id blocked-id))

    ;; Add new task to FIRST_TASKS since it has no blockers
    (org-with-point-at project-marker
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
      (save-buffer))

    ;; Fix TODO keywords
    (org-gtd-projects-fix-todo-keywords project-marker)

    new-task-id))

(defun org-gtd-graph--add-root-internal (task-title project-marker)
  "Internal function to add a root task to a project.
TASK-TITLE is the title for the new task.
PROJECT-MARKER is the project marker.
Returns the ID of the newly created task."
  (let ((project-id (org-with-point-at project-marker
                      (org-entry-get (point) "ID")))
        new-task-id)
    ;; Create new task
    (org-with-point-at project-marker
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (insert "** " task-title "\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq new-task-id (org-id-get-create))
      (org-todo "TODO")
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (save-buffer))

    ;; Add task to project's FIRST_TASKS
    (org-with-point-at project-marker
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
      (save-buffer))

    ;; Fix TODO keywords
    (org-gtd-projects-fix-todo-keywords project-marker)

    new-task-id))

;;;; Footer

(provide 'org-gtd-graph-transient)

;;; org-gtd-graph-transient.el ends here
