;;; org-gtd-projects.el --- project management in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Project management for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'f)
(require 'org)
(require 'org-element)
(require 'org-edna)

(require 'org-gtd-core)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-dependencies)
(require 'org-gtd-task-management)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-add-to-project-func #'org-gtd-project-extend--apply
  "Function called when organizing item at point as a new task in a project.")

(defconst org-gtd-project-func #'org-gtd-project-new--apply
  "Function called when organizing item at point as a project.")

(defconst org-gtd-project-headings
  "+ORG_GTD=\"Projects\""
  "How to tell `org-mode' to find project headings.")

(defconst org-gtd-projects--malformed
  "A 'project' in GTD is a finite set of steps after which a given task is
complete. In Org GTD, this is defined as a top-level org heading with at least
one second-level org headings. When the item you are editing is intended to be
a project, create such a headline structure, like so:

* Project heading
** First task
** Second task
** Third task

If you do not need sub-headings, then organize this item as a 'single action'
instead.")

(defconst org-gtd-projects-template
  (format "* Projects
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-projects))

;;;###autoload
(defun org-gtd-stuck-projects ()
  "Return the stuck projects configuration for the current GTD setup."
  `(,org-gtd-project-headings
    (,(org-gtd-keywords--next) ,(org-gtd-keywords--wait))
    nil
    ""))

;;;; Commands

;;;###autoload
(defun org-gtd-project-cancel ()
  "With point on topmost project heading, mark all undone tasks canceled."
  (interactive)
  (org-edna-mode -1)
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-projects--incomplete-task-p)
           (let ((org-inhibit-logging 'note))
             (org-todo (org-gtd-keywords--canceled)))))
       nil
       'tree))
  (org-edna-mode 1))

;;;###autoload
(defun org-gtd-project-cancel-from-agenda ()
  "Cancel the project that has the highlighted task."
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (set-marker-insertion-type marker t)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        ;; Verify this is actually a project task before going up
        (let ((org-gtd-refile (org-entry-get nil "ORG_GTD_REFILE" t))
              (org-gtd (org-entry-get nil "ORG_GTD" nil)))
          (unless (and (string-equal org-gtd-refile "Projects")
                       (string-equal org-gtd "Actions"))
            (user-error "This is not a project task - cannot cancel project from here")))
        (org-up-heading-safe)
        (org-gtd-project-cancel)))))

(defun org-gtd-project-extend ()
  "Organize, decorate and refile item as a new task in an existing project."
  (interactive)
  (org-gtd-organize--call org-gtd-add-to-project-func))

(defun org-gtd-projects-fix-todo-keywords-for-project-at-point ()
  "Ensure keywords for subheadings of project at point are sane.

This means one and only one `org-gtd-next' keyword, and it is the first non-done
state in the list - all others are `org-gtd-todo'."
  (interactive)
  (org-gtd-projects-fix-todo-keywords (point-marker)))

(defun org-gtd-project-new ()
  "Organize, decorate and refile item as a new project."
  (interactive)
  (org-gtd-organize--call org-gtd-project-func))

;;;; Functions

;;;;; Public

;;;;; Command: Reset All Task States

(defun org-gtd-project--reset-all-task-states (project-marker)
  "Set all undone tasks in project at PROJECT-MARKER to TODO state.
Only resets states that should be recalculated (preserves WAIT, DONE, CNCL)."
  (let ((all-task-markers (org-gtd-projects--collect-tasks-by-graph project-marker)))
    (dolist (task-marker all-task-markers)
      (org-with-point-at task-marker
        (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
          (let ((todo-state (org-entry-get (point) org-gtd-prop-todo)))
            (when (org-gtd-todo-state-should-reset-p todo-state)
              (org-entry-put (point) org-gtd-prop-todo (org-gtd-keywords--todo)))))))))

;;;;; Command: Mark Ready Tasks

(defun org-gtd-project--mark-ready-tasks (project-marker ready-task-ids)
  "Mark tasks in READY-TASK-IDS as NEXT for project at PROJECT-MARKER.
Respects WAIT tasks - if project has any WAIT task, don't mark any as NEXT."
  (let ((first-wait (org-with-point-at project-marker
                      (org-gtd-projects--first-wait-task))))
    (unless first-wait
      (dolist (task-id ready-task-ids)
        (org-gtd-set-task-state task-id (org-gtd-keywords--next))))))

;;;;; Command: Find Ready Tasks

(defun org-gtd-project--find-ready-tasks (project-marker)
  "Find all tasks ready to work on for project at PROJECT-MARKER.
Returns list of task IDs whose dependencies are satisfied."
  (org-with-point-at project-marker
    (let* ((project-id (org-entry-get (point) "ID"))
           (first-tasks (org-gtd-get-project-first-tasks project-marker)))
      (org-gtd-dependencies-find-ready-tasks project-id first-tasks))))

;;;;; Main Command: Fix TODO Keywords

(defun org-gtd-projects-fix-todo-keywords (heading-marker)
  "Ensure keywords for subheadings of project at HEADING-MARKER are sane.

Uses breadth-first traversal starting from ORG_GTD_FIRST_TASKS to find
all tasks whose dependencies (ORG_GTD_DEPENDS_ON) are satisfied (DONE/CNCL).
These tasks are set to NEXT. All other undone tasks are set to TODO.

This means at most one `org-gtd-next' or `org-gtd-wait' task and all
other undone tasks are marked as `org-gtd-todo'.

Orchestrates state updates:
1. Reset all resettable task states to TODO
2. Find tasks whose dependencies are satisfied
3. Mark ready tasks as NEXT (unless project has WAIT tasks)"
  (let* ((buffer (marker-buffer heading-marker))
         (position (marker-position heading-marker)))
    (save-excursion
      (with-current-buffer buffer
        (org-gtd-core-prepare-buffer)
        (goto-char position)

        (org-gtd-project--reset-all-task-states heading-marker)

        (let ((ready-task-ids (org-gtd-project--find-ready-tasks heading-marker)))
          (org-gtd-project--mark-ready-tasks heading-marker ready-task-ids))))))

;;;;; Private

;;;;; Command: Project Validation

(defun org-gtd-project--validate-format ()
  "Validate inbox item has correct format for project.
Throws user-error with helpful message if invalid."
  (when (org-gtd-projects--poorly-formatted-p)
    (org-gtd-projects--show-error)
    (throw 'org-gtd-error "Malformed project")))

;;;;; Command: Project Heading Transformation

(defun org-gtd-project--transform-heading ()
  "Transform current heading into project structure.
Returns marker to project heading."
  (org-gtd-configure-item (point) :project-heading)
  (setq-local org-gtd--organize-type 'project-heading)
  (org-gtd-organize-apply-hooks)
  (point-marker))

;;;;; Command: Task Configuration

(defun org-gtd-project--configure-tasks (project-marker)
  "Configure all tasks under project at PROJECT-MARKER.
Applies org-gtd properties and hooks to each task."
  (org-with-point-at project-marker
    (org-gtd-projects--configure-all-tasks)
    (setq-local org-gtd--organize-type 'project-task)
    (org-gtd-projects--apply-organize-hooks-to-tasks)))

;;;;; Command: Dependency Setup

(defun org-gtd-project--setup-dependencies (project-marker)
  "Setup default sequential dependencies for project at PROJECT-MARKER.
Creates dependencies between unconnected tasks and identifies root tasks."
  (org-with-point-at project-marker
    (org-gtd-projects--add-default-sequential-dependencies)
    (org-gtd-projects--set-first-tasks)))

;;;;; Command: Task State Calculation

(defun org-gtd-project--calculate-task-states (project-marker)
  "Calculate and set initial task states for project at PROJECT-MARKER.
Uses dependency service to determine which tasks are ready to work on."
  (org-gtd-projects-fix-todo-keywords project-marker))

;;;;; Command: Project Decoration

(defun org-gtd-project--decorate (project-marker)
  "Add project decorations like progress cookies at PROJECT-MARKER."
  (org-with-point-at project-marker
    (org-gtd-projects--add-progress-cookie)))

;;;;; Main Command: Create Project

(defun org-gtd-project-new--apply ()
  "Process GTD inbox item by transforming it into a project.

Orchestrates the project creation workflow:
1. Validate format
2. Transform heading to project
3. Configure tasks
4. Setup dependencies
5. Calculate initial states
6. Add decorations
7. Refile to projects file

Allow the user apply user-defined tags from `org-tag-persistent-alist',
`org-tag-alist' or file-local tags in the inbox.
Refile to `org-gtd-actionable-file-basename'."
  (org-gtd-project--validate-format)

  (let ((project-marker (org-gtd-project--transform-heading)))
    (org-gtd-project--configure-tasks project-marker)
    (org-gtd-project--setup-dependencies project-marker)
    (org-gtd-project--calculate-task-states project-marker)
    (org-gtd-project--decorate project-marker)

    (org-gtd-refile--do org-gtd-projects org-gtd-projects-template)))

(defun org-gtd-projects--set-project-name-on-task ()
  "Set ORG_GTD_PROJECT property on current task to its project heading name."
  (let ((original-point (point)))
    (save-excursion
      ;; Navigate up to find the project heading (has ORG_GTD="Projects")
      (while (and (org-up-heading-safe)
                  (not (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-projects))))
      (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-projects)
        (let ((project-name (org-get-heading t t t t))
              (project-id (or (org-entry-get (point) "ID")
                              (org-gtd-id-get-create))))
          ;; Go back to original task and set the properties
          (goto-char original-point)
          ;; Set the multi-valued project IDs property
          (org-entry-add-to-multivalued-property (point) org-gtd-prop-project-ids project-id)
          ;; Only set ORG_GTD_PROJECT if not already set (preserves first project for multi-project tasks)
          (unless (org-entry-get (point) org-gtd-prop-project)
            (org-entry-put (point) org-gtd-prop-project project-name)))))))

(defun org-gtd-projects--configure-all-tasks ()
  "Configure all sub-tasks in the project as project-task items."
  (let* ((project-id (save-excursion
                       (org-back-to-heading)
                       (or (org-entry-get (point) "ID")
                           (org-gtd-id-get-create))))
         (project-name (save-excursion
                         (org-back-to-heading)
                         (org-get-heading t t t t))))
    (org-map-entries
     (lambda ()
       (unless (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-projects)
         (org-gtd-configure-item (point) :project-task)
         (org-entry-add-to-multivalued-property (point) org-gtd-prop-project-ids project-id)
         ;; Only set ORG_GTD_PROJECT if not already set (preserves first project for multi-project tasks)
         (unless (org-entry-get (point) org-gtd-prop-project)
           (org-entry-put (point) org-gtd-prop-project project-name))))
     nil
     'tree)))

(defun org-gtd-projects--add-default-sequential-dependencies ()
  "Create default sequential dependencies for tasks without ORG_GTD_DEPENDS_ON.
For Story 7: Create chain where Task 1 → Task 2 → Task 3, etc.
For Story 8: Only apply to tasks that don't have ORG_GTD_DEPENDS_ON relationships."
  (require 'org-gtd-task-management) ; Ensure task management functions are available
  (let ((all-tasks (org-gtd-projects--collect-all-tasks))
        (unconnected-tasks '()))
    ;; Collect tasks that don't have ORG_GTD_DEPENDS_ON relationships (they can have ORG_GTD_BLOCKS)
    (dolist (task all-tasks)
      (save-excursion
        (goto-char task)
        (unless (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")
          (push task unconnected-tasks))))
    ;; Reverse to maintain document order
    (setq unconnected-tasks (nreverse unconnected-tasks))
    ;; Create sequential dependencies for consecutive unconnected tasks
    (when (> (length unconnected-tasks) 1)
      (cl-loop for i from 1 below (length unconnected-tasks)
               do (let ((current-task (nth i unconnected-tasks))
                        (previous-task (nth (1- i) unconnected-tasks)))
                    (org-gtd-projects--create-dependency-relationship previous-task current-task))))))

(defun org-gtd-projects--set-first-tasks ()
  "Set ORG_GTD_FIRST_TASKS property on project heading with IDs of root tasks.
Root tasks are tasks that have no ORG_GTD_DEPENDS_ON property.
Works cross-file by combining graph traversal with tree search."
  (let ((root-task-ids '())
        (project-marker (point-marker))
        (seen-task-ids (make-hash-table :test 'equal)))

    ;; Collect tasks from BOTH graph traversal (cross-file) AND tree (current buffer)
    ;; This handles both connected tasks in other files AND newly added unconnected tasks

    ;; Part 1: Try graph traversal for cross-file tasks
    (let ((graph-task-markers (condition-case nil
                                   (org-gtd-projects--collect-tasks-by-graph project-marker)
                                 (error nil))))
      (dolist (task-marker graph-task-markers)
        (org-with-point-at task-marker
          (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
            (let ((task-id (org-entry-get (point) "ID")))
              (when task-id
                (puthash task-id task-marker seen-task-ids)))))))

    ;; Part 2: Also check current buffer tree (finds newly added unconnected tasks)
    (org-map-entries
     (lambda ()
       (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
         (let ((task-id (org-entry-get (point) "ID")))
           (when (and task-id (not (gethash task-id seen-task-ids)))
             (puthash task-id (point-marker) seen-task-ids)))))
     nil
     'tree)

    ;; Now collect all task markers in document order and find root tasks
    ;; Use current buffer tree order for deterministic ordering
    (let ((ordered-task-markers '()))
      ;; Collect all seen tasks in document order from current buffer
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
           (let ((task-id (org-entry-get (point) "ID")))
             (when (gethash task-id seen-task-ids)
               (push (point-marker) ordered-task-markers)))))
       nil
       'tree)
      (setq ordered-task-markers (nreverse ordered-task-markers))

      ;; Add any cross-file tasks not found in tree (from graph traversal)
      (maphash
       (lambda (task-id task-marker)
         (unless (seq-find (lambda (m)
                            (and (equal (marker-buffer m) (marker-buffer task-marker))
                                 (equal (marker-position m) (marker-position task-marker))))
                          ordered-task-markers)
           (setq ordered-task-markers (append ordered-task-markers (list task-marker)))))
       seen-task-ids)

      ;; Find root tasks from ordered list
      (dolist (task-marker ordered-task-markers)
        (org-with-point-at task-marker
          (when (not (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
            (let ((task-id (org-entry-get (point) "ID")))
              (when task-id
                (push task-id root-task-ids)))))))

    ;; Set ORG_GTD_FIRST_TASKS property on project heading with space-separated IDs
    (org-with-point-at project-marker
      (org-back-to-heading t)
      (if root-task-ids
          (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (string-join (nreverse root-task-ids) " "))
        ;; No root tasks found - clear the property
        (org-entry-delete (point) "ORG_GTD_FIRST_TASKS")))))

(defun org-gtd-projects--collect-all-tasks ()
  "Collect all project tasks in document order.
Returns list of markers pointing to task headings with ORG_GTD=Actions."
  (let ((tasks '()))
    (org-map-entries
     (lambda ()
       (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
         (push (point-marker) tasks)))
     nil
     'tree)
    ;; Return tasks in document order (reverse since we pushed)
    (nreverse tasks)))

(defun org-gtd-projects--collect-tasks-by-graph (project-marker)
  "Collect all project tasks by traversing the dependency graph.

Starting from the project heading at PROJECT-MARKER, reads the ORG_GTD_FIRST_TASKS
property to find root task IDs, then traverses the graph by following
ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON relationships using org-id-find.

Only includes tasks that have the current project's ID in their ORG_GTD_PROJECT_IDS
property, respecting project boundaries for shared tasks.

Returns list of task markers in breadth-first order."
  (org-with-point-at project-marker
    (let* ((project-id (org-entry-get (point) "ID"))
           (first-tasks-str (org-entry-get (point) "ORG_GTD_FIRST_TASKS"))
           (first-task-ids (when first-tasks-str (split-string first-tasks-str)))
           (queue '())
           (visited-ids (make-hash-table :test 'equal))
           (result-tasks '()))

      ;; Initialize queue with first tasks
      (dolist (task-id first-task-ids)
        (push task-id queue))
      (setq queue (nreverse queue))

      ;; Breadth-first traversal
      (while queue
        (let* ((current-id (pop queue))
               ;; Try org-id-find first, fall back to searching current buffer
               (task-location (or (org-id-find current-id t)
                                  (save-excursion
                                    (goto-char (point-min))
                                    (when-let ((pos (org-find-entry-with-id current-id)))
                                      (goto-char pos)
                                      (point-marker))))))

          (when (and task-location (not (gethash current-id visited-ids)))
            (puthash current-id t visited-ids)

            ;; Include task if it either:
            ;; 1. Has current project ID in ORG_GTD_PROJECT_IDS, OR
            ;; 2. Has no ORG_GTD_PROJECT_IDS (simple task belonging only to this project)
            (org-with-point-at task-location
              (let ((task-project-ids (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
                (when (or (null task-project-ids)  ; No project IDs - belongs to this project
                          (member project-id task-project-ids))  ; Has this project's ID
                  (push task-location result-tasks)

                  ;; Find tasks this one blocks (children in the graph)
                  (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
                    (dolist (blocked-id blocks-list)
                      (unless (gethash blocked-id visited-ids)
                        (setq queue (append queue (list blocked-id))))))))))))

      ;; Return in breadth-first order
      (nreverse result-tasks))))

(defun org-gtd-projects--has-active-tasks-p (project-marker)
  "Return t if project at PROJECT-MARKER has at least one active task.

Active tasks are those with TODO states that are not in `org-done-keywords'.
Uses early exit optimization - stops checking as soon as an active task is found.

This function is intended for use in agenda views and filters to identify
projects that have work remaining to be done."
  (let ((tasks (org-gtd-projects--collect-tasks-by-graph project-marker))
        (has-active nil))
    ;; Check each task until we find an active one
    (while (and tasks (not has-active))
      (let ((task-marker (pop tasks)))
        (org-with-point-at task-marker
          (let ((todo-state (org-entry-get (point) org-gtd-prop-todo)))
            ;; Task is active if it has a TODO state and it's not done
            (when (and todo-state
                       (not (member todo-state org-done-keywords)))
              (setq has-active t))))))
    has-active))

(defun org-gtd-projects--is-stuck-p (project-marker)
  "Return t if project at PROJECT-MARKER is stuck.

A stuck project is one that has active tasks (work remaining) but no tasks
that are immediately actionable (NEXT) or waiting (WAIT). This typically
means all tasks are in TODO state, indicating planning is incomplete or
dependencies aren't set up properly."
  (let ((tasks (org-gtd-projects--collect-tasks-by-graph project-marker))
        (has-todo-tasks nil)
        (has-actionable-task nil))
    ;; Check each task to see if we have any TODO tasks and any actionable tasks
    (dolist (task-marker tasks)
      (org-with-point-at task-marker
        (let ((todo-state (org-entry-get (point) org-gtd-prop-todo)))
          (when todo-state
            (cond
             ;; Done or canceled tasks don't count
             ((member todo-state org-done-keywords) nil)
             ;; NEXT or WAIT tasks mean project is not stuck
             ((or (string= todo-state (org-gtd-keywords--next))
                  (string= todo-state (org-gtd-keywords--wait)))
              (setq has-actionable-task t))
             ;; Any other active state (like TODO) means work remains
             (t (setq has-todo-tasks t)))))))
    ;; Project is stuck if it has work remaining but nothing actionable
    (and has-todo-tasks (not has-actionable-task))))

(defun org-gtd-projects--create-dependency-relationship (blocker-marker dependent-marker)
  "Create bidirectional dependency: BLOCKER-MARKER blocks DEPENDENT-MARKER."
  (let ((blocker-id (save-excursion
                      (goto-char blocker-marker)
                      (or (org-entry-get (point) "ID")
                          (org-gtd-id-get-create))))
        (dependent-id (save-excursion
                        (goto-char dependent-marker)
                        (or (org-entry-get (point) "ID")
                            (org-gtd-id-get-create)))))
    ;; Add ORG_GTD_BLOCKS property to blocker task
    (save-excursion
      (goto-char blocker-marker)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" dependent-id))
    ;; Add ORG_GTD_DEPENDS_ON property to dependent task
    (save-excursion
      (goto-char dependent-marker)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" blocker-id))))

(defun org-gtd-projects--add-progress-cookie ()
  "Add progress tracking cookie to the project heading."
  (let ((org-special-ctrl-a t))
    (org-end-of-line))
  (insert " [/]")
  (org-update-statistics-cookies t))

;;;;; Command: Configure Single Task

(defun org-gtd-project--configure-single-task ()
  "Configure current task for addition to a project.
Returns marker to configured task."
  (org-gtd-configure-item (point) :project-task)
  (setq-local org-gtd--organize-type 'project-task)
  (org-gtd-organize-apply-hooks)
  (point-marker))

;;;;; Command: Update Project After Task Addition

(defun org-gtd-project--update-after-task-addition (project-marker)
  "Update project at PROJECT-MARKER after new task has been added.
Sets project name on task, recalculates root tasks, and updates states."
  (save-excursion
    (org-refile-goto-last-stored)
    (org-gtd-projects--set-project-name-on-task))

  (org-with-point-at project-marker
    (org-gtd-projects--set-first-tasks))

  (org-gtd-projects-fix-todo-keywords project-marker))

;;;;; Main Command: Extend Project

(defun org-gtd-project-extend--apply ()
  "Refile the org heading at point under a chosen heading in the agenda files.

Orchestrates adding a new task to an existing project:
1. Configure task for project
2. Refile to chosen project
3. Update project metadata
4. Recalculate task states"
  (org-gtd-project--configure-single-task)
  (org-gtd-refile--do-project-task)

  (let ((project-marker (save-excursion
                          (org-refile-goto-last-stored)
                          (org-up-heading-safe)
                          (point-marker))))
    (org-gtd-project--update-after-task-addition project-marker)))

(defun org-gtd-projects--apply-organize-hooks-to-tasks ()
  "Decorate tasks for project at point."
  (org-map-entries
   (lambda ()
     (when (string= (org-entry-get (point) org-gtd-prop-category) org-gtd-action)
       (org-narrow-to-element)
       (org-gtd-organize-apply-hooks)
       (widen)))
   nil
   'tree))

(defun org-gtd-projects--edna-next-project-action ()
  "`org-edna' extension to find the next action to show in the agenda."
  (org-edna-finder/relatives 'forward-no-wrap 'todo-only 1 'no-sort))

(defalias 'org-edna-finder/org-gtd-next-project-action
  'org-gtd-projects--edna-next-project-action)

(defun org-gtd-projects--edna-update-project-task (_last-entry)
  "`org-edna' extension to change the todo state to `org-gtd-next'."
  (with-org-gtd-context
      (org-todo (org-gtd-keywords--next))))

(defalias 'org-edna-action/org-gtd-update-project-task!
  'org-gtd-projects--edna-update-project-task)

(defun org-gtd-projects--edna-update-project-after-task-done (_last-entry)
  "`org-edna' action that updates blocked tasks when current task is DONE.
For each task this one blocks, checks if all its dependencies are satisfied,
and if so, marks it NEXT."
  (let ((blocked-ids (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
    (dolist (blocked-id blocked-ids)
      (when-let ((blocked-marker (org-gtd-projects--find-id-marker blocked-id)))
        (with-current-buffer (marker-buffer blocked-marker)
          (save-excursion
            (goto-char blocked-marker)
            (let* ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
                   (all-deps-done (cl-every #'org-gtd-task-management--task-is-done-p depends-on)))
              (when all-deps-done
                (org-entry-put (point) org-gtd-prop-todo (org-gtd-keywords--next))))))))))

(defalias 'org-edna-action/org-gtd-update-project-after-task-done!
  'org-gtd-projects--edna-update-project-after-task-done)

(defun org-gtd-projects--first-todo-task ()
  "Given an org tree at point, return the first subtask with `org-gtd-todo'.
Return nil if there isn't one."
  (let ((heading-level (org-current-level)))
    (car
     (seq-filter (lambda (x) x)
                 (org-map-entries
                  (lambda ()
                    (and (not (equal heading-level (org-current-level)))
                         (string-equal (org-gtd-keywords--todo)
                                       (org-entry-get (point) org-gtd-prop-todo))
                         (org-element-at-point)))
                  t
                  'tree)))))

(defun org-gtd-projects--first-wait-task ()
  "Given an org tree at point, return the first subtask with `org-gtd-wait'.
Return nil if there isn't one."
  (let ((heading-level (org-current-level)))
    (car
     (seq-filter (lambda (x) x)
                 (org-map-entries
                  (lambda ()
                    (and (not (equal heading-level (org-current-level)))
                         (string-equal (org-gtd-keywords--wait)
                                       (org-entry-get (point) org-gtd-prop-todo))
                         (org-element-at-point)))
                  t
                  'tree)))))

(defun org-gtd-projects--incomplete-task-p ()
  "Determine if current heading is a task that's not finished."
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(defun org-gtd-projects--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

(defun org-gtd-projects--poorly-formatted-p ()
  "Return non-nil if the project is composed of only one heading."
  (eql 1 (length (org-map-entries t))))

(defun org-gtd-projects--show-error ()
  "Tell the user something is wrong with the project."
  (let ((resize-mini-windows t)
        (max-mini-window-height 0))
    (display-message-or-buffer org-gtd-projects--malformed))
  ;; read-key changed in emacs 28
  (if (version< emacs-version "28")
      (read-key "Waiting for a keypress to return to clarifying... ")
    (read-key "Waiting for a keypress to return to clarifying... " t))
  (message ""))

;;;; Footer

(provide 'org-gtd-projects)

;;; org-gtd-projects.el ends here
