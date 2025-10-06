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

(defconst org-gtd-projects "Projects")

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
:ORG_GTD: %s
:END:
" org-gtd-projects))

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

(defun org-gtd-projects-fix-todo-keywords (heading-marker)
  "Ensure keywords for subheadings of project at HEADING-MARKER are sane.

Uses breadth-first traversal starting from ORG_GTD_FIRST_TASKS to find
all tasks whose dependencies (ORG_GTD_DEPENDS_ON) are satisfied (DONE/CNCL).
These tasks are set to NEXT. All other undone tasks are set to TODO.

This means at most one `org-gtd-next' or `org-gtd-wait' task and all
other undone tasks are marked as `org-gtd-todo'."
  (let* ((buffer (marker-buffer heading-marker))
         (position (marker-position heading-marker)))
    (save-excursion
      (with-current-buffer buffer
        (org-gtd-core-prepare-buffer)
        (goto-char position)

        ;; First, set all undone tasks to TODO (cross-file support via graph traversal)
        (let ((all-task-markers (org-gtd-projects--collect-tasks-by-graph heading-marker)))
          (dolist (task-marker all-task-markers)
            (with-current-buffer (marker-buffer task-marker)
              (save-excursion
                (goto-char task-marker)
                (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
                  (let ((todo-state (org-entry-get (point) "TODO")))
                    (unless (or (equal todo-state (org-gtd-keywords--wait))
                                (equal todo-state (org-gtd-keywords--canceled))
                                (org-gtd-keywords--is-done-p todo-state))
                      (org-entry-put (point) "TODO" (org-gtd-keywords--todo)))))))))

        ;; Find tasks whose dependencies are satisfied and set them ALL to NEXT
        (let* ((project-id (org-entry-get (point) "ID"))
               (first-tasks-str (org-entry-get (point) "ORG_GTD_FIRST_TASKS"))
               (first-tasks (when first-tasks-str (split-string first-tasks-str)))
               (ready-task-ids (org-gtd-dependencies-find-ready-tasks project-id first-tasks))
               (first-wait (org-gtd-projects--first-wait-task))
               (ready-to-mark (if first-wait
                                  '() ; Don't mark anything NEXT if there's a WAIT
                                ready-task-ids)))
          ;; Mark ALL ready tasks as NEXT (not just the first one)
          (dolist (task-id ready-to-mark)
            (when-let ((marker (org-gtd-projects--find-id-marker task-id)))
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char marker)
                  (org-entry-put (point) "TODO" (org-gtd-keywords--next)))))))))))

(defun org-gtd-projects--find-id-marker (id)
  "Find marker for task with ID.
First tries current buffer, then falls back to org-id-find."
  (or
   ;; Try current buffer first (for temp buffer tests)
   (save-excursion
     (goto-char (point-min))
     (when-let ((pos (org-find-entry-with-id id)))
       (goto-char pos)
       (point-marker)))
   ;; Fall back to org-id-find for production use
   (org-id-find id t)))


;;;;; Private

(defun org-gtd-project-new--apply ()
  "Process GTD inbox item by transforming it into a project.

Allow the user apply user-defined tags from `org-tag-persistent-alist',
`org-tag-alist' or file-local tags in the inbox.
Refile to `org-gtd-actionable-file-basename'."
  (when (org-gtd-projects--poorly-formatted-p)
    (org-gtd-projects--show-error)
    (throw 'org-gtd-error "Malformed project"))

  ;; Configure the main project heading using the new pattern
  (org-gtd-configure-item (point) :project-heading)
  (setq-local org-gtd--organize-type 'project-heading)
  (org-gtd-organize-apply-hooks)

  ;; Configure all sub-tasks as project tasks
  (org-gtd-projects--configure-all-tasks)
  (setq-local org-gtd--organize-type 'project-task)
  (org-gtd-projects--apply-organize-hooks-to-tasks)

  ;; Project-specific business logic
  (org-gtd-projects--add-default-sequential-dependencies)
  (org-gtd-projects--set-first-tasks)
  (org-gtd-projects-fix-todo-keywords-for-project-at-point)
  (org-gtd-projects--add-progress-cookie)

  (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))

(defun org-gtd-projects--set-project-name-on-task ()
  "Set ORG_GTD_PROJECT property on current task to its project heading name."
  (let ((original-point (point)))
    (save-excursion
      ;; Navigate up to find the project heading (has ORG_GTD="Projects")
      (while (and (org-up-heading-safe)
                  (not (string= (org-entry-get (point) "ORG_GTD") "Projects"))))
      (when (string= (org-entry-get (point) "ORG_GTD") "Projects")
        (let ((project-name (org-get-heading t t t t))
              (project-id (or (org-entry-get (point) "ID")
                              (org-gtd-id-get-create))))
          ;; Go back to original task and set the properties
          (goto-char original-point)
          ;; Set the multi-valued project IDs property
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
          ;; Only set ORG_GTD_PROJECT if not already set (preserves first project for multi-project tasks)
          (unless (org-entry-get (point) "ORG_GTD_PROJECT")
            (org-entry-put (point) "ORG_GTD_PROJECT" project-name)))))))

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
       (unless (string= (org-entry-get (point) "ORG_GTD") "Projects")
         (org-gtd-configure-item (point) :project-task)
         (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
         ;; Only set ORG_GTD_PROJECT if not already set (preserves first project for multi-project tasks)
         (unless (org-entry-get (point) "ORG_GTD_PROJECT")
           (org-entry-put (point) "ORG_GTD_PROJECT" project-name))))
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
          (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
            (let ((task-id (org-entry-get (point) "ID")))
              (when task-id
                (puthash task-id task-marker seen-task-ids)))))))

    ;; Part 2: Also check current buffer tree (finds newly added unconnected tasks)
    (org-map-entries
     (lambda ()
       (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
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
         (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
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
       (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
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
              (let ((task-project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
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

(defun org-gtd-project-extend--apply ()
  "Refile the org heading at point under a chosen heading in the agenda files."
  (org-gtd-configure-item (point) :project-task)
  (setq-local org-gtd--organize-type 'project-task)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do-project-task)
  (let ((marker (save-excursion
                  (org-refile-goto-last-stored)
                  (org-up-heading-safe)
                  (point-marker))))
    ;; Set project name after refiling
    (save-excursion
      (org-refile-goto-last-stored)
      (org-gtd-projects--set-project-name-on-task))
    ;; Update first tasks list to include new root task
    (org-with-point-at marker
      (org-gtd-projects--set-first-tasks))
    (org-gtd-projects-fix-todo-keywords marker)))

(defun org-gtd-projects--apply-organize-hooks-to-tasks ()
  "Decorate tasks for project at point."
  (org-map-entries
   (lambda ()
     (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
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
                (org-entry-put (point) "TODO" (org-gtd-keywords--next))))))))))

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
                                       (org-entry-get (point) "TODO"))
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
                                       (org-entry-get (point) "TODO"))
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
