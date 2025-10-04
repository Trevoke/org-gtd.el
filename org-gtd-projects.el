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
:TRIGGER: org-gtd-next-project-action org-gtd-update-project-task!
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

This means at most one `org-gtd-next' or `org-gtd-wait' task and all
other undone tasks are marked as `org-gtd-todo'."
  (let* ((buffer (marker-buffer heading-marker))
         (position (marker-position heading-marker))
         (heading-level (with-current-buffer buffer
                          (goto-char position)
                          (org-current-level))))
    (save-excursion
      (with-current-buffer buffer
        (org-gtd-core-prepare-buffer)
        (goto-char position)
        (org-map-entries
         (lambda ()
           (unless (or (equal heading-level (org-current-level))
                       (let ((todo-state (org-entry-get (point) "TODO")))
                         (or (equal todo-state (org-gtd-keywords--todo))
                             (equal todo-state (org-gtd-keywords--wait))
                             (equal todo-state (org-gtd-keywords--canceled))
                             (org-gtd-keywords--is-done-p todo-state))))
             (org-entry-put (point) "TODO" (org-gtd-keywords--todo))
             (setq org-map-continue-from (end-of-line))))
         t
         'tree)
        (let ((first-wait (org-gtd-projects--first-wait-task))
              (first-todo (org-gtd-projects--first-todo-task)))
          (unless first-wait
            (org-entry-put (org-gtd-projects--org-element-pom first-todo)
                           "TODO"
                           (org-gtd-keywords--next))))))))

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
  (org-gtd-projects-fix-todo-keywords-for-project-at-point)
  (org-gtd-projects--add-default-sequential-dependencies)
  (org-gtd-projects--add-progress-cookie)
  (org-gtd-projects--set-first-tasks)

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
Root tasks are tasks that have no ORG_GTD_DEPENDS_ON property."
  (let ((root-task-ids '()))
    ;; Find all tasks without ORG_GTD_DEPENDS_ON (root tasks)
    (org-map-entries
     (lambda ()
       (when (and (string= (org-entry-get (point) "ORG_GTD") "Actions")
                  (not (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
         (let ((task-id (org-entry-get (point) "ID")))
           (when task-id
             (push task-id root-task-ids)))))
     nil
     'tree)

    ;; Set ORG_GTD_FIRST_TASKS property on project heading with space-separated IDs
    (when root-task-ids
      (org-back-to-heading t)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (string-join (nreverse root-task-ids) " ")))))

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

            ;; Only include task if it has current project ID in ORG_GTD_PROJECT_IDS
            (org-with-point-at task-location
              (let ((task-project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
                (when (member project-id task-project-ids)
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
