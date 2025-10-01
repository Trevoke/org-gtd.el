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

  (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))

(defun org-gtd-projects--configure-all-tasks ()
  "Configure all sub-tasks in the project as project-task items."
  (org-map-entries
   (lambda ()
     (unless (string= (org-entry-get (point) "ORG_GTD") "Projects")
       (org-gtd-configure-item (point) :project-task)))
   nil
   'tree))

(defun org-gtd-projects--add-default-sequential-dependencies ()
  "Create default sequential dependencies for tasks without DEPENDS_ON.
For Story 7: Create chain where Task 1 → Task 2 → Task 3, etc.
For Story 8: Only apply to tasks that don't have DEPENDS_ON relationships."
  (require 'org-gtd-task-management) ; Ensure task management functions are available
  (let ((all-tasks (org-gtd-projects--collect-all-tasks))
        (unconnected-tasks '()))
    ;; Collect tasks that don't have DEPENDS_ON relationships (they can have BLOCKS)
    (dolist (task all-tasks)
      (save-excursion
        (goto-char task)
        (unless (org-entry-get-multivalued-property (point) "DEPENDS_ON")
          (push task unconnected-tasks))))
    ;; Reverse to maintain document order
    (setq unconnected-tasks (nreverse unconnected-tasks))
    ;; Create sequential dependencies for consecutive unconnected tasks
    (when (> (length unconnected-tasks) 1)
      (cl-loop for i from 1 below (length unconnected-tasks)
               do (let ((current-task (nth i unconnected-tasks))
                        (previous-task (nth (1- i) unconnected-tasks)))
                    (org-gtd-projects--create-dependency-relationship previous-task current-task))))))

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

(defun org-gtd-projects--collect-tasks-by-graph ()
  "Collect all project tasks within the current project scope.
Starting from the current project heading, find all tasks with ORG_GTD=Actions
connected via dependency relationships (BLOCKS/DEPENDS_ON)."
  (let ((all-tasks '())
        (result-tasks '())
        (processed-ids (make-hash-table :test 'equal)))

    ;; First collect all tasks with ORG_GTD=Actions within the current project tree
    (org-map-entries
     (lambda ()
       (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
         (let ((task-marker (point-marker))
               (task-id (org-entry-get (point) "ID")))
           (when task-id
             (push task-marker all-tasks)))))
     nil
     'tree)

    ;; Now traverse the graph from each task to find all connected components
    (dolist (task-marker all-tasks)
      (let ((task-id (save-excursion
                       (goto-char task-marker)
                       (org-entry-get (point) "ID"))))
        (when (and task-id (not (gethash task-id processed-ids)))
          ;; Use fresh visited-ids for each connected component
          (let* ((visited-ids (make-hash-table :test 'equal))
                 (connected-tasks (org-gtd-projects--traverse-graph task-marker visited-ids)))
            (dolist (task connected-tasks)
              (let ((connected-id (save-excursion
                                    (goto-char task)
                                    (org-entry-get (point) "ID"))))
                (when connected-id
                  (puthash connected-id t processed-ids)))
              (when (not (member task result-tasks))
                (push task result-tasks)))))))

    ;; Return tasks in document order (reverse since we pushed)
    (nreverse result-tasks)))

(defun org-gtd-projects--traverse-graph (start-marker visited-ids)
  "Traverse the dependency graph starting from START-MARKER.
VISITED-IDS is a hash table to track visited nodes to prevent cycles.
Returns list of all connected task markers."
  (let ((current-id (save-excursion
                      (goto-char start-marker)
                      (org-entry-get (point) "ID"))))

    ;; If we've already visited this node, return empty list to prevent cycles
    (if (and current-id (gethash current-id visited-ids))
        nil
      (let ((connected-tasks '()))
        ;; Mark current node as visited and add to result
        (when current-id
          (puthash current-id t visited-ids))
        (push start-marker connected-tasks)

        ;; Get connected IDs via BLOCKS and DEPENDS_ON properties
        (let ((blocks-ids (save-excursion
                            (goto-char start-marker)
                            (org-entry-get-multivalued-property (point) "BLOCKS")))
              (depends-ids (save-excursion
                             (goto-char start-marker)
                             (org-entry-get-multivalued-property (point) "DEPENDS_ON"))))

          ;; Visit all connected tasks
          (dolist (connected-id (append blocks-ids depends-ids))
            (let ((connected-marker (org-gtd-projects--find-task-by-id connected-id)))
              (when connected-marker
                (let ((sub-tasks (org-gtd-projects--traverse-graph connected-marker visited-ids)))
                  (dolist (task sub-tasks)
                    (when (not (member task connected-tasks))
                      (push task connected-tasks))))))))

        ;; Return tasks in order found
        (nreverse connected-tasks)))))

(defun org-gtd-projects--find-task-by-id (task-id)
  "Find task with given TASK-ID and return its marker if it has ORG_GTD=Actions."
  (let ((found-marker nil))
    (org-map-entries
     (lambda ()
       (when (and (string= (org-entry-get (point) "ID") task-id)
                  (string= (org-entry-get (point) "ORG_GTD") "Actions"))
         (setq found-marker (point-marker))))
     nil
     nil)
    found-marker))

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
    ;; Add BLOCKS property to blocker task
    (save-excursion
      (goto-char blocker-marker)
      (org-entry-add-to-multivalued-property (point) "BLOCKS" dependent-id))
    ;; Add DEPENDS_ON property to dependent task
    (save-excursion
      (goto-char dependent-marker)
      (org-entry-add-to-multivalued-property (point) "DEPENDS_ON" blocker-id))))

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
