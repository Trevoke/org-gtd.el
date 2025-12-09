;;; org-gtd-task-management.el --- Task dependency and relationship management -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2025 Aldric Giacomoni

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
;; Interactive commands for managing task relationships and dependencies
;; in the org-gtd system. Provides commands to create BLOCKS and BLOCKED_BY
;; relationships between tasks.
;;

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)
(require 'org-gtd-id)
(require 'org-gtd-configure)
(require 'org-gtd-refile)
(require 'org-gtd-single-action)
(require 'org-gtd-accessors)
(require 'org-gtd-dependencies)
(require 'org-gtd-projects)

;;;; Interactive Commands

;;;###autoload
(defun org-gtd-task-add-blockers ()
  "Add tasks that block the current task.
Prompts user to select multiple tasks, then creates bidirectional
BLOCKS/DEPENDS_ON relationships.  Prevents circular dependencies with
clear error messages."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (project-id (org-gtd-task-management--get-current-project))
         (selected-ids (org-gtd-task-management--select-multiple-task-ids
                        (format "Select tasks that block '%s' (complete with empty selection): " current-heading))))
    (when selected-ids
      ;; Check for circular dependencies before creating any relationships
      (dolist (selected-id selected-ids)
        (org-gtd-task-management--check-circular-dependency current-id selected-id))

      ;; Add selected tasks to the same project (if task belongs to a project)
      (when project-id
        (dolist (selected-id selected-ids)
          (org-gtd-task-management--add-task-to-project selected-id project-id)))

      ;; Add bidirectional relationships for each selected task
      (dolist (selected-id selected-ids)
        ;; Selected task BLOCKS current task, current task DEPENDS_ON selected task
        (org-gtd-task-management--add-to-multivalued-property "ORG_GTD_DEPENDS_ON" selected-id)
        (org-gtd-task-management--add-to-other-task-multivalued-property selected-id "ORG_GTD_BLOCKS" current-id))

      ;; Update project TODO states (if task belongs to a project)
      (when project-id
        (org-gtd-task-management--update-project-state project-id))

      (message "Added blocker relationships: %s block %s"
               (mapconcat (lambda (id) (org-gtd-task-management--get-heading-for-id id)) selected-ids ", ")
               current-heading))))

;;;###autoload
(defun org-gtd-task-remove-blockers ()
  "Remove blocking relationships from the current task.
Prompts user to select from current blockers, then removes bidirectional
BLOCKS/DEPENDS_ON relationships."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (project-id (org-gtd-task-management--get-current-project))
         (current-blockers (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

    (if (not current-blockers)
        (message "Task '%s' has no blockers to remove" current-heading)
      (let ((selected-ids (org-gtd-task-management--select-multiple-blocking-task-ids
                          (format "Select blockers to remove from '%s' (complete with empty selection): " current-heading)
                          current-blockers)))
        (when selected-ids
          ;; Remove bidirectional relationships for each selected task
          (dolist (selected-id selected-ids)
            ;; Remove selected task from current task's DEPENDS_ON property
            (org-gtd-task-management--remove-from-multivalued-property "ORG_GTD_DEPENDS_ON" selected-id)
            ;; Remove current task from selected task's BLOCKS property
            (org-gtd-task-management--remove-from-other-task-multivalued-property selected-id "ORG_GTD_BLOCKS" current-id))

          ;; Update project TODO states if task belongs to a project
          (when project-id
            (org-gtd-task-management--update-project-state project-id))

          (message "Removed blocker relationships: %s no longer block %s"
                   (mapconcat (lambda (id) (org-gtd-task-management--get-heading-for-id id)) selected-ids ", ")
                   current-heading))))))

;;;###autoload
(defun org-gtd-task-add-dependent ()
  "Add a task that depends on the current task.
Prompts user to select a task, then creates bidirectional
BLOCKS/DEPENDS_ON relationship."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (project-id (org-gtd-task-management--get-current-project))
         (selected-id (org-gtd-task-management--select-task-id
                       (format "Select task that depends on '%s': " current-heading))))
    (when selected-id
      (when project-id
        ;; Add dependent task to the same project
        (org-gtd-task-management--add-task-to-project selected-id project-id))

      ;; Add bidirectional relationship: current task BLOCKS selected task, selected task DEPENDS_ON current task
      (org-gtd-task-management--add-to-multivalued-property "ORG_GTD_BLOCKS" selected-id)
      (org-gtd-task-management--add-to-other-task-multivalued-property selected-id "ORG_GTD_DEPENDS_ON" current-id)

      ;; Update project TODO states if task belongs to a project
      (when project-id
        (org-gtd-task-management--update-project-state project-id))

      (message "Added dependency relationship: %s depends on %s"
               (org-gtd-task-management--get-heading-for-id selected-id)
               current-heading))))

;;;###autoload
(defun org-gtd-task-add-dependents ()
  "Add tasks that depend on the current task.
Prompts user to select multiple tasks, then creates bidirectional
BLOCKS/DEPENDS_ON relationships.  Prevents circular dependencies with
clear error messages."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (project-id (org-gtd-task-management--get-current-project))
         (selected-ids (org-gtd-task-management--select-multiple-task-ids
                        (format "Select tasks that depend on '%s' (complete with empty selection): " current-heading))))
    (when selected-ids
      ;; Check for circular dependencies before creating any relationships
      (dolist (selected-id selected-ids)
        (org-gtd-task-management--check-circular-dependency selected-id current-id))

      ;; Add selected tasks to the same project if applicable
      (when project-id
        (dolist (selected-id selected-ids)
          (org-gtd-task-management--add-task-to-project selected-id project-id)))

      ;; Add bidirectional relationships for each selected task
      (dolist (selected-id selected-ids)
        ;; Current task BLOCKS selected task, selected task DEPENDS_ON current task
        (org-gtd-task-management--add-to-multivalued-property "ORG_GTD_BLOCKS" selected-id)
        (org-gtd-task-management--add-to-other-task-multivalued-property selected-id "ORG_GTD_DEPENDS_ON" current-id))

      ;; Update project TODO states if task belongs to a project
      (when project-id
        (org-gtd-task-management--update-project-state project-id))

      (message "Added dependent relationships: %s blocks %s"
               current-heading
               (mapconcat (lambda (id) (org-gtd-task-management--get-heading-for-id id)) selected-ids ", ")))))

;;;###autoload
(defun org-gtd-task-clear-relationships ()
  "Clear all blocking and dependency relationships for the current task.
Removes all BLOCKS and DEPENDS_ON properties from the current task and
updates related tasks to maintain bidirectional consistency."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (org-entry-get (point) "ID"))
         (project-id (org-gtd-task-management--get-current-project))
         (blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
         (depends-on-list (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

    (if (or blocks-list depends-on-list)
        (progn
          ;; Only proceed if we have an ID (required for cross-task updates)
          (unless current-id
            (user-error "Task must have an ID to clear relationships"))

          ;; Remove current task's dependencies - update tasks that this task depends on
          (dolist (blocker-id depends-on-list)
            (org-gtd-task-management--remove-from-other-task-multivalued-property
             blocker-id "ORG_GTD_BLOCKS" current-id))

          ;; Remove current task's blockings - update tasks that depend on this task
          (dolist (blocked-id blocks-list)
            (org-gtd-task-management--remove-from-other-task-multivalued-property
             blocked-id "ORG_GTD_DEPENDS_ON" current-id))

          ;; Clear properties from current task
          (when blocks-list
            (org-entry-delete (point) "ORG_GTD_BLOCKS"))
          (when depends-on-list
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON"))

          ;; Remove project name since task is no longer connected to project
          (org-entry-delete (point) org-gtd-prop-project)

          ;; Update project TODO states if task belongs to a project
          (when project-id
            (org-gtd-task-management--update-project-state project-id))

          ;; Show confirmation message with proper pluralization
          (message "Cleared relationships for %s: removed %d %s and %d %s"
                   current-heading
                   (length depends-on-list)
                   (org-gtd-task-management--pluralize (length depends-on-list) "blocker" "blockers")
                   (length blocks-list)
                   (org-gtd-task-management--pluralize (length blocks-list) "dependent" "dependents")))

      (message "Task %s has no relationships to clear" current-heading))))

;;;; Circular Dependency Detection (Story 13)

(defun org-gtd-task-management--check-circular-dependency (dependent-id blocker-id)
  "Check if BLOCKER-ID blocking DEPENDENT-ID would create a circular dependency.
Throws an error with a descriptive path if a cycle is detected."
  (org-gtd-dependencies-validate-acyclic blocker-id dependent-id))


(defun org-gtd-task-management--get-task-dependencies (task-id)
  "Get list of task IDs that TASK-ID depends on (its ORG_GTD_DEPENDS_ON property).
Returns empty list if task not found or has no dependencies."
  (or (org-gtd-get-task-dependencies task-id) '()))

(defun org-gtd-task-management--get-blocked-tasks-for-cycle-detection (task-id)
  "Get list of task IDs that TASK-ID blocks (its ORG_GTD_BLOCKS property).
For circular dependency detection, we follow the ORG_GTD_BLOCKS relationship
to find chains.  Returns empty list if task not found or blocks nothing."
  (or (org-gtd-get-task-blockers task-id) '()))


;;;; Private Helper Functions

(defun org-gtd-task-management--get-current-project ()
  "Get the project ID for the task at point.
If task belongs to multiple projects, prompts user to select one.
Returns nil if task doesn't belong to any project."
  (let ((project-ids (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
    (cond
     ((null project-ids) nil)
     ((= 1 (length project-ids)) (car project-ids))
     (t
      ;; Multiple projects - ask user
      (let* ((choices (mapcar (lambda (id)
                               (cons (org-gtd-task-management--get-heading-for-id id) id))
                             project-ids))
             (selected (completing-read "Which project are you working in? " choices nil t)))
        (cdr (assoc selected choices)))))))

(defun org-gtd-task-management--add-task-to-project (task-id project-id)
  "Add TASK-ID to PROJECT-ID by updating ORG_GTD_PROJECT_IDS property."
  (when-let ((marker (org-id-find task-id t)))
    (org-with-point-at marker
      (let ((current-projects (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
        (unless (member project-id current-projects)
          (org-entry-add-to-multivalued-property (point) org-gtd-prop-project-ids project-id))))))

(defun org-gtd-task-management--update-project-state (project-id)
  "Update TODO states for PROJECT-ID after dependency changes."
  (when-let ((marker (org-id-find project-id t)))
    (org-gtd-projects-fix-todo-keywords marker)))

(defun org-gtd-task-management--pluralize (count singular plural)
  "Return SINGULAR if COUNT is 1, otherwise PLURAL."
  (if (= 1 count) singular plural))

(defun org-gtd-task-management--select-task-id (prompt)
  "Prompt user to select a task ID using PROMPT.
Returns the selected ID or nil if cancelled."
  ;; For now, simple implementation using completing-read
  ;; Could be enhanced with a more sophisticated picker later
  (let* ((all-ids (org-gtd-task-management--collect-all-task-ids))
         (id-alist (mapcar (lambda (id)
                            (cons (format "%s (%s)"
                                         (org-gtd-task-management--get-heading-for-id id)
                                         id)
                                  id))
                          all-ids))
         (selection (completing-read prompt id-alist nil t)))
    (cdr (assoc selection id-alist))))

(defun org-gtd-task-management--select-multiple-task-ids (prompt)
  "Prompt user to select multiple task IDs using PROMPT.
Returns a list of selected IDs or nil if cancelled/empty."
  (let* ((all-task-info (org-gtd-task-management--collect-all-task-info))
         (id-alist (mapcar (lambda (task-info)
                            (let ((id (plist-get task-info :id))
                                  (heading (plist-get task-info :heading))
                                  (project (plist-get task-info :project))
                                  (_file (plist-get task-info :file)))
                              (cons (if project
                                        (format "%s [%s] (%s)" heading project id)
                                      (format "%s (%s)" heading id))
                                    id)))
                          all-task-info))
         (selected-ids '())
         (continue t))
    ;; Multi-select loop: keep prompting until user selects empty or cancels
    (while (and continue id-alist)
      (let ((selection (completing-read prompt id-alist nil t)))
        (if (string-empty-p selection)
            (setq continue nil) ; Empty selection completes the multi-select
          ;; Add selected ID to list and remove from available options
          (let ((selected-id (cdr (assoc selection id-alist))))
            (when selected-id
              (push selected-id selected-ids)
              ;; Remove selected item from future selections
              (setq id-alist (remove (assoc selection id-alist) id-alist))
              (setq prompt (format "%s (selected: %d, complete with empty): " 
                                   (car (split-string prompt " ("))
                                   (length selected-ids))))))))
    (reverse selected-ids)))

(defun org-gtd-task-management--select-multiple-blocking-task-ids (prompt current-blockers)
  "Prompt user to select multiple task IDs from CURRENT-BLOCKERS using PROMPT.
Returns a list of selected IDs or nil if cancelled/empty."
  (let* ((id-alist (mapcar (lambda (id)
                            (cons (format "%s (%s)" 
                                         (org-gtd-task-management--get-heading-for-id id)
                                         id) 
                                  id))
                          current-blockers))
         (selected-ids '())
         (continue t))
    ;; Multi-select loop: keep prompting until user selects empty or cancels
    (while (and continue id-alist)
      (let ((selection (completing-read prompt id-alist nil t)))
        (if (string-empty-p selection)
            (setq continue nil) ; Empty selection completes the multi-select
          ;; Add selected ID to list and remove from available options
          (let ((selected-id (cdr (assoc selection id-alist))))
            (when selected-id
              (push selected-id selected-ids)
              ;; Remove selected item from future selections
              (setq id-alist (remove (assoc selection id-alist) id-alist))
              (setq prompt (format "%s (selected: %d, complete with empty): " 
                                   (car (split-string prompt " ("))
                                   (length selected-ids))))))))
    (reverse selected-ids)))

(defun org-gtd-task-management--collect-all-task-info ()
  "Collect all undone task information from current buffer and org-agenda-files.
Returns a list of plists with :id, :heading, :project, :file properties."
  (let ((task-info-list '()))
    ;; Collect from current buffer
    (setq task-info-list
          (org-gtd-task-management--collect-task-info-from-buffer (buffer-name)))
    
    ;; Collect from org-agenda-files (avoiding duplicates)
    (dolist (file (org-agenda-files))
      (when (and (file-exists-p file) (file-readable-p file))
        (let ((file-task-info (org-gtd-task-management--collect-task-info-from-file file)))
          ;; Only add tasks not already present (avoid duplicates from current buffer)
          (dolist (task-info file-task-info)
            (let ((id (plist-get task-info :id)))
              (unless (cl-find id task-info-list :key (lambda (info) (plist-get info :id)) :test 'string=)
                (push task-info task-info-list)))))))
    
    (reverse task-info-list)))

(defun org-gtd-task-management--collect-task-info-from-buffer (file-name)
  "Collect task information from current buffer with FILE-NAME context."
  (let ((task-info-list '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)[ \t]+\\([^*\n]*\\)" nil t)
        (when (org-at-heading-p)
          (when-let ((task-info (org-gtd-task-management--extract-task-info-at-point file-name)))
            (push task-info task-info-list)))))
    task-info-list))

(defun org-gtd-task-management--collect-task-info-from-file (file)
  "Collect task information from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-gtd-task-management--collect-task-info-from-buffer (file-name-base file))))

(defun org-gtd-task-management--extract-task-info-at-point (file-name)
  "Extract task information at current point, returning plist or nil.
FILE-NAME is used for the :file property.
Creates ID automatically if task doesn't have one (lazy ID creation)."
  (let ((todo-state (org-get-todo-state))
        (id (org-entry-get (point) "ID")))
    ;; Include tasks that are undone or have no TODO state (regular headings)
    (when (or (and todo-state (not (org-entry-is-done-p)))
              (not todo-state))
      ;; Create ID if it doesn't exist (lazy ID creation for Story 9)
      (unless id
        (setq id (org-gtd-id-get-create)))
      (list :id id 
            :heading (nth 4 (org-heading-components))
            :project (org-gtd-task-management--find-project-heading)
            :file file-name))))

(defun org-gtd-task-management--collect-all-task-ids ()
  "Collect all undone task IDs from current buffer and org-agenda-files.
This function is kept for backward compatibility."
  (mapcar (lambda (task-info) (plist-get task-info :id))
          (org-gtd-task-management--collect-all-task-info)))

(defun org-gtd-task-management--find-project-heading ()
  "Find the project heading for the current task.
Works for multi-file DAG by using ORG_GTD_PROJECT_IDS if available,
otherwise falls back to outline hierarchy (first level heading)."
  ;; First try to find project via ORG_GTD_PROJECT_IDS (multi-file DAG)
  (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
    (if project-ids
        ;; Use first project ID to find project heading
        (let ((project-marker (org-id-find (car project-ids) t)))
          (when project-marker
            (org-with-point-at project-marker
              (org-get-heading t t t t))))
      ;; Fallback: navigate up outline to find level-1 heading (same-file)
      (save-excursion
        (when (org-up-heading-safe)
          ;; Keep going up until we reach level 1 or can't go higher
          (while (and (> (org-current-level) 1) (org-up-heading-safe)))
          (when (= (org-current-level) 1)
            (nth 4 (org-heading-components))))))))

(defun org-gtd-task-management--add-to-multivalued-property (property value)
  "Add VALUE to the multivalued PROPERTY of the current heading."
  (org-entry-add-to-multivalued-property (point) property value))

(defun org-gtd-task-management--add-to-other-task-multivalued-property (task-id property value)
  "Find heading with TASK-ID and add VALUE to its multivalued PROPERTY."
  (org-gtd-add-to-multivalued-property task-id property value))

(defun org-gtd-task-management--remove-from-multivalued-property (property value)
  "Remove VALUE from the multivalued PROPERTY of the current heading."
  (org-entry-remove-from-multivalued-property (point) property value))

(defun org-gtd-task-management--remove-from-other-task-multivalued-property (task-id property value)
  "Find heading with TASK-ID and remove VALUE from its multivalued PROPERTY."
  (org-gtd-remove-from-multivalued-property task-id property value))

(defun org-gtd-task-management--get-heading-for-id (id)
  "Get heading text for ID, searching current buffer and GTD files."
  (or (org-gtd-task-management--resolve-from-current-buffer id)
      (org-gtd-task-management--resolve-from-id-system id)  
      (org-gtd-task-management--resolve-from-gtd-files id)
      (format "Unknown task (%s)" id)))

(defun org-gtd-task-management--resolve-from-current-buffer (id)
  "Resolve heading text for ID by searching current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
      ;; Go to the beginning of the property block, then find the heading
      (beginning-of-line)
      (while (and (not (org-at-heading-p)) (not (bobp)))
        (forward-line -1))
      (when (org-at-heading-p)
        (nth 4 (org-heading-components))))))

(defun org-gtd-task-management--resolve-from-id-system (id)
  "Resolve heading text for ID using org-id system."
  (let ((marker (org-id-find id t)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (nth 4 (org-heading-components))))))

(defun org-gtd-task-management--resolve-from-gtd-files (id)
  "Resolve heading text for ID by searching GTD files as last resort."
  (let ((gtd-files (org-gtd-core--agenda-files)))
    (catch 'found
      (dolist (file gtd-files)
        (when (and (file-exists-p file) (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (org-mode)
            (goto-char (point-min))
            (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
              ;; Go back to find the heading
              (beginning-of-line)
              (while (and (not (org-at-heading-p)) (not (bobp)))
                (forward-line -1))
              (when (org-at-heading-p)
                (throw 'found (nth 4 (org-heading-components)))))))))))

;;;; Automatic Next Action Updates (Story 15)

(defun org-gtd-task-management--update-dependent-tasks (task-id)
  "Automatically update tasks that depend on TASK-ID when it becomes DONE.
For Story 15: When a task is marked DONE, all tasks blocked by it should
become NEXT if all their dependencies are satisfied."
  (let ((blocked-tasks (org-gtd-task-management--get-blocked-tasks task-id)))
    (dolist (blocked-task-id blocked-tasks)
      (when (org-gtd-task-management--all-dependencies-satisfied-p blocked-task-id)
        (org-gtd-task-management--update-task-to-next blocked-task-id)))))

(defun org-gtd-task-management--get-blocked-tasks (blocker-id)
  "Get list of task IDs that are blocked by BLOCKER-ID.
Returns the ORG_GTD_BLOCKS property value as a list."
  (let ((marker (org-id-find blocker-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      '())))

(defun org-gtd-task-management--all-dependencies-satisfied-p (task-id)
  "Check if all dependencies for TASK-ID are satisfied (DONE).
A task's dependencies are satisfied when all tasks in its
ORG_GTD_DEPENDS_ON property are DONE."
  (let ((marker (org-id-find task-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((dependencies (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
              (if dependencies
                  ;; All dependencies must be DONE
                  (cl-every 'org-gtd-task-management--task-is-done-p dependencies)
                ;; No dependencies = ready to go
                t))))
      nil))) ; If we can't find the task, assume not ready

(defun org-gtd-task-management--task-is-done-p (task-id)
  "Check if TASK-ID is marked as DONE.
First tries current buffer, then falls back to org-id-find."
  (let ((marker (or
                 ;; Try current buffer first (for temp buffer tests)
                 (save-excursion
                   (goto-char (point-min))
                   (when-let ((pos (org-find-entry-with-id task-id)))
                     (goto-char pos)
                     (point-marker)))
                 ;; Fall back to org-id-find
                 (org-id-find task-id t))))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((todo-state (org-entry-get (point) org-gtd-prop-todo)))
              (and todo-state (org-gtd-keywords--is-done-p todo-state)))))
      nil))) ; If we can't find the task, assume not done

(defun org-gtd-task-management--update-task-to-next (task-id)
  "Update TASK-ID to NEXT state if it's currently TODO."
  (let ((marker (org-id-find task-id t)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let ((current-state (org-entry-get (point) org-gtd-prop-todo)))
            ;; Only update if it's currently TODO (not already NEXT, DONE, etc.)
            (when (string= current-state (org-gtd-keywords--todo))
              (org-todo (org-gtd-keywords--next)))))))))

(defun org-gtd-task-management--after-todo-state-change ()
  "Hook function to be called after TODO state changes.
For Story 15: Automatically update dependent tasks when a task becomes DONE."
  (when (org-entry-get (point) "ID")
    (let ((current-state (org-entry-get (point) org-gtd-prop-todo))
          (task-id (org-entry-get (point) "ID")))
      (when (and current-state (org-gtd-keywords--is-done-p current-state))
        (org-gtd-task-management--update-dependent-tasks task-id)))))

;; NOTE: Hook removed - org-edna TRIGGER handles this via
;; org-gtd-update-project-after-task-done! action

;;;###autoload
(defun org-gtd-task-show-relationships ()
  "Display what blocks this task and what it blocks.
Shows the dependency relationships for the task at point in a formatted
summary.

Returns formatted string showing what blocks this task and what this
task blocks."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (_current-id (org-entry-get (point) "ID"))
         (depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
         (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
         (result (org-gtd-task-show-relationships--format-display
                  current-heading depends-on blocks)))
    
    ;; Show in minibuffer when called interactively 
    (when (called-interactively-p 'interactive)
      (message "%s" result))
    
    result))

(defun org-gtd-task-show-relationships--format-display (heading depends-on blocks)
  "Format relationship display for HEADING with DEPENDS-ON and BLOCKS lists."
  ;; If task has no relationships
  (if (and (null depends-on) (null blocks))
      (format "%s has no dependency relationships." heading)
    
    ;; Format relationship display
    (let ((output (list (format "%s Dependencies:" heading))))
      
      ;; Show what blocks this task (dependencies)
      (if depends-on
          (let ((blocker-names (mapcar #'org-gtd-task-management--get-heading-for-id depends-on)))
            (push (format "Blocked by: %s" (string-join blocker-names ", ")) output))
        (push "Blocked by: none" output))

      ;; Show what this task blocks (dependents)
      (if blocks
          (let ((dependent-names (mapcar #'org-gtd-task-management--get-heading-for-id blocks)))
            (push (format "Blocks: %s" (string-join dependent-names ", ")) output))
        (push "Blocks: none" output))
      
      (string-join (reverse output) "\n"))))

;;;###autoload
(defun org-gtd-validate-project-dependencies ()
  "Check project dependencies for broken references and issues.
Scans all projects to detect broken dependency references, orphaned tasks,
and provides guidance for fixing them."
  (interactive)
  (let* ((all-existing-ids (org-gtd-validate-project-dependencies--collect-all-ids))
         (validation-results (org-gtd-validate-project-dependencies--check-all-files all-existing-ids))
         (broken-references (car validation-results))
         (orphaned-tasks (cdr validation-results))
         (guidance (org-gtd-validate-project-dependencies--generate-guidance 
                   broken-references orphaned-tasks)))
    
    (list :broken-references broken-references
          :orphaned-tasks orphaned-tasks
          :guidance guidance)))

(defun org-gtd-validate-project-dependencies--collect-all-ids ()
  "Collect all existing task IDs from agenda files and buffers.
Returns a list of all task IDs found."
  (let ((all-existing-ids '()))
    ;; Collect from file-based agenda files
    (dolist (file-name (org-agenda-files))
      (when (and file-name (file-exists-p file-name))
        (with-temp-buffer
          (insert-file-contents file-name)
          (org-mode)
          (setq all-existing-ids 
                (append all-existing-ids 
                        (org-gtd-validate-project-dependencies--collect-ids-from-buffer))))))

    ;; Collect from buffer-based agenda files (for testing)
    (dolist (buffer-name (org-agenda-files))
      (when (and (not (file-exists-p buffer-name))
                 (get-buffer buffer-name))
        (with-current-buffer buffer-name
          (setq all-existing-ids
                (append all-existing-ids
                        (org-gtd-validate-project-dependencies--collect-ids-from-buffer))))))

    all-existing-ids))

(defun org-gtd-validate-project-dependencies--collect-ids-from-buffer ()
  "Collect all task IDs from the current buffer.
Returns a list of task IDs."
  (let ((buffer-ids '()))
    (org-map-entries
     (lambda ()
       (let ((id (org-entry-get (point) "ID")))
         (when id
           (push id buffer-ids)))))
    buffer-ids))

(defun org-gtd-validate-project-dependencies--check-all-files (all-existing-ids)
  "Check all agenda files for broken references and orphaned tasks.
Returns a cons cell (BROKEN-REFERENCES . ORPHANED-TASKS)."
  (let ((broken-references '())
        (orphaned-tasks '()))
    
    ;; Check file-based agenda files
    (dolist (file-name (org-agenda-files))
      (when (and file-name (file-exists-p file-name))
        (with-temp-buffer
          (insert-file-contents file-name)
          (org-mode)
          (let ((result (org-gtd-validate-project-dependencies--check-buffer all-existing-ids)))
            (setq broken-references (append broken-references (car result)))
            (setq orphaned-tasks (append orphaned-tasks (cdr result)))))))
    
    ;; Check buffer-based agenda files (for testing)
    (dolist (buffer-name (org-agenda-files))
      (when (and (not (file-exists-p buffer-name))
                 (get-buffer buffer-name))
        (with-current-buffer buffer-name
          (let ((result (org-gtd-validate-project-dependencies--check-buffer all-existing-ids)))
            (setq broken-references (append broken-references (car result)))
            (setq orphaned-tasks (append orphaned-tasks (cdr result)))))))
    
    (cons broken-references orphaned-tasks)))

(defun org-gtd-validate-project-dependencies--check-buffer (all-existing-ids)
  "Check current buffer for broken references and orphaned tasks.
Returns a cons cell (BROKEN-REFERENCES . ORPHANED-TASKS)."
  (let ((broken-references '())
        (orphaned-tasks '()))
    (org-map-entries
     (lambda ()
       (let* ((id (org-entry-get (point) "ID"))
              (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
              (depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
              (heading (nth 4 (org-heading-components)))
              (level (org-current-level)))

         (when id
           ;; Check for broken ORG_GTD_BLOCKS references
           (dolist (blocked-id blocks)
             (unless (member blocked-id all-existing-ids)
               (push (list :referencing-task id
                           :missing-task blocked-id
                           :property "ORG_GTD_BLOCKS"
                           :heading heading)
                     broken-references)))

           ;; Check for broken ORG_GTD_DEPENDS_ON references
           (dolist (dependency-id depends-on)
             (unless (member dependency-id all-existing-ids)
               (push (list :referencing-task id
                           :missing-task dependency-id
                           :property "ORG_GTD_DEPENDS_ON"
                           :heading heading)
                     broken-references)))
           
           ;; Check for orphaned tasks (level-1 tasks with dependencies)
           (when (and (= level 1)
                      (or blocks depends-on))
             (push (list :id id
                         :heading heading
                         :level level)
                   orphaned-tasks))))))
    (cons broken-references orphaned-tasks)))

(defun org-gtd-validate-project-dependencies--generate-guidance (broken-references orphaned-tasks)
  "Generate guidance text based on BROKEN-REFERENCES and ORPHANED-TASKS."
  (let ((guidance-parts '()))
    
    (when broken-references
      (push "Found broken dependency references.  Remove invalid property values or create missing tasks."
            guidance-parts))
    
    (when orphaned-tasks
      (push "Found orphaned tasks with dependencies.  Organize these tasks into proper projects."
            guidance-parts))
    
    (if guidance-parts
        (string-join guidance-parts " ")
      "No dependency issues found.")))

;;;###autoload
(defun org-gtd-remove-task-from-project ()
  "Extract the task at point from its project.
Removes the project association while optionally reconnecting dependent
tasks to maintain project structure.

Removes the project ID from the task's ORG_GTD_PROJECT_IDS property.
If the task has multiple projects, prompts user to select which project
to remove from."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))

  (let* ((task-id (or (org-entry-get (point) "ID")
                     (user-error "Task must have an ID to remove from project")))
         (project-ids (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))

    (unless project-ids
      (user-error "Task does not belong to any projects"))

    ;; For now, just remove from the first (or only) project
    ;; TODO: If multiple projects, prompt user to select
    (let ((project-id (car project-ids))
          (children-ids (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))
          (parent-ids (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

      ;; If task has children, ask about reconnection
      (when (and children-ids
                 (y-or-n-p "Reconnect children to parents? "))
        ;; Handle reconnection based on number of parents
        (cond
         ;; One parent: auto-connect
         ((= (length parent-ids) 1)
          (let ((parent-id (car parent-ids)))
            (dolist (child-id children-ids)
              ;; Create parent → child relationship
              (org-gtd-task-management--add-to-other-task-multivalued-property parent-id "ORG_GTD_BLOCKS" child-id)
              (org-gtd-task-management--add-to-other-task-multivalued-property child-id "ORG_GTD_DEPENDS_ON" parent-id))))
         ;; Multiple parents: let user choose
         ;; TODO: Implement multi-parent selection
         ;; No parents: children become new first tasks
         ;; TODO: Implement first task promotion
         ))

      ;; Remove bidirectional relationships between this task and its parents/children
      (dolist (parent-id parent-ids)
        (org-gtd-task-management--remove-from-other-task-multivalued-property parent-id "ORG_GTD_BLOCKS" task-id))
      (dolist (child-id children-ids)
        (org-gtd-task-management--remove-from-other-task-multivalued-property child-id "ORG_GTD_DEPENDS_ON" task-id))

      ;; Clear this task's relationships
      (when parent-ids
        (org-entry-delete (point) "ORG_GTD_DEPENDS_ON"))
      (when children-ids
        (org-entry-delete (point) "ORG_GTD_BLOCKS"))

      ;; Remove project ID from task
      (org-entry-remove-from-multivalued-property (point) org-gtd-prop-project-ids project-id)

      ;; TODO: Remove from project's ORG_GTD_FIRST_TASKS if present

      ;; Check if this was the last project - if so, convert to single action
      (let ((remaining-projects (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
        (when (or (null remaining-projects) (equal remaining-projects '("")))
          ;; This was the last project - convert task to single action (property only, stay in place)
          (org-gtd-configure-as-type 'next-action)))

      (message "Removed task from project %s" project-id))))

;;;; Footer

(provide 'org-gtd-task-management)

;;; org-gtd-task-management.el ends here