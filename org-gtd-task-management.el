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

;;;; Interactive Commands

;;;###autoload
(defun org-gtd-task-add-blockers ()
  "Add tasks that block the current task.
Prompts user to select multiple tasks, then creates bidirectional BLOCKS/DEPENDS_ON relationships."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (selected-ids (org-gtd-task-management--select-multiple-task-ids 
                        (format "Select tasks that block '%s' (complete with empty selection): " current-heading))))
    (when selected-ids
      ;; Add bidirectional relationships for each selected task
      (dolist (selected-id selected-ids)
        ;; Selected task BLOCKS current task, current task DEPENDS_ON selected task
        (org-gtd-task-management--add-to-multivalued-property "DEPENDS_ON" selected-id)
        (org-gtd-task-management--add-to-other-task-multivalued-property selected-id "BLOCKS" current-id))
      (message "Added blocker relationships: %s block %s" 
               (mapconcat (lambda (id) (org-gtd-task-management--get-heading-for-id id)) selected-ids ", ")
               current-heading))))

;;;###autoload  
(defun org-gtd-task-add-dependent ()
  "Add a task that depends on the current task.
Prompts user to select a task, then creates bidirectional BLOCKS/DEPENDS_ON relationship."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point must be on an org heading"))
  
  (let* ((current-heading (nth 4 (org-heading-components)))
         (current-id (or (org-entry-get (point) "ID")
                        (org-gtd-id-get-create)))
         (selected-id (org-gtd-task-management--select-task-id
                       (format "Select task that depends on '%s': " current-heading))))
    (when selected-id
      ;; Add bidirectional relationship: current task BLOCKS selected task, selected task DEPENDS_ON current task
      (org-gtd-task-management--add-to-multivalued-property "BLOCKS" selected-id)
      (org-gtd-task-management--add-to-other-task-multivalued-property selected-id "DEPENDS_ON" current-id)
      (message "Added dependency relationship: %s depends on %s"
               (org-gtd-task-management--get-heading-for-id selected-id)
               current-heading))))

;;;; Private Helper Functions

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
                                  (file (plist-get task-info :file)))
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
FILE-NAME is used for the :file property."
  (let ((todo-state (org-get-todo-state))
        (id (org-entry-get (point) "ID")))
    (when (and id
               (or (and todo-state (not (org-entry-is-done-p)))
                   (not todo-state)))
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
  "Find the project heading (first level heading) for the current task."
  (save-excursion
    (when (org-up-heading-safe)
      ;; Keep going up until we reach level 1 or can't go higher
      (while (and (> (org-current-level) 1) (org-up-heading-safe)))
      (when (= (org-current-level) 1)
        (nth 4 (org-heading-components))))))

(defun org-gtd-task-management--add-to-multivalued-property (property value)
  "Add VALUE to the multivalued PROPERTY of the current heading."
  (org-entry-add-to-multivalued-property (point) property value))

(defun org-gtd-task-management--add-to-other-task-multivalued-property (task-id property value)
  "Find heading with TASK-ID and add VALUE to its multivalued PROPERTY."
  ;; First try current buffer
  (let ((pos (org-gtd-task-management--find-id-in-current-buffer task-id)))
    (if pos
        (progn
          (save-excursion
            (goto-char pos)
            (org-gtd-task-management--add-to-multivalued-property property value)))
      ;; Fall back to org-id system
      (let ((marker (org-id-find task-id t)))
        (if marker
            (with-current-buffer (marker-buffer marker)
              (goto-char marker)
              (org-gtd-task-management--add-to-multivalued-property property value))
          (user-error "Could not find task with ID: %s" task-id))))))

(defun org-gtd-task-management--find-id-in-current-buffer (id)
  "Find position of heading with ID in current buffer.
Returns position of the heading or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
      ;; Go to the beginning of the property block, then find the heading
      (beginning-of-line)
      (while (and (not (org-at-heading-p)) (not (bobp)))
        (forward-line -1))
      (when (org-at-heading-p)
        (point)))))

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
For Story 15: When a task is marked DONE, all tasks blocked by it should become NEXT
if all their dependencies are satisfied."
  (let ((blocked-tasks (org-gtd-task-management--get-blocked-tasks task-id)))
    (dolist (blocked-task-id blocked-tasks)
      (when (org-gtd-task-management--all-dependencies-satisfied-p blocked-task-id)
        (org-gtd-task-management--update-task-to-next blocked-task-id)))))

(defun org-gtd-task-management--get-blocked-tasks (blocker-id)
  "Get list of task IDs that are blocked by BLOCKER-ID.
Returns the BLOCKS property value as a list."
  (let ((marker (org-id-find blocker-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (org-entry-get-multivalued-property (point) "BLOCKS")))
      '())))

(defun org-gtd-task-management--all-dependencies-satisfied-p (task-id)
  "Check if all dependencies for TASK-ID are satisfied (DONE).
A task's dependencies are satisfied when all tasks in its DEPENDS_ON property are DONE."
  (let ((marker (org-id-find task-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((dependencies (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
              (if dependencies
                  ;; All dependencies must be DONE
                  (cl-every 'org-gtd-task-management--task-is-done-p dependencies)
                ;; No dependencies = ready to go
                t))))
      nil))) ; If we can't find the task, assume not ready

(defun org-gtd-task-management--task-is-done-p (task-id)
  "Check if TASK-ID is marked as DONE."
  (let ((marker (org-id-find task-id t)))
    (if marker
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((todo-state (org-entry-get (point) "TODO")))
              (and todo-state (org-gtd-keywords--is-done-p todo-state)))))
      nil))) ; If we can't find the task, assume not done

(defun org-gtd-task-management--update-task-to-next (task-id)
  "Update TASK-ID to NEXT state if it's currently TODO."
  (let ((marker (org-id-find task-id t)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (let ((current-state (org-entry-get (point) "TODO")))
            ;; Only update if it's currently TODO (not already NEXT, DONE, etc.)
            (when (equal current-state "TODO")
              (org-todo (org-gtd-keywords--next)))))))))

(defun org-gtd-task-management--after-todo-state-change ()
  "Hook function to be called after TODO state changes.
For Story 15: Automatically update dependent tasks when a task becomes DONE."
  (when (org-entry-get (point) "ID")
    (let ((current-state (org-entry-get (point) "TODO"))
          (task-id (org-entry-get (point) "ID")))
      (when (and current-state (org-gtd-keywords--is-done-p current-state))
        (org-gtd-task-management--update-dependent-tasks task-id)))))

;; Install the hook for automatic updates
(add-hook 'org-after-todo-state-change-hook #'org-gtd-task-management--after-todo-state-change)

;;;; Footer

(provide 'org-gtd-task-management)

;;; org-gtd-task-management.el ends here