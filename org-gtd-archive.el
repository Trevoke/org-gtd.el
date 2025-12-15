;;; org-gtd-archive.el --- Logic to archive tasks -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

;;;; Requirements

(require 'f)
(require 'org-archive)
(require 'org-element)

(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-files)
(require 'org-gtd-projects)

;;;; Customization

(defgroup org-gtd-archive nil
  "How to archive completed / canceled items."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-archive-location #'org-gtd-archive-location-func
  "Archive location for org-gtd, or nil to use `org-archive-location'.

When nil, org-gtd uses org-mode's standard `org-archive-location'
variable.
When a function, calls it to get the archive location string.

Set to nil if you want org-gtd to respect your org-mode archive
configuration.
The default function generates gtd_archive_<year>::datetree/ in
`org-gtd-directory'.

See `org-archive-location' for valid location string formats."
  :group 'org-gtd-archive
  :package-version '(org-gtd . "4.0.0")
  :type '(choice (const :tag "Use org-archive-location" nil)
                 (function :tag "Custom function")))

;;;; Constants

(defconst org-gtd-archive-file-format "gtd_archive_%s"
  "File name format for where org-gtd archives things by default.")

;;;; Functions

;;;;; Public

(defun org-gtd--effective-archive-location ()
  "Get effective archive location.
Uses `org-archive-location' if `org-gtd-archive-location' is nil,
otherwise calls `org-gtd-archive-location'."
  (if org-gtd-archive-location
      (funcall org-gtd-archive-location)
    org-archive-location))

;;;; Commands

;;;###autoload
(defun org-gtd-archive-completed-items ()
  "Archive everything that needs to be archived in your org-gtd."
  (interactive)
  ;; v4: Users configure org-agenda-files directly.
  ;; Bind org-archive-location locally for all archive operations in this function.
  (let ((org-archive-location (org-gtd--effective-archive-location)))
    ;; Update org-id locations to ensure graph traversal can find all tasks
    ;; Expand directories and filter to get only .org files
    (org-id-update-id-locations
     (org-gtd--expand-agenda-files-to-org-files org-agenda-files))

    ;; Archive projects
    (org-gtd--archive-complete-projects)

    ;; Archive actions, delegated, calendar, tickler, and quick items
    ;; Exclude project tasks (those with ORG_GTD_PROJECT or ORG_GTD_PROJECT_IDS properties)
    (org-map-entries #'org-gtd--archive-completed-actions
                     "+ORG_GTD=\"Actions\"-ORG_GTD_PROJECT={.+}-ORG_GTD_PROJECT_IDS={.+}"
                     'agenda)
    (org-map-entries #'org-gtd--archive-completed-actions
                     "+ORG_GTD=\"Delegated\""
                     'agenda)
    (org-map-entries #'org-gtd--archive-completed-actions
                     "+ORG_GTD=\"Quick\""
                     'agenda)
    (org-map-entries #'org-gtd--archive-completed-actions
                     "+ORG_GTD=\"Calendar\""
                     'agenda)
    (org-map-entries #'org-gtd--archive-completed-actions
                     "+ORG_GTD=\"Tickler\""
                     'agenda)))

(defun org-gtd-archive-item-at-point ()
  "Archive the subtree at point to the GTD archive location.
Moves the subtree to the archive file and removes it from current buffer."
  (interactive)
  (let ((org-archive-location (org-gtd--effective-archive-location)))
    (org-archive-subtree)))

;;;; Functions

;;;;; Public

(defun org-gtd-archive-location-func ()
  "Default function to define where to archive items."
  (let* ((year (number-to-string (caddr (calendar-current-date))))
         (full-org-gtd-path (expand-file-name org-gtd-directory))
         (filename (format org-gtd-archive-file-format year))
         (filepath (f-join full-org-gtd-path filename)))
    (string-join `(,filepath "::" "datetree/"))))

;;;;; Private

(defun org-gtd--expand-agenda-files-to-org-files (paths)
  "Expand PATHS (files and directories) to a list of .org files.
Directories are expanded to their contained .org files recursively."
  (seq-mapcat
   (lambda (path)
     (cond
      ;; If it's a regular file, keep it
      ((file-regular-p path) (list path))
      ;; If it's a directory, expand to .org files
      ((file-directory-p path)
       (directory-files path t "\\.org$" t))
      ;; Otherwise ignore
      (t nil)))
   paths))

(defun org-gtd--remove-project-id-from-task (pom project-id)
  "Remove PROJECT-ID from ORG_GTD_PROJECT_IDS property at position POM."
  (org-entry-remove-from-multivalued-property pom org-gtd-prop-project-ids project-id))

(defun org-gtd--archive-task-if-no-projects (pom project-id)
  "Remove PROJECT-ID from task at POM, archive task if no projects remain.
If ORG_GTD_PROJECT_IDS becomes empty after removing PROJECT-ID, archive
the task.  POM can be a marker or an integer position."
  ;; Check if marker/position is valid before proceeding
  (when (or (numberp pom)  ; position in current buffer
            (and (markerp pom)  ; or valid marker
                 (marker-buffer pom)
                 (marker-position pom)
                 (buffer-live-p (marker-buffer pom))))
    ;; First, remove the project ID
    (org-gtd--remove-project-id-from-task pom project-id)

    ;; Then check if any projects remain
    (let ((should-archive nil))
      (org-with-point-at pom
        (let ((remaining-projects (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
          (when (null remaining-projects)
            (setq should-archive t))))

      ;; Only archive if no projects remain
      ;; v4: Bind org-archive-location locally instead of using with-org-gtd-context
      (when should-archive
        (let ((org-archive-location (org-gtd--effective-archive-location)))
          (org-with-point-at pom
            (org-archive-subtree-default)))))))

(defun org-gtd--all-project-tasks-done-p ()
  "Return t if all tasks connected to current project are done.
Uses graph traversal to find all project tasks via BLOCKS/DEPENDS_ON
relationships.  Falls back to checking immediate children if no
dependency-tracked tasks exist."
  (require 'org-gtd-projects)
  (let* ((project-marker (point-marker))
         (tasks (org-gtd-projects--collect-tasks-by-graph project-marker)))
    (if tasks
        ;; Has dependency-tracked tasks - check if all done
        (seq-every-p
         (lambda (task-marker)
           (org-with-point-at task-marker
             (eq (org-element-property :todo-type (org-element-at-point)) 'done)))
         tasks)
      ;; No dependency tracking - check immediate children
      (let ((all-done t))
        (save-excursion
          (org-back-to-heading t)
          (let ((level (org-current-level)))
            (outline-next-heading)
            (while (and (not (eobp)) (> (org-current-level) level))
              (when (= (org-current-level) (1+ level))
                (unless (eq (org-element-property :todo-type (org-element-at-point)) 'done)
                  (setq all-done nil)))
              (outline-next-heading))))
        all-done))))

(defun org-gtd--archive-project-with-tasks ()
  "Archive current project heading and handle multi-project tasks.

For each task in the project graph:
1. Remove the current project's ID from ORG_GTD_PROJECT_IDS
2. Archive the task only if ORG_GTD_PROJECT_IDS becomes empty

Then archive the project heading itself (without child tasks, since they're
handled separately via graph traversal)."
  (require 'org-gtd-projects)
  (let* ((project-marker (point-marker))
         (project-id (org-entry-get (point) "ID"))
         (tasks (org-gtd-projects--collect-tasks-by-graph project-marker)))

    ;; Step 1: Process each task - remove project ID and archive if no projects remain
    (when (and project-id tasks)
      (dolist (task-marker tasks)
        ;; Check if task still exists and is valid
        (when (and (marker-buffer task-marker)
                   (marker-position task-marker)
                   (with-current-buffer (marker-buffer task-marker)
                     (save-excursion
                       (goto-char (marker-position task-marker))
                       (org-at-heading-p))))
          ;; Remove project ID and archive if no projects remain
          (org-gtd--archive-task-if-no-projects task-marker project-id))))

    ;; Step 2: Move shared tasks (those with remaining project IDs) out of project subtree
    ;; This prevents them from being archived with the project
    (org-with-point-at project-marker
      (save-excursion
        (org-back-to-heading t)
        (let ((level (org-current-level))
              (tasks-to-refile '()))
          (outline-next-heading)
          ;; Collect child tasks that still have project IDs (i.e., shared tasks)
          (while (and (not (eobp)) (> (org-current-level) level))
            (when (and (= (org-current-level) (1+ level))
                       (org-entry-get (point) org-gtd-prop-project-ids))
              (push (point-marker) tasks-to-refile))
            (outline-next-heading))
          ;; Refile shared tasks to Actions category at top level
          (dolist (task-marker (nreverse tasks-to-refile))
            (org-with-point-at task-marker
              (org-cut-subtree)
              ;; Find Actions heading and paste there
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (if (re-search-forward "^\\* Actions" nil t)
                    (progn
                      (org-end-of-subtree)
                      (newline)
                      (org-paste-subtree 2))
                  ;; If no Actions heading exists, create one
                  (goto-char (point-max))
                  (insert "\n* Actions\n")
                  (org-paste-subtree 2))))))))

    ;; Step 3: Archive the project heading (now without shared tasks)
    (org-with-point-at project-marker
      (org-archive-subtree-default))))

(defun org-gtd--archive-complete-projects ()
  "Archive all projects for which all actions/tasks are marked as done.

Done here is any done `org-todo-keyword'.  For org-gtd this means `org-gtd-done'
or `org-gtd-canceled'.

For multi-project tasks, removes project ID from ORG_GTD_PROJECT_IDS and only
archives the task when no project IDs remain."
  (org-map-entries
   (lambda ()
     ;; Skip category headings that contain other projects
     (let ((has-project-children nil))
       (save-excursion
         (org-back-to-heading t)
         (let ((level (org-current-level)))
           (outline-next-heading)
           (while (and (not (eobp)) (not has-project-children) (> (org-current-level) level))
             (when (string= (org-entry-get (point) "ORG_GTD") "Projects")
               (setq has-project-children t))
             (outline-next-heading))))

       (when (and (not has-project-children)
                  (org-gtd--all-project-tasks-done-p))
         (setq org-map-continue-from
               (org-element-property :begin (org-element-at-point)))
         (org-gtd--archive-project-with-tasks))))
   "+ORG_GTD=\"Projects\""
   'agenda))

(defun org-gtd--archive-completed-actions ()
  "Private function.  With point on heading, archive if entry is done."
  (if (org-entry-is-done-p)
      (progn
        (setq org-map-continue-from (org-element-property
                                     :begin
                                     (org-element-at-point)))
        (org-archive-subtree-default))))

;;;; Footer

(provide 'org-gtd-archive)

;;; org-gtd-archive.el ends here
