;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2019-2021 Aldric Giacomoni

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

(defconst org-gtd-complete-projects
  "+LEVEL=2+CATEGORY=\"Projects\""
  "How to identify projects in the GTD system.")

(defvar org-stuck-projects)

(defconst org-gtd-stuck-projects
  '("+LEVEL=2-TODO=\"DONE\"+CATEGORY=\"Projects\"" ("NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.
This is a list of four items, the same type as in `org-stuck-projects'.")

(defun org-gtd-archive-complete-projects ()
  "Archive all projects for which all actions/tasks are marked as done.
Done here is any done `org-todo-keyword'."
  (interactive)
  (let ((backup org-use-property-inheritance)
        (org-use-property-inheritance "CATEGORY"))
    (with-current-buffer (org-gtd--actionable-file)
      (org-map-entries
       (lambda ()
         (let ((task-states (org-gtd--current-project-states)))
           (when (or (org-gtd--project-complete-p task-states)
                     (org-gtd--project-canceled-p task-states))
             (setq org-map-continue-from (org-element-property
                                          :begin
                                          (org-element-at-point)))
             (org-archive-subtree-default))))

       org-gtd-complete-projects))
    (setq org-use-property-inheritance backup)))

(defun org-gtd-cancel-project ()
  "With point on project heading, mark all undone tasks canceled."
  (interactive)
  (when (eq (current-buffer) (org-gtd--actionable-file))
    (org-edna-mode -1)
    (org-map-entries
     (lambda ()
       (when (org-gtd--incomplete-task-p)
         (let ((org-inhibit-logging 'note))
           (org-todo "CANCELED"))))
     nil
     'tree)
    (org-edna-mode 1)))

(defun org-gtd-agenda-cancel-project ()
  "Cancel the project that has the highlighted task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-cancel-project nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (type (marker-insertion-type marker))
          (buffer (marker-buffer marker))
          (pos (marker-position marker))
          ts)
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (org-up-heading-safe)
         (org-gtd-cancel-project))
       (org-agenda-show-tags)))))

(defun org-gtd-show-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (let* ((user-stuck-projects org-stuck-projects)
         (org-stuck-projects org-gtd-stuck-projects)
         (stuck-projects-buffer (org-agenda-list-stuck-projects))
         (org-stuck-projects user-stuck-projects))
    stuck-projects-buffer))

(defun org-gtd--nextify ()
  "Add the NEXT keyword to the first action/task of the project.
Add the TODO keyword to all subsequent actions/tasks."
  (cl-destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
        (reverse rest-entries)
        'headline
      (lambda (myelt)
        (org-entry-put (org-gtd--org-element-pom myelt) "TODO" "TODO")))
    (org-entry-put (org-gtd--org-element-pom first-entry) "TODO" "NEXT")))

(defun org-gtd--current-project-states ()
  "Return a list of the task states for the current project."
  (cdr (org-map-entries
        (lambda ()
          (org-entry-get
           (org-gtd--org-element-pom (org-element-at-point))
           "TODO"))
        t
        'tree)))

(defun org-gtd--project-complete-p (task-states)
  "Return t if project complete, nil otherwise.
A project is considered complete when all TASK-STATES are
marked with a done `org-todo-keyword'."
  (seq-every-p (lambda (x) (string-equal x "DONE")) task-states))

(defun org-gtd--project-canceled-p (task-states)
  "Return t if project canceled, nil otherwise.
A project is considered canceled when the last of the TASK-STATES is
marked with a canceled `org-todo-keyword'."
  (string-equal "CANCELED" (car (last task-states))))

(defun org-gtd--project-heading-p ()
  "Determine if current heading is a project heading"
  (not (org-entry-is-todo-p)))

(defun org-gtd--incomplete-task-p ()
  "Determine if current heading is a task that's not finished"
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(provide 'org-gtd-projects)
