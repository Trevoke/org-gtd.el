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

(defun org-gtd-cancel-project ()
  "With point on project heading, mark all undone tasks canceled."
  (interactive)
  (org-edna-mode -1)
    (org-map-entries
     (lambda ()
       (when (org-gtd--incomplete-task-p)
         (let ((org-inhibit-logging 'note))
           (org-todo "CNCL"))))
     nil
     'tree)
    (org-edna-mode 1))

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
  (string-equal "CNCL" (car (last task-states))))

(defun org-gtd--project-heading-p ()
  "Determine if current heading is a project heading"
  (not (org-entry-is-todo-p)))

(defun org-gtd--incomplete-task-p ()
  "Determine if current heading is a task that's not finished"
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(defun org-gtd--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

(provide 'org-gtd-projects)
