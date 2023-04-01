;;; org-gtd-projects.el --- project management in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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

(require 'f)
(require 'org)
(require 'org-element)
(require 'org-edna)
(require 'org-gtd-core)
(require 'org-gtd-agenda)

;;;###autoload
(defun org-gtd-cancel-project ()
  "With point on topmost project heading, mark all undone tasks canceled."
  (interactive)
  (org-edna-mode -1)
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-projects--incomplete-task-p)
           (let ((org-inhibit-logging 'note))
             (org-todo "CNCL"))))
       nil
       'tree))
  (org-edna-mode 1))

;;;###autoload
(defun org-gtd-show-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (with-org-gtd-context
      (org-agenda-list-stuck-projects)))

;;;###autoload
(defun org-gtd-projects-fix-todo-keywords-for-project-at-point ()
  "Ensure keywords for subheadings of project at point are sane.

This means one and only one NEXT keyword, and it is the first of type TODO
in the list."
  (interactive)
  (org-gtd-projects-fix-todo-keywords (point-marker)))

(defun org-gtd-projects-fix-todo-keywords (marker)
  "Ensure project at MARKER has only one NEXT keyword. Ensures only the first non-done keyword is NEXT, all other non-done are TODO."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      ;; first, make sure all we have is TODO WAIT DONE CNCL
      (org-map-entries
       (lambda ()
         (unless (member
                  (org-element-property :todo-keyword (org-element-at-point))
                  '("TODO" "WAIT" "DONE" "CNCL"))
           (org-entry-put (org-gtd-projects--org-element-pom (org-element-at-point)) "TODO" "TODO")))
       "+LEVEL=3" 'tree))
    (save-excursion
      (goto-char (marker-position marker))
      (let* ((tasks (org-map-entries #'org-element-at-point "+LEVEL=3" 'tree))
             (first-wait (-any (lambda (x) (and (string-equal "WAIT" (org-element-property :todo-keyword x)) x)) tasks))
             (first-todo (-any (lambda (x) (and (string-equal "TODO" (org-element-property :todo-keyword x)) x)) tasks)))
        (unless first-wait
          (org-entry-put (org-gtd-projects--org-element-pom first-todo) "TODO" "NEXT"))))))

(defun org-gtd-projects--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

;; TODO rename to something like initialize TODO states
(defun org-gtd-projects--nextify ()
  "Add the NEXT keyword to the first action/task of the project.

Add the TODO keyword to all subsequent actions/tasks."
  (org-map-entries (lambda () (org-gtd-organize--decorate-element (org-element-at-point)) ) "LEVEL=2" 'tree)
  (cl-destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
        (reverse rest-entries)
        'headline
      (lambda (myelt)
        (org-entry-put (org-gtd-projects--org-element-pom myelt) "TODO" "TODO")))
    (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TODO" "NEXT")))

(defun org-gtd-projects--incomplete-task-p ()
  "Determine if current heading is a task that's not finished."
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(defun org-gtd-projects--poorly-formatted-p ()
  "Return non-nil if the project is composed of only one heading."
  ;(basic-save-buffer)
  (eql 1 (length (org-map-entries t))))

(defun org-gtd-projects--show-error-and-return-to-editing ()
  "Tell the user something is wrong with the project."
  (display-message-or-buffer
   "A 'project' in GTD is a finite set of steps after which a given task is
complete. In Org GTD, this is defined as a top-level org heading with at least
one second-level org headings. When the item you are editing is intended to be
a project, create such a headline structure, like so:

* Project heading
** First task
** Second task
** Third task

If you do not need sub-headings, then make a single action instead.")
  (org-gtd-process-inbox))

(provide 'org-gtd-projects)
;;; org-gtd-projects.el ends here
