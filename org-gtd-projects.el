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

(defconst org-gtd-project-headings
  "+LEVEL=2&+ORG_GTD=\"Projects\""
  "How to tell org-mode to find project headings")

(defconst org-gtd-stuck-projects
  `(,org-gtd-project-headings
    (,org-gtd-next ,org-gtd-wait)
    nil
    "")
  "How to identify stuck projects in the GTD system.

This is a list of four items, the same type as in `org-stuck-projects'.")

(defun org-edna-action/org-gtd-update-project-task! (_last-entry)
  (org-todo org-gtd-next))

(defun org-edna-finder/org-gtd-next-project-action ()
  (org-edna-finder/relatives 'forward-no-wrap 'todo-only 1 'no-sort))

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
             (org-todo org-gtd-canceled))))
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

This means one and only one `org-gtd-next' keyword, and it is the first non-done
state in the list - all others are `org-gtd-todo'.."
  (interactive)
  (org-gtd-projects-fix-todo-keywords (point-marker)))

(defun org-gtd-projects-fix-todo-keywords (marker)
  "Ensure project at MARKER has only one `org-gtd-next' keyword. Ensures only
the first non-done keyword is `org-gtd-next', all other non-done are
`org-gtd-todo'."
  (with-current-buffer (marker-buffer marker)
    (org-gtd-core-prepare-buffer)
    (save-excursion
      (goto-char (marker-position marker))
      ;; first, make sure all we have is TODO WAIT DONE CNCL
      (org-map-entries
       (lambda ()
         (unless (member
                  (org-element-property :todo-keyword (org-element-at-point))
                  `(,org-gtd-todo ,org-gtd-wait ,org-gtd-done ,org-gtd-canceled)
                  )
           (org-entry-put (org-gtd-projects--org-element-pom (org-element-at-point))
                          "TODO" org-gtd-todo)))
       "+LEVEL=3" 'tree))
    (save-excursion
      (goto-char (marker-position marker))
      (let* ((tasks (org-map-entries #'org-element-at-point "+LEVEL=3" 'tree))
             (first-wait (-any (lambda (x) (and (string-equal org-gtd-wait (org-element-property :todo-keyword x)) x)) tasks))
             (first-todo (-any (lambda (x) (and (string-equal org-gtd-todo (org-element-property :todo-keyword x)) x)) tasks)))
        (unless first-wait
          (org-entry-put (org-gtd-projects--org-element-pom first-todo) "TODO" org-gtd-next))))))

(defun org-gtd-projects--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

;; TODO rename to something like initialize TODO states
(defun org-gtd-projects--nextify ()
  "Add the `org-gtd-next' keyword to the first action/task of the project.

Add the `org-gtd-todo' keyword to all subsequent actions/tasks."
  (org-map-entries (lambda () (org-gtd-organize--decorate-element
                               (org-element-at-point)) )
                   "LEVEL=2"
                   'tree)
  (cl-destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
        (reverse rest-entries)
        'headline
      (lambda (myelt)
        (org-entry-put (org-gtd-projects--org-element-pom myelt) "TODO" org-gtd-todo)))
    (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TODO" org-gtd-next)))

(defun org-gtd-projects--incomplete-task-p ()
  "Determine if current heading is a task that's not finished."
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(defun org-gtd-projects--poorly-formatted-p ()
  "Return non-nil if the project is composed of only one heading."
  (eql 1 (length (org-map-entries t))))

(defun org-gtd-projects--show-error ()
  "Tell the user something is wrong with the project."
  (let ((resize-mini-windows t)
        (max-mini-window-height 0))
    (display-message-or-buffer org-gtd-projects--malformed))
  (read-key "Waiting for a keypress to return to clarifying... " t)
  (message ""))

(provide 'org-gtd-projects)
;;; org-gtd-projects.el ends here
