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

;;;; Requirements

(require 'f)
(require 'org)
(require 'org-element)
(require 'org-edna)

(require 'org-gtd-core)
(require 'org-gtd-refile)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-add-to-project-func #'org-gtd-project-extend--apply
  "Function called when organizing item at point as a new task in a project.")

(defconst org-gtd-project-func #'org-gtd-project-new--apply
  "Function called when organizing item at point as a project.")

(defconst org-gtd-project-headings
  "+LEVEL=2&+ORG_GTD=\"Projects\""
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

(defconst org-gtd-stuck-projects
  `(,org-gtd-project-headings
    (,org-gtd-next ,org-gtd-wait)
    nil
    "")
  "How to identify stuck projects in the GTD system.

This is a list of four items, the same type as in `org-stuck-projects'.")

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
             (org-todo org-gtd-canceled))))
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
                       (member (org-entry-get (point) "TODO")
                               `(,org-gtd-todo ,org-gtd-wait ,org-gtd-done ,org-gtd-canceled)))
             (org-entry-put (point) "TODO" org-gtd-todo)
             (setq org-map-continue-from (end-of-line))))
         t
         'tree)
        (let ((first-wait (org-gtd-projects--first-wait-task))
              (first-todo (org-gtd-projects--first-todo-task)))
          (unless first-wait
            (org-entry-put (org-gtd-projects--org-element-pom first-todo)
                           "TODO"
                           org-gtd-next)))))))

;;;;; Private

(defun org-gtd-project-new--apply ()
  "Process GTD inbox item by transforming it into a project.

Allow the user apply user-defined tags from `org-tag-persistent-alist',
`org-tag-alist' or file-local tags in the inbox.
Refile to `org-gtd-actionable-file-basename'."
  (when (org-gtd-projects--poorly-formatted-p)
    (org-gtd-projects--show-error)
    (throw 'org-gtd-error "Malformed project"))

  (setq-local org-gtd--organize-type 'project-heading)
  (org-gtd-organize-apply-hooks)

  (setq-local org-gtd--organize-type 'project-task)
  (org-gtd-projects--apply-organize-hooks-to-tasks)

  (org-gtd-projects-fix-todo-keywords-for-project-at-point)
  (let ((org-special-ctrl-a t))
    (org-end-of-line))
  (insert " [/]")
  (org-update-statistics-cookies t)

  (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))

(defun org-gtd-project-extend--apply ()
  "Refile the org heading at point under a chosen heading in the agenda files."
  (with-org-gtd-context
      (setq-local org-gtd--organize-type 'project-task)
      (org-gtd-organize-apply-hooks)

    (org-gtd-refile--do-project-task)
    (let ((marker (save-excursion
                    (org-refile-goto-last-stored)
                    (org-up-heading-safe)
                    (point-marker))))
      (org-gtd-projects-fix-todo-keywords marker))))

(defun org-gtd-projects--apply-organize-hooks-to-tasks ()
  "Decorate tasks for project at point."
  (org-map-entries
   (lambda ()
     (org-narrow-to-element)
     (org-gtd-organize-apply-hooks)
     (widen))
   "LEVEL=2"
   'tree))

(defun org-gtd-projects--edna-next-project-action ()
  "`org-edna' extension to find the next action to show in the agenda."
  (org-edna-finder/relatives 'forward-no-wrap 'todo-only 1 'no-sort))

(defalias 'org-edna-finder/org-gtd-next-project-action
  'org-gtd-projects--edna-next-project-action)

(defun org-gtd-projects--edna-update-project-task (_last-entry)
  "`org-edna' extension to change the todo state to `org-gtd-next'."
  (org-todo org-gtd-next))

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
                         (string-equal org-gtd-todo
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
                         (string-equal org-gtd-wait
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
