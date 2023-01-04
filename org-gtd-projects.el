;;; org-gtd-projects.el --- project management in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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

(defun org-gtd-projects--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

(defun org-gtd-projects--nextify ()
  "Add the NEXT keyword to the first action/task of the project.

Add the TODO keyword to all subsequent actions/tasks."
  (cl-destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
        (reverse rest-entries)
        'headline
      (lambda (myelt)
        (org-entry-put (org-gtd-projects--org-element-pom myelt) "TODO" "TODO")))
    (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TODO" "NEXT")))



(defun org-gtd-projects--renextify (marker)
  "Documentation TK"
    (let ((mybuf (marker-buffer marker))
	  (mypos (marker-position marker)))
      (with-current-buffer mybuf
	(goto-char mypos)
	(cl-destructuring-bind
	    (first-entry . rest-entries)
	    (cdr (org-map-entries #'org-element-at-point t 'tree))
	  (org-element-map
	      rest-entries
	      'headline
	    (lambda (elt)
	      (if (string-equal (org-element-property :todo-keyword elt) "NEXT")
		  (org-entry-put (org-gtd-projects--org-element-pom elt) "TODO" "TODO"))))
	  (org-entry-put (org-gtd-projects--org-element-pom first-entry) "TODO" "NEXT")))))

(defun org-gtd-projects--incomplete-task-p ()
  "Determine if current heading is a task that's not finished."
  (and (org-entry-is-todo-p)
       (not (org-entry-is-done-p))))

(provide 'org-gtd-projects)
;;; org-gtd-projects.el ends here
