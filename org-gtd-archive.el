;;; org-gtd-archive.el --- Logic to archive tasks -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

(require 'org-archive)
(require 'org-element)
(require 'org-gtd-core)
(require 'org-gtd-agenda)

;;;###autoload
(defun org-gtd-archive-completed-items ()
  "Archive everything that needs to be archived in your org-gtd."
  (interactive)
  (with-org-gtd-context
   (org-gtd--archive-complete-projects)
   (org-map-entries #'org-gtd--archive-completed-actions
                    "+LEVEL=2&+ORG_GTD=\"Actions\""
                    'agenda)
   (org-map-entries #'org-gtd--archive-completed-actions
                    "+LEVEL=2&+ORG_GTD=\"Calendar\""
                    'agenda)
   (org-map-entries #'org-gtd--archive-completed-actions
                    "+LEVEL=2&+ORG_GTD=\"Incubated\""
                    'agenda)))

(defun org-gtd--archive-complete-projects ()
  "Archive all projects for which all actions/tasks are marked as done.

Done here is any done `org-todo-keyword'.  For org-gtd this means DONE or CNCL."
  (org-map-entries
   (lambda ()

     (when (org-gtd--all-subheadings-in-done-type-p)
       (setq org-map-continue-from
             (org-element-property :begin (org-element-at-point)))

       (org-archive-subtree-default)))
   "+LEVEL=2&+ORG_GTD=\"Projects\""
   'agenda))

(defun org-gtd--all-subheadings-in-done-type-p ()
  "Private function. Returns t if every sub-heading is in a DONE or CNCL state."
  (seq-every-p (lambda (x) (eq x 'done))
               (org-map-entries (lambda ()
                                  (org-element-property :todo-type (org-element-at-point)))
                                "+LEVEL=3"
                                'tree)))

(defun org-gtd--archive-completed-actions ()
  "Private function.  With point on heading, archive if entry is done."
  (if (org-entry-is-done-p)
      (progn
        (setq org-map-continue-from (org-element-property
                                     :begin
                                     (org-element-at-point)))
        (org-archive-subtree-default))))

(provide 'org-gtd-archive)
;;; org-gtd-archive.el ends here
