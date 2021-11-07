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
;; Agenda management for org-gtd.
;;
;;; Code:

(require 'org-agenda)
(require 'org-gtd-projects)

(defconst org-gtd-stuck-projects
  '("+LEVEL=2&+CATEGORY=\"Projects\"" ("NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.
This is a list of four items, the same type as in `org-stuck-projects'.")

(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-todo-list "NEXT"))

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

(provide 'org-gtd-agenda)
