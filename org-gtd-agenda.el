;;; org-gtd-agenda.el --- Manage the agenda view -*- lexical-binding: t; coding: utf-8 -*-
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

(require 'winner)
(require 'org-agenda)

(defconst org-gtd-stuck-projects
  '("+LEVEL=2&+ORG_GTD=\"Projects\"" ("NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.

This is a list of four items, the same type as in `org-stuck-projects'.")

;;;###autoload
(defun org-gtd-agenda-daily ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (with-org-gtd-context
   (org-agenda nil "g")))

;;;###autoload
(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (with-org-gtd-context
      (org-todo-list "NEXT")))

;;;###autoload
(defun org-gtd-agenda-projectify ()
  "Transform the current agenda item into a gtd project.

This function is intended to be used on incubated items that come up."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-projectify nil t nil
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
         (org-up-element)
         (org-narrow-to-element)
         (org-show-subtree)
         (display-buffer-same-window buffer '())
         (org-gtd--project)
         (widen)
         (winner-undo))
       (org-agenda-show-tags)))))

;;;###autoload
(defun org-gtd-agenda-delegate ()
  "Delegate current agenda task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-delegate nil t nil
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
         (org-gtd-delegate))
       (org-agenda-show-tags)))))

;;;###autoload
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

(provide 'org-gtd-agenda)
;;; org-gtd-agenda.el ends here
