;;; org-gtd-agenda.el --- Manage the agenda view -*- lexical-binding: t; coding: utf-8 -*-
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
;; Agenda management for org-gtd.
;;
;;; Code:

(require 'winner)
(require 'org-agenda)
(require 'org-gtd-customize)
(require 'org-gtd-core)

;;;###autoload
(defun org-gtd-engage ()
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


(defun org-gtd-agenda--run-function (func &optional moving-operation)
  "Run FUNC on the current agenda item.

If item will be moved, set MOVING-OPERATION to t.
Used as a helper function to run all possible actions on an item."
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   (lambda () (apply org-gtd-agenda-run-function '(func))) nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (when moving-operation
           (org-up-element)
           (org-narrow-to-element)
           (org-fold-show-subtree)
           (display-buffer-same-window buffer '()))
         (apply func nil)
         (when moving-operation (widen)
               (winner-undo)))
       (org-agenda-show-tags)))))

;;;###autoload
(defun org-gtd-agenda-projectify ()
  "Transform the current agenda item into a gtd project.

This function is intended to be used on incubated items that come up."
  (interactive)
  (org-gtd-agenda--run-function 'org-gtd--project t))

;;;###autoload
(defun org-gtd-agenda-delegate ()
  "Delegate current agenda task."
  (interactive)
  (org-gtd-agenda--run-function 'org-gtd-delegate))

;;;###autoload
(defun org-gtd-agenda-cancel-project ()
  "Cancel the project that has the highlighted task."
  (interactive)
  (org-gtd-agenda--run-function (lambda () (progn (org-up-heading-safe) (org-gtd-cancel-project)))))

;;;###autoload
(defun org-gtd-agenda-trash ()
  "Move the current agenda item to trash."
  (interactive)
  (org-gtd-agenda--run-function 'org-gtd--trash t))

;;;###autoload
(defun org-gtd-agenda-single-action ()
  "Transform the current agenda item into a single action.

This function is intended to be used on incubated items that come up."
  (interactive)
  (org-gtd-agenda--run-function 'org-gtd--single-action t))

;;;###autoload
(defun org-gtd-agenda-quick-action ()
  "Process the current agenda item by doing it now.

This function is intended to be used on incubated items that come up."
  (interactive)
  (org-gtd-agenda--run-function 'org-gtd--quick-action t))

(provide 'org-gtd-agenda)
;;; org-gtd-agenda.el ends here
