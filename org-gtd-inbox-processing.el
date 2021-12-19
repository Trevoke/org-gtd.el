;;; org-gtd-inbox-processing.el --- Code to process inbox -*- lexical-binding: t; coding: utf-8 -*-
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
;; Inbox processing management for org-gtd.
;;
;;; Code:

(defvar org-gtd-process-map (make-sparse-keymap)
  "Keymap for `org-gtd-process-mode', a minor mode.")

(define-minor-mode org-gtd-process-mode
  "Minor mode for org-gtd."
  nil " GPR" org-gtd-process-map
  :global nil
  (if org-gtd-process-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-process-map>Clarify item.  Finish `\\[org-gtd-choose]'."))
    (setq-local header-line-format nil)))

(require 'transient)
(transient-define-prefix org-gtd-choose ()
  ["Actionable"
   ("p" "project" org-gtd--project)
   ("q" "quick" org-gtd--quick-action)
   ("c" "calendar" org-gtd--calendar)
   ("d" "delegate" org-gtd--delegate)
   ("s" "single" org-gtd--single-action)]
  ["Non-actionable"
   ("t" "trash" org-gtd--trash)
   ("a" "archive" org-gtd--archive)
   ("i" "incubate" org-gtd--incubate)]
  ["Org GTD"
   ("x" "exit early" org-gtd--stop-processing)])

(defun org-gtd--archive ()
  "Process GTD inbox item as a reference item."
  (interactive)
  (org-todo "DONE")
  (org-archive-subtree)
  (org-gtd-process-inbox))

(defun org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-gtd-projects--nextify)
  (beginning-of-buffer)
  (forward-word)
  (backward-word)
  (insert "[/] ")
  (org-update-statistics-cookies t)
  (org-gtd-refile--do org-gtd-projects)
  (org-gtd-process-inbox))

(defun org-gtd--calendar ()
  "Process GTD inbox item by scheduling it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd-refile--do org-gtd-calendar)
  (org-gtd-process-inbox))

(defun org-gtd--delegate ()
  "Process GTD inbox item by delegating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set it as a waiting action and refile to
`org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-gtd-delegate)
  (org-gtd-refile--do org-gtd-actions)
  (org-gtd-process-inbox))

(defun org-gtd--incubate ()
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to any org-gtd incubate target (see manual)."
  (interactive)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd-refile--do org-gtd-incubated)
  (org-gtd-process-inbox))

(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  (interactive)
  (org-back-to-heading)
  (org-gtd--decorate-item)
  (org-todo "DONE")
  (org-archive-subtree)
  (org-gtd-process-inbox))

(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (interactive)
  (org-gtd--decorate-item)
  (org-todo "NEXT")
  (org-gtd-refile--do org-gtd-actions)
  (org-gtd-process-inbox))

(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  (interactive)
  (org-gtd--decorate-item)
  (org-todo "CNCL")
  (org-archive-subtree)
  (org-gtd-process-inbox))

(defun org-gtd--stop-processing ()
  "Private function.

Stop processing the inbox."
  (interactive)
  (widen)
  (org-gtd-process-mode nil)
  (whitespace-cleanup))

(defun org-gtd--decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (goto-char (point-min))
  (dolist (hook org-gtd-process-item-hooks)
    (save-excursion
      (save-restriction
        (funcall hook)))))

(provide 'org-gtd-inbox-processing)
;;; org-gtd-inbox-processing.el ends here
