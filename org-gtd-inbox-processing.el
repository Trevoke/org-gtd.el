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

(defun org-gtd--process-inbox-element ()
  "With point on an item, choose which GTD action to take."
  (let ((action
         (read-multiple-choice
          "What to do with this item?"
          '((?q "quick" "quick item: < 2 minutes, done!")
            (?t "throw out" "this has no value to me")
            (?p "project" "multiple steps required to completion")
            (?c "calendar" "do this at a certain time")
            (?d "delegate it" "give it to someone")
            (?s "single action" "do this when possible")
            (?a "archive this knowledge" "Store this where you store knowledge")
            (?i "incubate it" "I'll come back to this later")))))
    (cl-case (car action)
      (?q (org-gtd--quick-action))
      (?t (org-gtd--trash))
      (?p (org-gtd--project))
      (?c (org-gtd--calendar))
      (?d (org-gtd--delegate))
      (?s (org-gtd--single-action))
      (?a (org-gtd--archive))
      (?i (org-gtd--incubate)))))

(defun org-gtd--archive ()
  "Process GTD inbox item as a reference item."
  (org-gtd-clarify--clarify-item)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-gtd-projects--nextify)
  (org-gtd-refile--do org-gtd-projects)
  ;; TODO update statistics more intelligently, probably in inbox
  (with-current-buffer (org-gtd--default-action-file)
    (org-update-statistics-cookies t)))

(defun org-gtd--calendar ()
  "Process GTD inbox item by scheduling it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd-refile--do org-gtd-calendar))

(defun org-gtd--delegate ()
  "Process GTD inbox item by delegating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set it as a waiting action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-gtd-delegate)
  (org-gtd-refile--do org-gtd-actions))

(defun org-gtd--incubate ()
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to any org-gtd incubate target (see manual)."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd-refile--do org-gtd-incubated))

(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "NEXT")
  (org-gtd-refile--do org-gtd-actions))

(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  (org-gtd-clarify--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "CNCL")
  (org-archive-subtree))

(defun org-gtd--decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (goto-char (point-min))
  (dolist (hook org-gtd-process-item-hooks)
    (funcall hook)))

(provide 'org-gtd-inbox-processing)
;;; org-gtd-inbox-processing.el ends here
