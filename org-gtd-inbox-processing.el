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
;; Inbox processing management for org-gtd.
;;
;;; Code:

(defvar org-gtd-command-map (make-sparse-keymap)
  "Keymap for function `org-gtd-user-input-mode', a minor mode.")

(define-minor-mode org-gtd-user-input-mode
  "Minor mode for org-gtd."
  nil "GTD " org-gtd-command-map
  (setq-local header-line-format
              (substitute-command-keys
               "\\<org-gtd-command-map>Clarify buffer.  Finish \
`\\[org-gtd-clarify-finalize]'.")))

(defun org-gtd--clarify-item ()
  "User interface to reflect on and clarify the current inbox item."
  (org-gtd-user-input-mode 1)
  (recursive-edit))

;;;###autoload
(defun org-gtd-clarify-finalize ()
  "Finalize the clarify process."
  (interactive)
  (org-gtd-user-input-mode -1)
  (exit-recursive-edit))

;;;###autoload
(defun org-gtd-process-inbox ()
  "Process the GTD inbox.
Use this once a day and/or weekly as part of the weekly review."
  (interactive)
  (set-buffer (org-gtd--inbox-file))
  (display-buffer-same-window (org-gtd--inbox-file) '())
  (delete-other-windows)

  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (setq org-map-continue-from (org-element-property
                                      :begin
                                      (org-element-at-point)))
         (org-narrow-to-element)
         (org-show-subtree)
         (org-gtd--process-inbox-element)
         (widen))))
  (setq-local header-line-format nil))

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
  (org-gtd--clarify-item)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-gtd--nextify)
  (org-gtd--refile org-gtd-projects)
  ;; TODO update statistics more intelligently, probably in inbox
  (with-current-buffer (org-gtd--default-action-file)
    (org-update-statistics-cookies t)))

(defun org-gtd--calendar ()
  "Process GTD inbox item by scheduling it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-calendar))

(defun org-gtd--delegate ()
  "Process GTD inbox item by delegating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set it as a waiting action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-gtd-delegate)
  (org-gtd--refile org-gtd-actions))

(defun org-gtd--incubate ()
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to any org-gtd incubate target (see manual)."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-incubated))

(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "NEXT")
  (org-gtd--refile org-gtd-actions))

(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  (org-gtd--clarify-item)
  (org-gtd--decorate-item)
  (org-todo "CNCL")
  (org-archive-subtree))

(defun org-gtd--decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (goto-char (point-min))
  (dolist (hook org-gtd-process-item-hooks)
    (funcall hook)))

(provide 'org-gtd-inbox-processing)
