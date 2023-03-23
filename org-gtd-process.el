;;; org-gtd-process.el --- Code to process inbox -*- lexical-binding: t; coding: utf-8 -*-
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
;; Inbox processing management for org-gtd.
;;
;;; Code:

(require 'transient)
(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-organize)

(defcustom org-gtd-process-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element.  The default value has
one function: setting org tags on the item.  Some built-in examples are
provided as options here.  You can create your own functions to enhance/decorate
the items once they have been processed and add them to that list."
  :group 'org-gtd
  :package-version '(org-gtd . "1.0.4")
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

;;;###autoload
(defvar org-gtd-process-map (make-sparse-keymap)
  "Keymap for command `org-gtd-process-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-gtd-process-mode
  "Minor mode for org-gtd."
  :lighter " GPM"
  :keymap org-gtd-process-map
  (if org-gtd-process-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-process-map>Clarify item.  Let Org GTD store it with `\\[org-gtd-choose]'."))
    (setq-local header-line-format nil)))

;;;###autoload
(defun org-gtd-process-inbox ()
  "Process the GTD inbox."
  (interactive)
  (set-buffer (org-gtd--inbox-file))
  (display-buffer-same-window (current-buffer) '())
  (delete-other-windows)

  (org-gtd-process-mode)

  (condition-case err
      (progn
        (widen)
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-back-to-heading)
        (org-narrow-to-subtree))
    (user-error (org-gtd-process--stop))))

;;;###autoload
(defmacro org-gtd-process-action (fun-name docstring &rest body)
  "Creates a function to hook into the transient for inbox item organization"
  (declare (debug t) (indent defun))
  `(defun ,fun-name ()
     ,docstring
     (interactive)
     (goto-char (point-min))
     (unwind-protect (progn ,@body))
     (org-gtd-process-inbox)))

(org-gtd-process-action
    org-gtd--archive
  "Process GTD inbox item as a reference item."
  (org-gtd-organize-task-at-point-as-archived-knowledge))

(org-gtd-process-action
  org-gtd--project
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd-organize-task-at-point-as-new-project))

(org-gtd-process-action
  org-gtd--modify-project
  "Refile the org heading at point under a chosen heading in the agenda files."
  (org-gtd-organize-add-task-at-point-to-existing-project))

(org-gtd-process-action
  org-gtd--modify-project
  "Refile the org heading at point under a chosen heading in the agenda files."
  (org-gtd-organize-add-task-at-point-to-existing-project))

(org-gtd-process-action
  org-gtd--calendar
  "Process GTD inbox item by scheduling it."
  (org-gtd-organize-task-at-point-as-appointment))

(org-gtd-process-action
  org-gtd--delegate
  "Process GTD inbox item by delegating it."
  (org-gtd-organize-delegate-task-at-point))

(org-gtd-process-action
  org-gtd--incubate
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to any org-gtd incubate target (see manual)."
  (org-gtd-organize-incubate-task-at-point))

(org-gtd-process-action
  org-gtd--quick-action
  "This was a quick action, and you've just done it."
  (org-gtd-organize-task-at-point-was-quick-action))

(org-gtd-process-action
  org-gtd--single-action
  "Set this as a single action to be done when possible."
  (org-gtd-organize-task-at-point-as-single-action))

(org-gtd-process-action
  org-gtd--trash
  "You're not going to do this, set this as cancelled."
  (org-gtd-organize-task-at-point-as-trash))

;;;###autoload
(defun org-gtd-process--stop ()
  "Stop processing the inbox."
  (interactive)
  (widen)
  (org-gtd-process-mode -1)
  (whitespace-cleanup))

(provide 'org-gtd-process)
;;; org-gtd-process.el ends here
