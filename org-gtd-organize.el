;;; org-gtd-organize.el --- Move tasks where they belong -*- lexical-binding: t; coding: utf-8 -*-
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
;; Move tasks where they need to be in the org-gtd system.
;;
;;; Code:

(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)

(defgroup org-gtd-organize nil
  "Manage the functions for organizing the GTD actions."
  :package-version '(org-gtd . "3.0.0")
  :group 'org-gtd)

(defcustom org-gtd-organize-quick-action-func
  #'org-gtd-organize-task-at-point-was-quick-action
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as an action that you started and finished just now."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-foo
  #'org-gtd-organize-foo
  "Can be an atom or a sexp. Function called when org-gtd..."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item.

Note that this function is intended to be used only during inbox processing.
Each action continues inbox processing, so you may put your Emacs in an
undefined state."
  ["Actionable"
   [("q" "Quick action" org-gtd--quick-action)
    ("s" "Single action" org-gtd--single-action)]
   [("d" "Delegate" org-gtd--delegate)
    ("c" "Calendar" org-gtd--calendar)]
   [("p" "Project (multi-step)" org-gtd--project)
    ("m" "Modify project: add this task" org-gtd--modify-project)]
   ]
  ["Non-actionable"
   [("i" "Incubate" org-gtd--incubate)
    ("a" "Archive this knowledge" org-gtd--archive)]
   [("t" "Trash" org-gtd--trash)]]
  ["Org GTD"
   ("x"
    "Exit. Stop processing the inbox for now."
    org-gtd-process--stop)])

(defalias 'org-gtd-choose
  #'org-gtd-organize)

(make-obsolete 'org-gtd-choose
               #'org-gtd-organize
               "2.3.0")

(defun org-gtd-organize-task-at-point-was-quick-action ()
  "Process GTD inbox item by doing it now."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize-task-at-point-as-archived-knowledge ()
  "Archive (in the GTD sense, which means file this knowledge)."
  (interactive)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize-task-at-point-as-new-project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)

  (if (org-gtd-projects--poorly-formatted-p)
      ;; TODO the error message can't also
      ;; call the process inbox, that is not its job
      (org-gtd-projects--show-error-and-return-to-editing)

    (org-gtd-organize-decorate-item)
    (org-gtd-projects--nextify)
    (let ((org-special-ctrl-a t))
      (org-end-of-line))
    (insert " [/]")
    (org-update-statistics-cookies t)
    (org-gtd--refile org-gtd-projects)))

(defun org-gtd-organize-add-task-at-point-to-existing-project ()
  "Refile the org heading at point under a chosen heading in the agenda files."
  (interactive)
  (with-org-gtd-context
      (let* ((org-gtd-refile-to-any-target nil)
             (org-use-property-inheritance '("ORG_GTD"))
             (headings (org-map-entries
                        (lambda () (org-get-heading t t t t))
                        org-gtd-project-headings
                        'agenda))
             (chosen-heading (completing-read "Choose a heading: " headings nil t))
             (heading-marker (org-find-exact-heading-in-directory chosen-heading org-gtd-directory)))
        (org-gtd-organize-decorate-item)
        (org-refile nil nil `(,chosen-heading
                              ,(buffer-file-name (marker-buffer heading-marker))
                              nil
                              ,(marker-position heading-marker))
                    nil)
        (org-gtd-projects-fix-todo-keywords heading-marker))))

(defun org-gtd-organize-task-at-point-as-appointment ()
  "Add a date/time to this item and store in org gtd."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-calendar))

(defun org-gtd-organize-delegate-task-at-point ()
  "Delegate this item and file it in the org-gtd system."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-gtd-delegate)
  (org-gtd--refile org-gtd-actions))

(defun org-gtd-organize-incubate-task-at-point ()
  "Incubate this item through org-gtd."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-incubated))

(defun org-gtd-organize-task-at-point-as-single-action ()
  "Process GTD inbox item as a single action."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-todo "NEXT")
  (org-gtd--refile org-gtd-actions))

(defun org-gtd-organize-task-at-point-as-trash ()
  "Mark GTD inbox item as cancelled and move it to the org-gtd task archives."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-todo "CNCL")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize-decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (goto-char (point-min))
  (dolist (hook org-gtd-process-item-hooks)
    (save-excursion
      (save-restriction
        (funcall hook)))))

(defun org-gtd-organize--decorate-element (element)
  "Apply `org-gtd--decorate-item' to org-element ELEMENT."
  (org-with-point-at (org-gtd-projects--org-element-pom element)
    (org-narrow-to-element)
    (org-gtd-organize-decorate-item)))

(provide 'org-gtd-organize)
;;; org-gtd-organize.el ends here
