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

(require 'transient)
(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)

(defgroup org-gtd-organize nil
  "Manage the functions for organizing the GTD actions."
  :package-version '(org-gtd . "3.0.0")
  :group 'org-gtd)

(defcustom org-gtd-organize-quick-action-func
  #'org-gtd-organize--task-at-point-was-quick-action
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as an action that you started and finished just now."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-single-action-func
  #'org-gtd-organize--task-at-point-as-single-action
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as a single next action."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-archive-func
  #'org-gtd-organize-task-at-point-as-archived-knowledge
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as knowledge to be stored. Note that this function is used inside loops,
for instance to process the inbox, so if you have manual steps you need to take
when storing a heading as knowledge, take them before calling this function
(for instance, during inbox processing, take the manual steps during the clarify
step, before you call `org-gtd-organize')."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-project-func
  #'org-gtd-organize--task-at-point-as-new-project
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as a project. You *probably* should not change this from the default, as a
lot of fiddly bits depend on the way org-gtd structures and organizes the
projects, but who's gonna stop you?"
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-add-to-project-func
  #'org-gtd-organize--task-at-point-add-to-existing-project
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as a new task in an existing project"
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-appointment-func
  #'org-gtd-organize--task-at-point-as-appointment
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as a task that must happen on a given day. Keep this clean and don't load
your calendar with things that aren't actually appointments or deadlines."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-delegate-func
  #'org-gtd-organize--task-at-point-as-delegated
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as an action delegated to someone else."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-incubate-func
  #'org-gtd-organize--task-at-point-as-incubated
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as incubated."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-trash-func
  #'org-gtd-organize--task-at-point-as-trash
  "Can be an atom or a sexp. Function called when org-gtd organizes the item at
point as something to be discarded."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

;;;###autoload
(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item. This function works for both one-off clarification and whole-inbox clarification."
  (interactive)
  (if (and (boundp org-gtd-clarify--inbox-p) org-gtd-clarify--inbox-p)
      (transient-setup 'org-gtd-organize-inbox)
    (transient-setup 'org-gtd-organize-one-off)))

(transient-define-prefix org-gtd-organize-one-off ()
  ["Actionable"
   [("q" "Quick action" (lambda ()
                          (interactive)
                          (org-gtd-organize--call
                           org-gtd-organize-quick-action-func)))
    ("s" "Single action" (lambda ()
                           (interactive)
                           (org-gtd-organize--call
                            org-gtd-organize-single-action-func)))]
   [("d" "Delegate" (lambda ()
                      (interactive)
                      (org-gtd-organize--call
                       org-gtd-organize-delegate-func)))
    ("c" "Calendar" (lambda ()
                      (interactive)
                      (org-gtd-organize--call
                       org-gtd-organize-appointment-func)))]]
  [("p" "Project (multi-step)" (lambda ()
                                 (interactive)
                                 (org-gtd-organize--call
                                  org-gtd-organize-project-func)))
   ("m" "Modify project: add this task" (lambda ()
                                          (interactive)
                                          (org-gtd-organize--call
                                           org-gtd-organize-add-to-project-func)))]
  ["Non-actionable"
   [("i" "Incubate" (lambda () (interactive)
                      (org-gtd-organize--call org-gtd-organize-incubate-func)))
    ("a" "Archive this knowledge" (lambda () (interactive)
                                    (org-gtd-organize--call org-gtd-organize-archive-func)))]
   [("t" "Trash" (lambda () (interactive)
                   (org-gtd-organize--call org-gtd-organize-trash-func)))]])

(transient-define-prefix org-gtd-organize-inbox ()
  ["Actionable"
   [("q" "Quick action" (lambda ()
                          (interactive)
                          (org-gtd-organize-inbox-item
                           org-gtd-organize-quick-action-func)))
    ("s" "Single action" (lambda ()
                           (interactive)
                           (org-gtd-organize-inbox-item
                            org-gtd-organize-single-action-func)))]
   [("d" "Delegate" (lambda ()
                      (interactive)
                      (org-gtd-organize-inbox-item
                       org-gtd-organize-delegate-func)))
    ("c" "Calendar" (lambda ()
                      (interactive)
                      (org-gtd-organize-inbox-item
                       org-gtd-organize-appointment-func)))]]
  [("p" "Project (multi-step)" (lambda ()
                                 (interactive)
                                 (org-gtd-organize-inbox-item
                                  org-gtd-organize-project-func)))
   ("m" "Modify project: add this task" (lambda ()
                                          (interactive)
                                          (org-gtd-organize-inbox-item
                                           org-gtd-organize-add-to-project-func)))]
  ["Non-actionable"
   [("i" "Incubate" (lambda ()
                      (interactive)
                      (org-gtd-organize-inbox-item
                       org-gtd-organize-incubate-func)))
    ("a" "Archive this knowledge" (lambda ()
                                    (interactive)
                                    (org-gtd-organize-inbox-item
                                     org-gtd-organize-archive-func)))]
   [("t" "Trash" (lambda ()
                   (interactive)
                   (org-gtd-organize-inbox-item
                    org-gtd-organize-trash-func)))]]

  ["Org GTD"
   ("x"
    "Exit. Stop processing the inbox for now."
    org-gtd-process--stop)])

(defalias 'org-gtd-choose #'org-gtd-organize)
(make-obsolete 'org-gtd-choose #'org-gtd-organize "2.3.0")

(defun org-gtd-organize--call (func)
  "Organize the item within org-gtd. FUNC is the function that does the actual
work. This is simply a wrapper function that cleans up emacs as required."
  (beginning-of-buffer)
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  (catch 'org-gtd-error
    (save-excursion (funcall func))
    (let ((task-id org-gtd-clarify--clarify-id)
          (window-config org-gtd-clarify--window-config)
          (buffer (marker-buffer org-gtd-clarify--source-heading-marker))
          (position (marker-position org-gtd-clarify--source-heading-marker)))
      (with-current-buffer buffer
        (goto-char position)
        (org-cut-subtree))
      (set-window-configuration window-config)
      (kill-buffer (org-gtd-clarify--buffer-name task-id)))))

(defun org-gtd-organize-inbox-item (func)
  "Run FUNC as part of a flow that loops over each item in the inbox."
  (org-gtd-organize--call func)
  (org-gtd-process-inbox))

(defun org-gtd-organize-decorate-item ()
  "Apply hooks to add metadata to a given GTD item."
  (dolist (hook org-gtd-process-item-hooks)
    (save-excursion
      (beginning-of-buffer)
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (save-restriction
        (funcall hook)))))

(defun org-gtd-organize--task-at-point-as-single-action ()
  "Item at point is a one-off NEXT action."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-todo "NEXT")
  (org-gtd--refile org-gtd-actions))

(defun org-gtd-organize--task-at-point-was-quick-action ()
  "Process GTD inbox item by doing it now."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize--task-at-point-as-archived-knowledge ()
  "Archive (in the GTD sense, which means file this knowledge)."
  (interactive)
  (org-todo "DONE")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize--task-at-point-as-new-project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (interactive)
  (when (org-gtd-projects--poorly-formatted-p)
    (org-gtd-projects--show-error)
    (throw 'org-gtd-error "Malformed project"))


    (org-gtd-organize-decorate-item)
    (org-gtd-projects--nextify)
    (let ((org-special-ctrl-a t))
      (org-end-of-line))
    (insert " [/]")
    (org-update-statistics-cookies t)
    (org-gtd--refile org-gtd-projects))

(defun org-gtd-organize--task-at-point-add-to-existing-project ()
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
        (org-refile 3 nil `(,chosen-heading
                              ,(buffer-file-name (marker-buffer heading-marker))
                              nil
                              ,(marker-position heading-marker))
                    nil)
        (org-gtd-projects-fix-todo-keywords heading-marker))))

(defun org-gtd-organize--task-at-point-as-appointment ()
  "Add a date/time to this item and store in org gtd."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-calendar))

(defun org-gtd-organize--task-at-point-as-delegated ()
  "Delegate this item and file it in the org-gtd system."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-gtd-delegate)
  (org-gtd--refile org-gtd-actions))

(defun org-gtd-organize--task-at-point-as-incubated ()
  "Incubate this item through org-gtd."
  (interactive)
  (org-gtd-organize-decorate-item)
  (org-schedule 0)
  (org-gtd--refile org-gtd-incubated))

(defun org-gtd-organize--task-at-point-as-trash ()
  "Mark GTD inbox item as cancelled and move it to the org-gtd task archives."
  (interactive)
  (org-todo "CNCL")
  (with-org-gtd-context (org-archive-subtree)))

(defun org-gtd-organize--decorate-element (element)
  "Apply `org-gtd--decorate-item' to org-element ELEMENT."
  (org-with-point-at (org-gtd-projects--org-element-pom element)
    (org-narrow-to-element)
    (org-gtd-organize-decorate-item)))

(provide 'org-gtd-organize)
;;; org-gtd-organize.el ends here
