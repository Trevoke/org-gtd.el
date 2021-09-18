;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 1.0.2
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "26.1") (org-edna "1.0.2") (f "0.20.0") (org "9.3.1") (org-agenda-property "1.3.1"))

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

;; This package tries to replicate as closely as possible the GTD workflow.
;; This package assumes familiarity with GTD.
;;
;; This package provides a system that allows you to capture incoming things
;; into an inbox, then process the inbox and categorize each item based on the
;; GTD categories. It leverages org-agenda to show today's items as well as the
;; NEXT items. It also has a simple project management system, which currently
;; assumes all tasks in a project are sequential.
;;
;; For a comprehensive instruction manual, see the file `README.org'.
;;
;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'cl-lib)
(require 'f)
(require 'org)
(require 'org-element)
(require 'org-agenda-property)
(require 'org-edna)

;;;; Variables

(defvar org-gtd-command-map (make-sparse-keymap)
  "Keymap for function `org-gtd-user-input-mode', a minor mode.")

(defvar org-stuck-projects)

;;;; Constants

(defconst org-gtd-actionable-file-basename "actionable"
  "Name of Org file listing all actionable items.")

(defconst org-gtd-inbox-file-basename "inbox"
  "Name of Org file listing all captured items.")

(defconst org-gtd-incubate-file-basename "incubate"
  "Name of Org file listing all someday/maybe items.")

(defconst org-gtd-actions   ".*Actions")
(defconst org-gtd-delegated ".*Delegated")
(defconst org-gtd-incubate  ".*Incubate.*")
(defconst org-gtd-scheduled ".*Scheduled")
(defconst org-gtd-projects  ".*Projects")

(defconst org-gtd-complete-projects
  "+LEVEL=2+CATEGORY=\"Projects\""
  "How to identify projects in the GTD system.")

(defconst org-gtd-stuck-projects
  '("+LEVEL=2-DONE+CATEGORY=\"Projects\"" ("TODO" "NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.
This is a list of four items, the same type as in `org-stuck-projects'.")

(defconst org-gtd-actionable-template
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CANCELED(c@)

* Actions
:PROPERTIES:
:CATEGORY: Action
:END:

* Delegated
:PROPERTIES:
:CATEGORY: Delegated
:END:

* Scheduled
:PROPERTIES:
:CATEGORY: Scheduled
:END:

* Projects
:PROPERTIES:
:TRIGGER: next-sibling todo!(NEXT)
:CATEGORY: Projects
:END:
"
  "Template for the GTD actionable list.")

(defconst org-gtd-inbox-template
  "#+STARTUP: overview hidestars logrefile indent logdone
#+TODO: NEXT TODO WAIT | DONE CANCELED TRASH
#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
  "Template for the GTD inbox.")

(defconst org-gtd-incubate-template
  "#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
It's suggested that you categorize the items in here somehow, such as:
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment

* Auto-generated incubate headline
"
  "Template for the GTD someday/maybe list.")

;;;; Customization

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :version 0.1
  :group 'emacs)

(defcustom org-gtd-directory "~/gtd/"
  "Directory of Org based GTD files.
This is the directory where to look for the files used in
this Org-mode based GTD implementation."
  :type 'directory)

;;;; Commands

(defun org-gtd-find-or-create-and-save-files ()
  (interactive)
  (mapcar
   (lambda (buffer) (with-current-buffer buffer (save-buffer) buffer))
   `(,(org-gtd--actionable-file) ,(org-gtd--incubate-file) ,(org-gtd--inbox-file))))

(defun org-gtd-archive-complete-projects ()
  "Archive all projects for which all actions/tasks are marked as done.
Done here is any done `org-todo-keyword'."
  (interactive)
  (org-map-entries
   (lambda ()
     (if (org-gtd--project-complete-p)
         (progn
           (setq org-map-continue-from (org-element-property
                                        :begin
                                        (org-element-at-point)))
           (org-archive-subtree-default))))
   org-gtd-complete-projects))

(defun org-gtd-capture (&optional GOTO KEYS)
  "Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.

For GOTO and KEYS, see `org-capture' documentation for the variables of the same name."
  (interactive)
  (kill-buffer (org-gtd--inbox-file))
  (org-capture GOTO KEYS))

(defun org-gtd-clarify-finalize ()
  "Finalize the clarify process."
  (interactive)
  (org-gtd-user-input-mode -1)
  (exit-recursive-edit))

(defun org-gtd-process-inbox ()
  "Process the GTD inbox.
Use this once a day and/or weekly as part of the weekly review."
  (interactive)
  (set-buffer (org-gtd--inbox-file))
  (display-buffer-same-window (org-gtd--inbox-file) '())
  (delete-other-windows)

  (org-gtd-find-or-create-and-save-files)
  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property
                                  :begin
                                  (org-element-at-point)))
     (org-narrow-to-element)
     (org-show-subtree)
     (org-gtd--process-inbox-element)
     (widen)))
  (setq-local header-line-format nil)
  (org-gtd-find-or-create-and-save-files))

(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-todo-list "NEXT"))

(defun org-gtd-show-stuck-projects ()
  "Show all projects that do not have a next action."
  (interactive)
  (let* ((user-stuck-projects org-stuck-projects)
         (org-stuck-projects org-gtd-stuck-projects)
         (stuck-projects-buffer (org-agenda-list-stuck-projects))
         (org-stuck-projects user-stuck-projects))
    stuck-projects-buffer))

;;;; File work

(defun org-gtd--inbox-file ()
  "Create or return the buffer to the GTD inbox file."
  (org-gtd--gtd-file org-gtd-inbox-file-basename))

(defun org-gtd--actionable-file ()
  "Create or return the buffer to the GTD actionable file."
  (org-gtd--gtd-file org-gtd-actionable-file-basename))

(defun org-gtd--incubate-file ()
  "Create or return the buffer to the GTD incubate file."
  (org-gtd--gtd-file org-gtd-incubate-file-basename))

(defun org-gtd--gtd-file (gtd-type)
  "Return a buffer to GTD-TYPE.org.
Create the file and template first if it doesn't already exist."
  (let* ((file-path (org-gtd--path gtd-type))
         (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
        (with-current-buffer file-buffer
          (org-mode)
          (insert (symbol-value
                   (intern
                    (string-join
                     `("org-gtd-" ,gtd-type "-template")))))
          (org-mode-restart)
          (save-buffer)))
    file-buffer))

;;;; Just a minor mode because keymaps are cool

(define-minor-mode org-gtd-user-input-mode
  "Minor mode for org-gtd."
  nil "GTD " org-gtd-command-map
  (setq-local header-line-format
              (substitute-command-keys
               "\\<org-gtd-command-map>Clarify buffer.  Finish \
`\\[org-gtd-clarify-finalize]'.")))

;;;; actual GTD operations

(defun org-gtd--archive ()
  "Process GTD inbox item as a reference item."
  (org-gtd--clarify-item)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--calendar ()
  "Process GTD inbox item by scheduling it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-scheduled)))

(defun org-gtd--delegate ()
  "Process GTD inbox item by delegating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set it as a waiting action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "WAIT")
  (org-set-property "DELEGATED_TO" (read-string "Who will do this? "))
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-delegated)))

(defun org-gtd--clarify-item ()
  "User interface to reflect on and clarify the current inbox item."
  (org-gtd-user-input-mode 1)
  (recursive-edit))

(defun org-gtd--incubate ()
  "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-incubate-file-basename'."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-schedule 0)
  (org-gtd--refile-incubate))

;;;; sorting things is hard I need to make multiple files

(defun org-gtd--nextify ()
  "Add the NEXT keyword to the first action/task of the project.
Add the TODO keyword to all subsequent actions/tasks."
  (cl-destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
        (reverse rest-entries)
        'headline
      (lambda (myelt)
        (org-entry-put (org-gtd--org-element-pom myelt) "TODO" "TODO")))
    (org-entry-put (org-gtd--org-element-pom first-entry) "TODO" "NEXT")))

(defun org-gtd--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

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

(defun org-gtd--project ()
  "Process GTD inbox item by transforming it into a project.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-gtd--nextify)
  (org-refile nil nil (org-gtd--refile-target org-gtd-projects))
  (with-current-buffer (org-gtd--actionable-file)
    (org-update-statistics-cookies t)))

(defun org-gtd--project-complete-p ()
  "Return t if project complete, nil otherwise.
A project is considered complete when all its actions/tasks are
marked with a done `org-todo-keyword'."
  (let ((entries (cdr (org-map-entries
                       (lambda ()
                         (org-entry-get
                          (org-gtd--org-element-pom (org-element-at-point))
                          "TODO"))
                       t
                       'tree))))
    (seq-every-p (lambda (x) (string-equal x "DONE")) entries)))

(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--refile-incubate ()
  (setq user-refile-targets org-refile-targets)
  (setq org-refile-targets `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)))
  (org-refile)
  (setq org-refile-targets user-refile-targets))

(defun org-gtd--refile-target (heading-regexp)
  "Filters refile targets generated from `org-gtd--refile-targets' using HEADING-REGEXP."
  (let* ((org-refile-targets (org-gtd--refile-targets))
         (results (cl-find-if
                   (lambda (rfloc)
                     (string-match heading-regexp
                                   (car rfloc)))
                   (org-refile-get-targets))))
    results))

(defun org-gtd--refile-targets ()
  "Return the refile targets specific to org-gtd."
  (append (org-gtd--refile-incubate-targets) (org-gtd--refile-action-targets))
  ;; `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)
  ;;   (,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1))
  )

(defun org-gtd--refile-incubate-targets ()
  `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)))

(defun org-gtd--refile-action-targets ()
  `((,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1)))

(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "NEXT")
  (org-refile nil nil (org-gtd--refile-target org-gtd-actions)))

(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "CANCELED")
  (org-archive-subtree))

(provide 'org-gtd)

;;; org-gtd.el ends here
