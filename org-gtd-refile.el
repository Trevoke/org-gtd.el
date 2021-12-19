;;; org-gtd-refile.el --- refiling logic for org gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Refiling logic for org-gtd.
;;
;;; Code:

(defconst org-gtd-projects-template
  "* Projects
:PROPERTIES:
:TRIGGER: next-sibling todo!(NEXT)
:ORG_GTD: Projects
:END:
")

(defconst org-gtd-calendar-template
  "* Calendar
:PROPERTIES:
:ORG_GTD: Calendar
:END:
")

(defconst org-gtd-actions-template
  "* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
")

(defconst org-gtd-incubated-template
  "#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
Add your own categories as necessary, with the ORG_GTD property, such as
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment

* Incubate
:PROPERTIES:
:ORG_GTD: Incubated
:END:
"
  "Template for the GTD someday/maybe list.")

(defconst org-gtd--file-template
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions org-gtd-actions-template myhash)
    (puthash org-gtd-calendar org-gtd-calendar-template myhash)
    (puthash org-gtd-projects org-gtd-projects-template myhash)
    (puthash org-gtd-incubated org-gtd-incubated-template myhash)
    myhash))

(defconst org-gtd-refile--prompt
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Refile single action to: " myhash)
    (puthash org-gtd-incubated "Refile incubated item to: " myhash)
    (puthash org-gtd-delegated "Refile delegated item to: " myhash)
    (puthash org-gtd-projects "Refile project to: " myhash)
    (puthash org-gtd-calendar "Refile calendar item to: " myhash)
    myhash))

(defun org-gtd-refile--do (type)
  "Refile an item to the single action file.

TYPE is one of the org-gtd action types.  This is a private function."
  (with-org-gtd-refile type
    (unless (org-refile-get-targets) (org-gtd-refile--add-target type))
    (if org-gtd-refile-to-any-target
        (org-refile nil nil (car (org-refile-get-targets)))
      (org-refile nil nil nil (org-gtd-refile--prompt type)))))

;;;###autoload
(defmacro with-org-gtd-refile (type &rest body)
  "Macro to refile specifically within org-gtd context.

TYPE is the org-gtd action type.  BODY is the rest of the code."
  (declare (debug t) (indent 1))
  `(let ((org-refile-target-verify-function (lambda () (org-gtd-refile--group-p ,type)))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (with-org-gtd-context (progn ,@body)))))

(defun org-gtd-refile--add-target (gtd-type)
  "Private function used to create a missing org-gtd refile target.

GTD-TYPE is an action type."
  (with-current-buffer (org-gtd--default-file)
    (end-of-buffer)
    (newline)
    (insert (gethash gtd-type org-gtd--file-template))
    (basic-save-buffer)))

(defun org-gtd-refile--group-p (type)
  "Determine whether the current heading is of a given gtd TYPE."
  (string-equal (org-gtd-refile--group type)
                (org-element-property :ORG_GTD (org-element-at-point))))

(defun org-gtd-refile--group (type)
  "What kind of gtd group is TYPE."
  (gethash type org-gtd--properties))

(defun org-gtd-refile--prompt (type)
  "What is the right refile prompt for this gtd TYPE."
  (gethash type org-gtd-refile--prompt))

(provide 'org-gtd-refile)
;;; org-gtd-refile.el ends here
