;;; org-gtd-core.el --- Core code for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Core logic for org-gtd
;; Creating this file because straight.el seems unhappy.
;;
;;; Code:

(require 'org-agenda-property)
(require 'org-capture)
(require 'org-gtd-customize)

(defconst org-gtd-inbox "inbox")
(defconst org-gtd-incubated "incubated")
(defconst org-gtd-projects "projects")
(defconst org-gtd-actions "actions")
(defconst org-gtd-delegated "delegated")
(defconst org-gtd-calendar "calendar")

(defconst org-gtd--properties
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Actions" myhash)
    (puthash org-gtd-incubated "Incubated" myhash)
    (puthash org-gtd-projects "Projects" myhash)
    (puthash org-gtd-calendar "Calendar" myhash)
    myhash))

(defconst org-gtd-stuck-projects
  '("+LEVEL=2&+ORG_GTD=\"Projects\"" ("NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.

This is a list of four items, the same type as in `org-stuck-projects'.")

;;;###autoload
(defmacro with-org-gtd-context (&rest body)
  "Wrap any BODY in this macro to inherit the org-gtd settings for your logic."
  (declare (debug t) (indent 2))
  `(let* ((org-use-property-inheritance "ORG_GTD")
          (org-archive-location (funcall org-gtd-archive-location))
          (org-capture-templates (seq-concatenate
                                  'list
                                  (org-gtd--capture-templates)
                                  org-capture-templates))
          (org-refile-use-outline-path nil)
          (org-stuck-projects org-gtd-stuck-projects)
          (org-odd-levels-only nil)
          (org-agenda-files `(,org-gtd-directory))
          (org-agenda-property-list '("DELEGATED_TO"))
          (org-agenda-custom-commands org-gtd-agenda-custom-commands))
     (unwind-protect
         (progn ,@body))))

;; move this here to make a clear load path to make straight.el happy
;; it was originally in org-gtd-capture.el
(defun org-gtd--capture-templates ()
  "Private function.

Return valid `org-capture' templates based on `org-gtd-capture-templates'."
  (mapcar #'org-gtd--gen-capture-templates
          org-gtd-capture-templates))

;; move this here to make a clear load path to make straight.el happy
;; it was originally in org-gtd-capture.el
(defun org-gtd--gen-capture-templates (template)
  "Private function.

Given an `org-capture-template' TEMPLATE string, generate a valid
`org-gtd-capture' item."
  (cl-destructuring-bind (key description template-string) template
    `(,key ,description entry
           (file (lambda () (org-gtd-inbox-path)))
                 ,template-string :kill-buffer t)))

(provide 'org-gtd-core)
;;; org-gtd-core.el ends here
