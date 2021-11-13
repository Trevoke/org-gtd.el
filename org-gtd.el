;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 1.1.1
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "26.1") (org-edna "1.1.2") (f "0.20.0") (org "9.3.1") (org-agenda-property "1.3.1"))

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
;; Upgrading? make sure you read the CHANGELOG.
;;
;; This package provides a system that allows you to capture incoming things
;; into an inbox, then process the inbox and categorize each item based on the
;; GTD categories.  It leverages org-agenda to show today's items as well as the
;; NEXT items.  It also has a simple project management system, which currently
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
(require 'org-capture)
(require 'org-element)
(require 'org-agenda-property)
(require 'org-edna)

(defconst org-gtd-inbox "inbox")
(defconst org-gtd-incubated "incubated")
(defconst org-gtd-projects "projects")
(defconst org-gtd-actions "actions")
(defconst org-gtd-delegated "delegated")
(defconst org-gtd-scheduled "scheduled")

(defconst org-gtd--refile-properties
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Actions" myhash)
    (puthash org-gtd-incubated "Incubated" myhash)
    (puthash org-gtd-projects "Projects" myhash)
    (puthash org-gtd-scheduled "Scheduled" myhash)
    myhash))

(require 'org-gtd-customize)
(require 'org-gtd-archive)
(require 'org-gtd-files)
(require 'org-gtd-refile)
(require 'org-gtd-projects)
(require 'org-gtd-agenda)
(require 'org-gtd-inbox-processing)

;;;###autoload
(define-minor-mode org-gtd-mode
  "global minor mode to force org-agenda to be bounded to the org-gtd settings"
  :lighter " GTD"
  :global t
  (if org-gtd-mode
      (org-gtd--override-agenda)
    (org-gtd--restore-agenda)))

(defun org-gtd--wrap (fun &rest r)
  (with-org-gtd-context (apply fun r)))

(defun org-gtd--override-agenda ()
  (advice-add 'org-agenda :around #'org-gtd--wrap)
  (advice-add 'org-agenda-list-stuck-projects :around #'org-gtd--wrap))

(defun org-gtd--restore-agenda ()
  (advice-remove 'org-agenda #'org-gtd--wrap)
  (advice-remove 'org-agenda-list-stuck-projects #'org-gtd--wrap))

;;;###autoload
(defun org-gtd-capture (&optional goto keys)
  "Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.

For GOTO and KEYS, see `org-capture' documentation for the variables of the same name."
  (interactive)
  (with-org-gtd-context
      (kill-buffer (org-gtd--inbox-file))
      (org-capture goto keys)))

(defun org-gtd-delegate ()
  (interactive)
  (let ((delegated-to (read-string "Who will do this? "))
        (org-inhibit-logging 'note))
    (org-set-property "DELEGATED_TO" delegated-to)
    (org-todo "WAIT")
    (org-schedule 0)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "programmatically delegated to %s\n" delegated-to)))))

(defmacro with-org-gtd-context (&rest body)
  (declare (debug t) (indent 2))
  `(let* ((org-use-property-inheritance "ORG_GTD")
          (org-archive-location (funcall org-gtd-archive-location))
          (org-capture-templates org-gtd-capture-templates)
          (org-refile-use-outline-path nil)
          (org-stuck-projects org-gtd-stuck-projects)
          (org-odd-levels-only nil)
          (org-agenda-files `(,org-gtd-directory))
          (org-agenda-property-list '("DELEGATED_TO"))
          ;(org-agenda-buffer-name "*Org Agenda(g)*")
          (org-agenda-custom-commands org-gtd-agenda-custom-commands))
     (unwind-protect
         (progn ,@body))))

(provide 'org-gtd)

;;; org-gtd.el ends here
