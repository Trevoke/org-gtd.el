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
(require 'org-element)
(require 'org-agenda-property)
(require 'org-edna)
(require 'org-gtd-files)
(require 'org-gtd-customize)
(require 'org-gtd-projects)
(require 'org-gtd-agenda)
(require 'org-gtd-inbox-processing)

;;;; Constants

(defconst org-gtd-actions   ".*Actions")
(defconst org-gtd-delegated ".*Delegated")
(defconst org-gtd-incubate  ".*Incubate.*")
(defconst org-gtd-scheduled ".*Scheduled")
(defconst org-gtd-projects  ".*Projects")

;;;; Commands

(defun org-gtd-capture (&optional goto keys)
  "Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.

For GOTO and KEYS, see `org-capture' documentation for the variables of the same name."
  (interactive)
  (kill-buffer (org-gtd--inbox-file))
  (org-capture goto keys))

;;;; File work

;;;; sorting things is hard I need to make multiple files

(defun org-gtd--org-element-pom (element)
  "Return buffer position for start of Org ELEMENT."
  (org-element-property :begin element))

(defun org-gtd--refile-incubate ()
  "Refile an item to the incubate file."
  (setq user-refile-targets org-refile-targets)
  (setq org-refile-targets (org-gtd--refile-incubate-targets))
  (org-refile)
  (setq org-refile-targets user-refile-targets))

(defun org-gtd--project-group-p ()
  (string-equal "Projects" (org-element-property :ORG_GTD (org-element-at-point))))

(defmacro with-org-gtd-project-context (&rest body)
  "Override org variables for org-gtd and evaluate BODY there like `progn'."
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function 'org-gtd--project-group-p)
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--refile-project ()
  "Refile a project"
  (with-org-gtd-project-context
   (org-gtd--ensure-project-refile-target-exists)
   (org-refile nil nil nil "Refile project to: ")))

(defun org-gtd--ensure-project-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' project target
unless one exists.`"
  (with-org-gtd-project-context
   (unless (org-refile-get-targets)
     (org-gtd--gtd-file-buffer "projects"))))

(defun org-gtd--refile-target (heading-regexp)
  "Filters refile targets generated from `org-gtd--refile-targets' using HEADING-REGEXP."
  (let* ((org-refile-targets (org-gtd--refile-targets))
         (results (cl-find-if
                   (lambda (rfloc)
                     (string-match heading-regexp
                                   (car rfloc)))
                   (org-refile-get-targets)))
         )
    results))

(defun org-gtd--refile-targets ()
  "Return the refile targets specific to org-gtd."
  (append (org-gtd--refile-incubate-targets) (org-gtd--refile-action-targets)))

(defun org-gtd--refile-incubate-targets ()
  "Generate refile targets for incubation items."
  `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)))

(defun org-gtd--refile-action-targets ()
  "Generate refile targets for actionable items."
  `((,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1)))

(provide 'org-gtd)

;;; org-gtd.el ends here
