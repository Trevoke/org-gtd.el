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
;; Refiling logic for org-gtd.
;;
;;; Code:

(defun org-gtd--refile-incubate ()
  "Refile an item to the incubate file."
  (setq user-refile-targets org-refile-targets)
  (setq org-refile-targets (org-gtd--refile-incubate-targets))
  (org-refile)
  (setq org-refile-targets user-refile-targets))

(defun org-gtd--refile-project ()
  "Refile a project"
  (with-org-gtd-project-context
   (org-gtd--ensure-project-refile-target-exists)
   (org-refile nil nil nil "Refile project to: ")))

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

(defun org-gtd--ensure-project-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' project target
unless one exists.`"
  (with-org-gtd-project-context
   (unless (org-refile-get-targets)
     (org-gtd--gtd-file-buffer "projects"))))

(defun org-gtd--refile-scheduled-item ()
  "Refile a project"
  (with-org-gtd-scheduled-context
   (org-gtd--ensure-scheduled-refile-target-exists)
   (org-refile nil nil nil "Refile scheduled item to: ")))

(defun org-gtd--scheduled-group-p ()
  (string-equal "Scheduled" (org-element-property :ORG_GTD (org-element-at-point))))

(defmacro with-org-gtd-scheduled-context (&rest body)
  "Override org variables for org-gtd and evaluate BODY there like `progn'."
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function 'org-gtd--scheduled-group-p)
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--ensure-scheduled-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' scheduled target
unless one exists.`"
  (with-org-gtd-scheduled-context
   (unless (org-refile-get-targets)
     (org-gtd--gtd-file-buffer "scheduled"))))

(defun org-gtd--refile-delegated-item ()
  "Refile a project"
  (with-org-gtd-delegated-context
   (org-gtd--ensure-delegated-refile-target-exists)
   (org-refile nil nil nil "Refile delegated item to: ")))

(defun org-gtd--delegated-group-p ()
  (string-equal "Delegated" (org-element-property :ORG_GTD (org-element-at-point))))

(defmacro with-org-gtd-delegated-context (&rest body)
  "Override org variables for org-gtd and evaluate BODY there like `progn'."
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function 'org-gtd--delegated-group-p)
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--ensure-delegated-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' delegated target
unless one exists.`"
  (with-org-gtd-delegated-context
   (unless (org-refile-get-targets)
     (org-gtd--gtd-file-buffer "delegated"))))

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
  (append (org-gtd--refile-incubate-targets) (org-gtd--refile-action-targets)))

(defun org-gtd--refile-incubate-targets ()
  "Generate refile targets for incubation items."
  `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)))

(defun org-gtd--refile-action-targets ()
  "Generate refile targets for actionable items."
  `((,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1)))

(provide 'org-gtd-refile)
