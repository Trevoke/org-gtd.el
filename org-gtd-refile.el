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

(defconst org-gtd-refile-properties
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Action" myhash)
    (puthash org-gtd-incubated "Incubated" myhash)
    (puthash org-gtd-delegated "Delegated" myhash)
    (puthash org-gtd-projects "Projects" myhash)
    (puthash org-gtd-scheduled "Scheduled" myhash)
    myhash))

(defconst org-gtd-refile-filter-function
  (let ((myhash (make-hash-table :test 'eq)))
    (puthash org-gtd-actions 'org-gtd--single-action-group-p myhash)
    (puthash org-gtd-incubated 'org-gtd--incubated-group-p myhash)
    (puthash org-gtd-delegated 'org-gtd--delegated-group-p myhash)
    (puthash org-gtd-projects 'org-gtd--projects-group-p myhash)
    (puthash org-gtd-scheduled 'org-gtd--scheduled-group-p myhash)
    myhash))

(defun org-gtd--group-p (type)
  (string-equal (org-gtd--group org-gtd-actions)
                (org-element-property :ORG_GTD (org-element-at-point))))

(defun org-gtd--group (type)
  (gethash type org-gtd-refile-properties))

(defun org-gtd--refile (type)
  "Refile an item to the single action file."
  (with-org-gtd-context type
                        (unless (org-refile-get-targets) (org-gtd--gtd-file-buffer type))
                        (org-refile nil nil nil "Refile single action to:"))
  ;; (with-org-gtd-single-action-context
  ;;  (org-gtd--ensure-single-action-refile-target-exists)
  ;;  (org-refile nil nil nil "Refile single action to: "))
  )

(defun org-gtd--refile-single-action ()
  (org-gtd--refile org-gtd-actions))

(defun org-gtd--single-action-group-p ()
  (org-gtd--group-p org-gtd-actions))

(defmacro with-org-gtd-context (type &rest body)
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function (lambda () (org-gtd--group-p ,type)))
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defmacro with-org-gtd-single-action-context (&rest body)
  "Override org variables for org-gtd and evaluate BODY there like `progn'."
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function 'org-gtd--single-action-group-p)
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--ensure-single-action-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' single action target
unless one exists.`"
  (with-org-gtd-single-action-context
   (unless (org-refile-get-targets)
     (org-gtd--default-action-file))))

(defun org-gtd--refile-incubate ()
  "Refile an item to the incubate file."
  (with-org-gtd-incubated-context
   (org-gtd--ensure-incubated-refile-target-exists)
   (org-refile nil nil nil "Refile incubated item to: ")))

(defun org-gtd--incubated-group-p ()
  (string-equal "Incubated" (org-element-property :ORG_GTD (org-element-at-point))))

(defmacro with-org-gtd-incubated-context (&rest body)
  "Override org variables for org-gtd and evaluate BODY there like `progn'."
  (declare (debug t))
  `(let ((org-refile-use-outline-path nil)
         (org-odd-levels-only nil)
         (org-refile-target-verify-function 'org-gtd--incubated-group-p)
         (org-agenda-files `(,org-gtd-directory))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--ensure-incubated-refile-target-exists ()
  "Create a file in `org-gtd-directory' with a `org-gtd' incubated target
unless one exists.`"
  (with-org-gtd-incubated-context
   (unless (org-refile-get-targets)
     (org-gtd--default-incubate-file))))

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
     (org-gtd--default-projects-file))))

(defun org-gtd--refile-scheduled-item ()
  "Refile a scheduled item"
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
     (org-gtd--default-scheduled-file))))

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
     (org-gtd--default-delegated-file))))

(provide 'org-gtd-refile)
