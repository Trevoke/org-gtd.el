;;; org-gtd-graph-undo.el --- Undo/redo system for graph operations -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

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
;; This module provides an undo/redo system for graph modification operations.
;; It tracks operations like adding/removing dependencies and allows users to
;; undo and redo them.
;;
;; Each operation stores:
;; - Type: :add-dependency, :remove-dependency, etc.
;; - Data: Operation-specific information
;; - Reverse function: How to undo the operation
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)

(declare-function org-gtd-graph-view-refresh "org-gtd-graph-view")

;;;; Data Structures

(cl-defstruct (org-gtd-graph-operation
               (:constructor org-gtd-graph-operation-create)
               (:copier nil))
  "Represents a reversible graph operation.

Fields:
  type        - Operation type (:add-dependency, :remove-dependency, etc.)
  data        - Plist with operation-specific data
  reverse-fn  - Function to reverse the operation"
  type data reverse-fn)

(defvar-local org-gtd-graph-undo--stack nil
  "Stack of operations that can be undone.")

(defvar-local org-gtd-graph-undo--redo-stack nil
  "Stack of operations that can be redone.")

;;;; Recording Operations

(defun org-gtd-graph-undo--clear-redo-stack ()
  "Clear the redo stack when a new operation is recorded."
  (setq org-gtd-graph-undo--redo-stack nil))

(defun org-gtd-graph-undo-record (operation)
  "Record OPERATION for undo.
Adds OPERATION to the undo stack and clears the redo stack."
  (push operation org-gtd-graph-undo--stack)
  (org-gtd-graph-undo--clear-redo-stack))

(defun org-gtd-graph-undo-record-add-dependency (blocker-id blocked-id)
  "Record adding dependency from BLOCKER-ID to BLOCKED-ID for undo."
  (let ((operation (org-gtd-graph-operation-create
                    :type :add-dependency
                    :data (list :blocker-id blocker-id
                                :blocked-id blocked-id)
                    :reverse-fn (lambda ()
                                  ;; Reverse is to remove the dependency
                                  (org-gtd-remove-from-multivalued-property
                                   blocker-id org-gtd-prop-blocks blocked-id)
                                  (org-gtd-remove-from-multivalued-property
                                   blocked-id org-gtd-prop-depends-on blocker-id)))))
    (org-gtd-graph-undo-record operation)))

(defun org-gtd-graph-undo-record-remove-dependency (blocker-id blocked-id)
  "Record removing dependency from BLOCKER-ID to BLOCKED-ID for undo."
  (let ((operation (org-gtd-graph-operation-create
                    :type :remove-dependency
                    :data (list :blocker-id blocker-id
                                :blocked-id blocked-id)
                    :reverse-fn (lambda ()
                                  ;; Reverse is to add the dependency back
                                  (org-gtd-dependencies-create blocker-id blocked-id)))))
    (org-gtd-graph-undo-record operation)))

;;;; Undo/Redo Commands

(defun org-gtd-graph-undo ()
  "Undo last graph operation."
  (interactive)
  (if (null org-gtd-graph-undo--stack)
      (message "Nothing to undo")
    (let ((operation (pop org-gtd-graph-undo--stack)))
      ;; Execute reverse function
      (condition-case err
          (funcall (org-gtd-graph-operation-reverse-fn operation))
        (error (message "Error in undo: %s" err)))
      ;; Push to redo stack
      (push operation org-gtd-graph-undo--redo-stack)
      (message "Undid: %s" (org-gtd-graph-operation-type operation))
      ;; Refresh graph
      (ignore-errors (org-gtd-graph-view-refresh)))))

(defun org-gtd-graph-redo ()
  "Redo last undone operation."
  (interactive)
  (if (null org-gtd-graph-undo--redo-stack)
      (message "Nothing to redo")
    (let* ((operation (pop org-gtd-graph-undo--redo-stack))
           (data (org-gtd-graph-operation-data operation))
           (type (org-gtd-graph-operation-type operation)))
      ;; Re-execute the forward operation
      (condition-case err
          (cond
           ((eq type :add-dependency)
            (let ((blocker-id (plist-get data :blocker-id))
                  (blocked-id (plist-get data :blocked-id)))
              (when (plist-get data :forward-fn)
                (funcall (plist-get data :forward-fn)))
              (org-gtd-dependencies-create blocker-id blocked-id)))
           ((eq type :remove-dependency)
            (let ((blocker-id (plist-get data :blocker-id))
                  (blocked-id (plist-get data :blocked-id)))
              (when (plist-get data :forward-fn)
                (funcall (plist-get data :forward-fn)))
              (org-gtd-remove-from-multivalued-property
               blocker-id org-gtd-prop-blocks blocked-id)
              (org-gtd-remove-from-multivalued-property
               blocked-id org-gtd-prop-depends-on blocker-id))))
        (error (message "Error in redo: %s" err)))
      ;; Push back to undo stack
      (push operation org-gtd-graph-undo--stack)
      (message "Redid: %s" type)
      ;; Refresh graph
      (ignore-errors (org-gtd-graph-view-refresh)))))

;;;; Footer

(provide 'org-gtd-graph-undo)

;;; org-gtd-graph-undo.el ends here
