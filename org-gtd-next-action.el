;;; org-gtd-next-action.el --- Define next action items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Next action items have their own logic, defined here.  Historically
;; this module was named `org-gtd-single-action'; `single-action' and
;; `next-action' are synonyms, with `next-action' being the canonical
;; GTD term.  Obsolete function aliases preserve backward compatibility
;; for user keybindings and external callers.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-types)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-organize-core)
(require 'org-gtd-create)

;;;; Variables

(defvar org-state)  ; dynamically bound by org-mode during state changes

;;;; Constants

;;;; Commands

(defun org-gtd-next-action ()
  "DWIM: organize the heading at point as a next action.
Dispatches via `org-gtd--dispatch', which handles agenda-buffer
markers and the clarify/WIP flow."
  (interactive)
  (org-gtd--dispatch 'next-action))

;;;; Functions

;;;;; Public

(defun org-gtd-next-action-create (topic)
  "Automatically create a next action in the GTD flow.

TOPIC is what you want to see in the agenda view.

Obsolete: use `org-gtd-create-item' instead."
  (declare (obsolete org-gtd-create-item "4.1.0"))
  (org-gtd-create-item 'next-action topic nil))

;;;;; Private

(defun org-gtd-next-action--maybe-convert-to-delegated ()
  "Keep next action and delegated metadata aligned with TODO state.

This function is intended for `org-after-todo-state-change-hook'.
When a next action changes to WAIT, it offers to convert the item to a
proper delegated item.  When a delegated item changes to NEXT, it restores
the next-action classification and removes delegation-only properties."
  (cond
   ((and (equal org-state (org-gtd-keywords--wait))
         (equal (org-entry-get (point) "ORG_GTD")
                (org-gtd-type-org-gtd-value 'next-action)))
    (when (y-or-n-p "Convert to delegated item? ")
      (org-gtd-next-action--convert-to-delegated)))
   ((and (equal org-state (org-gtd-keywords--next))
         (equal (org-entry-get (point) "ORG_GTD")
                (org-gtd-type-org-gtd-value 'delegated)))
    (org-gtd--clear-foreign-properties 'next-action)
    (org-entry-put (point) "ORG_GTD"
                   (org-gtd-type-org-gtd-value 'next-action)))))

(defun org-gtd-next-action--convert-to-delegated ()
  "Convert current next action to a delegated item.

Prompts for who to delegate to and when to check in, then updates
the item's properties accordingly."
  (let* ((who (read-string "Delegated to: "))
         (when-date (org-read-date nil nil nil "Check-in date: ")))
    ;; Set ORG_GTD to Delegated
    (org-entry-put (point) "ORG_GTD" (org-gtd-type-org-gtd-value 'delegated))
    ;; Set DELEGATED_TO property
    (org-entry-put (point) (org-gtd-type-property 'delegated :who) who)
    ;; Set ORG_GTD_TIMESTAMP property
    (org-entry-put (point) (org-gtd-type-property 'delegated :when)
                   (format "<%s>" when-date))))

;;;;; Obsolete aliases (public surface)

(define-obsolete-function-alias 'org-gtd-single-action
  'org-gtd-next-action "4.1.0")
(with-suppressed-warnings ((obsolete org-gtd-next-action-create))
  (define-obsolete-function-alias 'org-gtd-single-action-create
    'org-gtd-next-action-create "4.1.0"))
(define-obsolete-function-alias 'org-gtd-single-action--maybe-convert-to-delegated
  'org-gtd-next-action--maybe-convert-to-delegated "4.1.0")
(define-obsolete-function-alias 'org-gtd-single-action--convert-to-delegated
  'org-gtd-next-action--convert-to-delegated "4.1.0")
;;;; Footer

(provide 'org-gtd-next-action)
;; Back-compat: old feature name for users/modules that still `(require 'org-gtd-single-action)'.
(provide 'org-gtd-single-action)

;;; org-gtd-next-action.el ends here
