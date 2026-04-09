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

;;;; Variables

(defvar org-state)  ; dynamically bound by org-mode during state changes

;;;; Constants

(defconst org-gtd-action-template
  (format "* Actions
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-action))

(defconst org-gtd-next-action-func #'org-gtd-next-action--apply
  "Function called when organizing item at point as a next action.")

;;;; Commands

(defun org-gtd-next-action ()
  "Organize, decorate and refile item at point as a next action."
  (interactive)
  (org-gtd-organize--call org-gtd-next-action-func))

;;;; Functions

;;;;; Public

(defun org-gtd-next-action-create (topic)
  "Automatically create a next action in the GTD flow.

TOPIC is what you want to see in the agenda view."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-next-action))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-next-action--maybe-convert-to-delegated ()
  "Prompt to convert next action to delegated item when changed to WAIT.

This function is intended for `org-after-todo-state-change-hook'.
It checks if:
1. The item is a next action (ORG_GTD=Actions)
2. The new state is WAIT

If conditions are met, prompts the user to convert to a delegated item.
If they confirm, prompts for who to delegate to and when to check in."
  (when (and (equal org-state (org-gtd-keywords--wait))
             (equal (org-entry-get (point) "ORG_GTD")
                    (org-gtd-type-org-gtd-value 'next-action)))
    (when (y-or-n-p "Convert to delegated item? ")
      (org-gtd-next-action--convert-to-delegated))))

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

(defun org-gtd-next-action--configure ()
  "Configure item at point as a next action."
  (org-gtd-configure-as-type 'next-action))

(defun org-gtd-next-action--finalize ()
  "Finalize next action organization and refile."
  (setq-local org-gtd--organize-type 'next-action)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-action org-gtd-action-template)))

(defun org-gtd-next-action--apply ()
  "Process GTD inbox item by transforming it into a next action.

Orchestrates the next action organization workflow:
1. Configure as next action
2. Finalize and refile to actions file"
  (org-gtd-next-action--configure)
  (org-gtd-next-action--finalize))

;;;;; Obsolete aliases (public surface)

(define-obsolete-function-alias 'org-gtd-single-action
  'org-gtd-next-action "4.1.0")
(define-obsolete-function-alias 'org-gtd-single-action-create
  'org-gtd-next-action-create "4.1.0")
(define-obsolete-function-alias 'org-gtd-single-action--maybe-convert-to-delegated
  'org-gtd-next-action--maybe-convert-to-delegated "4.1.0")
(define-obsolete-function-alias 'org-gtd-single-action--convert-to-delegated
  'org-gtd-next-action--convert-to-delegated "4.1.0")
(defvaralias 'org-gtd-single-action-func 'org-gtd-next-action-func)
(make-obsolete-variable 'org-gtd-single-action-func 'org-gtd-next-action-func "4.1.0")

;;;; Footer

(provide 'org-gtd-next-action)
;; Back-compat: old feature name for users/modules that still `(require 'org-gtd-single-action)'.
(provide 'org-gtd-single-action)

;;; org-gtd-next-action.el ends here
