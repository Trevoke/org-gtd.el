;;; org-gtd-single-action.el --- Define single action items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Single action items have their own logic, defined here
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


;;;; Constants

(defconst org-gtd-action-template
  (format "* Actions
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-action))

(defconst org-gtd-single-action-func #'org-gtd-single-action--apply
  "Function called when organizing item at point as a single next action.")

;;;; Commands

(defun org-gtd-single-action ()
  "Organize, decorate and refile item at point as a single action."
  (interactive)
  (org-gtd-organize--call org-gtd-single-action-func))

;;;; Functions

;;;;; Public

(defun org-gtd-single-action-create (topic)
  "Automatically create a single action in the GTD flow.

TOPIC is what you want to see in the agenda view."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-single-action))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-single-action--maybe-convert-to-delegated ()
  "Prompt to convert single action to delegated item when changed to WAIT.

This function is intended for `org-after-todo-state-change-hook'.
It checks if:
1. The item is a single action (ORG_GTD=Actions)
2. The new state is WAIT

If conditions are met, prompts the user to convert to a delegated item.
If they confirm, prompts for who to delegate to and when to check in."
  (when (and (equal org-state (org-gtd-keywords--wait))
             (equal (org-entry-get (point) "ORG_GTD")
                    (org-gtd-type-org-gtd-value 'next-action)))
    (when (y-or-n-p "Convert to delegated item? ")
      (org-gtd-single-action--convert-to-delegated))))

(defun org-gtd-single-action--convert-to-delegated ()
  "Convert current single action to a delegated item.

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

(defun org-gtd-single-action--configure ()
  "Configure item at point as a single action."
  (org-gtd-configure-as-type 'next-action))

(defun org-gtd-single-action--finalize ()
  "Finalize single action organization and refile."
  (setq-local org-gtd--organize-type 'single-action)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-action org-gtd-action-template)))

(defun org-gtd-single-action--apply ()
  "Process GTD inbox item by transforming it into a single action.

Orchestrates the single action organization workflow:
1. Configure as next action
2. Finalize and refile to actions file"
  (org-gtd-single-action--configure)
  (org-gtd-single-action--finalize))

;;;; Footer

(provide 'org-gtd-single-action)

;;; org-gtd-single-action.el ends here
