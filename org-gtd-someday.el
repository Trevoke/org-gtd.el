;;; org-gtd-someday.el --- Define someday/maybe items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Someday/Maybe items have their own logic, defined here.
;; Unlike tickler items, someday/maybe items have NO timeframe.
;; They are categorized by refile targets with ORG_GTD_REFILE: Someday property.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-reactivate)
(require 'org-gtd-organize-core)

;;;; Constants

(defconst org-gtd-someday-func #'org-gtd-someday--apply
  "Function called when organizing item as someday/maybe.")

(defconst org-gtd-someday-template
  (format "* Someday/Maybe
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-someday)
  "Template for the GTD someday/maybe list.")

;;;; Commands

(defun org-gtd-someday ()
  "Decorate, organize and refile item at point as someday/maybe.

Someday/maybe items are for things you might want to do eventually,
but with no specific timeframe. They are stored in refile targets
with ORG_GTD_REFILE: Someday property."
  (interactive)
  (org-gtd-organize--call
   (lambda () (org-gtd-someday--apply))))

;;;; Functions

;;;;; Public

(defun org-gtd-someday-create (topic)
  "Automatically create a someday/maybe item in the GTD flow.

TOPIC is the string you want to see when reviewing someday/maybe items."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-someday--apply))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-someday--configure ()
  "Configure item at point as someday/maybe.

Saves current state to PREVIOUS_* properties, then sets ORG_GTD
property to Someday, clears TODO keyword, and removes any timestamp properties."
  ;; Save current state before changing type
  (org-gtd-save-state)

  ;; Configure as someday type (no properties needed - no timestamps!)
  (org-gtd-configure-as-type 'someday)

  ;; Clear TODO keyword - someday items are not actionable
  (org-todo "")

  ;; Explicitly remove any timestamp properties that might exist
  (org-entry-delete (point) org-gtd-timestamp)
  (org-entry-delete (point) "SCHEDULED")
  (org-entry-delete (point) "DEADLINE"))

(defun org-gtd-someday--finalize ()
  "Finalize someday/maybe item organization and refile."
  (setq-local org-gtd--organize-type 'someday)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-someday org-gtd-someday-template)))

(defun org-gtd-someday--apply ()
  "Process GTD inbox item by transforming it into a someday/maybe item.

Orchestrates the someday/maybe organization workflow:
1. Configure with someday settings (no timestamps)
2. Finalize and refile to someday/maybe category"
  (org-gtd-someday--configure)
  (org-gtd-someday--finalize))

;;;; Footer

(provide 'org-gtd-someday)

;;; org-gtd-someday.el ends here
