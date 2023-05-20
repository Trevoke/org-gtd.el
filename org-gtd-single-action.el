;;; org-gtd-single-action.el --- Define single action items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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

(require 'org-gtd-clarify)
(require 'org-gtd-refile)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-action "Actions")

(defconst org-gtd-action-template
  (format "* Actions
:PROPERTIES:
:ORG_GTD: %s
:END:
" org-gtd-action))

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
  "Automatically create a delegated task in the GTD flow.

TOPIC is what you want to see in the agenda view."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-single-action))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-single-action--apply ()
  "Item at point is a one-off action, ready to be executed."
  (org-todo org-gtd-next)
  (setq-local org-gtd--organize-type 'single-action)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-action org-gtd-action-template))

;;;; Footer

(provide 'org-gtd-single-action)

;;; org-gtd-single-action.el ends here
