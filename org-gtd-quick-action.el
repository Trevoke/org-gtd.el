;;; org-gtd-quick-action.el --- Define quick-action items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Quick action items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-archive)
(require 'org-gtd-configure)

(declare-function org-gtd-organize--call 'org-gtd-organize)
(declare-function org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-quick-action-func #'org-gtd-quick-action--apply
  "Function called when organizing item at point as quick action.")

;;;; Commands

(defun org-gtd-quick-action ()
  "Organize, decorate and refile item at point as a quick action."
  (interactive)
  (org-gtd-organize--call org-gtd-quick-action-func))

;;;; Functions

;;;;; Private

(defun org-gtd-quick-action--configure ()
  "Configure item at point as a quick action."
  (org-gtd-configure-item (point) :quick-action))

(defun org-gtd-quick-action--finalize ()
  "Finalize quick action organization and archive."
  (setq-local org-gtd--organize-type 'quick-action)
  (org-gtd-organize-apply-hooks)
  (org-gtd-archive-item-at-point))

(defun org-gtd-quick-action--apply ()
  "Process GTD inbox item by doing it now.

Orchestrates the quick action organization workflow:
1. Configure as quick action
2. Finalize and archive"
  (org-gtd-quick-action--configure)
  (org-gtd-quick-action--finalize))

;;;; Footer

(provide 'org-gtd-quick-action)

;;; org-gtd-quick-action.el ends here
