;;; org-gtd-trash.el --- Define trash items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Trash items have their own logic, defined here.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-archive)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-trash-func #'org-gtd-trash--apply
  "Function called when organizing item at point as trash.")

;;;; Commands

(defun org-gtd-trash ()
  "Organize and refile item at point as trash."
  (interactive)
  (org-gtd-organize--call org-gtd-trash-func))

;;;; Functions

;;;;; Private

(defun org-gtd-trash--apply ()
  "Mark GTD inbox item as cancelled and move it to the org-gtd task archives."
  (org-todo org-gtd-canceled)
  (setq-local org-gtd--organize-type 'trash)
  (org-gtd-organize-apply-hooks)
  (org-gtd-archive-item-at-point))

;;;; Footer

(provide 'org-gtd-trash)

;;; org-gtd-trash.el ends here
