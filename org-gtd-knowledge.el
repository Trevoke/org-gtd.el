;;; org-gtd-knowledge.el --- Define logic for handling knowledge in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; GTD needs logic to handle items that are knowledge, this is it.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-archive)
(require 'org-gtd-clarify)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-knowledge-func #'org-gtd-knowledge--apply
  "Function called when item at point is knowledge to be stored.
Note that this function is used inside loops,for instance to process the inbox,
so if you have manual steps you need to take when storing a heading
as knowledge, take them before calling this function
\(for instance, during inbox processing, take the manual steps during the
clarify step, before you call `org-gtd-organize').")

;;;; Commands

(defun org-gtd-knowledge ()
  "Decorate, organize and refile item at point as knowledge."
  (interactive)
  (org-gtd-organize--call org-gtd-knowledge-func))

;;;; Functions

;;;;; Private

(defun org-gtd-knowledge--apply ()
  "Once the user has filed this knowledge, we can execute this logic."
  (org-todo org-gtd-done)
  (setq-local org-gtd--organize-type 'knowledge)
  (org-gtd-organize-apply-hooks)
  (org-gtd-archive-item-at-point))

;;;; Footer

(provide 'org-gtd-knowledge)

;;; org-gtd-knowledge.el ends here
