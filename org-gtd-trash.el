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

(defcustom org-gtd-organize-trash-func
  #'org-gtd-trash--apply
  "Function called when  item at point is to be discarded."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

;;;###autoload
(defun org-gtd-trash ()
  "Organize and refile item at point as trash."
  (interactive)
  (org-gtd-organize--call org-gtd-organize-trash-func))

(defun org-gtd-trash--apply ()
  "Mark GTD inbox item as cancelled and move it to the org-gtd task archives."
  (org-todo org-gtd-canceled)
  (with-org-gtd-context
      (org-gtd-archive-item-at-point)))

(provide 'org-gtd-trash)
;;; org-gtd-trash.el ends here
