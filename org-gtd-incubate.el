;;; org-gtd-incubate.el --- Define incubated items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Incubated items have their own logic, defined here
;;
;;; Code:

(defconst org-gtd-incubate-property "ORG_GTD_INCUBATE"
  "The property storing the active timestamp used for agenda/incubation.")

(defcustom org-gtd-organize-incubate-func
  #'org-gtd-incubate--apply
  "Function called when item at point is to be incubated."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

;;;###autoload
(defun org-gtd-incubate ()
  "Decorate, organize and refile item at point as incubated."
  (interactive)
  (org-gtd-organize--call org-gtd-organize-incubate-func))

(defun org-gtd-incubate--apply ()
  "Incubate this item through org-gtd."
  (let ((date (org-read-date t nil nil "When would you like this item to come up again? ")))
    (org-entry-put (point) org-gtd-incubate-property (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date))))
  (org-gtd-organize-decorate-item)
  (org-gtd--refile org-gtd-incubated))

(provide 'org-gtd-incubate)
;;; org-gtd-incubate.el ends here
