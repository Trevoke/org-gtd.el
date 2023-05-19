;;; org-gtd-horizons.el --- manage the horizons buffer -*- lexical-binding: t; coding: utf-8 -*-
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
;; The horizons help us ensure our lives are on the right track.
;; The buffer can show up when clarifying to make sure we don't
;; get distracted.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-files)

;;;; Customization

(defgroup org-gtd-horizons nil
  "Variables handling GTD horizons-related logic."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-horizons-file "horizons.org"
  "File holding your GTD horizons.

This may get displayed during item clarification for context and focus.
This file must be in the `org-gtd-directory'."
  :group 'org-gtd-horizons
  :package-version '(org-gtd . "3.0")
  :type 'file)

;;;; Constants

(defconst org-gtd-file-horizons-template
  "* Purpose and principles (why)
* Vision (what)
* Goals
* Areas of focus / accountabilities
")

;;;; Functions

;;;;; Private

(defun org-gtd--horizons-file ()
  "Create or return the buffer to the file containing the GTD horizons."
  (let ((path (f-join org-gtd-directory org-gtd-horizons-file)))
    (org-gtd--ensure-file-exists path org-gtd-file-horizons-template)
    (find-file-noselect path)))

;;;; Footer

(provide 'org-gtd-horizons)

;;; org-gtd-horizons.el ends here
