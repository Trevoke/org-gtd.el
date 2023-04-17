;;; org-gtd-areas-of-focus.el --- Areas of Focus for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Areas of Focus are horizon 2 for GTD.
;; This logic helps handle them;
;;
;;; Code:

(require 'org)

(defcustom org-gtd-areas-of-focus '("Home" "Health" "Family" "Career")
  "The current major areas in your life where you don't want to drop balls.")

(defun org-gtd-areas-of-focus ()
  "Use as a hook when decorating items after clarifying them.

This function requires that the user input find a match amongst the options.
If a new area of focus pops up for you, change the value of the eponymous
variable."
  (let ((chosen-area (completing-read
                      "Which area of focus does this belong to? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
    (org-entry-put (point) "CATEGORY" chosen-area)))

(provide 'org-gtd-areas-of-focus)
;;; org-gtd-areas-of-focus.el ends here
