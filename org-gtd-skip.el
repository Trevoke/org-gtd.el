;;; org-gtd-skip.el --- various org-agenda-skip-functions -*- lexical-binding: t; coding: utf-8 -*-
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
;; Skip functions for org-agenda views.  Most filtering in org-gtd v4 is
;; handled by the view-language module using org-ql, but these functions
;; remain for specific use cases.

;;; Code:

;;;; Requirements

(require 'org-gtd-types)

(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-agenda-skip-entry-if "org-agenda" (&rest conditions))

;;;; Functions

(defun org-gtd-skip-unless-in-progress ()
  "Skip-function: only keep if it's not one of the DONE keywords."
  (org-agenda-skip-entry-if 'todo org-done-keywords))

(defun org-gtd-skip-unless-area-of-focus (area)
  "Skip-function: only keep this if it's a specific GTD AREA of focus."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal (downcase area)
                      (downcase (org-entry-get (point) org-gtd-prop-area-of-focus)))
        nil
      subtree-end)))

(defun org-gtd-skip-unless-area-of-focus-func (area)
  "Return a skip-function to only keep if it's a specific GTD AREA of focus."
  (apply-partially #'org-gtd-skip-unless-area-of-focus area))

;;;; Footer

(provide 'org-gtd-skip)

;;; org-gtd-skip.el ends here
