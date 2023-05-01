;;; org-gtd-skip.el --- various org-agenda-skip-functions -*- lexical-binding: t; coding: utf-8 -*-
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
;; Building agenda views is complex, and filtering them effectively can truly
;; require its own language. This is that language.
;;
;;; Code:

(defun org-gtd-skip-AND (funcs)
  "Ensure none of the functions FUNCS want to skip the current entry."
  (let ((non-nil-funcs (seq-drop-while (lambda (x) (not (funcall x))) funcs)))
    (if non-nil-funcs
        (funcall (car non-nil-funcs)))))

(defun org-gtd-skip-unless-timestamp-empty-or-invalid ()
  "Return non-nil if the current headline's ORG_GTD_TIMESTAMP property is not set, null, or not a date."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (prop (org-entry-get nil org-gtd-timestamp)))
    (if (and prop
             (org-string-match-p org-ts-regexp-both prop))
        subtree-end
      nil)))

(defun org-gtd-skip-unless-calendar ()
  "Skip-function: only keep this if it's an org-gtd calendar entry."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal (org-entry-get (point) "ORG_GTD" t)
                      org-gtd-calendar)
        nil
      subtree-end)))

(defun org-gtd-skip-unless-habit ()
  "Skip-function: only keep this if it's a habit."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal "habit" (org-entry-get (point) "STYLE"))
        nil
      subtree-end)))

(defun org-gtd-skip-if-habit ()
  "Skip-function: only keep this if it's a habit."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal "habit" (org-entry-get (point) "STYLE"))
        subtree-end
      nil)))

(defun org-gtd-skip-unless-area-of-focus-func (area)
  "Return a skip-function to only keep if it's a specific GTD AREA of focus."
  (apply-partially #'org-gtd-skip-unless-area-of-focus area))

(defun org-gtd-skip-unless-area-of-focus (area)
  "Skip-function: only keep this if it's a specific GTD AREA of focus."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal (downcase area)
                      (downcase (org-entry-get (point) "CATEGORY")))
        nil
      subtree-end)))

(provide 'org-gtd-skip)
;;; org-gtd-skip.el ends here
