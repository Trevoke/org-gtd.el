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
;; This logic helps handle them.
;;
;;; Code:

;;;; Requirements

(require 'org)

(require 'org-gtd-core)
(require 'org-gtd-horizons)
(require 'org-gtd-organize)
(require 'org-gtd-projects)

;;;; Customization

(defcustom org-gtd-areas-of-focus '("Home" "Health" "Family" "Career")
  "The current major areas in your life where you don't want to drop balls."
  :group 'org-gtd-horizons
  :package-version '(org-gtd . "3.0.0")
  :type 'list)

;;;; Commands

(defun org-gtd-area-of-focus-set-on-item-at-point ()
  (declare (modes org-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((chosen-area (completing-read
                      "Which area of focus does this belong to? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
    (org-entry-put (point) "CATEGORY" chosen-area)))

(defun org-gtd-area-of-focus-set-on-agenda-item ()
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (set-marker-insertion-type marker t)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (if (string-equal (org-entry-get nil "ORG_GTD" t)
                          org-gtd-projects)
            (org-up-heading-safe))
        (org-gtd-area-of-focus-set-on-item-at-point)))))

;;;; Functions

;;;;; Public

(defalias 'org-gtd-set-area-of-focus 'org-gtd-areas-of-focus--set)

;;;;; Private

(defun org-gtd-areas-of-focus--set ()
  "Use as a hook when decorating items after clarifying them.

This function requires that the user input find a match amongst the options.
If a new area of focus pops up for you, change the value of the eponymous
variable."
  (unless (org-gtd-organize-type-member-p '(project-task trash knowledge quick-action))
    (org-gtd-area-of-focus-set-on-item-at-point)))

;;;; Footer

(provide 'org-gtd-areas-of-focus)

;;; org-gtd-areas-of-focus.el ends here
