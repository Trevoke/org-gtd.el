;;; org-gtd-calendar.el --- Define calendar items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Calendar items have their own state and logic, defined here.
;;
;;; Code:

(defconst org-gtd-calendar-property "ORG_GTD_CALENDAR"
  "Org property used to store date when item shows up in agenda.")

;; (defconst org-gtd-calendar-engage
;;   `(tags
;;     ,(string-join '(org-gtd-calendar-property "=\"<today>\""))
;;     (;;options go in here
;;      )))

;; (defconst org-gtd-calendar-area-of-focus)
;; (defconst org-gtd-calendar-weekly-review)
;; (defconst org-gtd-calendar-missed)

(defcustom org-gtd-calendar-func
  #'org-gtd-calendar--apply
  "Function called when item at point is a task that must happen on a given day.

Keep this clean and don't load your calendar with things that aren't
actually appointments or deadlines."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

(defun org-gtd-calendar--apply ()
  "Add a date/time to this item and store in org gtd."
  (let ((date (org-read-date t nil nil "When is this going to happen? ")))
    (org-entry-put (point) org-gtd-calendar-property (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date))))
  (org-gtd-organize-decorate-item)
  (org-gtd--refile org-gtd-calendar))

;;;###autoload
(defun org-gtd-calendar--one-off ()
  (interactive)
  (org-gtd-organize--call org-gtd-calendar-func))

;;;###autoload
(defun org-gtd-calendar--inbox-loop ()
  (interactive)
  (org-gtd-organize-inbox-item org-gtd-calendar-func))

(provide 'org-gtd-calendar)
;;; org-gtd-calendar.el ends here
