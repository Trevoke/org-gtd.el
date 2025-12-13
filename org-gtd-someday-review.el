;;; org-gtd-someday-review.el --- Review someday/maybe items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Iterative review of someday/maybe items.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)

;;;; Variables

(defvar org-gtd-someday-review--session-active nil
  "Non-nil when a someday review session is active.")

(defvar org-gtd-someday-review--state nil
  "State for active someday review session.
Plist with :queue (list of org-ids), :position (current index),
:list-name (which list being reviewed), :reviewed (count),
:clarified (count).")

;;;; Functions

;;;;; Session Management

(defun org-gtd-someday-review--start-session (list-filter)
  "Start a review session for items matching LIST-FILTER."
  (let ((items (org-gtd-someday-review--find-items list-filter)))
    (setq org-gtd-someday-review--session-active t
          org-gtd-someday-review--state
          (list :queue items
                :position 0
                :list-name list-filter
                :reviewed 0
                :clarified 0))))

(defun org-gtd-someday-review--end-session ()
  "End the current review session."
  (let ((reviewed (plist-get org-gtd-someday-review--state :reviewed))
        (clarified (plist-get org-gtd-someday-review--state :clarified)))
    (setq org-gtd-someday-review--session-active nil
          org-gtd-someday-review--state nil)
    (message "Review complete. %d items reviewed, %d clarified." reviewed clarified)))

;;;;; Private

(defun org-gtd-someday-review--find-items (list-filter)
  "Find someday items, optionally filtered by LIST-FILTER.
LIST-FILTER can be:
  - nil: find all someday items
  - a string: find items with matching ORG_GTD_SOMEDAY_LIST
  - symbol `unassigned': find items without ORG_GTD_SOMEDAY_LIST"
  (let ((items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (string= (org-entry-get (point) "ORG_GTD") org-gtd-someday)
               (let ((item-list (org-entry-get (point) org-gtd-prop-someday-list)))
                 (when (org-gtd-someday-review--item-matches-filter-p item-list list-filter)
                   (push (org-id-get-create) items)))))))))
    (nreverse items)))

(defun org-gtd-someday-review--item-matches-filter-p (item-list list-filter)
  "Return t if ITEM-LIST matches LIST-FILTER.
ITEM-LIST is the value of ORG_GTD_SOMEDAY_LIST property (or nil).
LIST-FILTER is nil (match all), a string (match exact), or `unassigned'."
  (cond
   ((null list-filter) t)
   ((eq list-filter 'unassigned) (null item-list))
   ((stringp list-filter) (equal item-list list-filter))
   (t nil)))

(defun org-gtd-someday-review--add-reviewed-entry ()
  "Add a 'Reviewed' entry to the LOGBOOK drawer at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((drawer-pos (org-log-beginning t))
           (has-drawer (save-excursion
                         (goto-char drawer-pos)
                         (looking-back ":LOGBOOK:\n" (line-beginning-position 0)))))
      (goto-char drawer-pos)
      (unless has-drawer
        (insert ":LOGBOOK:\n")
        (save-excursion
          (insert ":END:\n")))
      (insert (format "- Reviewed %s\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

;;;; Footer

(provide 'org-gtd-someday-review)

;;; org-gtd-someday-review.el ends here
