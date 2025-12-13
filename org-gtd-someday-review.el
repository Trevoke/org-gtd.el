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
(require 'org-gtd-wip)

;;;; Variables

(defvar org-gtd-someday-review--session-active nil
  "Non-nil when a someday review session is active.")

(defvar org-gtd-someday-review--state nil
  "State for active someday review session.
Plist with :queue (list of org-ids), :position (current index),
:list-name (which list being reviewed), :reviewed (count),
:clarified (count).")

(defvar-local org-gtd-someday-review--current-item-id nil
  "ID of the item currently being reviewed in this buffer.")

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

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-someday-review-mode org-mode "GTD-Review"
  "Major mode for reviewing someday/maybe items.
Derived from `org-mode' and provides read-only review interface
with keybindings for defer, clarify, and quit actions."
  :group 'org-gtd
  (setq buffer-read-only t))

;;;;; Buffer Display

(defun org-gtd-someday-review--display-current-item ()
  "Display the current item in a WIP review buffer."
  (let* ((queue (plist-get org-gtd-someday-review--state :queue))
         (pos (plist-get org-gtd-someday-review--state :position))
         (item-id (nth pos queue))
         (marker (org-id-find item-id 'marker)))
    (when marker
      ;; Clean up previous review buffer if exists
      (org-gtd-someday-review--cleanup-current-buffer)
      ;; Get WIP buffer for this item
      (let ((buf (org-gtd-wip--get-buffer item-id)))
        (org-gtd-someday-review--initialize-buffer marker buf)
        (with-current-buffer buf
          (org-gtd-someday-review-mode)
          (setq org-gtd-someday-review--current-item-id item-id)
          (plist-put org-gtd-someday-review--state :current-buffer-id item-id)
          (setq buffer-read-only t)
          (setq header-line-format
                (format "[d] Defer  [c] Clarify  [q] Quit  (%d/%d)"
                        (1+ pos) (length queue))))
        (pop-to-buffer buf)))))

(defun org-gtd-someday-review--initialize-buffer (marker buffer)
  "Initialize BUFFER with content from item at MARKER."
  (when (= (buffer-size buffer) 0)
    (let ((last-command nil))
      (org-with-point-at marker
        (org-copy-subtree))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (org-paste-subtree)
          (goto-char (point-min)))))))

(defun org-gtd-someday-review--cleanup-current-buffer ()
  "Clean up the current review WIP buffer."
  (when-let ((item-id (plist-get org-gtd-someday-review--state :current-buffer-id)))
    (org-gtd-wip--cleanup-temp-file item-id)
    (plist-put org-gtd-someday-review--state :current-buffer-id nil)))

;;;; Footer

(provide 'org-gtd-someday-review)

;;; org-gtd-someday-review.el ends here
