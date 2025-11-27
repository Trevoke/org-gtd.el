;;; org-gtd-process.el --- Code to process inbox -*- lexical-binding: t; coding: utf-8 -*-
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
;; Inbox processing management for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-capture)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-clarify)

;;;; Customization

(defcustom org-gtd-additional-inbox-files nil
  "List of additional inbox files to process after the main inbox.
When processing the inbox, after the main inbox is empty, org-gtd will
continue processing items from these files in order.

Each entry should be an absolute file path to an org file."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0.0")
  :type '(repeat file))

;;;; Variables

(defvar org-gtd-process--pending-inboxes nil
  "List of additional inbox files remaining to process in current session.
This is initialized from `org-gtd-additional-inbox-files' when processing
starts and consumed as each inbox is emptied.")

(defvar org-gtd-process--session-active nil
  "Non-nil when inbox processing session is active.
Used to track whether we're in the middle of processing across recursive calls.")

;;;; Commands

;;;###autoload
(defun org-gtd-process-inbox ()
  "Process all items in the GTD inbox one by one.
Walks through each inbox item sequentially, opening the clarification
interface for decision-making and organization.

After the main inbox is empty, continues processing items from
`org-gtd-additional-inbox-files' in order."
  (interactive)
  ;; Initialize session state on first call
  (unless org-gtd-process--session-active
    (setq org-gtd-process--session-active t
          org-gtd-process--pending-inboxes (copy-sequence org-gtd-additional-inbox-files)))
  (org-gtd-process--next-inbox (org-gtd-inbox-path)))

(defun org-gtd-process--next-inbox (inbox-file)
  "Process items from INBOX-FILE, then continue to pending inboxes."
  (let ((buffer (find-file-noselect inbox-file)))
    (set-buffer buffer)
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1)
      (org-N-empty-lines-before-current 1))
    (if (org-at-heading-p)
        (org-gtd-clarify-inbox-item (point-marker)
                                    (current-window-configuration))
      ;; Current inbox is empty, try next pending inbox
      (org-gtd-process--try-next-inbox))))

(defun org-gtd-process--try-next-inbox ()
  "Try to process the next pending inbox, or stop if none remain."
  (if org-gtd-process--pending-inboxes
      (let ((next-inbox (pop org-gtd-process--pending-inboxes)))
        (if (file-exists-p next-inbox)
            (org-gtd-process--next-inbox next-inbox)
          ;; File doesn't exist, try next
          (org-gtd-process--try-next-inbox)))
    ;; No more inboxes to process
    (message "All inboxes are empty. No items to process.")
    (org-gtd-process--stop)))

;;;; Functions

;;;;; Private

(defun org-gtd-process--stop ()
  "Stop processing the inbox."
  (org-gtd-clarify--cleanup-horizons-view)
  (whitespace-cleanup)
  ;; Clear session state
  (setq org-gtd-process--session-active nil
        org-gtd-process--pending-inboxes nil)
  ;; Save GTD buffers after inbox processing completes
  (org-gtd-save-buffers))

;;;; Footer

(provide 'org-gtd-process)

;;; org-gtd-process.el ends here
