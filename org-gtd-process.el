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
(require 'org-gtd-walk)
(require 'org-gtd-inbox-walk)

;;;; Customization

(defcustom org-gtd-additional-inbox-files nil
  "List of additional inbox files to process after the main inbox.
When processing the inbox, after the main inbox is empty, org-gtd will
continue processing items from these files in order.

Each entry should be an absolute file path to an org file."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0.0")
  :type '(repeat file))

;;;; Commands

;;;###autoload
(defun org-gtd-process-inbox ()
  "Process all items in the GTD inbox one by one.
Walks through each inbox item sequentially, opening the clarification
interface for decision-making and organization.

After the main inbox is empty, continues processing items from
`org-gtd-additional-inbox-files' in order.

Drives the generic walk engine (`org-gtd-walk-start') over the inbox
walk spec (see org-gtd-inbox-walk.el): the multi-source scan
\(`org-gtd-inbox-walk--build-model') collapses the old
pending-inboxes/next-inbox iteration and the duplicate queue into one
walk model.  See
docs/plans/2026-07-17-walk-engine-phase-4-plan.md."
  (interactive)
  (let* ((files (org-gtd-inbox-walk--file-list))
         (model (org-gtd-inbox-walk--build-model))
         (window-config (current-window-configuration)))
    (if (org-gtd-walk-model-done-p model)
        (message "All inboxes are empty. No items to process.")
      (org-gtd-walk-start (org-gtd-inbox-walk--spec files)
                          (org-gtd-inbox-walk--surface)
                          model)
      ;; Stash the pre-processing window configuration on the surface so
      ;; `org-gtd-clarify-stop' (quit path) can restore it, matching the
      ;; pre-engine inbox flow.  `org-gtd-walk-start' leaves the surface
      ;; current on success (see `org-gtd-walk-start'), and :render never
      ;; touches `org-gtd-clarify--window-config', so this survives every
      ;; later advance.
      (when org-gtd-walk--active
        (setq-local org-gtd-clarify--window-config window-config)))))

;;;; Footer

(provide 'org-gtd-process)

;;; org-gtd-process.el ends here
