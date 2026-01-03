;;; org-gtd-engage.el --- GTD Engage views -*- lexical-binding: t; coding: utf-8 -*-
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
;; The GTD "Engage" step - viewing and working with your actionable items.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-backward-compatibility)
(require 'org-gtd-view-language)

;;;; Customization

(defgroup org-gtd-engage nil
  "Customize the engage views in the org-gtd package."
  :group 'org-gtd
  :package-version '(org-gtd . "3.1"))

;; Backward compatibility alias for renamed variable
(define-obsolete-variable-alias 'org-gtd-engage-prefix-width
  'org-gtd-prefix-width "4.0")

;;;; GTD View Specifications

(defun org-gtd-engage-view-spec ()
  "Return GTD view specification for the engage view.
Shows:
- Calendar day view (Calendar + Habit items with timestamps for today)
- Tickler items due today
- Delegated items with check-ins due today
- All next actions"
  `((name . "GTD Engage View")
    (prefix . (project area-of-focus "—"))
    (prefix-width . ,org-gtd-prefix-width)
    (blocks . (((name . "Today's Schedule")
                (block-type . calendar-day))
               ((name . "Tickler items ready for today")
                (type . tickler)
                (when . today))
               ((name . "Delegated items to check in on today")
                (type . delegated)
                (when . today))
               ((name . "All actions ready to be executed")
                (type . next-action))))))

(defun org-gtd-engage-tagged-view-spec (tag)
  "Return GTD view specification for next actions with TAG."
  `((name . ,(format "Next Actions: %s" tag))
    (prefix . (project area-of-focus "—"))
    (prefix-width . ,org-gtd-prefix-width)
    (type . next-action)
    (tags . (,tag))))

(defun org-gtd-show-all-next-view-spec ()
  "Return GTD view specification for showing all next actions."
  '((name . "All Next Actions")
    (type . next-action)))

;;;; Commands

;;;###autoload
(defun org-gtd-engage ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (org-gtd-view-show (org-gtd-engage-view-spec)))

;;;###autoload
(defun org-gtd-engage-tagged (&optional tag)
  "Show next actions filtered by TAG.
Prompts for a tag if not provided."
  (interactive
   (list (completing-read "Show next actions with tag: "
                          (org-global-tags-completion-table))))
  (org-gtd-view-show (org-gtd-engage-tagged-view-spec tag)))

;;;###autoload
(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-gtd-view-show (org-gtd-show-all-next-view-spec)))

;;;; Backward Compatibility

;;;###autoload
(define-obsolete-function-alias 'org-gtd-engage-grouped-by-context
  #'org-gtd-engage-tagged "4.0"
  "Use `org-gtd-engage-tagged' instead.
The new function prompts for any tag, not just @-prefixed context tags.")

;;;; Footer

(provide 'org-gtd-engage)

;;; org-gtd-engage.el ends here
