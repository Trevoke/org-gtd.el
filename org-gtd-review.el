;;; org-gtd-review.el --- Guided review sessions for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

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
;; Guided, profile-driven review sessions — Weekly Review by default.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'f)
(require 'seq)
(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-checklist)
(require 'org-gtd-create)

;;;; Customization

(defcustom org-gtd-review-profiles
  '(("Weekly Review"
     ("Get Clear"
      (:title "Gather loose materials"
       :type prompt
       :instruction "Collect loose papers, receipts, and notes.  Capture each one into the inbox.")
      (:title "Mind sweep"
       :type checklist
       :checklist "Weekly Review triggers"
       :instruction "Walk each trigger.  Press c to capture whatever it shakes loose.")
      (:title "Inbox to zero"
       :type command
       :command org-gtd-process-inbox
       :instruction "Process every inbox item.  Come back here and press n when the inbox is empty."))
     ("Get Current"
      (:title "Review missed items"
       :type view
       :view org-gtd-reflect-missed-items
       :instruction "Reschedule, complete, or cancel anything that slipped.")
      (:title "Review Waiting-For"
       :type view
       :view org-gtd-reflect-upcoming-delegated
       :instruction "Nudge, close, or re-delegate each delegated item.")
      (:title "Review next actions"
       :type view
       :view org-gtd-show-all-next
       :instruction "Mark done what is done; check these still feel current.")
      (:title "Review stuck projects"
       :type view
       :view org-gtd-reflect-stuck-projects
       :instruction "Give every active project a next action."))
     ("Get Creative"
      (:title "Review Someday/Maybe"
       :type view
       :view org-gtd-reflect-someday-maybe
       :instruction "Reactivate anything whose time has come.")
      (:title "Capture new ideas"
       :type prompt
       :instruction "Any creative, risky, or fun ideas?  Capture them."))))
  "Alist of guided review profiles.
Each entry is (PROFILE-NAME . PHASES); each phase is
\(PHASE-NAME . STEPS); each step is a plist with :title, :type
\(one of `prompt', `command', `view', `checklist'), an optional
:instruction, and the type-specific key :command, :view, or
:checklist (a template name in the checklists file)."
  :group 'org-gtd
  :type 'sexp)

;;;; Variables

(defvar org-gtd-review--state nil
  "State plist of the active review session.
Keys: :profile (name string), :phase (index), :step (index),
:acted (step-local flag), :walk-items, :walk-pos, :done, :skipped.")

(defvar org-gtd-review--window-config nil
  "Window configuration to restore when the session ends.")

(defconst org-gtd-review--buffer-name "*GTD Review*")

;;;; Accessors

(defun org-gtd-review--phases ()
  "Return the phases of the active profile."
  (cdr (assoc (plist-get org-gtd-review--state :profile)
              org-gtd-review-profiles)))

(defun org-gtd-review--current-phase ()
  "Return the (NAME . STEPS) phase the session is in."
  (nth (plist-get org-gtd-review--state :phase) (org-gtd-review--phases)))

(defun org-gtd-review--current-step ()
  "Return the step plist the session is on."
  (nth (plist-get org-gtd-review--state :step)
       (cdr (org-gtd-review--current-phase))))

;;;; Footer

(provide 'org-gtd-review)

;;; org-gtd-review.el ends here
