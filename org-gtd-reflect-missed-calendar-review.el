;;; org-gtd-reflect-missed-calendar-review.el --- Actionable overdue-calendar review -*- lexical-binding: t; coding: utf-8 -*-
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
;; The actionable counterpart of the read-only `org-gtd-reflect-missed-calendar'
;; view: a walk that shows each overdue Calendar item one at a time and lets the
;; user decide -- with consent -- what each one becomes now (done, migrate to a
;; next action, reschedule, trash, clarify, or skip).  A walk consumer,
;; structurally identical to `org-gtd-someday-review'.  See
;; docs/plans/2026-07-19-overdue-calendar-review-design.md.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-skip)
(require 'org-gtd-types)
(require 'org-gtd-organize-core)
(require 'org-gtd-archive)
(require 'org-gtd-clarify)
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Variables

(defconst org-gtd-reflect-missed-calendar-review--surface-key "missed-calendar-review"
  "Fixed WIP key for the single missed-calendar-review surface buffer.")

;;;; Detection

(defun org-gtd-reflect-missed-calendar-review--find-items ()
  "Return the org-ids of every overdue Calendar item across `org-agenda-files'.

Composes `org-gtd-skip.el' predicates matching the design's definition of
overdue calendar: ORG_GTD = Calendar, not done, ORG_GTD_TIMESTAMP strictly
before today, and not an org-gtd habit.  The not-habit clause is redundant
given the Calendar/Habit type invariant (an entry cannot be both) but is
kept for parity with that stated definition.  The predicate factories are
captured once in the outer `let' and `funcall'ed per heading."
  (let ((calendar-p (org-gtd-pred--property-equals
                     "ORG_GTD" (org-gtd-type-org-gtd-value 'calendar)))
        (not-done-p (org-gtd-pred--not-done))
        (overdue-p (org-gtd-pred--property-ts<
                    (org-gtd-type-property 'calendar :when) "today"))
        (not-habit-p (org-gtd-pred--property-not-equals
                      "ORG_GTD" (org-gtd-type-org-gtd-value 'habit)))
        (items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (and (funcall calendar-p)
                        (funcall not-done-p)
                        (funcall overdue-p)
                        (funcall not-habit-p))
               (push (org-id-get-create) items)))))))
    (nreverse items)))

(defun org-gtd-reflect-missed-calendar-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))

;;;; Footer

(provide 'org-gtd-reflect-missed-calendar-review)

;;; org-gtd-reflect-missed-calendar-review.el ends here
