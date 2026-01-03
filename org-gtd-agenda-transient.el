;;; org-gtd-agenda-transient.el --- Transient menu for agenda views -*- lexical-binding: t; coding: utf-8 -*-
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
;; Provides a transient menu for working with tasks in org-agenda views.
;; Supports task execution (state changes, clocking, metadata) and triage
;; (time adjustments, clarification).
;;
;; Suggested keybinding for `org-agenda-mode-map':
;;   (define-key org-agenda-mode-map (kbd "?\") #\\='org-gtd-agenda-transient)
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'org-agenda)
(require 'org-gtd-core)
(require 'org-gtd-configure)
(require 'org-gtd-clarify)
(require 'org-gtd-areas-of-focus)

;;;; Context Detection

(defun org-gtd-agenda-transient--task-at-point ()
  "Get marker and info for task at agenda point.
Returns plist with :marker, :title, :org-gtd-type, :has-timestamp.
Returns nil if not on an agenda line with a task."
  (when-let ((marker (org-get-at-bol 'org-marker)))
    (org-with-point-at marker
      (list :marker marker
            :title (org-get-heading t t t t)
            :org-gtd-type (org-entry-get (point) "ORG_GTD")
            :has-timestamp (org-gtd-agenda-transient--has-timestamp-p)))))

(defun org-gtd-agenda-transient--has-timestamp-p ()
  "Return non-nil if current heading's GTD type has deferrable timestamps.
Excludes Habit since deferring habits has different semantics."
  (when-let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (member org-gtd-value '("Calendar" "Delegated" "Tickler"))))

;;;; Header Display

(defun org-gtd-agenda-transient--show-context ()
  "Show context about task at agenda point."
  (if-let ((info (org-gtd-agenda-transient--task-at-point)))
      (format "Selected: %s" (plist-get info :title))
    "No task at point"))

(defun org-gtd-agenda-transient--show-time-p ()
  "Return non-nil if Time section should be shown."
  (when-let ((info (org-gtd-agenda-transient--task-at-point)))
    (plist-get info :has-timestamp)))

;;;; Helper Macro

(defmacro org-gtd-agenda-transient--with-task (&rest body)
  "Execute BODY at the org heading for agenda item at point."
  `(when-let ((marker (org-get-at-bol 'org-marker)))
     (org-with-point-at marker
       ,@body
       (save-buffer))))

;;;; State Change Actions

(defun org-gtd-agenda-transient--done ()
  "Mark task at point as DONE."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (org-todo (org-gtd-keywords--done))))

(defun org-gtd-agenda-transient--waiting ()
  "Set task to WAITING state."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (org-todo (org-gtd-keywords--wait))))

(defun org-gtd-agenda-transient--next ()
  "Set task to NEXT state."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (org-todo (org-gtd-keywords--next))))

(defun org-gtd-agenda-transient--cancel ()
  "Cancel task at point."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (org-todo (org-gtd-keywords--canceled))))

(defun org-gtd-agenda-transient--cycle-todo ()
  "Cycle TODO state of task at point."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (org-todo)))

;;;; Time Operations

(defun org-gtd-agenda-transient--add-days-to-timestamp (timestamp days)
  "Add DAYS to TIMESTAMP string and return new timestamp string.
TIMESTAMP is an Org active timestamp like \"<2025-11-28>\"."
  (when (and timestamp (string-match "<\\([0-9]+-[0-9]+-[0-9]+\\)" timestamp))
    (let* ((date-str (match-string 1 timestamp))
           (time (org-time-string-to-time date-str))
           (new-time (time-add time (days-to-time days)))
           (new-date-str (format-time-string "%F" new-time)))
      (format "<%s>" new-date-str))))

(defun org-gtd-agenda-transient--defer ()
  "Add 1 day to task's GTD timestamp."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (when-let ((current (org-entry-get (point) org-gtd-timestamp)))
     (let ((new-date (org-gtd-agenda-transient--add-days-to-timestamp current 1)))
       (when new-date
         (org-entry-put (point) org-gtd-timestamp new-date))))))

(defun org-gtd-agenda-transient--set-date ()
  "Set new date for task's GTD timestamp."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (let ((new-date (org-gtd-prompt-for-active-date "New date")))
     (org-entry-put (point) org-gtd-timestamp new-date))))

;;;; Clarify Actions

(defun org-gtd-agenda-transient--clarify-refile ()
  "Open clarify workflow, refile when done."
  (interactive)
  (org-gtd-clarify-agenda-item))

(defun org-gtd-agenda-transient--clarify-in-place ()
  "Open clarify workflow, update in place when done."
  (interactive)
  (let ((current-prefix-arg '(4)))  ; Simulate C-u
    (org-gtd-clarify-agenda-item)))

;;;; Clocking Actions

(defun org-gtd-agenda-transient--clock-in ()
  "Clock in on task at point."
  (interactive)
  (org-agenda-clock-in))

(defun org-gtd-agenda-transient--clock-out ()
  "Clock out from current task."
  (interactive)
  (org-agenda-clock-out))

;;;; Metadata Actions

(defun org-gtd-agenda-transient--effort ()
  "Set effort estimate on task at point."
  (interactive)
  (org-agenda-set-effort))

(defun org-gtd-agenda-transient--priority ()
  "Set priority on task at point."
  (interactive)
  (org-agenda-priority))

(defun org-gtd-agenda-transient--tags ()
  "Set tags on task at point."
  (interactive)
  (org-agenda-set-tags))

(defun org-gtd-agenda-transient--note ()
  "Add note to task at point."
  (interactive)
  (org-agenda-add-note))

(defun org-gtd-agenda-transient--area-of-focus ()
  "Set area of focus for task at point."
  (interactive)
  (org-gtd-area-of-focus-set-on-agenda-item))

;;;; Main Transient Definition

;;;###autoload (autoload 'org-gtd-agenda-transient "org-gtd-agenda-transient" nil t)
(transient-define-prefix org-gtd-agenda-transient ()
  "Actions for GTD task at agenda point.

Suggested keybinding for `org-agenda-mode-map':
  (define-key org-agenda-mode-map (kbd \"C-c .\") #\\='org-gtd-agenda-transient)"
  [:description org-gtd-agenda-transient--show-context
   :class transient-row]
  ["State"
   ("d" "Mark DONE" org-gtd-agenda-transient--done)
   ("w" "Set WAITING" org-gtd-agenda-transient--waiting)
   ("n" "Set NEXT" org-gtd-agenda-transient--next)
   ("x" "Cancel" org-gtd-agenda-transient--cancel)
   ("t" "Cycle TODO" org-gtd-agenda-transient--cycle-todo)]
  [:if org-gtd-agenda-transient--show-time-p
   "Time"
   ("+" "Defer 1 day" org-gtd-agenda-transient--defer)
   ("s" "Set date" org-gtd-agenda-transient--set-date)]
  ["Clocking"
   ("I" "Clock in" org-gtd-agenda-transient--clock-in)
   ("O" "Clock out" org-gtd-agenda-transient--clock-out)]
  ["Metadata"
   ("e" "Set effort" org-gtd-agenda-transient--effort)
   ("," "Set priority" org-gtd-agenda-transient--priority)
   (":" "Set tags" org-gtd-agenda-transient--tags)
   ("z" "Add note" org-gtd-agenda-transient--note)
   ("a" "Area of focus" org-gtd-agenda-transient--area-of-focus)]
  ["Clarify"
   ("c" "Clarify (refile)" org-gtd-agenda-transient--clarify-refile)
   ("C" "Clarify (in place)" org-gtd-agenda-transient--clarify-in-place)]
  ["" ("q" "Quit" transient-quit-one)])

;;;; Footer

(provide 'org-gtd-agenda-transient)

;;; org-gtd-agenda-transient.el ends here
