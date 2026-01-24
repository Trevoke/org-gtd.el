;;; org-gtd-command-center.el --- Command center transient menu -*- lexical-binding: t; coding: utf-8 -*-
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
;; Global command center transient menu for org-gtd.
;; Provides a single entry point to all major GTD operations,
;; organized for both discovery (new users) and quick access (experienced users).
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'org-gtd-archive)
(require 'org-gtd-capture)
(require 'org-gtd-clarify)
(require 'org-gtd-engage)
(require 'org-gtd-process)
(require 'org-gtd-reflect)

;;;; Main Transient

;;;###autoload (autoload 'org-gtd-command-center "org-gtd-command-center" nil t)
(transient-define-prefix org-gtd-command-center ()
  "GTD command center - entry point to all org-gtd operations."
  [["Engage"
    ("e" "Daily view" org-gtd-engage)
    ("@" "By tag" org-gtd-engage-tagged)
    ("n" "All next actions" org-gtd-show-all-next)]
   ["Capture & Process"
    ("c" "Capture to inbox" org-gtd-capture)
    ("p" "Process inbox" org-gtd-process-inbox)
    ("k" "Clarify at point" org-gtd-clarify-item)]]
  [["Reflect"
    ("a" "Area of focus" org-gtd-reflect-area-of-focus)
    ("y" "Someday/maybe" org-gtd-reflect-someday-maybe)
    ("d" "Upcoming delegated" org-gtd-reflect-upcoming-delegated)
    ("r" "Completed items" org-gtd-reflect-completed-items)
    ("R" "Completed projects" org-gtd-reflect-completed-projects)]
   ["Archive"
    ("A" "Archive completed" org-gtd-archive-completed-items)]]
  ["Review System"
   ("S" "Stuck items..." org-gtd-command-center--stuck)
   ("M" "Missed items..." org-gtd-command-center--missed)]
  [("q" "Quit" transient-quit-one)])

;;;; Sub-menus

(transient-define-prefix org-gtd-command-center--stuck ()
  "Review stuck items by category."
  ["Stuck Items"
   ("p" "Projects" org-gtd-reflect-stuck-projects)
   ("c" "Calendar" org-gtd-reflect-stuck-calendar-items)
   ("d" "Delegated" org-gtd-reflect-stuck-delegated-items)
   ("h" "Habits" org-gtd-reflect-stuck-habit-items)
   ("t" "Tickler" org-gtd-reflect-stuck-tickler-items)
   ("s" "Single actions" org-gtd-reflect-stuck-single-action-items)]
  [("q" "Back" transient-quit-one)])

(transient-define-prefix org-gtd-command-center--missed ()
  "Review missed items by category."
  ["Missed Items"
   ("a" "All missed" org-gtd-reflect-missed-engagements)
   ("c" "Calendar only" org-gtd-reflect-missed-calendar)
   ("d" "Delegated only" org-gtd-reflect-missed-delegated)
   ("p" "Projects only" org-gtd-reflect-missed-projects)]
  [("q" "Back" transient-quit-one)])

;;;; Footer

(provide 'org-gtd-command-center)

;;; org-gtd-command-center.el ends here
