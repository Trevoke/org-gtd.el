;;; org-gtd-upgrades.el --- Define upgrade logic across org-gtd versions -*- lexical-binding: t; coding: utf-8 -*-
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
;; Major versions aren't backward compatible.  This code helps users move
;; their data forward.
;;
;;; Code:

;;;; Requirements

(require 'org-habit)

(require 'org-gtd-delegate)
(require 'org-gtd-habit)

;;;; Commands

(defun org-gtd-upgrade-v2-to-v3 ()
  "Use only when upgrading org-gtd from v2 to v3.

Changes state of org-gtd tasks to move away from incorrectly used SCHEDULED
planning keyword in `org-mode'."
  (interactive)
  (org-gtd-upgrades-calendar-items-to-v3)
  (org-gtd-upgrades-delegated-items-to-v3)
  (org-gtd-upgrades-incubated-items-to-v3)
  (org-gtd-upgrades-habits-to-v3))

;;;; Functions

;;;;; Public

(defun org-gtd-upgrades-calendar-items-to-v3 ()
  "Change calendar items away from SCHEDULED to using a custom property."
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-upgrades--scheduled-item-p)
           (let ((date (org-entry-get (point) "SCHEDULED")))
             (org-schedule '(4)) ;; pretend I am a universal argument
             (org-entry-put (point) org-gtd-timestamp date)
             (org-end-of-meta-data t)
             (open-line 1)
             (insert date))))
       "+ORG_GTD=\"Calendar\"+LEVEL=2"
       'agenda)))

(defun org-gtd-upgrades-delegated-items-to-v3 ()
  "Change delegated items away from SCHEDULED to using a custom property."
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-upgrades--delegated-item-p)
           (let ((date (org-entry-get (point) "SCHEDULED")))
             (org-schedule '(4)) ;; pretend I am a universal argument
             (org-entry-put (point) org-gtd-timestamp date)
             (org-end-of-meta-data t)
             (open-line 1)
             (insert date))))
       "+ORG_GTD=\"Actions\"+LEVEL=2"
       'agenda)))

(defun org-gtd-upgrades-habits-to-v3 ()
  "Move habits from wherever they may be to their own subtree."
  (with-org-gtd-context
      (org-gtd-refile--add-target org-gtd-habit-template)

      (let ((org-gtd-refile-to-any-target t))
        (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                         "+LEVEL=2&+ORG_GTD=\"Actions\""
                         'agenda)
        (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                         "+LEVEL=2&+ORG_GTD=\"Incubated\""
                         'agenda)
        (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                         "+LEVEL=2&+ORG_GTD=\"Calendar\""
                         'agenda))))

(defun org-gtd-upgrades-incubated-items-to-v3 ()
  "Change incubated items away from SCHEDULED to using a custom property."
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-upgrades--scheduled-item-p)
           (let ((date (org-entry-get (point) "SCHEDULED")))
             (org-schedule '(4)) ;; pretend I am a universal argument
             (org-entry-put (point) org-gtd-timestamp date)
             (org-end-of-meta-data t)
             (open-line 1)
             (insert date))))
       "+ORG_GTD=\"Incubated\"+LEVEL=2"
       'agenda)))

;;;;; Private

(defun org-gtd-upgrades--delegated-item-p ()
  "Return t if item at point is delegated."
  (and (org-entry-get (point) org-gtd-delegate-property)
       (string-equal (org-entry-get (point) "TODO") (org-gtd-keywords--wait))))

(defun org-gtd-upgrades--organize-habits-v3 ()
  "Move element at point to the habits home if it's a habit."
  (when (org-is-habit-p)
    (setq org-map-continue-from (- (org-element-property :begin
                                                         (org-element-at-point))
                                   1))
    (org-gtd-refile--do org-gtd-habit org-gtd-habit-template)))

(defun org-gtd-upgrades--scheduled-item-p ()
  "Return t if item at point is SCHEDULED and not a habit."
  (and (not (org-is-habit-p))
       (org-get-scheduled-time (point))))

(defun org-gtd-upgrade--migrate-level-to-property-based ()
  "Migrate existing level-based projects to property-based system.
This migration:
1. Adds ORG_GTD=Projects property to level 2 items under Projects categories
2. Adds ORG_GTD=Actions property to level 3+ items under project headings
This supports the new property-based task identification system."
  (interactive)
  (with-org-gtd-context
      ;; Step 1: Find Projects category headings and process their level 2 children as project headings
      (org-map-entries
       (lambda ()
         (let ((category-level (org-current-level)))
           (outline-next-heading)
           (while (and (not (eobp))
                       (> (org-current-level) category-level))
             (when (= (org-current-level) (1+ category-level))
               ;; This is a level 2 item under Projects category - it's a project heading
               (unless (org-entry-get (point) "ORG_GTD")
                 (org-entry-put (point) "ORG_GTD" "Projects")))
             (outline-next-heading))))
       "+ORG_GTD=\"Projects\"+LEVEL=1"
       'agenda)

      ;; Step 2: Find all project headings and process their children as project tasks
      (org-map-entries
       (lambda ()
         (let ((project-level (org-current-level)))
           (outline-next-heading)
           (while (and (not (eobp))
                       (> (org-current-level) project-level))
             (when (> (org-current-level) project-level)
               ;; This is any descendant (level 3+) under project heading - it's a project task
               (unless (org-entry-get (point) "ORG_GTD")
                 (org-entry-put (point) "ORG_GTD" "Actions")))
             (outline-next-heading))))
       "+ORG_GTD=\"Projects\"+LEVEL=2"
       'agenda)))

;;;; Footer

(provide 'org-gtd-upgrades)

;;; org-gtd-upgrades.el ends here
