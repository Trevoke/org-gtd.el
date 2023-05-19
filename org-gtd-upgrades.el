;;; org-gtd-upgrades.el --- Define upgrade logic across org-gtd versions -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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
       (string-equal (org-entry-get (point) "TODO") org-gtd-wait)))

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

;;;; Footer

(provide 'org-gtd-upgrades)

;;; org-gtd-upgrades.el ends here
