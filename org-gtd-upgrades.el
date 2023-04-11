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
;; Major versions aren't backward compatible. This code helps users move
;; their data forward.
;;
;;; Code:

(require 'org-habit)

(defun org-gtd-upgrades-calendar-items-to-v3 ()
  (interactive)
  (with-org-gtd-context
      (org-map-entries
       (lambda ()
         (when (org-gtd-upgrades--upgradable-calendar-p)
           (let ((date (org-entry-get (point) "SCHEDULED")))
             (org-schedule '(4)) ;; pretend I am a universal argument
             (org-entry-put (point) org-gtd-calendar-property date)
             (org-end-of-meta-data t)
             (open-line 1)
             (insert date))))
       "+ORG_GTD=\"Calendar\"+LEVEL=2"
       'agenda)))

(defun org-gtd-upgrades--upgradable-calendar-p ()
  (and (not (org-is-habit-p))
       (org-get-scheduled-time (point))))

(provide 'org-gtd-upgrades)
;;; org-gtd-upgrades.el ends here
