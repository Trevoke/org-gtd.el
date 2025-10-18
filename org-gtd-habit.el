;;; org-gtd-habit.el --- Define habits in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Habits have org-mode requirements, we satisfy them here.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-habit "Habits")

(defconst org-gtd-habit-func #'org-gtd-habit--apply
  "Function called when organizing item as a habit.")

(defconst org-gtd-habit-template
  (format "* Habits
:PROPERTIES:
:ORG_GTD_REFILE: %s
:END:
" org-gtd-habit))

;;;; Commands

(defun org-gtd-habit (&optional repeater)
  "Organize and refile item at point as a calendar item.

If you want to call this non-interactively,
REPEATER is `org-mode'-style repeater string (.e.g \".+3d\") which will
determine how often you'll be reminded of this habit."
  (interactive)
  (let ((config-override (when repeater
                           `(('active-timestamp-with-repeater . ,(lambda (x) (format "<%s %s>" (format-time-string "%Y-%m-%d") repeater)))))))
    (org-gtd-organize--call
     (lambda () (org-gtd-habit--apply config-override)))))

;;;; Functions

;;;;; Public

(defun org-gtd-habit-create (topic repeater)
  "Automatically create a habit in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REPEATER is `org-mode'-style repeater string (.e.g \".+3d\") which will
determine how often you'll be reminded of this habit."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config-override `(('active-timestamp-with-repeater . ,(lambda (x) (format "<%s %s>" (format-time-string "%Y-%m-%d") repeater))))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-habit--apply config-override))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-habit--configure (&optional config-override)
  "Configure item at point as a habit.

CONFIG-OVERRIDE can provide input configuration to override default prompting behavior."
  (org-gtd-configure-item (point) :habit nil config-override))

(defun org-gtd-habit--finalize ()
  "Finalize habit organization and refile."
  (setq-local org-gtd--organize-type 'habit)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-habit org-gtd-habit-template))

(defun org-gtd-habit--apply (&optional config-override)
  "Process GTD inbox item by transforming it into a habit.

Orchestrates the habit organization workflow:
1. Configure with habit settings
2. Finalize and refile to habits file

CONFIG-OVERRIDE can provide input configuration to override default prompting behavior."
  (org-gtd-habit--configure config-override)
  (org-gtd-habit--finalize))

;;;; Footer

(provide 'org-gtd-habit)

;;; org-gtd-habit.el ends here
