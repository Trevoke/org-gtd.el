;;; org-gtd-incubate.el --- Define incubated items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Incubated items have their own logic, defined here
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

(defconst org-gtd-incubate "Incubated")

(defconst org-gtd-incubate-func #'org-gtd-incubate--apply
  "Function called when organizing item as incubated.")

(defconst org-gtd-incubate-template
  (format "* Incubate
:PROPERTIES:
:ORG_GTD: %s
:END:
" org-gtd-incubate)
  "Template for the GTD someday/maybe list.")

;;;; Commands

(defun org-gtd-incubate (&optional reminder-date)
  "Decorate, organize and refile item at point as incubated.

If you want to call this non-interactively,
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (interactive)
  (let ((config-override (when reminder-date
                           `(('active-timestamp . ,(lambda (x) (format "<%s>" reminder-date)))))))
    (org-gtd-organize--call
     (lambda () (org-gtd-incubate--apply config-override)))))

;;;; Functions

;;;;; Public

(defun org-gtd-incubate-create (topic reminder-date)
  "Automatically create a delegated task in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config-override `(('active-timestamp . ,(lambda (x) (format "<%s>" reminder-date))))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-incubate--apply config-override))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-incubate--configure (&optional config-override)
  "Configure item at point as incubated.

CONFIG-OVERRIDE can provide input configuration to override default prompting behavior."
  (org-gtd-configure-item (point) :incubate nil config-override))

(defun org-gtd-incubate--insert-timestamp ()
  "Insert timestamp from ORG_GTD_TIMESTAMP property into item content."
  (let ((timestamp (org-entry-get (point) "ORG_GTD_TIMESTAMP")))
    (when timestamp
      (save-excursion
        (org-end-of-meta-data t)
        (open-line 1)
        (insert timestamp)))))

(defun org-gtd-incubate--finalize ()
  "Finalize incubated item organization and refile."
  (setq-local org-gtd--organize-type 'incubated)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-incubate org-gtd-incubate-template))

(defun org-gtd-incubate--apply (&optional config-override)
  "Process GTD inbox item by transforming it into an incubated item.

Orchestrates the incubate organization workflow:
1. Configure with incubate settings
2. Insert timestamp in content
3. Finalize and refile to incubate file

CONFIG-OVERRIDE can provide input configuration to override default prompting behavior."
  (org-gtd-incubate--configure config-override)
  (org-gtd-incubate--insert-timestamp)
  (org-gtd-incubate--finalize))

;;;; Footer

(provide 'org-gtd-incubate)

;;; org-gtd-incubate.el ends here
