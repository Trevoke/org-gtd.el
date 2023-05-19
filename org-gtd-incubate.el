;;; org-gtd-incubate.el --- Define incubated items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Incubated items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-clarify)
(require 'org-gtd-refile)

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
  (org-gtd-organize--call
   (apply-partially org-gtd-incubate-func
                    reminder-date)))

;;;; Functions

;;;;; Public

(defun org-gtd-incubate-create (topic reminder-date)
  "Automatically create a delegated task in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-incubate reminder-date))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-incubate--apply (&optional reminder-date)
  "Incubate this item through org-gtd.

If you want to call this non-interactively,
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (let ((date (or reminder-date
                  (org-read-date t nil nil "When would you like this item to come up again? "))))
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date))))
  (setq-local org-gtd--organize-type 'incubated)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-incubate org-gtd-incubate-template))

;;;; Footer

(provide 'org-gtd-incubate)

;;; org-gtd-incubate.el ends here
