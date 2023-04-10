;;; org-gtd-delegate.el --- logic to delegate items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Item delegation logic for Org GTD.
;;
;;; Code:

(require 'org)

(defcustom org-gtd-delegate-read-func (lambda () (read-string "Who will do this? "))
  "Function that is called to read in the Person the task is delegated to.

Needs to return a string that will be used as the persons name."
  :group 'org-gtd
  :package-version '(org-gtd . "2.3.0")
  :type 'function )

(defcustom org-gtd-organize-delegate-func
  #'org-gtd-delegate--apply
  "Function called when item at at point is an action delegated to someone else."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

;;;###autoload
(defun org-gtd-delegate--one-off ()
  (interactive)
  (org-gtd-organize--call org-gtd-organize-delegate-func))

;;;###autoload
(defun org-gtd-delegate--inbox-loop ()
  (interactive)
  (org-gtd-organize-inbox-item org-gtd-organize-delegate-func))

(defun org-gtd-delegate--apply ()
  "Delegate this item and file it in the org-gtd system."
  (org-gtd-organize-decorate-item)
  (org-gtd-delegate)
  (org-gtd--refile org-gtd-actions))

;; TODO this was requested as a surfaced bit of logic, now with the clarify
;; interface I think this can go away?
;;;###autoload
(defun org-gtd-delegate ()
  "Delegate item at point."
  (interactive)
  (let ((delegated-to (apply org-gtd-delegate-read-func nil))
        (org-inhibit-logging 'note))
    (org-set-property "DELEGATED_TO" delegated-to)
    (org-todo org-gtd-wait)
    (org-schedule 0)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "programmatically delegated to %s\n" delegated-to)))))

(provide 'org-gtd-delegate)
;;; org-gtd-delegate.el ends here
