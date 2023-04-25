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

(defvar org-gtd-calendar-property)

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
(defun org-gtd-delegate (&optional delegated-to checkin-date)
  "Organize and refile item at point as a delegated item."
  (interactive)
  (org-gtd-organize--call
   (apply-partially org-gtd-organize-delegate-func
                    delegated-to
                    checkin-date)))

(defun org-gtd-delegate--apply (&optional delegated-to checkin-date)
  "Organize, decorate and refile this as a delegated item in the `org-gtd' system."
  (org-gtd-delegate-item-at-point delegated-to checkin-date)
  (org-gtd-organize-decorate-item)
  (org-gtd--refile org-gtd-actions))

;;;###autoload
(defun org-gtd-delegate-item-at-point (&optional delegated-to checkin-date)
  ;; TODO should this be called org-gtd-project-delegate-task?
  "Delegate item at point.  Use this if you do not want to refile the item."
  (interactive nil agenda-mode)
  (let ((delegated-to (or delegated-to
                          (apply org-gtd-delegate-read-func nil)))
        (date (or checkin-date
                  (org-read-date t nil nil "When do you want to check in on this task? ")))
        (org-inhibit-logging 'note))
    (org-set-property "DELEGATED_TO" delegated-to)
    (org-entry-put (point) org-gtd-calendar-property (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date)))
    (org-todo org-gtd-wait)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "programmatically delegated to %s\n" delegated-to)))))

(defun org-gtd-delegate-create (topic delegated-to checkin-date)
  "Automatically create a delegated task in the GTD flow."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-delegate delegated-to checkin-date))
    (kill-buffer buffer)))

(provide 'org-gtd-delegate)
;;; org-gtd-delegate.el ends here
