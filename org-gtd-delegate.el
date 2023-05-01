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

(require 'org-gtd-calendar)

(defconst org-gtd-delegate-property "DELEGATED_TO")

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
  "Organize and refile item at point as a delegated item.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively."
  (interactive)
  (org-gtd-organize--call
   (apply-partially org-gtd-organize-delegate-func
                    delegated-to
                    checkin-date)))

(defun org-gtd-delegate--apply (&optional delegated-to checkin-date)
  "Organize and refile this as a delegated item in the `org-gtd' system.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively."
  (org-gtd-delegate-item-at-point delegated-to checkin-date)
  (setq-local org-gtd--organize-type 'delegated)
  (org-gtd-organize-apply-hooks)
  (org-gtd--refile org-gtd-actions org-gtd-actions-template))

;;;###autoload
(defun org-gtd-delegate-item-at-point (&optional delegated-to checkin-date)
  "Delegate item at point.  Use this if you do not want to refile the item.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively.
If you call this interactively, the function will ask for the name of the
person to whom to delegate by using `org-gtd-delegate-read-func'."
  (interactive nil agenda-mode)
  (let ((delegated-to (or delegated-to
                          (apply org-gtd-delegate-read-func nil)))
        (date (or checkin-date
                  (org-read-date t nil nil "When do you want to check in on this task? ")))
        (org-inhibit-logging 'note))
    (org-set-property org-gtd-delegate-property delegated-to)
    (org-entry-put (point) org-gtd-calendar-property (format "<%s>" date))
    (save-excursion
      (org-end-of-meta-data t)
      (open-line 1)
      (insert (format "<%s>" date)))
    (org-todo org-gtd-wait)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "programmatically delegated to %s\n" delegated-to)))))

;;;###autoload
(defun org-gtd-delegate-agenda-item ()
  "Delegate item at point on agenda view."
  (interactive nil '(org-agenda-mode))
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((heading-marker (or (org-get-at-bol 'org-marker)
                             (org-agenda-error)))
         (heading-buffer (marker-buffer heading-marker))
         (heading-position (marker-position heading-marker)))
    (with-current-buffer heading-buffer
      (goto-char heading-position)
      (org-gtd-delegate-item-at-point))))

(defun org-gtd-delegate-create (topic delegated-to checkin-date)
  "Automatically create a delegated task in the GTD flow.

TOPIC is the string you want to see in the agenda when this comes up.
DELEGATED-TO is the name of the person to whom this was delegated.
CHECKIN-DATE is the YYYY-MM-DD string of when you want `org-gtd' to remind
you."
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
