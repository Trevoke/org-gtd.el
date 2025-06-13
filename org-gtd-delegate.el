;;; org-gtd-delegate.el --- logic to delegate items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Item delegation logic for Org GTD.
;;
;;; Code:

;;;; Requirements

(require 'org)

(require 'org-gtd-core)
(require 'org-gtd-single-action)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)

(declare-function 'org-gtd-organize--call 'org-gtd-organize)
(declare-function 'org-gtd-organize-apply-hooks 'org-gtd-organize)

;;;; Customization

(defcustom org-gtd-delegate-read-func (lambda () (read-string "Who will do this? "))
  "Function that is called to read in the Person the task is delegated to.

Needs to return a string that will be used as the persons name."
  :group 'org-gtd-organize
  :package-version '(org-gtd . "2.3.0")
  :type 'function )

;;;; Constants

(defconst org-gtd-delegate-property "DELEGATED_TO")

(defconst org-gtd-delegate-func #'org-gtd-delegate--apply
  "Function called when organizing item at at point as delegated.")

;;;; Commands

(defun org-gtd-delegate (&optional delegated-to checkin-date)
  "Organize and refile item at point as a delegated item.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively."
  (interactive)
  (org-gtd-organize--call
   (apply-partially org-gtd-delegate-func
                    delegated-to
                    checkin-date)))

;;;###autoload
(defun org-gtd-delegate-agenda-item ()
  "Delegate item at point on agenda view."
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((heading-marker (or (org-get-at-bol 'org-marker)
                             (org-agenda-error)))
         (heading-buffer (marker-buffer heading-marker))
         (heading-position (marker-position heading-marker)))
    (with-current-buffer heading-buffer
      (goto-char heading-position)
      (org-gtd-delegate-item-at-point))))

;;;###autoload
(defun org-gtd-delegate-item-at-point (&optional delegated-to checkin-date)
  "Delegate item at point.  Use this if you do not want to refile the item.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively.
If you call this interactively, the function will ask for the name of the
person to whom to delegate by using `org-gtd-delegate-read-func'."
  (declare (modes org-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((org-inhibit-logging 'note))
    (if (and delegated-to checkin-date)
        ;; Non-interactive: set properties directly
        (progn
          (org-entry-put (point) "ORG_GTD" "Action")
          (org-entry-put (point) "TODO" org-gtd-wait)
          (org-entry-put (point) "ID" (org-gtd-id-get-create))
          (org-entry-put (point) org-gtd-delegate-property delegated-to)
          (org-entry-put (point) org-gtd-timestamp (format "<%s>" checkin-date))
          ;; Insert timestamp in content
          (save-excursion
            (org-end-of-meta-data t)
            (open-line 1)
            (insert (format "<%s>" checkin-date))))
      ;; Interactive: use configuration system
      (org-gtd-configure-item (point) :delegated)
      ;; Insert timestamp in content
      (let ((timestamp (org-entry-get (point) org-gtd-timestamp)))
        (save-excursion
          (org-end-of-meta-data t)
          (open-line 1)
          (insert timestamp))))
    ;; Add delegation note
    (let ((person (org-entry-get (point) org-gtd-delegate-property)))
      (save-excursion
        (goto-char (org-log-beginning t))
        (insert (format "programmatically delegated to %s\n" person))))))

;;;; Functions

;;;;; Public

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

;;;;; Private

(defun org-gtd-delegate--apply (&optional delegated-to checkin-date)
  "Organize and refile this as a delegated item in the `org-gtd' system.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively."
  (org-gtd-delegate-item-at-point delegated-to checkin-date)
  (setq-local org-gtd--organize-type 'delegated)
  (org-gtd-organize-apply-hooks)
  (org-gtd-refile--do org-gtd-action org-gtd-action-template))

;;;; Footer

(provide 'org-gtd-delegate)

;;; org-gtd-delegate.el ends here
