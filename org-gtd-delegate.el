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
(require 'org-gtd-types)
(require 'org-gtd-next-action)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-organize-core)

;;;; Customization

;;;; Commands

(defun org-gtd-delegate (&optional delegated-to checkin-date)
  "DWIM: organize the heading at point as a delegated item.

DELEGATED-TO is the name of the person, CHECKIN-DATE the YYYY-MM-DD
string, both optional for non-interactive use.  When invoked from
within the clarify/WIP transient flow, keeps the
`org-gtd-organize--call' wrapping so queue/source-cut/window-restore
behavior is preserved.  Otherwise dispatches directly via
`org-gtd--dispatch'."
  (interactive)
  (let ((config (when (or delegated-to checkin-date)
                  `(,@(when delegated-to `((:who . ,delegated-to)))
                    ,@(when checkin-date `((:when . ,(format "<%s>" checkin-date))))))))
    (if (and (boundp 'org-gtd-clarify--clarify-id) org-gtd-clarify--clarify-id)
        (org-gtd-organize--call
         (lambda () (org-gtd-process-heading (point-marker) 'delegated config)))
      (org-gtd--dispatch 'delegated))))

;;;###autoload
(define-obsolete-function-alias 'org-gtd-delegate-agenda-item
  #'org-gtd-delegate "4.1.0"
  "Use `org-gtd-delegate' directly; it reads the agenda marker via
`org-get-at-bol' in its DWIM dispatch.")


;;;; Functions

;;;;; Public

(defun org-gtd-delegate-create (topic delegated-to checkin-date)
  "Automatically create a delegated task in the GTD flow.

TOPIC is the string you want to see in the agenda when this comes up.
DELEGATED-TO is the name of the person to whom this was delegated.
CHECKIN-DATE is the YYYY-MM-DD string of when you want `org-gtd' to remind
you."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config `((:who . ,delegated-to)
                  (:when . ,(format "<%s>" checkin-date)))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (goto-char (point-min))
      (org-gtd-process-heading (point-marker) 'delegated config))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-delegate--organize (type config)
  "Configure heading at point as TYPE (delegated) and add delegation note.
CONFIG is forwarded to `org-gtd-configure-as-type'."
  (org-gtd-configure-as-type type config)
  (org-gtd-delegate--add-delegation-note))

(defun org-gtd-delegate--add-delegation-note ()
  "Add delegation note with person's name from the delegated type's :who property."
  (let ((person (org-entry-get (point) (org-gtd-type-property 'delegated :who))))
    (when person
      (save-excursion
        (goto-char (org-log-beginning t))
        (insert (format "programmatically delegated to %s\n" person))))))

;;;; Footer

(provide 'org-gtd-delegate)

;;; org-gtd-delegate.el ends here
