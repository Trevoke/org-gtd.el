;;; org-gtd-oops.el --- Define view for missed events in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Life doesn't go as we expect sometimes.  Here we can find all the things
;; that for did not get updated when they should have.
;;
;;; Code:

;;; code for days overdue
;; (defun stag-day-counter ()
;;   (let ((date (org-entry-get nil "CREATED")))
;;     (abs (org-time-stamp-to-now date))))

;; (let ((org-agenda-files '("/tmp/foo.org"))
;;       (org-agenda-custom-commands
;;        '(("g" "wakatara country"
;;           ((tags "+CREATED<=\"<today\""
;;                  ((org-agenda-overriding-header "Ongoing situations")
;;                   (org-agenda-prefix-format '((tags . " %(stag-day-counter) days: "))))))))))
;;   (org-agenda nil "g"))

;;;; Commands

(defun org-gtd-oops ()
  "Agenda view for all non-respected timely events."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             '(("o" "Show oopses"
                ((tags "+DELEGATED_TO={.+}"
                       ((org-agenda-overriding-header "Missed check-ins on delegated items")
                        (org-agenda-skip-additional-timestamps-same-entry t)
                        (org-agenda-skip-function 'org-gtd-skip-unless-timestamp-in-the-past)))
                 (tags "+ORG_GTD=\"Calendar\"+LEVEL=2"
                       ((org-agenda-overriding-header "Missed appointments")
                        (org-agenda-skip-additional-timestamps-same-entry t)
                        (org-agenda-skip-function
                         '(org-gtd-skip-AND
                           '(org-gtd-skip-unless-calendar
                             org-gtd-skip-unless-timestamp-in-the-past)))))
                 ;; (tags "+LEVEL=2+ORG_GTD=\"Projects\"+DEADLINE<\"<today>\""
                 ;;       ((org-agenda-overriding-header "??")))
                 ;; (tags "+LEVEL=2+ORG_GTD=\"Projects\"+SCHEDULED<\"<today>\""
                 ;;       ((org-agenda-overriding-header "!!")))
                 (agenda ""
                         ((org-agenda-overriding-header "Projects that should have finished")
                          (org-agenda-entry-types '(:deadline))
                          (org-agenda-skip-deadline-prewarning-if-scheduled nil)
                          (org-agenda-include-deadlines t)
                          (org-deadline-warning-days 0)
                          (org-agenda-span 1)
                          (org-agenda-skip-function
                           'org-gtd-skip-unless-deadline-in-the-past)))
                 (agenda ""
                         ((org-agenda-overriding-header "Projects that should have started")
                          (org-agenda-entry-types '(:scheduled))
                          (org-agenda-skip-scheduled-delay-if-deadline nil)
                          (org-agenda-skip-scheduled-if-deadline-is-shown nil)
                          (org-agenda-span 1)
                          (org-agenda-skip-function
                           '(org-gtd-skip-AND
                             '(org-gtd-skip-if-habit
                               org-gtd-skip-unless-scheduled-start-in-the-past))))))))))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;; Footer

(provide 'org-gtd-oops)

;;; org-gtd-oops.el ends here
