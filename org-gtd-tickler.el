;;; org-gtd-tickler.el --- Define tickler items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Tickler items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-reactivate)
(require 'org-gtd-organize-core)
(require 'org-gtd-create)

(declare-function org-gtd-project-incubate "org-gtd-projects")

;;;; Commands

(defun org-gtd-tickler (&optional reminder-date)
  "DWIM: tickler the heading at point.

Dispatches to the project organize-fn when on a project heading or
project task; otherwise processes as a plain tickler item.
REMINDER-DATE is an optional YYYY-MM-DD string for non-interactive
use."
  (interactive)
  (let ((config (when reminder-date
                  `((:when . ,(format "<%s>" reminder-date))))))
    (org-gtd--dispatch 'tickler config)))

;;;; Functions

;;;;; Public

(defun org-gtd-tickler-create (topic reminder-date)
  "Automatically create a tickler task in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again.

Obsolete: use `org-gtd-create-item' instead.

\(fn TOPIC REMINDER-DATE)"
  (declare (obsolete org-gtd-create-item "4.1.0"))
  (org-gtd-create-item 'tickler topic
                       `((:when . ,(format "<%s>" reminder-date)))))

;;;;; Private

(defun org-gtd-tickler--organize (type config)
  "Configure heading at point as TYPE (tickler).
Saves current state, configures properties from CONFIG, then clears
the TODO keyword since tickler items are not actionable."
  (org-gtd-save-state)
  (org-gtd-configure-as-type type config)
  (org-todo ""))

(defun org-gtd-tickler--organize-project (pom config)
  "Tickler the project at POM.
CONFIG is an alist; when it contains a :when entry its value (a
timestamp string like \"<2026-05-01>\") is parsed to a YYYY-MM-DD
review date.  Otherwise the user is prompted."
  (require 'org-gtd-projects)
  (let* ((when-val (cdr (assq :when config)))
         (review-date
          (cond
           ((and when-val (string-match "<\\([^>]+\\)>" when-val))
            (match-string 1 when-val))
           (when-val when-val)
           (t (org-read-date nil nil nil "Review date: ")))))
    (org-gtd-project-incubate pom review-date)))

;;;; Backward Compatibility Aliases

;; Incubate → Tickler rename (v4.0)
;;;###autoload
(define-obsolete-function-alias 'org-gtd-incubate
  #'org-gtd-tickler "4.0")

;;;###autoload
(with-suppressed-warnings ((obsolete org-gtd-tickler-create))
  (define-obsolete-function-alias 'org-gtd-incubate-create
    #'org-gtd-tickler-create "4.0"))

;;;; Footer

(provide 'org-gtd-tickler)

;;; org-gtd-tickler.el ends here
