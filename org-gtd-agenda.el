;;; org-gtd-agenda.el --- Manage the agenda view -*- lexical-binding: t; coding: utf-8 -*-
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
;; Agenda management for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'winner)
(require 'org-agenda)

(require 'org-gtd-core)
(require 'org-gtd-backward-compatibility)

(defgroup org-gtd-engage nil
  "Customize the engage views in the org-gtd package."
  :group 'org-gtd
  :package-version '(org-gtd . "3.1"))

(defcustom org-gtd-engage-prefix-width 12
  "How many characters to dedicate to the agenda prefix in the engage view.

This is where the project name is displayed, on the left side."
  :group 'org-gtd-engage
  :package-version '(org-gtd . "3.1")
  :type 'integer)

;;;; Commands

;;;###autoload
(defun org-gtd-engage ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (let* ((project-format-prefix
              (format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
                      org-gtd-engage-prefix-width))
             (org-agenda-custom-commands
             `(("g" "Scheduled today and all NEXT items"
                ((agenda ""
                         ((org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t)))
                 (todo org-gtd-next
                       ((org-agenda-overriding-header "All actions ready to be executed.")
                        (org-agenda-prefix-format
                         '((todo . ,project-format-prefix)))))
                 (todo org-gtd-wait
                       ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-overriding-header "Delegated/Blocked items")
                        (org-agenda-prefix-format
                         '((todo . ,project-format-prefix))))))))))
        (org-agenda nil "g")
        (goto-char (point-min)))))

;;;###autoload
(defun org-gtd-engage-grouped-by-context ()
  "Show all `org-gtd-next' actions grouped by context (tag prefixed with @)."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (let* ((contexts (seq-map
                        (lambda (x) (substring-no-properties x))
                        (seq-uniq
                         (flatten-list
                          (org-map-entries
                           (lambda () org-scanner-tags)
                           (format "{^@}+TODO=\"%s\"" org-gtd-next)
                           'agenda)))))
             (blocks (seq-map
                      (lambda (x) `(tags ,(format "+%s+TODO=\"%s\"" x org-gtd-next)
                                         ((org-agenda-overriding-header ,x))))
                      contexts))
             (org-agenda-custom-commands `(("g" "actions by context" ,blocks))))
        (org-agenda nil "g"))))

;;;###autoload
(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (org-todo-list org-gtd-next)))

;;;; Functions

;;;;; Private

(defun org-gtd--truncate-project-to-width (st)
  "Truncates the string to the width indicated by org-gtd-engage-prefix-width."
  (truncate-string-to-width (string-trim st) org-gtd-engage-prefix-width nil ?\s  "…")
  )

(defun org-gtd-agenda--prefix-format ()
  "Format prefix for items in agenda buffer."
  (let* ((elt (org-element-at-point))
         (level (org-element-property :level elt))
         (category (org-entry-get (point) "CATEGORY" t))
         (parent-title (org-element-property
                        :raw-value
                        (org-element-property :parent elt)))
         (tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))
    (org-gtd--truncate-project-to-width
     ;; if level 3, use the parent
     ;; otherwise if it has category, use category
     ;; else "no project" so we avoid failing
     (cond
      ((eq level 3) (replace-regexp-in-string tally-cookie-regexp "" parent-title))
      (category     category)
      (t  "no project")
      ))))

;;;; Footer

(provide 'org-gtd-agenda)

;;; org-gtd-agenda.el ends here
