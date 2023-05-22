;;; org-gtd-engage.el --- Engage actionable tasks -*- lexical-binding: t; coding: utf-8 -*-
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

;;;; Commands

;;;###autoload
(defun org-gtd-engage ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             `(("g" "Scheduled today and all NEXT items"
                ((agenda ""
                         ((org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t)))
                 (todo org-gtd-next
                       ((org-agenda-overriding-header "All actions ready to be executed.")
                        (org-agenda-prefix-format
                         '((todo . " %i %-12:(org-gtd-agenda--prefix-format)")))))
                 (todo org-gtd-wait
                       ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-overriding-header "Delegated/Blocked items")
                        (org-agenda-prefix-format
                         '((todo . " %i %-12 (org-gtd-agenda--prefix-format)"))))))))))
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
(defun org-gtd-engage-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (org-todo-list org-gtd-next)))

;;;###autoload
(defalias 'org-gtd-show-all-next 'org-gtd-engage-next)
(make-obsolete 'org-gtd-show-all-next "use `org-gtd-engage-next' instead." "3.0.0")

;;;; Functions

;;;;; Private

(defun org-gtd-agenda--prefix-format ()
  "Format prefix for items in agenda buffer."
  (let* ((elt (org-element-at-point))
         (level (org-element-property :level elt))
         (category (org-entry-get (point) "CATEGORY" t))
         (parent-title (org-element-property
                        :raw-value
                        (org-element-property :parent elt)))
         (tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))

    (cond
     ((eq level 3)
      (concat
       (substring (string-pad
                   (replace-regexp-in-string tally-cookie-regexp "" parent-title)
                   11)
                  0 10)
       "…"))
     (category (concat (substring (string-pad category 11) 0 10) "…")))))

;;;; Footer

(provide 'org-gtd-engage)

;;; org-gtd-engage.el ends here
