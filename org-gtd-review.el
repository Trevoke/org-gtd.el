;;; org-gtd-review.el --- GTD review logic for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Reviews are a crucial part of GTD. This code determines how to use
;; the agenda views for review purposes.
;;
;;; Code:

(require 'org)
(require 'org-gtd-core)
(require 'org-gtd-areas-of-focus)
(require 'org-gtd-agenda)

(define-error
 'org-gtd-invalid-area-of-focus
 "`%s' is not a member of `%s'"
 'org-gtd-error)

;;;###autoload
(defun org-gtd-review-area-of-focus (&optional area)
  "Generate an overview agenda for a given area of focus."
  (interactive (list (completing-read
                      "Which area of focus would you like to review? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
  (when (not (member area org-gtd-areas-of-focus))
    (signal 'org-gtd-invalid-area-of-focus `(,area ,org-gtd-areas-of-focus)))
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             `(("a" ,(format "Area of Focus: %s" area)
                ((tags "+LEVEL=2+ORG_GTD=\"Projects\""
                       ((org-agenda-overriding-header "Ongoing projects")))
                 (tags ,(format "+TODO=\"%s\"" org-gtd-next)
                       ((org-agenda-overriding-header "Ready actions")))
                 (agenda ""
                         ((org-agenda-overriding-header "Habits")
                          (org-agenda-skip-function '(org-gtd--AND-skips '(org-gtd--skip-unless-habit
                                                                           ,(org-gtd--skip-unless-area-of-focus-func area))))
                          (org-agenda-span 1)))
                 (tags "+ORG_GTD=\"Incubated\""
                       ((org-agenda-overriding-header "Incubated items"))))
                ((org-agenda-skip-function ,(org-gtd--skip-unless-area-of-focus-func area)))))))
        (org-agenda nil "a")
        (goto-char (point-min)))))

(defun org-gtd--AND-skips (funcs)
  "Ensure none of the functions want to skip the current entry"
  (let ((non-nil-funcs (seq--drop-while-list (lambda (x) (not (funcall x))) funcs)))
    (if non-nil-funcs
        (funcall (car non-nil-funcs)))))

(defun org-gtd--skip-unless-habit ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string-equal "habit" (org-entry-get (point) "STYLE"))
        nil
      subtree-end)))

(defun org-gtd--skip-unless-area-of-focus-func (area)
  `(lambda ()
     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
       (if (string-equal (downcase ,area)
                         (downcase (org-entry-get (point) "CATEGORY")))
           nil
         subtree-end))))

(provide 'org-gtd-review)
;;; org-gtd-review.el ends here
