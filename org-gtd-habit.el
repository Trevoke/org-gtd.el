;;; org-gtd-habit.el --- Define habits in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Habits have org-mode requirements, we satisfy them here.
;;
;;; Code:

(defcustom org-gtd-habit-func
  #'org-gtd-habit--apply
  "Function called when item at point is a habit."
  :group 'org-gtd-organize
  :type 'function
  :package-version '(org-gtd . "3.0.0"))

;;;###autoload
(defun org-gtd-habit (&optional repeater)
  "Decorate and refile item at point as a calendar item."
  (interactive)
  (org-gtd-organize--call
   (apply-partially org-gtd-habit-func
                    repeater)))

(defun org-gtd-habit--apply (&optional repeater)
  "Add a repeater to this item and store in org gtd."
  (let ((repeater (or repeater
                      (read-from-minibuffer "How do you want this to repeat? ")))
        (today (format-time-string "%Y-%m-%d")))
    (org-schedule nil (format "<%s %s>" today repeater))
    (org-entry-put (point) "STYLE" "habit"))
  (org-gtd-organize-decorate-item)
  (org-gtd--refile org-gtd-calendar))

(defun org-gtd-habit-create (topic repeater)
  "Automatically create a habit in the GTD flow."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-habit repeater))
    (kill-buffer buffer)))

(provide 'org-gtd-habit)
;;; org-gtd-habit.el ends here
