;;; org-gtd-mode.el --- global minor mode for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2021 Aldric Giacomoni

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
;; Archiving logic for org-gtd
;;
;;; Code:

(require 'org-agenda)

;;;###autoload
(define-minor-mode org-gtd-mode
  "Global minor mode to bound `org-agenda' to the org-gtd settings."
  :lighter " GTD"
  :global t
  (if org-gtd-mode
      (org-gtd--override-agenda)
    (org-gtd--restore-agenda)))

(defun org-gtd--wrap (fun &rest r)
  "Private function.

Programmatic wrapper to add org-gtd context to any FUN using `defadvice'.
Argument R is there to be passed through."
  (with-org-gtd-context (apply fun r)))

(defconst org-gtd--agenda-functions
  (seq-filter #'commandp (mapcar #'car (apropos "org-agenda-")))
  "List of commands available to the user through `org-agenda'.

Org-gtd wraps these functions with its own context when `org-gtd-mode'
is enabled.")

(defun org-gtd--override-agenda ()
  "Private function.

`org-gtd-mode' uses this to `defadvice' all `org-agenda' commands."
  (mapc
   (lambda (x) (advice-add x :around #'org-gtd--wrap))
   org-gtd--agenda-functions))

(defun org-gtd--restore-agenda ()
  "Private function.

`org-gtd-mode' uses this to stop overriding all the `org-agenda' behavior."
  (mapc
   (lambda (x) (advice-remove x #'org-gtd--wrap))
   org-gtd--agenda-functions))

(provide 'org-gtd-mode)
;;; org-gtd-mode.el ends here
