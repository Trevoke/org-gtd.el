;;; org-gtd-mode.el --- global minor mode for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

;;;; Requirements

(require 'org-agenda)
(require 'org-edna)

(require 'org-gtd-core)

;;;; Constants

(defconst org-gtd--agenda-functions (apropos-internal "org-agenda" #'commandp)
  "List of commands available to the user through `org-agenda'.
Org-gtd wraps these functions with its own context when command `org-gtd-mode'
is enabled.")

;;;; Variables

(defvar org-gtd-edna nil "Private.")
(defvar org-gtd-edna-inheritance nil "Private.")

;;;; Macros

;;;###autoload
(define-minor-mode org-gtd-mode
  "Global minor mode to bound `org-agenda' to the org-gtd settings."
  :lighter " GTD"
  :global t
  :group 'org-gtd
  (if org-gtd-mode
      (org-gtd--enable-org-gtd-mode)
    (org-gtd--disable-org-gtd-mode)))

;;;; Functions

;;;;; Private

(defun org-gtd--disable-org-gtd-mode ()
  "Private function.

`org-gtd-mode' uses this to restore the overridden settings to their
previous values."
  (mapc
   (lambda (x) (advice-remove x #'org-gtd--wrap))
   org-gtd--agenda-functions)
  (setq org-edna-use-inheritance org-gtd-edna-inheritance)
  (org-edna-mode org-gtd-edna))

(defun org-gtd--enable-org-gtd-mode ()
  "Private function.

`org-gtd-mode' uses this to override a number of settings in Emacs.
Disabling the mode reverts the settings to their previous values.
It should be safe to turn this on if you do not have extensive `org-mode'
configuration."
  (mapc
   (lambda (x) (advice-add x :around #'org-gtd--wrap))
   org-gtd--agenda-functions)
  (setq org-gtd-edna-inheritance org-edna-use-inheritance
        org-gtd-edna org-edna-mode)
  (setq org-edna-use-inheritance 1)
  (org-edna-mode 1))

(defun org-gtd--wrap (fun &rest r)
  "Private function.

Programmatic wrapper to add org-gtd context to any FUN using `defadvice'.
Argument R is there to be passed through."
  (with-org-gtd-context (apply fun r)))

;;;; Footer

(provide 'org-gtd-mode)

;;; org-gtd-mode.el ends here
