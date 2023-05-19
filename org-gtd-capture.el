;;; org-gtd-capture.el --- capturing items to the inbox -*- lexical-binding: t; coding: utf-8 -*-
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
;; capturing items to the inbox for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'org-capture)

(require 'org-gtd-files)

;;;; Customization

(defgroup org-gtd-capture nil
  "Manage the functions for organizing the GTD actions."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-capture-templates
  `(("i" "Inbox"
     entry  (file ,#'org-gtd-inbox-path)
     "* %?\n%U\n\n  %i"
     :kill-buffer t)
    ("l" "Inbox with link"
     entry (file ,#'org-gtd-inbox-path)
     "* %?\n%U\n\n  %i\n  %a"
     :kill-buffer t))
  "Capture templates to be used when adding something to the inbox.

See `org-capture-templates' for the format of each capture template.
Make the sure the template string starts with a single asterisk to denote a
top level heading, or the behavior of org-gtd will be undefined."
  :group 'org-gtd-capture
  :package-version '(org-gtd . "2.0.0")
  :type 'sexp)

;;;; Constants

(defconst org-gtd-inbox "inbox")

(defconst org-gtd-inbox-template
  "#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
  "Template for the GTD inbox.")

;;;; Macros

(defmacro with-org-gtd-capture (&rest body)
  "Wrap BODY... with let-bound `org-gtd' variables for capture purposes."
  (declare (debug t) (indent 2))
  `(let ((org-capture-templates org-gtd-capture-templates))
     (unwind-protect
         (progn ,@body))))

;;;; Commands

;;;###autoload
(defun org-gtd-capture (&optional goto keys)
  "Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.
For GOTO and KEYS, see `org-capture' documentation for the variables of the
same name."
  (interactive)
  (with-org-gtd-capture
   (org-capture goto keys)))

;;;; Functions

;;;; Public

;;;###autoload
(defun org-gtd-inbox-path ()
  "Return the full path to the inbox file."
  (let ((path (org-gtd--path org-gtd-inbox)))
    (org-gtd--ensure-file-exists path org-gtd-inbox-template)
    path))

;;;; Footer

(provide 'org-gtd-capture)

;;; org-gtd-capture.el ends here
