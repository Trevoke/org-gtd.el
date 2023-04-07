;;; org-gtd-process.el --- Code to process inbox -*- lexical-binding: t; coding: utf-8 -*-
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
;; Inbox processing management for org-gtd.
;;
;;; Code:

(require 'org-gtd-core)
(require 'org-gtd-agenda)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-organize)

(defcustom org-gtd-process-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element.  The default value has
one function: setting org tags on the item.  Some built-in examples are
provided as options here.  You can create your own functions to enhance/decorate
the items once they have been processed and add them to that list."
  :group 'org-gtd
  :package-version '(org-gtd . "1.0.4")
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

;;;###autoload
(defun org-gtd-process-inbox ()
  "New stuff whodis"
  (interactive)
  (set-buffer (org-gtd--inbox-file))

  (condition-case err
      (progn
        (goto-char (point-min))
        (when (org-before-first-heading-p)
          (outline-next-visible-heading 1))
        (org-N-empty-lines-before-current 1)
        (org-gtd-clarify-inbox-item))
    (user-error (org-gtd-process--stop))))

(defun org-gtd-process--stop ()
  "Stop processing the inbox."
  (interactive)
  (whitespace-cleanup))

(provide 'org-gtd-process)
;;; org-gtd-process.el ends here
