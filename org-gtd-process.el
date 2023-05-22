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

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-capture)
(require 'org-gtd-engage)
(require 'org-gtd-projects)
(require 'org-gtd-refile)
(require 'org-gtd-clarify)

;;;; Commands

;;;###autoload
(defun org-gtd-process-inbox ()
  "Start the inbox processing item, one heading at a time."
  (interactive)
  (let ((buffer (find-file-noselect (org-gtd-inbox-path))))
    (set-buffer buffer)
    (condition-case _err
        (progn
          (goto-char (point-min))
          (when (org-before-first-heading-p)
            (outline-next-visible-heading 1))
          (org-N-empty-lines-before-current 1)
          (org-gtd-clarify-inbox-item))
      (user-error (org-gtd-process--stop)))))

;;;; Functions

;;;;; Private

(defun org-gtd-process--stop ()
  "Stop processing the inbox."
  (whitespace-cleanup))

;;;; Footer

(provide 'org-gtd-process)

;;; org-gtd-process.el ends here
