;;; org-gtd-quick-action.el --- Define quick-action items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Quick action items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-archive)
(require 'org-gtd-configure)
(require 'org-gtd-organize-core)

;;;; Commands

(defun org-gtd-quick-action ()
  "DWIM: organize the heading at point as a quick action."
  (interactive)
  (if (and (boundp 'org-gtd-clarify--clarify-id) org-gtd-clarify--clarify-id)
      (org-gtd-organize--call
       (lambda () (org-gtd-process-heading (point-marker) 'quick-action nil)))
    (org-gtd--dispatch 'quick-action)))

;;;; Footer

(provide 'org-gtd-quick-action)

;;; org-gtd-quick-action.el ends here
