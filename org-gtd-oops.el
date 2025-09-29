;;; org-gtd-oops.el --- Define view for missed events in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Life doesn't go as we expect sometimes.  Here we can find all the things
;; that for did not get updated when they should have.
;;
;; This module has been rewritten to use the declarative GTD view language
;; for better performance and maintainability.
;;
;;; Code:

;;;; Requirements
(require 'org-gtd-core)
(require 'org-gtd-view-language)

;;;; GTD View Definitions

(defconst org-gtd-oops-view-specs
  '(((name . "Missed check-ins on delegated items")
     (filters . ((category . delegated)
                 (timestamp . past))))

    ((name . "Missed appointments")
     (filters . ((category . calendar)
                 (level . 2)
                 (timestamp . past))))

    ((name . "Projects that should have finished")
     (filters . ((category . projects)
                 (level . 2)
                 (deadline . past))))

    ((name . "Projects that should have started")
     (filters . ((category . projects)
                 (level . 2)
                 (scheduled . past)
                 (not-habit . t)))))
  "GTD view specifications for oops views using the declarative language.")

;;;; Commands

;;;###autoload
(defun org-gtd-oops ()
  "Agenda view for all non-respected timely events using GTD view language."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             (org-gtd-view-lang--create-custom-commands
              org-gtd-oops-view-specs
              "o"
              "GTD Oops Views")))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;###autoload
(defun org-gtd-oops-delegated ()
  "Show only missed delegated items."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             (org-gtd-view-lang--create-custom-commands
              (list (car org-gtd-oops-view-specs)))))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;###autoload
(defun org-gtd-oops-calendar ()
  "Show only missed calendar appointments."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             (org-gtd-view-lang--create-custom-commands
              (list (cadr org-gtd-oops-view-specs)))))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;###autoload
(defun org-gtd-oops-projects ()
  "Show only overdue projects."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             (org-gtd-view-lang--create-custom-commands
              (list (caddr org-gtd-oops-view-specs)
                    (cadddr org-gtd-oops-view-specs)))))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;; Customization

(defcustom org-gtd-oops-custom-views nil
  "Additional custom oops views defined by the user.
Each view should be a GTD view specification alist with 'name and 'filters keys.

Example:
'(((name . \"My Custom View\")
   (filters . ((category . delegated)
               (area-of-focus . \"Work\")))))"
  :group 'org-gtd
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :package-version '(org-gtd . "3.1"))

;;;###autoload
(defun org-gtd-oops-with-custom ()
  "Show oops views including user-defined custom views."
  (interactive)
  (with-org-gtd-context
      (let* ((all-views (append org-gtd-oops-view-specs org-gtd-oops-custom-views))
             (org-agenda-custom-commands
              (org-gtd-view-lang--create-custom-commands all-views)))
        (org-agenda nil "o")
        (goto-char (point-min)))))

;;;; Footer

(provide 'org-gtd-oops)

;;; org-gtd-oops.el ends here