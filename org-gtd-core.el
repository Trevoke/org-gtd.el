;;; org-gtd-core.el --- Core code for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Core logic for org-gtd
;; Creating this file because straight.el seems unhappy.
;;
;;; Code:

(require 'org-agenda-property)
(require 'org-capture)

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :link '(url-link "https://github.com/Trevoke/org-gtd.el")
  :package-version '(org-gtd . "0.1")
  :group 'org)

(defcustom org-gtd-directory "~/gtd/"
  "Directory for org-gtd.

The package will use this directory for all its functionality, whether it is
building the agenda or refiling items.  This is the directory where you will
find the default org-gtd file, and it is the directory where you should place
your own files if you want multiple refile targets (projects, etc.)."
  :group 'org-gtd
  :package-version '(org-gtd . "0.1")
  :type 'directory)

(defconst org-gtd-inbox "inbox")
(defconst org-gtd-incubated "incubated")
(defconst org-gtd-projects "projects")
(defconst org-gtd-actions "actions")
(defconst org-gtd-delegated "delegated")
(defconst org-gtd-calendar "calendar")

(defconst org-gtd--properties
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Actions" myhash)
    (puthash org-gtd-incubated "Incubated" myhash)
    (puthash org-gtd-projects "Projects" myhash)
    (puthash org-gtd-calendar "Calendar" myhash)
    myhash))

(defconst org-gtd-project-headings
  "+LEVEL=2&+ORG_GTD=\"Projects\""
  "How to tell org-mode to find project headings")

(defconst org-gtd-stuck-projects
  `(,org-gtd-project-headings ("NEXT" "WAIT") nil "")
  "How to identify stuck projects in the GTD system.

This is a list of four items, the same type as in `org-stuck-projects'.")

;;;###autoload
(defmacro with-org-gtd-context (&rest body)
  "Wrap any BODY in this macro to inherit the org-gtd settings for your logic."
  (declare (debug t) (indent 2))
  `(let* ((org-use-property-inheritance "ORG_GTD")
          (org-archive-location (funcall org-gtd-archive-location))
          (org-capture-templates org-gtd-capture-templates)
          (org-refile-use-outline-path nil)
          (org-stuck-projects org-gtd-stuck-projects)
          (org-odd-levels-only nil)
          (org-agenda-files (org-gtd-core--agenda-files))
          (org-agenda-property-list '("DELEGATED_TO"))
          (org-agenda-custom-commands org-gtd-agenda-custom-commands))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd-core--agenda-files ()
  "Return the value of the `org-agenda-files' variable with `org-gtd-directory'
added to it."
  (if (stringp org-agenda-files)
      (append (org-read-agenda-file-list)
              (ensure-list org-gtd-directory))
    (append (ensure-list org-agenda-files)
            (ensure-list org-gtd-directory))))

(provide 'org-gtd-core)
;;; org-gtd-core.el ends here
