;;; org-gtd-files.el --- File management for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; File management for org-gtd.
;;
;;; Code:

(require 'f)
(require 'org-gtd-core)

(defconst org-gtd-inbox-template
  "#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
  "Template for the GTD inbox.")

(defconst org-gtd-default-file-name "org-gtd-tasks")

(defconst org-gtd-file-horizons-template
  "* Purpose and principles (why)
* Vision (what)
* Goals
* Areas of focus / accountabilities
")

;;;###autoload
(defun org-gtd-inbox-path ()
  "Return the full path to the inbox file."
  (let ((path (org-gtd--path org-gtd-inbox)))
    (org-gtd--ensure-file-exists path org-gtd-inbox-template)
    path))

(defun org-gtd--horizons-file ()
  (let ((path (f-join org-gtd-directory org-gtd-horizons-file)))
    (org-gtd--ensure-file-exists path org-gtd-file-horizons-template)
    (find-file-noselect path)))

(defun org-gtd--inbox-file ()
  "Create or return the buffer to the GTD inbox file."
  (find-file-noselect (org-gtd-inbox-path)))

(defun org-gtd--default-file ()
  "Create or return the buffer to the default GTD file."
  (let ((path (org-gtd--path org-gtd-default-file-name)))
    (org-gtd--ensure-file-exists path)
    (find-file-noselect path)))

(defun org-gtd--ensure-file-exists (path &optional initial-contents)
  "Create the file at PATH with INITIAL-CONTENTS if it does not exist."
  (unless (f-exists-p path)
    (with-current-buffer (find-file-noselect path)
      (insert (or initial-contents ""))
      (org-gtd-core-prepare-buffer)
      (basic-save-buffer))))

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

(provide 'org-gtd-files)
;;; org-gtd-files.el ends here
