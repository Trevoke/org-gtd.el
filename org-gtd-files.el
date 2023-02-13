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

(defconst org-gtd-inbox-template
  "#+STARTUP: overview hidestars logrefile indent logdone
#+TODO: NEXT TODO WAIT | DONE CNCL TRASH
#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
  "Template for the GTD inbox.")

(defconst org-gtd-file-header
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)
")


(defconst org-gtd-default-file-name "org-gtd-tasks")

;;;###autoload
(defun org-gtd-inbox-path ()
  "Return the full path to the inbox file."
  (org-gtd--path org-gtd-inbox))

(defun org-gtd--inbox-file ()
  "Create or return the buffer to the GTD inbox file."
  (find-file-noselect (org-gtd--ensure-inbox)))

(defun org-gtd--ensure-inbox ()
  "Ensure GTD inbox file exists, and return its full path."
  (org-gtd--ensure-path (org-gtd--path org-gtd-inbox)
                        org-gtd-inbox-template))

(defun org-gtd--default-file ()
  "Create or return the buffer to the default GTD file."
  (find-file-noselect (org-gtd--ensure-default)))

(defun org-gtd--ensure-default ()
  "Ensure default GTD file exists, and return its path."
  (org-gtd--ensure-path (org-gtd--path org-gtd-default-file-name)
                        org-gtd-file-header))

(defun org-gtd--ensure-path (path initial-contents)
  "Return PATH, creating the file with INITIAL-CONTENTS if necessary."
  (unless (f-exists-p path)
    (with-current-buffer (find-file-noselect path)
      (insert initial-contents)
      (org-mode-restart)
      (basic-save-buffer)))
  path)

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

(provide 'org-gtd-files)
;;; org-gtd-files.el ends here
