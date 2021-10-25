;; -*- lexical-binding: t; -*-
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
;; File management for org-gtd.
;;
;;; Code:

(defconst org-gtd-actionable-file-basename "actionable"
  "Name of Org file listing all actionable items.")

(defconst org-gtd-inbox-file-basename "inbox"
  "Name of Org file listing all captured items.")

(defconst org-gtd-incubate-file-basename "incubate"
  "Name of Org file listing all someday/maybe items.")

(defconst org-gtd-file-header
    "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)")

(defconst org-gtd-projects-template
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)
* Projects
:PROPERTIES:
:TRIGGER: next-sibling todo!(NEXT)
:ORG_GTD: Projects
:END:
")

(defconst org-gtd-scheduled-template
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)
* Scheduled
:PROPERTIES:
:ORG_GTD: Scheduled
:END:
")

(defconst org-gtd-actionable-template
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)

* Actions
:PROPERTIES:
:ORG_GTD: Action
:END:

* Delegated
:PROPERTIES:
:ORG_GTD: Delegated
:END:

* Scheduled
:PROPERTIES:
:ORG_GTD: Scheduled
:END:

* Projects
:PROPERTIES:
:TRIGGER: next-sibling todo!(NEXT)
:ORG_GTD: Projects
:END:
"
  "Template for the GTD actionable list.")

(defconst org-gtd-inbox-template
  "#+STARTUP: overview hidestars logrefile indent logdone
#+TODO: NEXT TODO WAIT | DONE CNCL TRASH
#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
  "Template for the GTD inbox.")

(defconst org-gtd-incubate-template
  "#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
It's suggested that you categorize the items in here somehow, such as:
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment

* Auto-generated incubate headline
"
  "Template for the GTD someday/maybe list.")

(defun org-gtd-find-or-create-and-save-files ()
  "Call this function to bootstrap the files used by org-gtd."
  (interactive)
  (mapcar
   (lambda (buffer) (with-current-buffer buffer (save-buffer) buffer))
   `(,(org-gtd--actionable-file) ,(org-gtd--incubate-file) ,(org-gtd--inbox-file))))

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd-inbox-path ()
  "Return the full path to the inbox file."
  (org-gtd--path org-gtd-inbox-file-basename))

(defun org-gtd--inbox-file ()
  "Create or return the buffer to the GTD inbox file."
  (org-gtd--gtd-file-buffer org-gtd-inbox-file-basename))

(defun org-gtd--actionable-file ()
  "Create or return the buffer to the GTD actionable file."
  (org-gtd--gtd-file-buffer org-gtd-actionable-file-basename))

(defun org-gtd--actionable-archive ()
  "Create or return the buffer to the archive file for the actionable items."
  (let* ((filename (string-join `(,(buffer-file-name (org-gtd--actionable-file)) "archive") "_"))
        (archive-file (f-join org-gtd-directory filename)))
    (find-file archive-file)))

(defun org-gtd--incubate-file ()
  "Create or return the buffer to the GTD incubate file."
  (org-gtd--gtd-file-buffer org-gtd-incubate-file-basename))

(defun org-gtd--gtd-file-buffer (gtd-type)
  "Return a buffer to GTD-TYPE.org.
Create the file and template first if it doesn't already exist."
  (let* ((file-path (org-gtd--path gtd-type))
         (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
        (with-current-buffer file-buffer
          (org-mode)
          (insert (symbol-value
                   (intern
                    (string-join
                     `("org-gtd-" ,gtd-type "-template")))))
          (org-mode-restart)
          (save-buffer)))
    file-buffer))

(provide 'org-gtd-files)
