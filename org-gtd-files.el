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
  "* Scheduled
:PROPERTIES:
:ORG_GTD: Scheduled
:END:
")

(defconst org-gtd-actions-template
  "* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
")

(defconst org-gtd-incubated-template
  "#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
Add your own categories as necessary, with the ORG_GTD property, such as
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment

* Incubate
:PROPERTIES:
:ORG_GTD: Incubated
:END:
"
  "Template for the GTD someday/maybe list.")

(defconst org-gtd--file-template
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions org-gtd-actions-template myhash)
    (puthash org-gtd-scheduled org-gtd-scheduled-template myhash)
    (puthash org-gtd-projects org-gtd-projects-template myhash)
    (puthash org-gtd-incubated org-gtd-incubated-template myhash)
    myhash))

(defun org-gtd-inbox-path ()
  "Return the full path to the inbox file."
  (org-gtd--path org-gtd-inbox))

(defun org-gtd--inbox-file ()
  "Create or return the buffer to the GTD inbox file."
  (org-gtd--gtd-file-buffer org-gtd-inbox))

(defun org-gtd--default-projects-file ()
  "Create or return the buffer to the default GTD projects file."
  (org-gtd--gtd-file-buffer org-gtd-projects))

(defun org-gtd--default-action-file ()
  "Create or return the buffer to the GTD actionable file."
  (org-gtd--gtd-file-buffer org-gtd-actions))

(defun org-gtd--default-incubated-file ()
  "Create or return the buffer to the GTD incubate file."
  (org-gtd--gtd-file-buffer org-gtd-incubated))

(defun org-gtd--default-delegated-file ()
  (org-gtd--gtd-file-buffer org-gtd-actions))

(defun org-gtd--default-scheduled-file ()
  (org-gtd--gtd-file-buffer org-gtd-scheduled))

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd--gtd-file-buffer (gtd-type)
  (let ((file-path (org-gtd--path gtd-type)))
    (unless (f-file-p file-path)
      (with-current-buffer (find-file-noselect file-path)
        (if (string-equal org-gtd-inbox gtd-type)
            (insert org-gtd-inbox-template)
          (insert org-gtd-file-header)
          (insert (gethash gtd-type org-gtd--file-template)))
        (org-mode-restart)
        (save-buffer)
        (current-buffer)))
    (find-file-noselect file-path)))

(provide 'org-gtd-files)
