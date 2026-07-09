;;; org-gtd-view-manager.el --- Interactive manager for saved GTD views -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

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
;; Interactive management layer for custom org-gtd views, built strictly on
;; top of the released view DSL (see `org-gtd-view-language.el').  Provides a
;; plain-file `name -> spec' store (`views.eld' in `org-gtd-directory'), a
;; builder transient with live org-agenda preview, a list transient to
;; browse/act on saved views, and a `completing-read' recall command.
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'f)
(require 'subr-x)
(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-view-language)

;;;; Store

(defconst org-gtd-view-manager--store-file-name "views.eld"
  "Base name of the saved-views store inside `org-gtd-directory'.")

(defconst org-gtd-view-manager--store-header
  ";; Managed by org-gtd's View Manager (M-x org-gtd-view-manager).
;; A name -> spec alist of saved GTD views.  Edit via the manager, not by hand.
"
  "Guidance comment written to the top of a freshly created store.")

(defun org-gtd-view-manager--store-path ()
  "Return the absolute path to the views store."
  (f-join org-gtd-directory org-gtd-view-manager--store-file-name))

(defun org-gtd-view-manager--store-read ()
  "Return the saved-views alist, creating an empty store if absent.
Returns nil when the store is empty.  Fail-soft: a corrupt store
yields nil with a `message', never an error."
  (let ((path (org-gtd-view-manager--store-path)))
    ;; Lazily create the store with the header AND an explicit empty form.  Do
    ;; NOT use `org-gtd--ensure-file-exists' here: it runs
    ;; `org-gtd-core-prepare-buffer' -> `org-mode-restart', which is for .org
    ;; files, not a .eld store.  Writing a real `()' form (rather than
    ;; header-only) means the empty state reads back through a genuine form, so
    ;; a truncated/corrupt store can be distinguished from a fresh empty one.
    (unless (f-exists-p path)
      (org-gtd-view-manager--store-write nil))
    (condition-case err
        (let ((text (f-read-text path)))
          ;; The store holds the alist as one top-level form; the reader skips
          ;; the leading header-comment lines.  A fresh store reads back as
          ;; `()' -> nil.
          (if (string-blank-p text) nil
            (car (read-from-string text))))
      ;; `end-of-file' here means a truncated/partial write, not an empty
      ;; store (the empty store is a complete `()' form) -- report it.
      (error (message "org-gtd: could not read views store: %s"
                      (error-message-string err))
             nil))))

(defun org-gtd-view-manager--store-write (views)
  "Persist VIEWS (a name -> spec alist) to the store, header preserved.
An empty store is written as a genuine `()' form (never header-only) so
that reads can tell an empty store from a truncated one."
  (let ((path (org-gtd-view-manager--store-path))
        ;; Never let a user's `print-length'/`print-level' truncate a spec
        ;; with `...' and produce an unreadable store.
        (print-length nil)
        (print-level nil)
        (print-circle t)
        (coding-system-for-write 'utf-8))
    (make-directory (f-dirname path) t)
    (with-temp-file path
      (insert org-gtd-view-manager--store-header)
      (insert (prin1-to-string views))
      (insert "\n"))))

;;;; Footer

(provide 'org-gtd-view-manager)

;;; org-gtd-view-manager.el ends here
