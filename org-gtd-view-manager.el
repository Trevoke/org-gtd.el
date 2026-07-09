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
    ;; Lazily create the store by writing the header DIRECTLY.  Do NOT use
    ;; `org-gtd--ensure-file-exists' here: it runs `org-gtd-core-prepare-buffer'
    ;; -> `org-mode-restart', which is for .org files, not a .eld store.
    (unless (f-exists-p path)
      (f-mkdir-full-path (f-dirname path))
      (f-write-text org-gtd-view-manager--store-header 'utf-8 path))
    (condition-case err
        (let ((text (f-read-text path)))
          ;; The store holds the alist as one top-level form; the reader skips
          ;; the leading header-comment lines.  A store with only the header
          ;; (no form yet) reads as empty.
          (if (string-blank-p text) nil
            (car (read-from-string text))))
      ;; A header-only store (no form yet) is the normal empty state.
      (end-of-file nil)
      (error (message "org-gtd: could not read views store: %s"
                      (error-message-string err))
             nil))))

(defun org-gtd-view-manager--store-write (views)
  "Persist VIEWS (a name -> spec alist) to the store, header preserved."
  (let ((path (org-gtd-view-manager--store-path)))
    (with-temp-file path
      (insert org-gtd-view-manager--store-header)
      (insert (prin1-to-string views))
      (insert "\n"))))

;;;; Footer

(provide 'org-gtd-view-manager)

;;; org-gtd-view-manager.el ends here
