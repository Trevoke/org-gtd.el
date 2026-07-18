;;; org-gtd-inbox-walk.el --- Inbox walk adapter -*- lexical-binding: t; coding: utf-8 -*-
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
;; The inbox walk consumer adapter (see
;; docs/plans/2026-07-17-walk-engine-phase-4-plan.md Tasks 1-3).
;;
;; This module is additive: it implements `:find'/`:render' for a future
;; `inbox' walk spec without wiring into `org-gtd-walk-start' or the live
;; `org-gtd-process-inbox' entry point.  That wiring is a later task; the
;; old inbox/process/clarify code stays fully intact and untouched.
;;
;; Handle representation (D2, ruled): every inbox walk handle is a
;; synthetic string token, never persisted (resume is deferred --
;; `:resumable nil').  A model's `:meta' is an alist of (TOKEN . VALUE)
;; pairs where VALUE is either:
;;   - a live marker, for an original inbox heading (`:find' time), or
;;   - a (:title TITLE :content CONTENT) plist, for a duplicate (D4a,
;;     created at enqueue time -- a later task).
;; `:render' resolves a token through this meta table and dispatches on
;; which shape it finds.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-core)
(require 'org-gtd-walk-model)
(require 'org-gtd-capture)

;; `org-gtd-additional-inbox-files' is defined in org-gtd-process.el.
;; Declared (not required) to avoid a require cycle: a later task rewrites
;; org-gtd-process.el to require this module.
(defvar org-gtd-additional-inbox-files)

;;;; Functions

;;;;; Token minting

(defun org-gtd-inbox-walk--token ()
  "Return a fresh, unique synthetic string token for an inbox walk entry.
Tokens are never serialized (resume is deferred, D5b) -- any unique
string works; it exists only to key the walk model's `:meta' table."
  (format "inbox-%s" (org-id-uuid)))

;;;;; Multi-source scan (D6a, D2)

(defun org-gtd-inbox-walk--file-list ()
  "Return the multi-source inbox file list.
The main inbox first, then `org-gtd-additional-inbox-files' in listed
order (D6a file ordering)."
  (cons (org-gtd-inbox-path) org-gtd-additional-inbox-files))

(defun org-gtd-inbox-walk--scan ()
  "Scan the multi-source inbox and return (TOKENS . META).
TOKENS is the list of synthetic string tokens in scan order: every
heading in the main inbox, then every heading in each file of
`org-gtd-additional-inbox-files' in listed order.  META is an alist of
\(TOKEN . MARKER) -- the live marker of each token's source heading.

Missing or empty files are skipped.  Keeps the source files' buffers
open (`find-file-noselect') so the markers returned stay live for the
session.  Does NOT assign any org-id -- ids stay lazily assigned at
`:render' (D2), so a skipped/never-reached item is never stamped."
  (let (tokens meta)
    (dolist (file (org-gtd-inbox-walk--file-list))
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (let ((token (org-gtd-inbox-walk--token)))
               (push token tokens)
               (push (cons token (point-marker)) meta)))))))
    (cons (nreverse tokens) (nreverse meta))))

(defun org-gtd-inbox-walk--build-model ()
  "Return a fresh walk model built from `org-gtd-inbox-walk--scan'.
Combines the scanned tokens (the model's entries) with the scanned
token->marker meta into a single model, ready to drive an inbox walk.
\(A later task wires this into `org-gtd-walk-start'.)"
  (let ((scanned (org-gtd-inbox-walk--scan)))
    (org-gtd-walk-model-create (car scanned) (cdr scanned))))

;;;; Footer

(provide 'org-gtd-inbox-walk)

;;; org-gtd-inbox-walk.el ends here
