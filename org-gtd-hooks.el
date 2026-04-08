;;; org-gtd-hooks.el --- Six-stage hook infrastructure for org-gtd -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025, 2026 Aldric Giacomoni

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
;; Six-stage hook infrastructure for the unified type+hook model.
;; See docs/plans/2026-04-07-unified-type-and-hook-model-design.md for
;; the full design rationale.
;;
;; Each of the six stages (before/after × clarify/organize/file) exists
;; as a global defvar hook and optionally as a per-type local hook
;; declared via :hooks in the type plist.  `org-gtd-hooks-run' invokes
;; global hooks first, then local hooks, for a given stage.  Hooks
;; observe — they cannot gate the pipeline.  Errors are caught and
;; logged so a buggy hook cannot break organization.

;;; Code:

(require 'org-gtd-types)

;;;; Global hook variables

(defvar org-gtd-before-clarify-hook nil
  "Functions run before the clarify buffer is entered.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

(defvar org-gtd-after-clarify-hook nil
  "Functions run after the clarify buffer is committed.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

(defvar org-gtd-before-organize-hook nil
  "Functions run immediately before a type's :organize-fn is invoked.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

(defvar org-gtd-after-organize-hook nil
  "Functions run immediately after a type's :organize-fn returns.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

(defvar org-gtd-before-file-hook nil
  "Functions run before the heading is refiled or updated in place.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

(defvar org-gtd-after-file-hook nil
  "Functions run after the heading has been refiled or updated in place.
Each function is called with one argument, a point-or-marker locating
the heading being organized.  Return value is ignored.")

;;;; Internal

(defconst org-gtd-hooks--stage-alist
  '((:before-clarify  . org-gtd-before-clarify-hook)
    (:after-clarify   . org-gtd-after-clarify-hook)
    (:before-organize . org-gtd-before-organize-hook)
    (:after-organize  . org-gtd-after-organize-hook)
    (:before-file     . org-gtd-before-file-hook)
    (:after-file      . org-gtd-after-file-hook))
  "Maps stage keywords to their global hook variables.")

(defun org-gtd-hooks--global-var (stage)
  "Return the global hook variable symbol for STAGE.
Signals an error when STAGE is not one of the six documented stages."
  (or (cdr (assq stage org-gtd-hooks--stage-alist))
      (error "Unknown org-gtd hook stage: %s" stage)))

(defun org-gtd-hooks--call-safely (fn pom)
  "Call FN with POM, catching and logging any error."
  (condition-case err
      (funcall fn pom)
    (error
     (message "org-gtd hook %S errored: %s"
              fn (error-message-string err)))))

;;;; Entry point

(defun org-gtd-hooks-run (stage type pom)
  "Run global then local hooks for STAGE and TYPE with POM.
STAGE is a keyword — one of `:before-clarify', `:after-clarify',
`:before-organize', `:after-organize', `:before-file', `:after-file'.
TYPE is a registered org-gtd type symbol (or nil/unknown, in which
case only global hooks fire).  POM is the point-or-marker of the
heading being organized and is forwarded unchanged to each hook.

Hooks observe; they cannot gate the pipeline.  Errors are caught
and logged so a buggy hook cannot break organization."
  (let ((global (symbol-value (org-gtd-hooks--global-var stage)))
        (local (plist-get (org-gtd-type-hooks type) stage)))
    (dolist (fn global)
      (org-gtd-hooks--call-safely fn pom))
    (dolist (fn local)
      (org-gtd-hooks--call-safely fn pom))))

(provide 'org-gtd-hooks)

;;; org-gtd-hooks.el ends here
