;;; org-gtd-walk.el --- Generic item-walk session driver -*- lexical-binding: t; coding: utf-8 -*-
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
;; The generic walk session driver: the one place walk state moves
;; (transition -> render -> checkpoint).  Owns the org-gtd-walks registry,
;; consumer-spec validation, scope identity/locking, and opt-in checkpoint
;; persistence.  Consumers register a spec and start a walk against a
;; caller-provided surface.  See docs/plans/2026-07-17-walk-engine-design.md
;; §5-§9.  Phase 0 registers no real consumers.
;;
;;; Code:

(require 'cl-lib)
(require 'org-gtd-walk-model)
(require 'org-gtd-core)

;;;; Registry

(defvar org-gtd-walks nil
  "Alist of registered walks: (NAME . SPEC).
SPEC is a plist (:name :find :render :actions :on-finish :resumable
:resolve :scope).  Mirrors `org-gtd-types'.  Empty until consumers
register (none in Phase 0).")

(defun org-gtd-walk-register (name spec)
  "Register SPEC under NAME in `org-gtd-walks', replacing any existing entry."
  (setf (alist-get name org-gtd-walks) spec))

(defun org-gtd-walk-get (name)
  "Return the walk spec registered under NAME, or nil."
  (alist-get name org-gtd-walks))

;;;; Spec validation

(defun org-gtd-walk--callable-p (x)
  "Return non-nil when X can be `funcall'ed (a function or symbol with one)."
  (or (functionp x)
      (and (symbolp x) x (fboundp x))))

(defun org-gtd-walk-spec-valid-p (spec)
  "Return non-nil when SPEC is a well-formed consumer spec.
Requires a symbol :name, callable :find and :render, and a non-nil
:scope.  :actions, :on-finish and :resolve are optional but, when
present and non-nil, :on-finish and :resolve must be callable."
  (and (listp spec)
       (symbolp (plist-get spec :name))
       (plist-get spec :name)
       (org-gtd-walk--callable-p (plist-get spec :find))
       (org-gtd-walk--callable-p (plist-get spec :render))
       (plist-get spec :scope)
       (let ((on-finish (plist-get spec :on-finish))
             (resolve (plist-get spec :resolve)))
         (and (or (null on-finish) (org-gtd-walk--callable-p on-finish))
              (or (null resolve) (org-gtd-walk--callable-p resolve))))
       t))

;;;; Scope and locking

(defun org-gtd-walk--scope-key (scope)
  "Return a stable string key identifying SCOPE.
SCOPE is a string (file path or org-id) or a list of strings (a
file-set).  A list keys order-independently so the same set of files
always locks the same container."
  (if (listp scope)
      (mapconcat #'identity
                 (sort (copy-sequence scope) #'string<)
                 "|")
    (format "%s" scope)))

(defvar org-gtd-walk--locked-scopes nil
  "List of scope keys currently locked by an active walk.
The concurrency lock: no two walks may run over the same scope at once
\(design §5).  Global, not buffer-local, because the lock spans buffers.")

(defun org-gtd-walk--scope-locked-p (scope)
  "Return non-nil when SCOPE is currently locked."
  (and (member (org-gtd-walk--scope-key scope) org-gtd-walk--locked-scopes) t))

(defun org-gtd-walk--lock-scope (scope)
  "Mark SCOPE as locked."
  (cl-pushnew (org-gtd-walk--scope-key scope) org-gtd-walk--locked-scopes
              :test #'equal))

(defun org-gtd-walk--unlock-scope (scope)
  "Release the lock on SCOPE."
  (setq org-gtd-walk--locked-scopes
        (delete (org-gtd-walk--scope-key scope) org-gtd-walk--locked-scopes)))

;;;; Footer

(provide 'org-gtd-walk)

;;; org-gtd-walk.el ends here
