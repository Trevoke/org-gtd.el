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

;;;; Footer

(provide 'org-gtd-walk)

;;; org-gtd-walk.el ends here
