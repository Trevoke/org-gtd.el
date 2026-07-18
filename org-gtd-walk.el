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

;;;; Checkpoint persistence

(defun org-gtd-walk--checkpoint-path (name scope)
  "Return the checkpoint file path for walk NAME over SCOPE.
Keyed by NAME and SCOPE so distinct resumable sessions never collide
\(design §5)."
  (expand-file-name
   (format "walk-%s-%s.eld" name (md5 (org-gtd-walk--scope-key scope)))
   org-gtd-directory))

(defun org-gtd-walk--save-checkpoint (path model)
  "Write MODEL to PATH as a serialized walk model."
  (with-temp-file path
    (insert (org-gtd-walk-model-serialize model))))

(defun org-gtd-walk--load-checkpoint (path)
  "Return the model stored at PATH, or nil if absent/unreadable/corrupt."
  (when (file-exists-p path)
    (org-gtd-walk-model-deserialize
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

(defun org-gtd-walk--delete-checkpoint (path)
  "Delete the checkpoint file at PATH if it exists."
  (when (file-exists-p path) (delete-file path)))

;;;; Session driver

(defvar-local org-gtd-walk--active nil
  "Buffer-local active-walk bundle on the surface buffer.
Plist: :model :spec :surface :checkpoint-path :skipped.  Nil when no
walk is active in this buffer.")

(defun org-gtd-walk--surface-buffer (surface)
  "Return the buffer of SURFACE.
SURFACE is a buffer, or a plist carrying :buffer (region support is
carried in SURFACE and passed to :render untouched)."
  (if (bufferp surface) surface (plist-get surface :buffer)))

(defun org-gtd-walk--render-current ()
  "Call the spec's :render with the current handle and surface."
  (let* ((spec (plist-get org-gtd-walk--active :spec))
         (model (plist-get org-gtd-walk--active :model)))
    (funcall (plist-get spec :render)
             (org-gtd-walk-model-current model)
             (plist-get org-gtd-walk--active :surface))))

(defun org-gtd-walk--checkpoint ()
  "Persist the current model if the walk is resumable."
  (let ((path (plist-get org-gtd-walk--active :checkpoint-path)))
    (when path
      (org-gtd-walk--save-checkpoint path (plist-get org-gtd-walk--active :model)))))

(defun org-gtd-walk--settle ()
  "Skip stale handles, then render+checkpoint, or finish if exhausted.
Runs in the surface buffer.  With a :resolve fn, auto-advances past
handles that no longer resolve, counting skips (design §9)."
  (let ((resolve (plist-get (plist-get org-gtd-walk--active :spec) :resolve)))
    (when resolve
      (while (and (not (org-gtd-walk-model-done-p
                        (plist-get org-gtd-walk--active :model)))
                  (not (funcall resolve
                                (org-gtd-walk-model-current
                                 (plist-get org-gtd-walk--active :model)))))
        (setf (plist-get org-gtd-walk--active :model)
              (org-gtd-walk-model-advance (plist-get org-gtd-walk--active :model)))
        (setf (plist-get org-gtd-walk--active :skipped)
              (1+ (plist-get org-gtd-walk--active :skipped))))))
  (if (org-gtd-walk-model-done-p (plist-get org-gtd-walk--active :model))
      (org-gtd-walk-finish)
    (org-gtd-walk--render-current)
    (org-gtd-walk--checkpoint)))

(defun org-gtd-walk-start (spec surface)
  "Start a walk described by SPEC, rendering into SURFACE.
Refuses if SPEC's scope is already locked.  Loads a checkpoint when
:resumable and one is valid, else runs :find fresh.  An empty find
finishes immediately without activating (design §6, §9)."
  (let ((scope (plist-get spec :scope)))
    (when (org-gtd-walk--scope-locked-p scope)
      (error "A walk is already active over scope %s" scope))
    (let* ((name (plist-get spec :name))
           (path (and (plist-get spec :resumable)
                      (org-gtd-walk--checkpoint-path name scope)))
           (model (or (and path (org-gtd-walk--load-checkpoint path))
                      (org-gtd-walk-model-create (funcall (plist-get spec :find)))))
           (buffer (org-gtd-walk--surface-buffer surface)))
      (if (org-gtd-walk-model-done-p model)
          (progn
            (when path (org-gtd-walk--delete-checkpoint path))
            (when (plist-get spec :on-finish)
              (funcall (plist-get spec :on-finish)))
            nil)
        (org-gtd-walk--lock-scope scope)
        (with-current-buffer buffer
          (setq org-gtd-walk--active
                (list :model model :spec spec :surface surface
                      :checkpoint-path path :skipped 0))
          (org-gtd-walk--settle))))))

(defun org-gtd-walk-advance ()
  "Advance the active walk to the next item and re-render (design §6).
Finishes when the walk runs off the end.  Runs in the surface buffer."
  (setf (plist-get org-gtd-walk--active :model)
        (org-gtd-walk-model-advance (plist-get org-gtd-walk--active :model)))
  (org-gtd-walk--settle))

(defun org-gtd-walk-finish ()
  "Finish the active walk: delete checkpoint, unlock, run :on-finish.
Runs in the surface buffer (design §9)."
  (let* ((spec (plist-get org-gtd-walk--active :spec))
         (path (plist-get org-gtd-walk--active :checkpoint-path)))
    (when path (org-gtd-walk--delete-checkpoint path))
    (org-gtd-walk--unlock-scope (plist-get spec :scope))
    (setq org-gtd-walk--active nil)
    (when (plist-get spec :on-finish)
      (funcall (plist-get spec :on-finish)))))

;;;; Footer

(provide 'org-gtd-walk)

;;; org-gtd-walk.el ends here
