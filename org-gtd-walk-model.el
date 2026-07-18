;;; org-gtd-walk-model.el --- Pure headless walk model -*- lexical-binding: t; coding: utf-8 -*-
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
;; The pure, headless walk model: a serializable plist and pure functions
;; over it.  No buffers, no org, no I/O.  This is the Tier 1 core of the
;; walk engine (see docs/plans/2026-07-17-walk-engine-design.md §4).
;;
;; A model is a plist: (:entries LIST :cursor INT :meta LIST).
;; The cursor is the index of the current item; entries before it are
;; handled, entries after it are pending, cursor == (length entries) is done.
;;
;;; Code:

(require 'seq)

;;;; Construction

(defun org-gtd-walk-model-create (entries &optional meta)
  "Return a fresh walk model over ENTRIES with optional META plist.
ENTRIES is copied; the cursor starts at 0."
  (list :entries (copy-sequence entries)
        :cursor 0
        :meta meta))

;;;; Queries

(defun org-gtd-walk-model-current (model)
  "Return the handle at MODEL's cursor, or nil when the walk is done."
  (nth (plist-get model :cursor) (plist-get model :entries)))

(defun org-gtd-walk-model-done-p (model)
  "Return non-nil when MODEL's cursor has run off the end of its entries."
  (>= (plist-get model :cursor)
      (length (plist-get model :entries))))

(defun org-gtd-walk-model-remaining (model)
  "Return the count of entries at or after MODEL's cursor."
  (max 0 (- (length (plist-get model :entries))
            (plist-get model :cursor))))

;;;; Transitions (pure — take a model, return a new model)

(defun org-gtd-walk-model-advance (model)
  "Return a copy of MODEL with the cursor advanced by one.
The cursor never moves past the entry count (the done position)."
  (let ((len (length (plist-get model :entries)))
        (cursor (plist-get model :cursor)))
    (list :entries (plist-get model :entries)
          :cursor (min len (1+ cursor))
          :meta (plist-get model :meta))))

(defun org-gtd-walk-model--insert-index (model where)
  "Return the insertion index in MODEL's entries for WHERE.
Both positions insert into the remaining queue, after the cursor, so a
handle enqueued while the current item is still being processed never
displaces it: `top' (index cursor+1, handled next) or `bottom' (index
end, handled last).  Signals an error for any other value.  The index
is clamped to [0, (length entries)]."
  (let* ((cursor (plist-get model :cursor))
         (len (length (plist-get model :entries)))
         (raw (cond
               ((eq where 'top) (1+ cursor))
               ((eq where 'bottom) len)
               (t (error "Unknown enqueue position: %s" where)))))
    (max 0 (min len raw))))

(defun org-gtd-walk-model-enqueue (model handle where)
  "Return a copy of MODEL with HANDLE inserted at WHERE.
WHERE is `top' (handled next, after the current item) or `bottom'
\(handled last).  Both insert after the cursor so the current item is
never displaced (see design §4)."
  (let* ((entries (plist-get model :entries))
         (idx (org-gtd-walk-model--insert-index model where))
         (new-entries (append (seq-take entries idx)
                              (list handle)
                              (seq-drop entries idx))))
    (list :entries new-entries
          :cursor (plist-get model :cursor)
          :meta (plist-get model :meta))))

;;;; Validation

(defun org-gtd-walk-model--handle-serializable-p (handle)
  "Return non-nil when HANDLE is a `prin1'/`read'-safe walk handle.
Handles are strings, symbols, or numbers (org-ids in practice); live
markers and buffers are rejected so they can never be persisted."
  (or (stringp handle) (symbolp handle) (numberp handle)))

(defun org-gtd-walk-model-valid-p (model)
  "Return non-nil when MODEL is internally coherent and serializable.
Checks that entries is a list of serializable handles and cursor is an
integer in [0, (length entries)].  Used to reject corrupt checkpoints
(see design §8)."
  (and (listp model)
       (let ((entries (plist-get model :entries))
             (cursor (plist-get model :cursor)))
         (and (listp entries)
              (integerp cursor)
              (>= cursor 0)
              (<= cursor (length entries))
              (seq-every-p #'org-gtd-walk-model--handle-serializable-p entries)))))

;;;; Serialization

(defun org-gtd-walk-model-serialize (model)
  "Return MODEL as a `read'-able string via `prin1'."
  (let ((print-length nil) (print-level nil))
    (prin1-to-string model)))

(defun org-gtd-walk-model-deserialize (string)
  "Return the model encoded in STRING, or nil.
Returns nil when STRING is unreadable or the decoded model fails
`org-gtd-walk-model-valid-p' — the caller falls back to a fresh walk."
  (let ((model (ignore-errors (car (read-from-string string)))))
    (when (org-gtd-walk-model-valid-p model)
      model)))

;;;; Footer

(provide 'org-gtd-walk-model)

;;; org-gtd-walk-model.el ends here
