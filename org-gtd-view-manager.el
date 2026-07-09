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

(defun org-gtd-view-manager--store-get (name)
  "Return the stored spec for NAME, or nil."
  (cdr (assoc name (org-gtd-view-manager--store-read))))

(defun org-gtd-view-manager--store-upsert (name spec)
  "Store SPEC under NAME, replacing any existing entry, and persist."
  (let ((views (assoc-delete-all name (org-gtd-view-manager--store-read))))
    (org-gtd-view-manager--store-write
     (append views (list (cons name spec))))))

(defun org-gtd-view-manager--store-delete (name)
  "Remove NAME from the store and persist."
  (org-gtd-view-manager--store-write
   (assoc-delete-all name (org-gtd-view-manager--store-read))))

;;;; Filter-spec metadata (the builder's source of truth, per design 2.1)

;; Placeholder readers/formatters.  These are intentionally trivial STUBS so
;; the filter-spec table below names live symbols and the file byte-compiles
;; cleanly under `--warnings-as-errors'.  Real bodies arrive in Task 4
;; (formatters) and Task 7 (readers) -- do NOT build reader/formatter logic
;; here.
(defun org-gtd-view-manager--fmt-symbol (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-string (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-time (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-flag (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-effort (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-prefix (v) "Placeholder formatter for V." (format "%s" v))
(defun org-gtd-view-manager--fmt-number (v) "Placeholder formatter for V." (format "%s" v))

(defun org-gtd-view-manager--read-type (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-time (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-string (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-flag (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-area (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-effort (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-prefix (&rest _) "Placeholder reader." nil)
(defun org-gtd-view-manager--read-width (&rest _) "Placeholder reader." nil)

;; Each entry: (DSL-KEY :group G :key "L" :reader FN :formatter FN)
;; :reader reads a value interactively (returns the value to store, or nil to unset).
;; :formatter renders a stored value for the badge/summary (a short string).
(defconst org-gtd-view-manager--filter-specs
  '((type          :group type       :key "t"
                   :reader org-gtd-view-manager--read-type
                   :formatter org-gtd-view-manager--fmt-symbol)
    (when          :group time       :key "w"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (deadline      :group time       :key "D"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (scheduled     :group time       :key "C"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (todo          :group structural :key "o"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (done          :group structural :key "O"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (not-done      :group structural :key "N"
                   :reader org-gtd-view-manager--read-flag
                   :formatter org-gtd-view-manager--fmt-flag)
    (area-of-focus :group metadata   :key "A"
                   :reader org-gtd-view-manager--read-area
                   :formatter org-gtd-view-manager--fmt-string)
    (effort        :group metadata   :key "e"
                   :reader org-gtd-view-manager--read-effort
                   :formatter org-gtd-view-manager--fmt-effort)
    (who           :group metadata   :key "W"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (tags          :group metadata   :key "G"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (priority      :group metadata   :key "P"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (prefix        :group prefix     :key "x"
                   :reader org-gtd-view-manager--read-prefix
                   :formatter org-gtd-view-manager--fmt-prefix)
    (prefix-width  :group prefix     :key "X"
                   :reader org-gtd-view-manager--read-width
                   :formatter org-gtd-view-manager--fmt-number))
  "Curated user-facing filter keys the builder exposes, per design 2.1.
Each key MUST be a member of `org-gtd-view-lang--known-filter-keys'
\(asserted at load).  Structural/reserved keys are deliberately excluded.")

(defun org-gtd-view-manager--type-candidates ()
  "Return all selectable type values from the DSL type constants."
  (append org-gtd-view-lang--simple-types
          org-gtd-view-lang--complex-types))

;; Anti-drift guard: fail loudly at load if the curated table names a key the
;; DSL no longer knows about.
(let ((unknown (seq-remove
                (lambda (key) (memq key org-gtd-view-lang--known-filter-keys))
                (mapcar #'car org-gtd-view-manager--filter-specs))))
  (when unknown
    (error "org-gtd-view-manager: filter-spec keys not in the DSL: %S" unknown)))

;;;; Footer

(provide 'org-gtd-view-manager)

;;; org-gtd-view-manager.el ends here
