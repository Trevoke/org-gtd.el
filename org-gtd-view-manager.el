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

;; Readers are defined in the `Infix readers' section below; the filter-spec
;; table references them by symbol.  The formatters live in the
;; `Badge / summary' section below (Task 4).

;; Each entry: (DSL-KEY :group G :key "L" :reader FN :formatter FN)
;; :reader reads a value interactively (returns the value to store, or nil to unset).
;; :formatter renders a stored value for the badge/summary (a short string).
;; Wrapped in `eval-and-compile' so the builder-generating macro (Task 9's
;; `org-gtd-view-manager--define-builder-transient') can read this table at
;; macro-expansion time during byte compilation, not only at load time.
(eval-and-compile
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
      (not-habit     :group structural :key "H"
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
\(asserted at load).  Structural/reserved keys are deliberately excluded."))

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
    (error "Org-gtd-view-manager: filter-spec keys not in the DSL: %S" unknown)))

;; A duplicate infix letter would silently break the builder transient (Task 9),
;; far from its cause -- this table is the single source of truth, so guard it
;; here.  Tally with a plain alist to stay free of cl-lib at load time.
(let ((tally nil)
      (dups nil))
  (dolist (entry org-gtd-view-manager--filter-specs)
    (let* ((key (plist-get (cdr entry) :key))
           (cell (assoc key tally)))
      (if cell
          (setcdr cell (1+ (cdr cell)))
        (push (cons key 1) tally))))
  (dolist (cell tally)
    (when (> (cdr cell) 1)
      (push (car cell) dups)))
  (when dups
    (error "Org-gtd-view-manager: duplicate filter-spec :key letters: %S" dups)))

;;;; Infix readers

;; `org-gtd-areas-of-focus' lives in `org-gtd-areas-of-focus', which pulls in a
;; heavy chain (organize, projects).  We only READ it in the area reader, so a
;; `defvar' forward-declaration keeps `--warnings-as-errors' clean without a
;; require that would risk a load cycle -- same pattern as the migration
;; section's `org-gtd-reflect-missed-custom-views' below.
(defvar org-gtd-areas-of-focus)

;; `org-gtd-view-manager--build-state' is defined with the builder transient
;; further down, but the flag reader below toggles against it, so forward-declare
;; it here to keep `--warnings-as-errors' clean.
(defvar org-gtd-view-manager--build-state)

(defconst org-gtd-view-manager--effort-regexp
  "\\`\\([<>]\\)\\([0-9]+[smhd]\\|[0-9]+:[0-9]+\\)\\'"
  "Matches a comparison effort like `<30m' or `>1:00'.")

(defun org-gtd-view-manager--effort->dsl (input)
  "Turn INPUT (e.g. \"<30m\") into a DSL effort list (< \"30m\").
Fail-soft: a malformed value raises a teaching `user-error'."
  (if (string-match org-gtd-view-manager--effort-regexp input)
      (list (intern (match-string 1 input)) (match-string 2 input))
    (user-error "Effort needs a duration like 30m (e.g. <30m, >1h)")))

(defun org-gtd-view-manager--read-type (&rest _)
  "Read a GTD type, completing over `org-gtd-view-manager--type-candidates'."
  (intern (completing-read
           "Type: " (mapcar #'symbol-name (org-gtd-view-manager--type-candidates))
           nil t)))

(defconst org-gtd-view-manager--time-regexp
  "\\`\\([<>=]\\)\\([+-]?[0-9]+[mhdwMy]\\|today\\)\\'"
  "Matches a comparison time like `<7d', `>-2w' or `=today'.
Mirrors the DSL's grammar: op in `< > =' and a duration matching
`org-gtd-view-lang--duration-regexp' (units m h d w M y) or the
literal `today'.")

(defun org-gtd-view-manager--time->dsl (input)
  "Turn INPUT into a DSL-consumable time value.
The literals \"past\"/\"today\"/\"future\" become their symbols; a
comparison like \"<7d\" becomes the list (< \"7d\"); blank unsets
\(returns nil).  Anything else raises a teaching `user-error'.

Shared by the when/deadline/scheduled and done keys.  The general
comparison-list + literals shape is DSL-consumable for all of them;
the DSL itself rejects key-specific nonsense (e.g. a future duration
on `done'), so this reader stays deliberately general."
  (cond
   ((string-blank-p input) nil)
   ((member input '("past" "today" "future")) (intern input))
   ((string-match org-gtd-view-manager--time-regexp input)
    (list (intern (match-string 1 input)) (match-string 2 input)))
   (t (user-error
       "Time needs past/today/future or a comparison like <7d, >2w, =1M"))))

(defun org-gtd-view-manager--read-time (&rest _)
  "Read a time value and parse it into the DSL shape."
  (org-gtd-view-manager--time->dsl
   (read-string "When (past/today/future or <7d, >2w, =1M): ")))

(defun org-gtd-view-manager--read-string (&rest _)
  "Read a free-form string value, returning nil when left blank."
  (let ((v (read-string "Value: ")))
    (if (string-blank-p v) nil v)))

(defun org-gtd-view-manager--read-flag (dsl-key &rest _)
  "Toggle flag DSL-KEY against the current builder state.
Returns t to SET the flag when it is currently off, or nil to UNSET
it when currently on.  A nil return routes through `--set-value',
which removes the key -- so a flag infix acts as an on/off toggle
rather than a set-only action."
  (unless (alist-get dsl-key org-gtd-view-manager--build-state) t))

(defun org-gtd-view-manager--read-area (&rest _)
  "Read an area of focus, completing over `org-gtd-areas-of-focus'."
  (let ((v (completing-read "Area of focus: " org-gtd-areas-of-focus nil nil)))
    (if (string-blank-p v) nil v)))

(defun org-gtd-view-manager--read-effort (&rest _)
  "Read a comparison effort and parse it into the DSL shape."
  (org-gtd-view-manager--effort->dsl
   (read-string "Effort (e.g. <30m, >1h): ")))

(defun org-gtd-view-manager--read-width (&rest _)
  "Read the numeric prefix column width."
  (read-number "Prefix width: "))

(defun org-gtd-view-manager--parse-prefix (input)
  "Parse INPUT into a prefix fallback CHAIN (a list), fail-soft.
Blank INPUT returns nil (unset; the DSL falls back to
`org-gtd-view-lang--default-prefix').  A read failure (empty form,
unbalanced parens) or a non-list value raises a teaching
`user-error' rather than surfacing as a stack trace (design 2.3)."
  (if (string-blank-p input)
      nil
    (let ((value (condition-case nil
                     (car (read-from-string input))
                   (error (user-error
                           "Prefix must be a list like (project area-of-focus \"—\")")))))
      (unless (listp value)
        (user-error "Prefix must be a list like (project area-of-focus \"—\")"))
      value)))

(defun org-gtd-view-manager--read-prefix (&rest _)
  "Read a prefix fallback CHAIN (a list), not a format string (design 2.3)."
  (org-gtd-view-manager--parse-prefix
   (read-string "Prefix chain (e.g. (project area-of-focus \"—\")): "
                (prin1-to-string org-gtd-view-lang--default-prefix))))

;;;; Badge / summary

(defun org-gtd-view-manager--fmt-symbol (v)
  "Format symbol value V for a badge."
  (format "%s" v))
(defun org-gtd-view-manager--fmt-string (v)
  "Format string value V for a badge."
  (format "%s" v))
(defun org-gtd-view-manager--fmt-number (v)
  "Format number value V for a badge."
  (format "%s" v))
(defun org-gtd-view-manager--fmt-flag (_v)
  "Format flag value V for a badge.
Returns nil because the badge label carries the key name itself."
  nil)
(defun org-gtd-view-manager--fmt-time (v)
  "Format time value V for a badge."
  (format "%s" v))
(defun org-gtd-view-manager--fmt-effort (v)
  "Format effort value V for a badge.
For example (< \"30m\") -> \"<30m\" and (> \"1h\") -> \">1h\"."
  (if (and (listp v) (= 2 (length v)))
      (format "%s%s" (car v) (cadr v))
    (format "%s" v)))
(defun org-gtd-view-manager--fmt-prefix (v)
  "Format prefix value V for a badge."
  (format "%s" v))

(defun org-gtd-view-manager--badge (spec)
  "Return a compact one-line summary of SPEC (name excluded)."
  (let (parts)
    (dolist (entry org-gtd-view-manager--filter-specs)
      (let* ((key (car entry))
             (cell (assq key spec)))
        (when cell
          (let* ((val (cdr cell))
                 (fmt (funcall (plist-get (cdr entry) :formatter) val)))
            (push (cond
                   ;; flag keys (not-done, not-habit): show the key name itself
                   ((memq key '(not-done not-habit)) (symbol-name key))
                   ;; who/tags/priority read better as key=value
                   ((memq key '(who tags priority)) (format "%s=%s" key fmt))
                   (t fmt))
                  parts)))))
    (string-join (nreverse parts) " · ")))

;;;; Compile

(defun org-gtd-view-manager--compile (state)
  "Compile builder STATE (a key -> value alist) into a flat view spec.
Keys whose value is nil are omitted so the DSL applies its own
defaults.  `name' and every curated filter key pass through as-is;
values are already in DSL shape (readers produce them)."
  (let ((allowed (cons 'name (mapcar #'car org-gtd-view-manager--filter-specs)))
        result)
    (dolist (cell state)
      (when (and (memq (car cell) allowed)
                 (not (null (cdr cell))))
        (push cell result)))
    (nreverse result)))

;;;; Migration (one-time, fail-soft)

;; Forward-declaration: the legacy defcustom lives in `org-gtd-reflect', which
;; already requires `org-gtd-view-language' (the same dependency this file
;; loads).  We only READ the variable here, so a `defvar' is lighter than a
;; full `require' and avoids pulling reflect's heavy dependency chain into the
;; manager.  At runtime the prelude/org-gtd load path binds it; this keeps
;; `--warnings-as-errors' quiet without risking a load cycle.
(defvar org-gtd-reflect-missed-custom-views)

(defun org-gtd-view-manager--flatten-entry (entry)
  "Flatten a legacy ENTRY: hoist any nested `filters' alist to top level.
Return the flat spec, or signal `error' if it names an unknown key."
  (let* ((allowed (cons 'name (mapcar #'car org-gtd-view-manager--filter-specs)))
         (nested (alist-get 'filters entry))
         (flat (append (assq-delete-all 'filters (copy-alist entry)) nested)))
    (dolist (cell flat)
      (unless (memq (car cell) allowed)
        (error "Unknown filter key %s" (car cell))))
    flat))

(defun org-gtd-view-manager--migrate ()
  "Import `org-gtd-reflect-missed-custom-views' into the store, fail-soft.
Flattens nested `filters'; skips and `message's any bad entry.  A
legacy entry whose name ALREADY EXISTS in the store is skipped, never
overwritten -- so a cross-session re-run cannot clobber a view the
user has since edited through the manager.  The legacy variable is
read via `bound-and-true-p': it is only forward-declared here, so it
may be void when `org-gtd-reflect' has not been loaded."
  (dolist (entry (bound-and-true-p org-gtd-reflect-missed-custom-views))
    (condition-case err
        (let* ((flat (org-gtd-view-manager--flatten-entry entry))
               (name (alist-get 'name flat)))
          (cond
           ((null name)
            (message "org-gtd: skipped migrating a view with no name"))
           ;; Non-clobbering: never overwrite an existing (possibly
           ;; user-edited) view of the same name on a later re-run.
           ((org-gtd-view-manager--store-get name) nil)
           (t (org-gtd-view-manager--store-upsert name flat))))
      ;; ENTRY may be a hand-edited junk atom (e.g. a bare string), so guard
      ;; the name extraction: `alist-get' on a non-list would itself throw an
      ;; UNCAUGHT `wrong-type-argument', aborting the whole loop and defeating
      ;; the "never abort on one bad entry" contract.
      (error (message "org-gtd: skipped migrating view %S: %s"
                      (and (consp entry) (alist-get 'name entry))
                      (error-message-string err))))))

;;;; Migration trigger

(defvar org-gtd-view-manager--migrated nil
  "Non-nil once legacy custom views have been imported this session.")

(defun org-gtd-view-manager--migrate-once ()
  "Run the one-time legacy import, guarded by a session flag."
  (unless org-gtd-view-manager--migrated
    (org-gtd-view-manager--migrate)
    (setq org-gtd-view-manager--migrated t)))

;;;; Builder transient

;; The builder is a transient prefix whose five infix columns (Type / Time /
;; Structural / Metadata / Prefix) are GENERATED from
;; `org-gtd-view-manager--filter-specs' -- every key letter, reader and
;; formatter comes from that table, so nothing here is hand-synced (design 2.1).

(defvar org-gtd-view-manager--build-state nil
  "Alist of the view spec being built (key -> value).")
(defvar org-gtd-view-manager--build-window-config nil
  "Window configuration to restore when the builder exits.")
(defvar org-gtd-view-manager--build-dirty nil
  "Non-nil when the builder has unsaved changes.")
(defvar org-gtd-view-manager--build-original-name nil
  "Name of the view being edited, or nil when creating a fresh view.
Set from the seeding spec on builder entry so that saving under a
CHANGED name is a rename (the old entry is removed) rather than a
save-as that leaves an orphan behind.")

(defun org-gtd-view-manager--build-summary ()
  "Return the summary line for the builder header."
  (concat "View: "
          (or (cdr (assq 'name org-gtd-view-manager--build-state)) "Untitled")
          "  —  "
          (org-gtd-view-manager--badge
           (org-gtd-view-manager--compile org-gtd-view-manager--build-state))))

(defun org-gtd-view-manager--infix-description (dsl-key label)
  "Return the builder row description for DSL-KEY.
LABEL is the human-readable prefix; the current value (from
`org-gtd-view-manager--build-state', rendered through the key's
formatter) follows, or `—' when the key is unset."
  (let* ((entry (assq dsl-key org-gtd-view-manager--filter-specs))
         (val (cdr (assq dsl-key org-gtd-view-manager--build-state)))
         (formatter (plist-get (cdr entry) :formatter)))
    (format "%-13s %s"
            label
            (if (null val) "—" (or (funcall formatter val) "on")))))

(defun org-gtd-view-manager--set-value (dsl-key)
  "Read a value for DSL-KEY and store it in the builder state.
A nil reader result UNSETS the key (removes it).  Marks the builder
dirty and asks the preview to refresh."
  (let* ((entry (assq dsl-key org-gtd-view-manager--filter-specs))
         (reader (plist-get (cdr entry) :reader))
         ;; Pass DSL-KEY so stateful readers (flags) can toggle against the
         ;; current spec; value readers take `&rest _' and ignore it.
         (value (funcall reader dsl-key)))
    (if (null value)
        (setq org-gtd-view-manager--build-state
              (assq-delete-all dsl-key org-gtd-view-manager--build-state))
      (setf (alist-get dsl-key org-gtd-view-manager--build-state) value))
    (setq org-gtd-view-manager--build-dirty t)
    (org-gtd-view-manager--preview-schedule)))

;;;; Live preview

(defvar org-gtd-view-manager--preview-last nil
  "Last compiled spec rendered in the preview, to skip redundant renders.")
(defvar org-gtd-view-manager--preview-timer nil
  "Idle timer for debounced preview refresh.")
(defconst org-gtd-view-manager--preview-delay 0.25
  "Debounce delay (seconds) before auto-refreshing the preview.")

(defun org-gtd-view-manager--preview-changed-p (spec)
  "Return non-nil if SPEC differs from the last previewed spec."
  (not (equal spec org-gtd-view-manager--preview-last)))

(defun org-gtd-view-manager--preview-now ()
  "Render the current build state immediately, fail-soft.
Skips the render when the compiled spec is unchanged from the last
one previewed.  Any `org-gtd-view-show' error is caught and surfaced
as a one-line teaching message, never a stack trace (design §8)."
  (let ((spec (org-gtd-view-manager--compile org-gtd-view-manager--build-state)))
    (when (org-gtd-view-manager--preview-changed-p spec)
      (condition-case err
          (progn
            (org-gtd-view-manager--render-preview spec)
            (setq org-gtd-view-manager--preview-last spec))
        (error (message "org-gtd view preview: %s"
                        (error-message-string err)))))))

(defun org-gtd-view-manager--preview (&rest _)
  "Explicit `RET' preview -- bypass the debounce and render now."
  (interactive)
  (org-gtd-view-manager--preview-now))

(defun org-gtd-view-manager--preview-schedule ()
  "Schedule a debounced preview refresh (called from the infixes).
Cancels any pending timer first, so rapid edits coalesce into at most
one render per `org-gtd-view-manager--preview-delay' idle window."
  (when (timerp org-gtd-view-manager--preview-timer)
    (cancel-timer org-gtd-view-manager--preview-timer))
  (setq org-gtd-view-manager--preview-timer
        (run-with-idle-timer org-gtd-view-manager--preview-delay nil
                             #'org-gtd-view-manager--preview-now)))

;;;; Sample-data preview

(defconst org-gtd-view-manager--sample-contents
  "\
* NEXT Buy stamps :errand:
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Home
:END:
* NEXT Draft quarterly plan
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Work
:END:
* WAIT Response from Sam
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Sam
:END:
"
  "A tiny representative dataset for previews when agenda-files are empty.")

(defun org-gtd-view-manager--sample-file ()
  "Return a path to a temp org file holding the sample dataset."
  (let ((path (f-join temporary-file-directory "org-gtd-view-sample.org")))
    ;; Write unconditionally: keeps the on-disk file in sync with
    ;; `--sample-contents' (a stale file would otherwise silently persist
    ;; across a future change to the sample).  Cheap: three headings, at most
    ;; once per debounced preview.
    (f-write-text org-gtd-view-manager--sample-contents 'utf-8 path)
    path))

(defun org-gtd-view-manager--render-preview (spec)
  "Render SPEC via `org-gtd-view-show', using sample data if needed.
When `org-agenda-files' is empty, a tiny built-in sample file is
`let'-bound into `org-agenda-files' FOR THE RENDER ONLY, with a
banner explaining the substitution.  Real agenda-files are left
untouched."
  (if org-agenda-files
      (org-gtd-view-show spec)
    (let ((org-agenda-files (list (org-gtd-view-manager--sample-file))))
      (message "sample data · your agenda-files are empty — previewing org-gtd's built-in set")
      (org-gtd-view-show spec))))

(defun org-gtd-view-manager--build-restore-windows ()
  "Restore the window layout snapshotted when the builder was entered.
Also cancels any pending debounced preview timer so it does not fire
into the restored layout after the builder has exited."
  (when (timerp org-gtd-view-manager--preview-timer)
    (cancel-timer org-gtd-view-manager--preview-timer)
    (setq org-gtd-view-manager--preview-timer nil))
  (when org-gtd-view-manager--build-window-config
    (set-window-configuration org-gtd-view-manager--build-window-config)))

(defun org-gtd-view-manager--build-resume ()
  "Re-open the builder transient WITHOUT reseeding its state.
Used by the fail-soft `stay' branches of save/abort: the exiting
suffix has already torn the transient down, so we redisplay on the
next command tick (the house idiom, see `run-at-time' in
`org-gtd-graph-transient')."
  (run-at-time 0 nil (lambda () (transient-setup 'org-gtd-view-manager--build))))

(defun org-gtd-view-manager--save ()
  "Persist the built view, guarding against blank names and silent overwrite.
Prompts for a name (defaulting to the current one).  A blank name is
rejected with a teaching `user-error' and nothing is written -- the
builder is reopened so a name can be entered (fail-soft), never
persisting a nameless `(name . \"\")' entry.  If a view of that name
already exists and the user declines to overwrite, the save is
likewise abandoned and the builder reopened.  On a successful save
the entry window layout is restored."
  (interactive)
  (let* ((current-name (or (cdr (assq 'name org-gtd-view-manager--build-state))
                           "Untitled"))
         (name (read-string "Name this view: " current-name))
         (spec (cons (cons 'name name)
                     (assq-delete-all
                      'name (org-gtd-view-manager--compile
                             org-gtd-view-manager--build-state)))))
    (cond
     ((string-blank-p name)
      (org-gtd-view-manager--build-resume)
      (user-error "A view needs a name"))
     ;; Overwrite guard fires only for a DIFFERENT existing view.  Saving back
     ;; to the very view being edited (name = original) is not an overwrite, so
     ;; do not prompt; renaming onto another existing name still prompts.
     ((and (org-gtd-view-manager--store-get name)
           (not (equal name org-gtd-view-manager--build-original-name))
           (not (y-or-n-p (format "A view named '%s' exists — overwrite? " name))))
      (message "Save cancelled")
      (org-gtd-view-manager--build-resume))
     (t
      (org-gtd-view-manager--store-upsert name spec)
      ;; Rename-move: when editing a view and saving it under a CHANGED name,
      ;; the old entry must be removed (a rename), not left behind as an
      ;; orphan/duplicate.  A fresh create has a nil original-name and so
      ;; deletes nothing.
      (when (and org-gtd-view-manager--build-original-name
                 (not (equal org-gtd-view-manager--build-original-name name)))
        (org-gtd-view-manager--store-delete
         org-gtd-view-manager--build-original-name))
      (setq org-gtd-view-manager--build-original-name nil)
      (setq org-gtd-view-manager--build-dirty nil)
      (message "Saved view '%s'" name)
      (org-gtd-view-manager--build-restore-windows)))))

(defun org-gtd-view-manager--abort ()
  "Abort the builder, first confirming any discard of unsaved edits.
When the builder is dirty the user is asked to confirm the discard;
declining reopens the builder with its state intact.  On confirm (or
when nothing is dirty) the entry window layout is restored."
  (interactive)
  (if (or (not org-gtd-view-manager--build-dirty)
          (y-or-n-p "Discard unsaved view? "))
      (org-gtd-view-manager--build-restore-windows)
    (org-gtd-view-manager--build-resume)))

(defmacro org-gtd-view-manager--define-builder-transient ()
  "Generate the builder's per-key set-commands and the prefix from the table.
DRY: each infix's key letter, reader and formatter are read from
`org-gtd-view-manager--filter-specs' at macro-expansion time, so the
five columns can never drift from the single source of truth."
  (let* ((groups '((type       . "Type")
                   (time       . "Time")
                   (structural . "Structural")
                   (metadata   . "Metadata")
                   (prefix     . "Prefix")))
         (set-defuns
          (mapcar
           (lambda (entry)
             (let* ((dsl-key (car entry))
                    (name (intern (format "org-gtd-view-manager--set-%s"
                                          dsl-key))))
               `(defun ,name ()
                  ,(format "Read and store the `%s' builder filter (generated)."
                           dsl-key)
                  (interactive)
                  (org-gtd-view-manager--set-value ',dsl-key))))
           org-gtd-view-manager--filter-specs))
         (columns
          (mapcar
           (lambda (grp)
             (apply
              #'vector
              (cdr grp)
              (delq nil
                    (mapcar
                     (lambda (entry)
                       (when (eq (plist-get (cdr entry) :group) (car grp))
                         (let* ((dsl-key (car entry))
                                (letter (plist-get (cdr entry) :key))
                                (name (intern
                                       (format "org-gtd-view-manager--set-%s"
                                               dsl-key)))
                                (label (symbol-name dsl-key)))
                           (list letter
                                 `(lambda ()
                                    (org-gtd-view-manager--infix-description
                                     ',dsl-key ,label))
                                 name
                                 :transient t))))
                     org-gtd-view-manager--filter-specs))))
           groups)))
    `(progn
       ,@set-defuns
       (transient-define-prefix org-gtd-view-manager--build (&optional starting-spec)
         "Build or edit a saved GTD view interactively.
The five infix columns are generated from
`org-gtd-view-manager--filter-specs'.  STARTING-SPEC seeds the
builder; nil starts a fresh Untitled next-action view."
         [:description (lambda () (org-gtd-view-manager--build-summary))]
         ,@columns
         ["Actions"
          ("RET" "Preview" org-gtd-view-manager--preview :transient t)
          ("s" "Save" org-gtd-view-manager--save)
          ("C-c C-k" "Abort" org-gtd-view-manager--abort)]
         (interactive)
         (setq org-gtd-view-manager--build-window-config
               (current-window-configuration))
         ;; Editing (or duplicating) seeds the original name so a save under a
         ;; changed name becomes a rename-move; a fresh create leaves it nil so
         ;; nothing is ever deleted on save.
         (setq org-gtd-view-manager--build-original-name
               (and starting-spec (alist-get 'name starting-spec)))
         (setq org-gtd-view-manager--build-state
               (copy-alist (or starting-spec
                               (list (cons 'name "Untitled")
                                     (cons 'type 'next-action)))))
         (setq org-gtd-view-manager--build-dirty nil)
         (setq org-gtd-view-manager--preview-last nil)
         (transient-setup 'org-gtd-view-manager--build)))))

(org-gtd-view-manager--define-builder-transient)

;;;; List transient

;; The flat "Your saved views" browser.  Built-in views (Engage) stay commands,
;; never seeded specs (Open Question §2): the empty-state `RET' opens Engage.

;; `org-gtd-engage' is an autoloaded command in `org-gtd-engage', whose heavy
;; dependency chain we deliberately do NOT require here.  Forward-declare it so
;; `--warnings-as-errors' stays quiet; the autoload resolves it at runtime.
(declare-function org-gtd-engage "org-gtd-engage")

(defvar org-gtd-view-manager--highlight 0
  "Index of the highlighted saved view in the list.")
(defvar org-gtd-view-manager--list-window-config nil
  "Window configuration to restore when the manager exits.")

(defun org-gtd-view-manager--rows ()
  "Return a description string for the saved views (teaching line if empty)."
  (let ((views (org-gtd-view-manager--store-read)))
    (if (null views)
        "No saved views yet.  Press c to build one, or RET to open Engage."
      ;; Clamp the same way `--list-highlighted-name' does, so the ▸ marker and
      ;; the action target are provably the same row even if the index is stale.
      (let ((highlight (min (max org-gtd-view-manager--highlight 0)
                            (1- (length views)))))
        (string-join
         (seq-map-indexed
          (lambda (v i)
            (format "%s %-24s %s"
                    (if (= i highlight) "▸" " ")
                    (car v)
                    (org-gtd-view-manager--badge (cdr v))))
          views)
         "\n")))))

(defun org-gtd-view-manager--list-highlighted-name ()
  "Return the highlighted view's name, or nil when the store is empty.
The highlight index is clamped to the store's bounds first, so a
stale index (e.g. after a delete) can never index out of range."
  (let ((views (org-gtd-view-manager--store-read)))
    (when views
      (let ((i (min (max org-gtd-view-manager--highlight 0)
                    (1- (length views)))))
        (car (nth i views))))))

(defun org-gtd-view-manager--list-highlighted-spec ()
  "Return the highlighted view's stored spec, or nil when the store is empty."
  (let ((name (org-gtd-view-manager--list-highlighted-name)))
    (when name (org-gtd-view-manager--store-get name))))

(defun org-gtd-view-manager--list-render ()
  "Render the highlighted view, or open Engage when the store is empty.
An empty store's `RET' opens the daily Engage view (built-ins stay
commands, never seeded specs).  Otherwise this is a REAL recall of the
saved spec via `org-gtd-view-show' -- NOT the builder's sample-data
preview (design §5): an empty agenda shows org-agenda's normal `no
matches' line, never fake sample data or a preview banner."
  (interactive)
  (let ((spec (org-gtd-view-manager--list-highlighted-spec)))
    (if (null spec)
        (org-gtd-engage)
      (org-gtd-view-show spec))))

(defun org-gtd-view-manager--list-create ()
  "Open the builder on a fresh spec."
  (interactive)
  (org-gtd-view-manager--build))

(defun org-gtd-view-manager--list-edit ()
  "Open the builder on the highlighted stored spec.
Fail-soft: an empty store simply does nothing."
  (interactive)
  (let ((spec (org-gtd-view-manager--list-highlighted-spec)))
    (when spec (org-gtd-view-manager--build spec))))

(defun org-gtd-view-manager--list-duplicate ()
  "Open the builder on a copy of the highlighted view named \"<name> copy\".
Fail-soft: an empty store does nothing.  The copy is NOT pre-persisted:
`--build' seeds `--build-original-name' to the copy name, so `--save'
creates it on save (and a rename then store-deletes the copy name,
harmless if it was never written).  Aborting therefore leaves no
orphan copy behind."
  (interactive)
  (let ((spec (org-gtd-view-manager--list-highlighted-spec)))
    (when spec
      (let* ((copy-name (concat (alist-get 'name spec) " copy"))
             (copy-spec (cons (cons 'name copy-name)
                              (assq-delete-all 'name (copy-alist spec)))))
        (org-gtd-view-manager--build copy-spec)))))

(defun org-gtd-view-manager--list-delete ()
  "Delete the highlighted view after a `y/n' confirm, then refresh.
Fail-soft: an empty store (nothing highlighted) does nothing.  After
deletion the highlight index is clamped to the shrunken store."
  (interactive)
  (let ((name (org-gtd-view-manager--list-highlighted-name)))
    (when (and name
               (y-or-n-p (format "Delete view '%s'? " name)))
      (org-gtd-view-manager--store-delete name)
      (let ((len (length (org-gtd-view-manager--store-read))))
        (setq org-gtd-view-manager--highlight
              (max 0 (min org-gtd-view-manager--highlight (1- len))))))))

(defun org-gtd-view-manager--list-up ()
  "Move the highlight up one row, clamped to the first view."
  (interactive)
  (setq org-gtd-view-manager--highlight
        (max 0 (1- org-gtd-view-manager--highlight))))

(defun org-gtd-view-manager--list-down ()
  "Move the highlight down one row, clamped to the last view."
  (interactive)
  (let ((len (length (org-gtd-view-manager--store-read))))
    (setq org-gtd-view-manager--highlight
          (min (max 0 (1- len)) (1+ org-gtd-view-manager--highlight)))))

(defun org-gtd-view-manager--list-quit ()
  "Quit the manager, restoring the window layout snapshotted on entry."
  (interactive)
  (when org-gtd-view-manager--list-window-config
    (set-window-configuration org-gtd-view-manager--list-window-config)))

;;;###autoload (autoload 'org-gtd-view-manager "org-gtd-view-manager" nil t)
(transient-define-prefix org-gtd-view-manager ()
  "Browse and manage saved GTD views."
  [:description org-gtd-view-manager--rows
   [("RET" "Render"    org-gtd-view-manager--list-render)
    ("c"   "Create"    org-gtd-view-manager--list-create)
    ("e"   "Edit"      org-gtd-view-manager--list-edit)
    ("d"   "Duplicate" org-gtd-view-manager--list-duplicate)
    ("D"   "Delete"    org-gtd-view-manager--list-delete)
    ("<up>"   "Up"     org-gtd-view-manager--list-up   :transient t)
    ("<down>" "Down"   org-gtd-view-manager--list-down :transient t)
    ("q"   "Quit"      org-gtd-view-manager--list-quit)]]
  (interactive)
  ;; Snapshot the layout so `q' (and any render that rearranges windows) can be
  ;; undone.  Reset the highlight to the top on each fresh open.
  (setq org-gtd-view-manager--list-window-config (current-window-configuration)
        org-gtd-view-manager--highlight 0)
  ;; KNOWN v1 LIMITATION: `--migrate-once' is session-scoped, so a DELETED
  ;; migrated view reappears next session -- migration re-runs per session and
  ;; its non-clobber guard only protects edited/existing names, not deletions.
  ;; Accepted v1 interim; a persistent migration marker is the future fix.
  (org-gtd-view-manager--migrate-once)
  (transient-setup 'org-gtd-view-manager))

;;;; Recall

;;;###autoload
(defun org-gtd-view-run ()
  "Prompt for a saved view by name and render it via `org-gtd-view-show'."
  (interactive)
  (let ((views (org-gtd-view-manager--store-read)))
    (unless views
      (user-error "No saved views yet — build one with M-x org-gtd-view-manager"))
    (let* ((name (completing-read "View: " (mapcar #'car views) nil t))
           (spec (cdr (assoc name views))))
      (org-gtd-view-show spec))))

;;;; Footer

(provide 'org-gtd-view-manager)

;;; org-gtd-view-manager.el ends here
