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
;; docs/plans/2026-07-17-walk-engine-phase-4-plan.md).
;;
;; `org-gtd-process-inbox' (org-gtd-process.el) drives this module's spec
;; through the walk engine.  The find->model seam: `org-gtd-walk-start'
;; normally builds its model from a nullary `:find', which cannot seed
;; `:meta'.  Since the inbox needs token->marker `:meta' from the moment
;; the walk starts (D2), `org-gtd-process-inbox' builds the full model via
;; `org-gtd-inbox-walk--build-model' and passes it to `org-gtd-walk-start'
;; as its optional INITIAL-MODEL argument, bypassing `:find' entirely for
;; the live entry point.  `:find' itself (`org-gtd-inbox-walk--find') is
;; still provided, returning just the token list, so the spec stays
;; well-formed for `org-gtd-walk-spec-valid-p' / the registry.
;;
;; Handle representation (D2, ruled): every inbox walk handle is a
;; synthetic string token, never persisted (resume is deferred --
;; `:resumable nil').  A model's `:meta' is an alist of (TOKEN . VALUE)
;; pairs where VALUE is either:
;;   - a live marker, for an original inbox heading (`:find' time), or
;;   - a (:title TITLE :content CONTENT) plist, for a duplicate (D4a,
;;     created at enqueue time by `org-gtd-clarify--enqueue-duplicate' in
;;     org-gtd-clarify.el).
;; `:render' resolves a token through this meta table and dispatches on
;; which shape it finds.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-core)
(require 'org-gtd-id)
(require 'org-gtd-wip)
(require 'org-gtd-clarify)
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)
(require 'org-gtd-capture)

;; `org-gtd-additional-inbox-files' is defined in org-gtd-process.el.
;; Declared (not required) to avoid a require cycle: a later task rewrites
;; org-gtd-process.el to require this module.
(defvar org-gtd-additional-inbox-files)

;;;; Variables

(defconst org-gtd-inbox-walk--surface-key "inbox-walk"
  "Fixed initial WIP key for the single reused inbox clarify surface.
The surface is rekeyed to each item's real clarify-id as it is
rendered (D3a); this is only the key used before any item has been
rendered into it.")

;;;; Functions

;;;;; Token minting

(defun org-gtd-inbox-walk--token ()
  "Return a fresh, unique synthetic string token for an inbox walk entry.
Tokens are never serialized (resume is deferred, D5b) -- any unique
string works; it exists only to key the walk model's `:meta' table."
  (format "inbox-%s" (org-id-uuid)))

;;;;; Meta accessors (D2 + D4a)

(defun org-gtd-inbox-walk--meta-put-marker (model token marker)
  "Return a copy of MODEL with TOKEN mapped to MARKER in its `:meta'.
MARKER is the live marker of an original inbox heading."
  (list :entries (plist-get model :entries)
        :cursor (plist-get model :cursor)
        :meta (cons (cons token marker) (plist-get model :meta))))

(defun org-gtd-inbox-walk--meta-put-dup (model token title content)
  "Return a copy of MODEL with TOKEN mapped to a duplicate entry.
The stored `:meta' value is (:title TITLE :content CONTENT), D4a's
representation for a duplicate handle."
  (list :entries (plist-get model :entries)
        :cursor (plist-get model :cursor)
        :meta (cons (cons token (list :title title :content content))
                    (plist-get model :meta))))

(defun org-gtd-inbox-walk--meta-get (model token)
  "Return the `:meta' value stored under TOKEN in MODEL, or nil."
  (cdr (assoc token (plist-get model :meta))))

(defun org-gtd-inbox-walk--meta-dup-p (value)
  "Return non-nil when VALUE is a duplicate (:title :content) plist.
VALUE is whatever `org-gtd-inbox-walk--meta-get' returned: a live
marker for an original inbox heading, or a duplicate plist.  nil (an
unknown/missing token) is not a duplicate."
  (and value (not (markerp value))))

;;;;; Multi-source scan (D6a, D2)

(defun org-gtd-inbox-walk--file-list ()
  "Return the multi-source inbox file list.
The main inbox first, then `org-gtd-additional-inbox-files' in listed
order (D6a file ordering)."
  (cons (org-gtd-inbox-path) org-gtd-additional-inbox-files))

(defun org-gtd-inbox-walk--scan (&optional files)
  "Scan the multi-source inbox and return (TOKENS . META).
TOKENS is the list of synthetic string tokens in scan order: every
heading in the main inbox, then every heading in each file of
`org-gtd-additional-inbox-files' in listed order.  META is an alist of
\(TOKEN . MARKER) -- the live marker of each token's source heading.

FILES defaults to `org-gtd-inbox-walk--file-list'; a caller that has
already computed the list (e.g. `org-gtd-process-inbox', which needs it
for the walk `:scope' too) passes it in to avoid recomputing it -- and
avoid re-running `org-gtd-inbox-path''s ensure-file side effect.

Missing or empty files are skipped.  Keeps the source files' buffers
open (`find-file-noselect') so the markers returned stay live for the
session.  Does NOT assign any org-id -- ids stay lazily assigned at
`:render' (D2), so a skipped/never-reached item is never stamped."
  (let (tokens meta)
    (dolist (file (or files (org-gtd-inbox-walk--file-list)))
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (let ((token (org-gtd-inbox-walk--token)))
               (push token tokens)
               (push (cons token (point-marker)) meta)))))))
    (cons (nreverse tokens) (nreverse meta))))

(defun org-gtd-inbox-walk--build-model (&optional files)
  "Return a fresh walk model built from `org-gtd-inbox-walk--scan'.
Combines the scanned tokens (the model's entries) with the scanned
token->marker meta into a single model, ready to drive an inbox walk.
Writes meta through `org-gtd-inbox-walk--meta-put-marker', the same
accessor the duplicate enqueue path uses.  FILES is forwarded to the
scan (see `org-gtd-inbox-walk--scan')."
  (let* ((scanned (org-gtd-inbox-walk--scan files))
         (tokens (car scanned))
         (raw-meta (cdr scanned))
         (model (org-gtd-walk-model-create tokens)))
    (dolist (pair raw-meta)
      (setq model (org-gtd-inbox-walk--meta-put-marker model (car pair) (cdr pair))))
    model))

;;;;; Surface (D3a: single reused editable clarify surface)

(defun org-gtd-inbox-walk--surface ()
  "Return the single reused WIP surface buffer for an inbox walk (D3a).
The buffer starts keyed under a fixed placeholder id; `:render' rekeys
it to each item's real clarify-id as it is drawn."
  (org-gtd-wip--get-buffer org-gtd-inbox-walk--surface-key))

;;;;; Render (D2, D3a, D4a)

(defun org-gtd-inbox-walk--render-duplicate (surface value)
  "Render duplicate VALUE into SURFACE (D4a).
VALUE is a (:title TITLE :content CONTENT) plist.  Inserts the content
fresh, strips any stale ID left over from the original item it was
duplicated from, and assigns a brand-new id -- mirrors
`org-gtd-clarify--process-next-queued-item''s duplicate-reuse handling."
  (let ((old-id (with-current-buffer surface
                  (or org-gtd-clarify--clarify-id
                      org-gtd-inbox-walk--surface-key))))
    (with-current-buffer surface
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (plist-get value :content))
        (goto-char (point-min))
        (org-entry-delete nil "ID")
        (let ((new-id (org-gtd-id-get-create)))
          (org-gtd-wip--rekey old-id new-id)
          (unless (derived-mode-p 'org-gtd-clarify-mode)
            (org-gtd-clarify-mode))
          (setq-local org-gtd-clarify--clarify-id new-id
                      org-gtd-clarify--source-heading-marker nil
                      org-gtd-clarify--skip-refile nil))))))

(defun org-gtd-inbox-walk--render-marker (surface marker)
  "Render the inbox item at MARKER into SURFACE (D2, D3a).
Copies the source subtree in, strips org-gtd state properties (reusing
`org-gtd-clarify--initialize-buffer-contents'), sets the source marker,
and lazily assigns the clarify id NOW on the source heading -- exactly
as `org-gtd-clarify-item' does at `org-gtd-clarify.el:254' -- so a
skipped/never-reached item is never stamped.

Deliberately does NOT bind `org-id-track-globally' around the copy
\(unlike someday-review's read-only render): SURFACE here is the live
staging buffer the item is about to be organized from, not a disposable
review copy, and `org-gtd-id-get-create' already registered the id
against a placeholder (\"Org GTD WIP buffer\") location.  Letting
`org-paste-subtree''s default tracking re-register the id against
SURFACE's real backing temp file (as the pre-engine
`org-gtd-clarify-item' always did) keeps `org-id-find' resolving to a
file that actually exists until the item is refiled; suppressing it
here left the id pointing at the placeholder, which forced later
`org-id-find' calls (e.g. `org-gtd-project-update-cookies' during a
SECOND project's clarify) into a full `org-id-update-id-locations'
rescan -- and since org-gtd buffers are usually unsaved
mid-session (`org-gtd-save-after-organize' is nil by default), that
rescan silently dropped not-yet-saved id registrations made moments
earlier (a real regression caught via `project-fix-keywords-test.el',
multi-project readiness computed via `org-id-find')."
  (let* ((old-id (with-current-buffer surface
                   (or org-gtd-clarify--clarify-id
                       org-gtd-inbox-walk--surface-key)))
         (new-id (org-gtd-id-get-create marker)))
    (with-current-buffer surface
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-gtd--without-kill-merge
          (org-gtd-clarify--initialize-buffer-contents marker surface))
        (goto-char (point-min))
        (org-gtd-wip--rekey old-id new-id)
        (unless (derived-mode-p 'org-gtd-clarify-mode)
          (org-gtd-clarify-mode))
        (setq-local org-gtd-clarify--clarify-id new-id
                    org-gtd-clarify--source-heading-marker marker
                    org-gtd-clarify--skip-refile nil)
        ;; Freshly rendered, untouched: mark unmodified so a later quit
        ;; can tell an *edited* item (save its edits back to the inbox --
        ;; the save-on-quit safety net) from a merely glanced one (discard,
        ;; no rewrite).  See `org-gtd-clarify--save-current-item-to-inbox'.
        (set-buffer-modified-p nil)))))

(defun org-gtd-inbox-walk--render (token surface)
  "Render inbox TOKEN into SURFACE, the walk `:render' contract.
Resolves TOKEN through the active walk model's `:meta': a duplicate
plist inserts fresh content and assigns a new id (D4a); a live marker
copies the source subtree in and lazily assigns the clarify id now
\(D2).  A stale marker -- source buffer killed or heading already gone,
the D2 durability caveat -- auto-skips via `org-gtd-walk-advance'
instead of erroring, same as a missing/unknown token.

`org-gtd-walk--active' is buffer-local, so this always runs with
SURFACE current (matches the engine's invariant that :render is
invoked in the surface buffer -- see `org-gtd-walk--render-current')."
  (with-current-buffer surface
    (let* ((model (plist-get org-gtd-walk--active :model))
           (value (org-gtd-inbox-walk--meta-get model token)))
      (cond
       ((org-gtd-inbox-walk--meta-dup-p value)
        (org-gtd-inbox-walk--render-duplicate surface value)
        (org-gtd-clarify-setup-windows surface)
        ;; Keep the pending-duplicates side window in sync after every
        ;; advance: shows the remaining duplicates, or hides itself when
        ;; none remain (D6a, model-backed).
        (org-gtd-clarify--queue-display))
       ((and (markerp value) (marker-buffer value) (marker-position value))
        (org-gtd-inbox-walk--render-marker surface value)
        (org-gtd-clarify-setup-windows surface)
        (org-gtd-clarify--queue-display))
       (t
        (org-gtd-walk-advance))))))

;;;;; Spec (Task 5)

(defun org-gtd-inbox-walk--find ()
  "Return the inbox walk's token list (the model's would-be :entries).
Provided so the spec is well-formed for `org-gtd-walk-spec-valid-p'
and the registry.  The live entry point (`org-gtd-process-inbox' in
org-gtd-process.el) does not call this: it builds the full token+marker
model itself via `org-gtd-inbox-walk--build-model' and passes it to
`org-gtd-walk-start' as INITIAL-MODEL, since `:find' alone cannot seed
`:meta' (see the find->model seam note at the top of this file)."
  (car (org-gtd-inbox-walk--scan)))

(defun org-gtd-inbox-walk--on-finish ()
  "End-of-walk cleanup for an inbox walk (design §9).
Folds in the old `org-gtd-process--stop' behavior: closes the horizons
view, tidies whitespace, cleans up the surface's WIP temp file (D3a --
the single reused surface's last-used clarify-id, not the fixed
placeholder key it started under), and saves GTD buffers.  Runs in the
surface buffer (`org-gtd-walk-finish')."
  (org-gtd-clarify--cleanup-horizons-view)
  (whitespace-cleanup)
  (when (bound-and-true-p org-gtd-clarify--clarify-id)
    (org-gtd-wip--cleanup-temp-file org-gtd-clarify--clarify-id))
  (org-gtd-save-buffers))

(defun org-gtd-inbox-walk--spec (&optional files)
  "Return the inbox walk spec, scoped over FILES.
Built fresh per call (mirroring `org-gtd-someday-review--spec') so
`:scope' can always reflect the current `org-gtd-additional-inbox-files'
rather than whatever it was when this module first loaded.

FILES defaults to just the main inbox's path, computed WITHOUT the
`org-gtd-inbox-path' side effect of creating the file if it is
missing (`org-gtd--path' is pure) -- this default is only used for the
spec registered at load time (`org-gtd-walk-get'/`org-gtd-walks'
introspection, e.g. `org-gtd-review.el' validating a hosted walk
step), which must not create files merely by being `require'd.  The
live entry point (`org-gtd-process-inbox' in org-gtd-process.el)
always passes the true current multi-source list
\(`org-gtd-inbox-walk--file-list', which DOES ensure the main inbox
file exists -- matching legacy `org-gtd-process-inbox' behavior)."
  (list :name 'inbox
        :find #'org-gtd-inbox-walk--find
        :render #'org-gtd-inbox-walk--render
        :actions org-gtd-clarify-mode-map
        :on-finish #'org-gtd-inbox-walk--on-finish
        :resumable nil
        :scope (or files (list (org-gtd--path org-gtd-inbox)))))

(org-gtd-walk-register 'inbox (org-gtd-inbox-walk--spec))

;;;; Footer

(provide 'org-gtd-inbox-walk)

;;; org-gtd-inbox-walk.el ends here
