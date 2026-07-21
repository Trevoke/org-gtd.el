;;; org-gtd-reflect-missed-calendar-review.el --- Actionable overdue-calendar review -*- lexical-binding: t; coding: utf-8 -*-
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
;; The actionable counterpart of the read-only `org-gtd-reflect-missed-calendar'
;; view: a walk that shows each overdue Calendar item one at a time and lets the
;; user decide -- with consent -- what each one becomes now (done, migrate to a
;; next action, reschedule, trash, clarify, or skip).  A walk consumer,
;; structurally identical to `org-gtd-someday-review'.  See
;; docs/plans/2026-07-19-overdue-calendar-review-design.md.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-core)
(require 'org-gtd-id)
(require 'org-gtd-wip)
(require 'org-gtd-skip)
(require 'org-gtd-types)
(require 'org-gtd-organize-core)
(require 'org-gtd-archive)
(require 'org-gtd-clarify)
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Variables

(defconst org-gtd-reflect-missed-calendar-review--surface-key "missed-calendar-review"
  "Fixed WIP key for the single missed-calendar-review surface buffer.")

(defvar-local org-gtd-reflect-missed-calendar-review--counters nil
  "Buffer-local plist of tallies for the active surface:
\(:reviewed N :done N :migrated N :rescheduled N :trashed N :skipped N).")

;;;; Keymaps

(defvar org-gtd-reflect-missed-calendar-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "d") #'org-gtd-reflect-missed-calendar-review-done)
    (define-key map (kbd "m") #'org-gtd-reflect-missed-calendar-review-migrate)
    (define-key map (kbd "r") #'org-gtd-reflect-missed-calendar-review-reschedule)
    (define-key map (kbd "t") #'org-gtd-reflect-missed-calendar-review-trash)
    (define-key map (kbd "c") #'org-gtd-reflect-missed-calendar-review-clarify)
    (define-key map (kbd "s") #'org-gtd-reflect-missed-calendar-review-skip)
    (define-key map (kbd "q") #'org-gtd-reflect-missed-calendar-review-quit)
    map)
  "Keymap for `org-gtd-reflect-missed-calendar-review-mode'.")

;;;; Detection

(defun org-gtd-reflect-missed-calendar-review--make-overdue-calendar-p ()
  "Return a closure answering whether a heading is still an overdue Calendar item.
The returned predicate takes one MARKER-OR-POINT argument and returns
non-nil at an overdue Calendar heading.

This is the single definition of \"overdue calendar\" shared by
`--find-items' (scanning) and `--current-overdue-marker' (the
mutating-disposition guard), so the two can never drift apart.  It
composes `org-gtd-skip.el' predicates matching the design's definition of
overdue calendar: ORG_GTD = Calendar, not done, ORG_GTD_TIMESTAMP
strictly before today, and not an org-gtd habit.  The not-habit clause is
redundant given the Calendar/Habit type invariant (an entry cannot be
both) but is kept for parity with that stated definition.

The four predicate factory closures are captured ONCE here (the codebase
factory-closure convention); the returned closure only `funcall's them
per heading, so callers build the predicate once and reuse it across a
whole scan rather than rebuilding it per heading."
  (let ((calendar-p (org-gtd-pred--property-equals
                     "ORG_GTD" (org-gtd-type-org-gtd-value 'calendar)))
        (not-done-p (org-gtd-pred--not-done))
        (overdue-p (org-gtd-pred--property-ts<
                    (org-gtd-type-property 'calendar :when) "today"))
        (not-habit-p (org-gtd-pred--property-not-equals
                      "ORG_GTD" (org-gtd-type-org-gtd-value 'habit))))
    (lambda (marker-or-point)
      (org-with-point-at marker-or-point
        (and (funcall calendar-p)
             (funcall not-done-p)
             (funcall overdue-p)
             (funcall not-habit-p))))))

(defun org-gtd-reflect-missed-calendar-review--find-items ()
  "Return the org-ids of every overdue Calendar item across `org-agenda-files'.
Builds the overdue-calendar predicate once (see
`--make-overdue-calendar-p') and applies it at every heading."
  (let ((overdue-p (org-gtd-reflect-missed-calendar-review--make-overdue-calendar-p))
        items)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (funcall overdue-p (point))
               (push (org-id-get-create) items)))))))
    (nreverse items)))

(defun org-gtd-reflect-missed-calendar-review--current-overdue-marker ()
  "Return the current walk item's marker iff it is STILL an overdue Calendar item.
Returns nil when the item no longer qualifies (e.g. it was clarified into
another type, rescheduled, or completed out of band) or cannot be resolved.
Used by the mutating dispositions to refuse to act on an item the user has
already handled another way (e.g. via `c' clarify)."
  (let* ((id (org-gtd-walk-model-current
              (plist-get org-gtd-walk--active :model)))
         (marker (and id (org-id-find id 'marker)))
         (overdue-p (org-gtd-reflect-missed-calendar-review--make-overdue-calendar-p)))
    (when (and marker (funcall overdue-p marker))
      marker)))

(defun org-gtd-reflect-missed-calendar-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))

;;;; Render

(defun org-gtd-reflect-missed-calendar-review--humanize-lapse (ts-string)
  "Return a humanized description of the lapsed date TS-STRING.
E.g. \"was: 2026-06-12 (37 days ago)\".  Returns \"date unknown\" when
TS-STRING cannot be parsed."
  (let ((ts (org-gtd--parse-timestamp ts-string)))
    (if (null ts)
        "date unknown"
      (let ((days (- (org-today) (time-to-days ts))))
        (format "was: %s (%d day%s ago)"
                (format-time-string "%F" ts)
                days
                (if (= days 1) "" "s"))))))

(defun org-gtd-reflect-missed-calendar-review--render (id surface)
  "Render the overdue Calendar item ID into SURFACE (the walk :render contract).
Resolves ID to a marker, refills SURFACE read-only with the teaching
framing, the humanized lapse, an optional area-of-focus line, and the
subtree body, then sets review mode, the header-line action bar, and
displays the buffer.

Before drawing -- indeed before even resolving ID to a marker -- undoes
any in-place clarify staging left on SURFACE by `--clarify-in-place'
\(the `c' disposition): when `org-gtd-clarify--clarify-id' is set, the
surface is still keyed under that item's own clarify id and may still
have a horizons reference window open.  `--clarify-in-place' deliberately
leaves `org-id-track-globally' on during its copy (see that function's
docstring), so `org-paste-subtree' there re-registers the item's real
org-id against SURFACE in the global `org-id-locations' table -- so if
this render is about to resolve that SAME id again (the cancel path:
ID is unchanged, the source was never cut), `org-id-find' below would
otherwise resolve into SURFACE, which this function is about to erase,
rather than back to the untouched source heading.  Fixing the id's
registered location back to the source file (`org-id-add-location',
using the still-live `org-gtd-clarify--source-heading-marker') MUST
happen before that `org-id-find', hence this runs first -- but ONLY on
that same-item cancel path: this function also runs right after an
organize completes and the walk advances to the NEXT item, where ID is
that next item's id while the buffer-locals still hold the
JUST-organized item's clarify id/marker.  Calling `org-id-add-location'
unconditionally there would re-point the just-organized item's id back
at its own now-emptied source file (its subtree was already cut by
`org-gtd-organize--call' once the refile completed elsewhere),
clobbering the correct location `org-refile' just wrote and leaving
that item unresolvable.  So the id-location fixup is gated on `(equal
id org-gtd-clarify--clarify-id)' -- true only on the cancel path.  Then
rekey the WIP registry back to `--surface-key' (matching
`org-gtd-wip--rekey''s use in `--clarify-in-place') and tear the
horizons view down -- both UNCONDITIONALLY, on either path: they are
internal org-gtd bookkeeping (WIP registry key, horizons window),
never global org-id state, and are correct to run regardless of which
item ID names.  All of this is a no-op for items that were never
clarified (clarify-id nil) -- e.g. the normal skip/done/migrate/etc.
path, which never rekeys the surface or touches org-id-locations at
all."
  (with-current-buffer surface
    (when (bound-and-true-p org-gtd-clarify--clarify-id)
      (when (and (equal id org-gtd-clarify--clarify-id)
                 (markerp org-gtd-clarify--source-heading-marker))
        (org-id-add-location
         org-gtd-clarify--clarify-id
         (buffer-file-name (marker-buffer org-gtd-clarify--source-heading-marker))))
      (org-gtd-wip--rekey org-gtd-clarify--clarify-id
                          org-gtd-reflect-missed-calendar-review--surface-key)
      (org-gtd-clarify--cleanup-horizons-view)))
  (let ((marker (org-id-find id 'marker)))
    (when marker
      (let ((ts (org-with-point-at marker
                  (org-entry-get (point) (org-gtd-type-property 'calendar :when))))
            (aof (org-with-point-at marker
                   (org-entry-get (point) org-gtd-prop-area-of-focus))))
        (with-current-buffer surface
          ;; Bind `org-id-track-globally' to nil so `org-paste-subtree' does
          ;; not re-register the pasted :ID: (via `org-id-paste-tracker') into
          ;; this disposable surface's temp file, which would corrupt the
          ;; global `org-id-locations' map.  This binding is load-bearing, not
          ;; dead code -- see `org-gtd-someday-review--render'.
          (let ((inhibit-read-only t)
                (org-id-track-globally nil))
            (erase-buffer)
            (insert "# This date has passed -- decide what it is now.\n")
            (insert (format "# %s\n"
                            (org-gtd-reflect-missed-calendar-review--humanize-lapse ts)))
            (when (and aof (not (string-empty-p aof)))
              (insert (format "# Area of focus: %s\n" aof)))
            (insert "\n")
            (org-gtd--without-kill-merge
              (org-with-point-at marker (org-copy-subtree)))
            (org-paste-subtree)
            (goto-char (point-min)))
          (unless (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode)
            (org-gtd-reflect-missed-calendar-review-mode))
          (setq buffer-read-only t)
          (let* ((model (plist-get org-gtd-walk--active :model))
                 (pos (1+ (plist-get model :cursor)))
                 (total (length (plist-get model :entries))))
            (setq header-line-format
                  (format (concat "[d] Done  [m] Migrate  [r] Reschedule  "
                                  "[t] Trash  [c] Clarify  [s] Skip  [q] Quit  (%d/%d)")
                          pos total)))
          (pop-to-buffer surface))))))

;;;; Walk Surface

(defun org-gtd-reflect-missed-calendar-review--surface ()
  "Return the fresh WIP surface buffer for a missed-calendar-review walk.
Activates `org-gtd-reflect-missed-calendar-review-mode' before setting the
buffer-local counters: (re-)running a major mode calls
`kill-all-local-variables', which would silently wipe them.  Doing this
here means the mode is already active by the time `org-gtd-walk-start'
calls :render, so :render's own mode-activation guard never fires and the
counters survive the whole walk (mirrors `org-gtd-someday-review--surface')."
  (let ((buf (org-gtd-wip--get-buffer
              org-gtd-reflect-missed-calendar-review--surface-key)))
    (with-current-buffer buf
      (org-gtd-reflect-missed-calendar-review-mode)
      (setq-local org-gtd-reflect-missed-calendar-review--counters
                  (list :reviewed 0 :done 0 :migrated 0
                        :rescheduled 0 :trashed 0 :skipped 0)))
    buf))

(defun org-gtd-reflect-missed-calendar-review--bump (key)
  "Increment counter KEY on the surface buffer's counters plist."
  (setq org-gtd-reflect-missed-calendar-review--counters
        (plist-put org-gtd-reflect-missed-calendar-review--counters key
                   (1+ (plist-get
                        org-gtd-reflect-missed-calendar-review--counters key)))))

(defun org-gtd-reflect-missed-calendar-review--summary ()
  "Return the human-readable tally string for the active surface."
  (let ((c org-gtd-reflect-missed-calendar-review--counters))
    (format "reviewed %d - done %d - migrated %d - rescheduled %d - trashed %d - skipped %d"
            (or (plist-get c :reviewed) 0)
            (or (plist-get c :done) 0)
            (or (plist-get c :migrated) 0)
            (or (plist-get c :rescheduled) 0)
            (or (plist-get c :trashed) 0)
            (or (plist-get c :skipped) 0))))

(defun org-gtd-reflect-missed-calendar-review--on-finish ()
  "End-of-walk: report the tally, clean up the surface buffer, and save.
Runs in the surface buffer after the engine has cleared its session.
Mutating dispositions (done/migrate/reschedule/trash) modify org-gtd
buffers directly; `org-gtd-save-buffers' persists them, honoring
`org-gtd-save-after-organize' (mirrors `org-gtd-inbox-walk--on-finish').

Cleans up whichever WIP key the surface is CURRENTLY registered under.
Ordinarily that is the fixed `--surface-key' (the console never rekeys,
and `--render' rekeys any in-place clarify staging back to it before
drawing the next item or the same item on cancel).  But when the walk
finishes immediately after an in-place clarify + organize on the LAST
item, there is no next item to trigger that rekey-back -- the surface
is still keyed under that item's own clarify id, and any horizons
window the clarify staging opened is still up.  Reading
`org-gtd-clarify--clarify-id' (falling back to `--surface-key' when
unset -- the ordinary case) and tearing down the horizons view
unconditionally (a no-op when none is showing) covers both cases
without needing to know which one happened, mirroring
`org-gtd-inbox-walk--on-finish'."
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-clarify--cleanup-horizons-view)
    (org-gtd-wip--cleanup-temp-file
     (or (bound-and-true-p org-gtd-clarify--clarify-id)
         org-gtd-reflect-missed-calendar-review--surface-key))
    (message "Missed-calendar review complete. %s" summary)
    (org-gtd-save-buffers)))

(defun org-gtd-reflect-missed-calendar-review--spec ()
  "Return the missed-calendar-review walk spec template.
The default :find covers all overdue calendar items."
  (list :name 'missed-calendar-review
        :find #'org-gtd-reflect-missed-calendar-review--find-items
        :render #'org-gtd-reflect-missed-calendar-review--render
        :actions org-gtd-reflect-missed-calendar-review-mode-map
        :on-finish #'org-gtd-reflect-missed-calendar-review--on-finish
        :resumable nil
        :resolve #'org-gtd-reflect-missed-calendar-review--resolve
        :scope (org-agenda-files)))

(org-gtd-walk-register 'missed-calendar-review
                       (org-gtd-reflect-missed-calendar-review--spec))

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-reflect-missed-calendar-review-mode org-mode "GTD-MissedCal"
  "Major mode for reviewing overdue calendar items one at a time.
Derived from `org-mode'; the buffer is read-only (set in the render
function) and offers disposition keys.

\\{org-gtd-reflect-missed-calendar-review-mode-map}"
  :group 'org-gtd)

;;;; Evil-mode Integration

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-reflect-missed-calendar-review-mode 'emacs)
  (add-hook 'org-gtd-reflect-missed-calendar-review-mode-hook #'evil-emacs-state))

;;;; Entry Point

;;;###autoload
(defun org-gtd-reflect-missed-calendar-review ()
  "Review overdue calendar items one at a time.
The actionable counterpart of the read-only `org-gtd-reflect-missed-calendar'
view: walks each open Calendar item whose date has passed and lets you
decide -- with consent -- what it becomes now.  Opens nothing when your
hard landscape is clean."
  (interactive)
  (let ((items (org-gtd-reflect-missed-calendar-review--find-items)))
    (if (null items)
        (message "No overdue calendar items -- your hard landscape is clean.")
      (let ((spec (org-gtd-reflect-missed-calendar-review--spec)))
        (setq spec (plist-put spec :find (lambda () items)))
        (setq spec (plist-put spec :scope (org-agenda-files)))
        (org-gtd-walk-start spec (org-gtd-reflect-missed-calendar-review--surface))))))

;;;; Commands

(defun org-gtd-reflect-missed-calendar-review-done ()
  "Mark the current item done and archive it (it happened), then advance.
Refuses to mutate when the current item is no longer an overdue Calendar
item (e.g. it was clarified away via `c' into some other type): the walk
still advances -- the review step still counts toward `:reviewed' since
the user is moving on, but not toward `:done', since nothing was
actually marked done."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let ((marker (org-gtd-reflect-missed-calendar-review--current-overdue-marker)))
       (if (null marker)
           (message "No longer an overdue calendar item -- advancing.")
         (org-with-point-at marker
           ;; Suppress the state-change note prompt: this is a programmatic
           ;; "it already happened" mark, and an unfinished note buffer would
           ;; leave the following archive yanking the subtree out from under it.
           (let ((org-inhibit-logging 'note))
             (org-todo (org-gtd-keywords--done)))
           (org-gtd-archive-item-at-point))
         (org-gtd-reflect-missed-calendar-review--bump :done))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-migrate ()
  "Migrate the current item to a Next Action (it still needs doing), then advance.
Runs the headless organize pipeline with the classic decoration hooks
bound off -- the item is already clarified, so it must not be re-prompted
for tags/effort/etc.  The pipeline auto-drops the Calendar-only
ORG_GTD_TIMESTAMP because next-action declares no properties.  Refuses
to mutate when the current item is no longer an overdue Calendar item
\(see `-done' for the guard's counter semantics)."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let ((marker (org-gtd-reflect-missed-calendar-review--current-overdue-marker)))
       (if (null marker)
           (message "No longer an overdue calendar item -- advancing.")
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'next-action))
         (org-gtd-reflect-missed-calendar-review--bump :migrated))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-trash ()
  "Trash the current item (irrelevant now: cancel + archive), then advance.
Reuses the `trash' type's cancel-and-archive disposition through the
headless pipeline, with decoration hooks bound off.  Refuses to mutate
when the current item is no longer an overdue Calendar item (see `-done'
for the guard's counter semantics)."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let ((marker (org-gtd-reflect-missed-calendar-review--current-overdue-marker)))
       (if (null marker)
           (message "No longer an overdue calendar item -- advancing.")
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'trash))
         (org-gtd-reflect-missed-calendar-review--bump :trashed))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review--read-future-date ()
  "Prompt for a date via `org-read-date', re-prompting until it is today or later.
Returns the chosen date as a \"YYYY-MM-DD\" string.  A past reschedule
is rejected, not silently accepted."
  (let ((today (org-today))
        date)
    (while (progn
             (setq date (org-read-date))
             (< (time-to-days (org-time-string-to-time date)) today))
      (message "That date is also in the past -- pick today or later.")
      (sit-for 1))
    date))

(defun org-gtd-reflect-missed-calendar-review-reschedule ()
  "Reschedule the current item to a new (today-or-later) date, then advance.
Stays a Calendar item; reuses the headless organize pipeline with the
decoration hooks bound off.  The :when config value is bracketed so it is
written verbatim as a valid org timestamp.  Refuses to mutate when the
current item is no longer an overdue Calendar item (see `-done' for the
guard's counter semantics); the guard runs BEFORE prompting for a date so
a doomed disposition never wastes the user's input."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let ((marker (org-gtd-reflect-missed-calendar-review--current-overdue-marker)))
       (if (null marker)
           (message "No longer an overdue calendar item -- advancing.")
         (let ((date (org-gtd-reflect-missed-calendar-review--read-future-date))
               (org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'calendar
                                    (list (cons :when (format "<%s>" date)))))
         (org-gtd-reflect-missed-calendar-review--bump :rescheduled))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-skip ()
  "Skip the current item (decide later -- not a change) and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (org-gtd-reflect-missed-calendar-review--bump :reviewed)
     (org-gtd-reflect-missed-calendar-review--bump :skipped)
     (org-gtd-walk-advance))))

(defun org-gtd-reflect-missed-calendar-review--duplicate-unavailable ()
  "Message that duplicate-clarify is not available in this review walk.
The shared `org-gtd-clarify-mode-map' binds `C-c d'/`C-c D' to
`org-gtd-clarify-duplicate'/`-exact', which enqueue a synthetic
duplicate token onto `org-gtd-clarify--duplicate-queue'.  This walk's
model has no representation for that token: `:resolve'
\(`--resolve', a plain `org-id-find') simply fails to resolve it and
`org-gtd-walk--settle' silently skips it -- the user's requested
duplicate would vanish with no feedback.  Bound over both keys in
`--install-cancel-override' instead of leaving them reachable."
  (interactive)
  (message "Duplicates are not available in the calendar review; use `c' only to clarify."))

(defun org-gtd-reflect-missed-calendar-review--install-cancel-override ()
  "Install a buffer-local override for keys unsafe in this in-place clarify.
The default `org-gtd-clarify-stop' (bound in the shared
`org-gtd-clarify-mode-map') dispatches, when a walk is active, to
`org-gtd-clarify--stop-walk', which abandons the WHOLE walk -- correct
for the inbox walk it was written for, wrong here: canceling one item's
in-place clarify must not end the whole missed-calendar review.  And
`C-c d'/`C-c D' (duplicate-clarify) would silently vanish an item the
walk model cannot represent -- see `--duplicate-unavailable'.

Installs a fresh sparse keymap parented on the shared
`org-gtd-clarify-mode-map' as the CURRENT buffer's local map, with
`C-c C-k' rebound to `--cancel-clarify' and `C-c d'/`C-c D' rebound to
`--duplicate-unavailable'.  Never mutates the shared map itself, so
other clarify buffers (including inbox-walk ones) are unaffected."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-gtd-clarify-mode-map)
    (define-key map (kbd "C-c C-k")
                #'org-gtd-reflect-missed-calendar-review--cancel-clarify)
    (define-key map (kbd "C-c d")
                #'org-gtd-reflect-missed-calendar-review--duplicate-unavailable)
    (define-key map (kbd "C-c D")
                #'org-gtd-reflect-missed-calendar-review--duplicate-unavailable)
    (use-local-map map)))

(defun org-gtd-reflect-missed-calendar-review--clarify-in-place (marker window-config)
  "Transform the current surface into an editable clarify buffer for MARKER.
Mirrors `org-gtd-inbox-walk--render-marker': copies the subtree at
MARKER into the surface (the CURRENT buffer), rekeys the WIP registry
from the surface's current key to the item's own (lazily-assigned)
clarify id, switches to `org-gtd-clarify-mode', and installs the
buffer-local `C-c C-k' cancel override (see
`--install-cancel-override') so canceling returns to the console
instead of abandoning the whole review.

`org-gtd-walk--active' is buffer-local and permanent-local, so it
survives the mode switch: when the user finishes organizing here,
`org-gtd-organize--call''s walk-active branch calls
`org-gtd-walk-advance' for us -- the review auto-advances, exactly as
the inbox walk does.

WINDOW-CONFIG is the window configuration captured by the caller
BEFORE `c' made any window changes; it is stored as the clarify
buffer-local `org-gtd-clarify--window-config' and consumed only by
`--cancel-clarify', which restores it after redrawing the console --
canceling therefore returns to exactly the window layout `c' was
pressed from.  It is not consumed on the organize/advance path: the
walk's own render takes care of that surface's display.

Deliberately does NOT bind `org-id-track-globally' around the copy --
see the long comment on `org-gtd-inbox-walk--render-marker' for why:
this surface is the live staging buffer the item is about to be
organized from, not a disposable review copy.  (See `--render' for the
other half of that tradeoff: it must fix the id's registered location
back to the source before re-resolving it on the cancel path.)"
  (let* ((surface (current-buffer))
         (old-id (or (bound-and-true-p org-gtd-clarify--clarify-id)
                     org-gtd-reflect-missed-calendar-review--surface-key))
         (new-id (org-gtd-id-get-create marker)))
    (let ((inhibit-read-only t))
      ;; The surface arrives here read-only (the console just displayed
      ;; it): the clarify buffer must be editable.  `inhibit-read-only'
      ;; above only covers the erase/copy/paste below; this `setq'
      ;; persists past this `let', which is what we want.
      (setq buffer-read-only nil)
      (erase-buffer)
      (org-gtd--without-kill-merge
        (org-gtd-clarify--initialize-buffer-contents marker surface))
      (goto-char (point-min))
      (org-gtd-wip--rekey old-id new-id)
      (unless (derived-mode-p 'org-gtd-clarify-mode)
        (org-gtd-clarify-mode))
      (setq-local org-gtd-clarify--clarify-id new-id
                  org-gtd-clarify--source-heading-marker marker
                  org-gtd-clarify--skip-refile nil
                  org-gtd-clarify--window-config window-config)
      ;; Freshly rendered, untouched: mirrors `org-gtd-inbox-walk--render-marker'
      ;; so a later save-on-quit safety net can tell an edited item from a
      ;; merely glanced one.
      (set-buffer-modified-p nil)
      (org-gtd-reflect-missed-calendar-review--install-cancel-override)
      (org-gtd-clarify-setup-windows surface))))

(defun org-gtd-reflect-missed-calendar-review--cancel-clarify ()
  "Cancel the in-place clarify on the current item; return to the console.
Bound to `C-c C-k' by the buffer-local override installed in
`--clarify-in-place' (see `--install-cancel-override'), shadowing the
default `org-gtd-clarify-stop', which would abandon the whole review.

Does not advance and does not quit: it simply redraws the console for
the CURRENT item.  All the undo work -- rekeying the surface back to
`--surface-key' and tearing down the horizons view -- lives in
`--render' itself (it runs at the top of every render), so this just
delegates to it.

Reads `org-gtd-clarify--window-config' (saved by `--clarify-in-place')
BEFORE delegating to `--render': `--render''s console mode switch calls
`kill-all-local-variables' on SURFACE, which would otherwise wipe the
buffer-local before we could read it.  Once the console is redrawn,
restores that window configuration, so canceling returns to exactly the
layout `c' was pressed from."
  (interactive)
  (let ((id (org-gtd-walk-model-current
             (plist-get org-gtd-walk--active :model)))
        (window-config (bound-and-true-p org-gtd-clarify--window-config)))
    (org-gtd-reflect-missed-calendar-review--render id (current-buffer))
    (when window-config
      (set-window-configuration window-config))))

(defun org-gtd-reflect-missed-calendar-review-clarify ()
  "Clarify the current item in place; the review auto-advances on organize.
Transforms the review surface itself into an editable clarify buffer
for the current item (see `--clarify-in-place'), instead of opening a
separate one-off clarify buffer via `org-gtd-clarify-item'.  Because
`org-gtd-walk--active' is permanent-local and survives the switch to
`org-gtd-clarify-mode', when the user finishes organizing here
`org-gtd-organize--call''s walk-active branch calls
`org-gtd-walk-advance' for us -- the review moves on to the next
overdue item automatically; no separate `s' (skip) needed.

Does not itself advance the walk or bump `:reviewed': clarifying is not
a resolution by itself, only a hand-off into the organize flow -- the
resolution (and the advance) happens when the user finishes organizing.
Clarify therefore bumps no counter on entry; see the design's \"Counters\"
section for the accepted v1 limitation (no dedicated \"clarified\"
tally).

Canceling out of the clarify (`C-c C-k') returns to the console for
this same item rather than abandoning the whole review -- see
`--cancel-clarify'.

Not wrapped in `org-gtd-walk-call-action': that helper exists for
actions that transition/advance and tears the walk down on error; this
command does neither itself (the transition happens later, on
organize), so a plain `interactive' command is correct here."
  (interactive)
  (let* ((window-config (current-window-configuration))
         (id (org-gtd-walk-model-current
              (plist-get org-gtd-walk--active :model)))
         (marker (and id (org-id-find id 'marker))))
    (if marker
        (org-gtd-reflect-missed-calendar-review--clarify-in-place marker window-config)
      (message "This item is no longer available."))))

(defun org-gtd-reflect-missed-calendar-review-quit ()
  "Abandon the review: report the tally, clean up, tear down the walk, and save.
Whatever mutating dispositions ran before quitting must still be persisted;
see `org-gtd-reflect-missed-calendar-review--on-finish'."
  (interactive)
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-walk-quit)
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)
    (message "Missed-calendar review complete. %s" summary)
    (org-gtd-save-buffers)))

;;;; Footer

(provide 'org-gtd-reflect-missed-calendar-review)

;;; org-gtd-reflect-missed-calendar-review.el ends here
