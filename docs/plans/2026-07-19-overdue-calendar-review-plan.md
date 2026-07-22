# Overdue Calendar Review (REC-UI-04) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build `org-gtd-reflect-missed-calendar-review`, an actionable walk-engine
console that walks each overdue Calendar item one at a time and lets the user decide,
with consent, what each one becomes now (done / migrate / reschedule / trash / clarify /
skip).

**Architecture:** A new module, `org-gtd-reflect-missed-calendar-review.el`, that is a
*walk consumer* — structurally a clone of `org-gtd-someday-review.el`. It registers a spec
into the `org-gtd-walks` registry and drives it through the generic engine
(`org-gtd-walk.el`). Detection reuses the exact `org-gtd-skip.el` factory predicates the
read-only missed-calendar view composes. Mutating dispositions reuse the headless
`org-gtd-process-heading` pipeline with the decoration hooks bound off (the item is already
clarified).

**Tech Stack:** Emacs Lisp; the e-unit test framework (`deftest`, run **only** via the
project `/test` skill); the walk engine (`org-gtd-walk.el`, `org-gtd-walk-model.el`); the
organize pipeline (`org-gtd-organize-core.el`).

---

## Orientation for the executor (read before Task 1)

You know Elisp but nothing about this codebase. Read these files first — the new module is
modeled directly on them:

- **`org-gtd-someday-review.el`** — the canonical walk-consumer template. Your new module
  mirrors its shape: a fixed WIP surface key, buffer-local counters, a mode + keymap, a
  `--find-items`/`--make-find`/`--resolve`/`--render`/`--surface`/`--bump`/`--on-finish`/`--spec`
  set, a `(org-gtd-walk-register ...)` call, the mode, and an autoloaded entry command that
  calls `org-gtd-walk-start`.
- **`org-gtd-walk.el`** — the engine. Key entry points you will call: `org-gtd-walk-start`,
  `org-gtd-walk-advance`, `org-gtd-walk-quit`, `org-gtd-walk-call-action`,
  `org-gtd-walk-register`, `org-gtd-walk-get`, `org-gtd-walk-model-current`. Note the
  `:scope` concurrency lock and the stale-handle auto-skip in `org-gtd-walk--settle` (a
  `:resolve` that returns nil makes the engine skip that item automatically).
- **`org-gtd-organize-core.el`** — `org-gtd-process-heading (pom type &optional config)` is
  the headless organize pipeline. Step 4 of its pipeline runs `org-gtd-organize-apply-hooks`
  which iterates the dynamic variable `org-gtd-organize-hooks`; binding it to nil at the
  call site suppresses re-decoration.
- **`org-gtd-skip.el`** — the predicate factories. Each returns a *closure*; capture the
  closure once in an outer `let`, then `funcall` it per heading (do not rebuild it inside
  the scan loop).
- **`test/unit/someday-review-test.el`** — the harness you copy: `deftest`, the
  `around-each` that wraps every test in `ogt-eunit-with-mock-gtd`, and the pattern of
  starting a walk then driving commands inside `(with-current-buffer (car
  (org-gtd-wip--get-buffers)) ...)`.

### Verified facts (these override the design doc where they differ)

The design doc (`docs/plans/2026-07-19-overdue-calendar-review-design.md`) names some
functions loosely. Use these **verified** names/values instead:

1. **Overdue predicate name.** The design's `org-gtd-pred--property-before-date` does **not
   exist**. The real "timestamp before today" factory is
   `org-gtd-pred--property-ts< (property reference-date)` and the view composes overdue as
   `(org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "today")`. Use that.
2. **The Calendar timestamp property** is `"ORG_GTD_TIMESTAMP"`. Get it via
   `(org-gtd-type-property 'calendar :when)` (returns `"ORG_GTD_TIMESTAMP"`) for DRY, exactly
   as the view language does.
3. **The ORG_GTD value strings** come from `(org-gtd-type-org-gtd-value 'calendar)` →
   `"Calendar"` and `(org-gtd-type-org-gtd-value 'habit)` → `"Habit"`. Do not hardcode.
4. **`not-habit` filter.** The view language implements `not-habit` as
   `(org-gtd-pred--property-not-equals "ORG_GTD" (org-gtd-type-org-gtd-value 'habit))`. Reuse
   that. (Note: because the find already requires `ORG_GTD = "Calendar"`, this predicate is
   logically redundant — an item can't be both Calendar and Habit — but we include it for
   parity with the design's stated definition of "overdue calendar.")
5. **Reschedule config value must include the angle brackets.**
   `org-gtd-configure-as-type` writes the `:when` value verbatim into `ORG_GTD_TIMESTAMP`,
   so the config must be `(list (cons :when (format "<%s>" date)))`, i.e. `"<2026-08-01>"`,
   never a bare `"2026-08-01"`. This mirrors `org-gtd-calendar` in `org-gtd-calendar.el`.
6. **Done path needs no manual subtree-kill guard.** `org-gtd-archive-item-at-point`
   (`org-gtd-archive.el`) already wraps `org-archive-subtree` in `org-gtd--without-kill-merge`
   internally. The `d` command just calls `(org-todo (org-gtd-keywords--done))` then
   `(org-gtd-archive-item-at-point)`. The `--render` copy path, however, *does* need the
   `org-gtd--without-kill-merge` wrapper around its `org-copy-subtree`, same as someday-review.

### Testing protocol (applies to EVERY task)

- **Never run `eldev` directly.** Run tests **only** through the project `/test` skill.
- The test command in every task below is written as: **Run the /test skill on
  `test/unit/missed-calendar-review-test.el`**. Do exactly that.
- Framework is **e-unit**: use `deftest`, `assert-equal`, `assert-true`, `assert-nil`,
  `assert-match`, `assert-same`. Not ERT.
- All behavioral tests must run inside the `ogt-eunit-with-mock-gtd` context via the
  `around-each` hook (copy it verbatim from `someday-review-test.el`).

### Test fixture helper (used by many tasks)

Because `make-task` (in `test/helpers/builders.el`) hardcodes `:ORG_GTD: Actions`, do **not**
use it for Calendar fixtures. Instead add this small helper near the top of the test file
(after the `around-each`) and use it to seed overdue Calendar items directly into the
default GTD file:

```elisp
(defun mcr-test--make-calendar (title timestamp)
  "Insert a Calendar item TITLE with ORG_GTD_TIMESTAMP TIMESTAMP into the GTD file.
TIMESTAMP is a full org stamp string, e.g. \"<2020-01-01>\".  Returns the id."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (let ((id (org-id-uuid)))
      (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: %s\n:END:\n"
                      title id timestamp))
      (save-buffer)
      id)))
```

A "past" stamp is any date before today, e.g. `"<2020-01-01>"`. A "future" stamp is e.g.
`"<2099-01-01>"`. For "today", compute `(format-time-string "<%Y-%m-%d>")`.

---

## Task 1: Module skeleton + detection (`:find` / `:resolve`) + load wiring

**Files:**
- Create: `org-gtd-reflect-missed-calendar-review.el`
- Modify: `org-gtd.el` (add one `require` after line 84)
- Create: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing tests**

Create `test/unit/missed-calendar-review-test.el`:

```elisp
;;; missed-calendar-review-test.el --- Tests for overdue calendar review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for the actionable overdue-calendar review walk (REC-UI-04).

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-reflect-missed-calendar-review)
(require 'org-gtd-walk)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun mcr-test--make-calendar (title timestamp)
  "Insert a Calendar item TITLE with ORG_GTD_TIMESTAMP TIMESTAMP into the GTD file.
Returns the id."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (let ((id (org-id-uuid)))
      (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: %s\n:END:\n"
                      title id timestamp))
      (save-buffer)
      id)))

;;; Detection

(deftest mcr/find-includes-overdue-calendar ()
  "An open Calendar item dated before today is detected."
  (mcr-test--make-calendar "Dentist" "<2020-01-01>")
  (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-today-and-future ()
  "A Calendar item dated today or in the future is not overdue."
  (mcr-test--make-calendar "Today thing" (format-time-string "<%Y-%m-%d>"))
  (mcr-test--make-calendar "Future thing" "<2099-01-01>")
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-done ()
  "A done Calendar item is not detected."
  (let ((id (mcr-test--make-calendar "Happened" "<2020-01-01>")))
    (let ((m (org-id-find id 'marker)))
      (org-with-point-at m
        (let ((org-inhibit-logging t)) (org-todo (org-gtd-keywords--done)))
        (save-buffer))))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-non-calendar ()
  "A non-Calendar heading with a past timestamp is not detected."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* A next action\n:PROPERTIES:\n:ID: mcr-na-1\n:ORG_GTD: Actions\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:END:\n")
    (save-buffer))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-habit ()
  "A Habit heading with a past timestamp is not detected."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* A routine\n:PROPERTIES:\n:ID: mcr-hab-1\n:ORG_GTD: Habit\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:STYLE: habit\n:END:\n")
    (save-buffer))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-includes-repeating-calendar ()
  "A repeating Calendar item with a past base date is included (view parity)."
  (mcr-test--make-calendar "Weekly sync" "<2020-01-01 +1w>")
  (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/resolve-rejects-missing-id ()
  "The :resolve predicate is nil for an unknown id, non-nil for a real one."
  (let ((id (mcr-test--make-calendar "Real" "<2020-01-01>")))
    (assert-true (org-gtd-reflect-missed-calendar-review--resolve id))
    (assert-nil (org-gtd-reflect-missed-calendar-review--resolve "no-such-id-xyz"))))

(provide 'missed-calendar-review-test)

;;; missed-calendar-review-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — `Cannot open load file: org-gtd-reflect-missed-calendar-review` (the module
does not exist yet).

**Step 3: Write the module skeleton + detection**

Create `org-gtd-reflect-missed-calendar-review.el`:

```elisp
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

;;;; Detection

(defun org-gtd-reflect-missed-calendar-review--find-items ()
  "Return the org-ids of every overdue Calendar item across `org-agenda-files'.

Composes the SAME `org-gtd-skip.el' predicates the read-only
`org-gtd-reflect-missed-calendar' view uses: ORG_GTD = Calendar, not
done, ORG_GTD_TIMESTAMP strictly before today, and not an org-gtd habit.
The predicate factories are captured once in the outer `let' and
`funcall'ed per heading."
  (let ((calendar-p (org-gtd-pred--property-equals
                     "ORG_GTD" (org-gtd-type-org-gtd-value 'calendar)))
        (not-done-p (org-gtd-pred--not-done))
        (overdue-p (org-gtd-pred--property-ts<
                    (org-gtd-type-property 'calendar :when) "today"))
        (not-habit-p (org-gtd-pred--property-not-equals
                      "ORG_GTD" (org-gtd-type-org-gtd-value 'habit)))
        (items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (and (funcall calendar-p)
                        (funcall not-done-p)
                        (funcall overdue-p)
                        (funcall not-habit-p))
               (push (org-id-get-create) items)))))))
    (nreverse items)))

(defun org-gtd-reflect-missed-calendar-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))

;;;; Footer

(provide 'org-gtd-reflect-missed-calendar-review)

;;; org-gtd-reflect-missed-calendar-review.el ends here
```

**Step 4: Wire the require into `org-gtd.el`**

In `org-gtd.el`, immediately after the line `(require 'org-gtd-someday-review)` (line 84),
add:

```elisp
(require 'org-gtd-reflect-missed-calendar-review)
```

**Step 5: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS (all 7 detection/resolve tests green).

**Step 6: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el org-gtd.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add overdue-calendar-review module skeleton + detection"
```

---

## Task 2: Mode, keymap, and read-only render

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing tests**

Append to the test file (before the `(provide ...)` line):

```elisp
;;; Mode + keymap

(deftest mcr/mode-is-derived-from-org-mode ()
  "Review mode is derived from org-mode."
  (with-temp-buffer
    (org-gtd-reflect-missed-calendar-review-mode)
    (assert-true (derived-mode-p 'org-mode))))

(deftest mcr/mode-has-disposition-keybindings ()
  "The mode keymap binds every disposition key to its command."
  (let ((map org-gtd-reflect-missed-calendar-review-mode-map))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-done
                  (lookup-key map (kbd "d")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-migrate
                  (lookup-key map (kbd "m")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-reschedule
                  (lookup-key map (kbd "r")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-trash
                  (lookup-key map (kbd "t")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-clarify
                  (lookup-key map (kbd "c")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-skip
                  (lookup-key map (kbd "s")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-quit
                  (lookup-key map (kbd "q")))))

;;; Render

(deftest mcr/render-fills-surface-read-only ()
  "Render draws the item, activates review mode read-only, humanizes the
lapse, and advertises the disposition keys."
  (let* ((id (mcr-test--make-calendar "Overdue thing" "<2020-01-01>"))
         (surface (org-gtd-wip--get-buffer
                   org-gtd-reflect-missed-calendar-review--surface-key)))
    (with-current-buffer surface
      (setq-local org-gtd-walk--active
                  (list :model (org-gtd-walk-model-create (list id))))
      (org-gtd-reflect-missed-calendar-review--render id surface)
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Overdue thing" (buffer-string))
      (assert-match "days ago" (buffer-string))
      (assert-match "\\[d\\] Done" header-line-format)
      (assert-match "\\[r\\] Reschedule" header-line-format)
      (assert-match "(1/1)" header-line-format))
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)))
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — the mode, keymap, and render function are undefined.

**Step 3: Implement mode, keymap, humanize helper, and render**

In `org-gtd-reflect-missed-calendar-review.el`, add a `;;;; Keymaps` section after the
`;;;; Variables` section:

```elisp
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
```

Add a `;;;; Render` section after the `;;;; Detection` section:

```elisp
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
displays the buffer."
  (let ((marker (org-id-find id 'marker)))
    (when marker
      (let ((ts (org-with-point-at marker
                  (org-entry-get (point) (org-gtd-type-property 'calendar :when))))
            (aof (org-with-point-at marker
                   (org-entry-get (point) org-gtd-prop-area-of-focus))))
        (with-current-buffer surface
          (let ((inhibit-read-only t)
                ;; SURFACE is a disposable read-only review copy; suppress
                ;; org-paste-subtree's id tracking so it never re-registers
                ;; the pasted :ID: against this copy's temp file (mirrors
                ;; someday-review--render).
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
```

Add a `;;;; Modes` section just before the `;;;; Footer`:

```elisp
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
```

**Step 4: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS (mode, keymap, and render tests green; Task 1 tests still green).

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add overdue-calendar-review mode, keymap, and render"
```

---

## Task 3: Counters, surface, spec + registration, entry command, quit, empty state

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing tests**

Append to the test file:

```elisp
;;; Spec registration + entry + empty state

(deftest mcr/registers-a-walk-consumer ()
  "Loading the module registers a `missed-calendar-review' walk."
  (let ((spec (org-gtd-walk-get 'missed-calendar-review)))
    (assert-true spec)
    (assert-same 'missed-calendar-review (plist-get spec :name))
    (assert-true (org-gtd-walk--callable-p (plist-get spec :render)))
    (assert-true (org-gtd-walk--callable-p (plist-get spec :find)))))

(deftest mcr/entry-opens-console-when-items-exist ()
  "The entry command opens a read-only review surface for the item."
  (mcr-test--make-calendar "Review me" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))
      (org-gtd-reflect-missed-calendar-review-quit))))

(deftest mcr/entry-empty-state-opens-no-console ()
  "With no overdue calendar items, the console never opens."
  (org-gtd-reflect-missed-calendar-review)
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

(deftest mcr/quit-cleans-up-surface ()
  "Quit tears down the walk and cleans up the surface buffer."
  (mcr-test--make-calendar "Item" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-reflect-missed-calendar-review-quit))
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — the spec, registration, entry command, and quit command are undefined.

**Step 3: Implement counters, surface, spec, registration, entry, quit**

Add a `defvar-local` in the `;;;; Variables` section (after the surface-key const):

```elisp
(defvar-local org-gtd-reflect-missed-calendar-review--counters nil
  "Buffer-local plist of tallies for the active surface:
\(:reviewed N :done N :migrated N :rescheduled N :trashed N :skipped N).")
```

Add a `;;;; Walk Surface` section after `;;;; Render`:

```elisp
;;;; Walk Surface

(defun org-gtd-reflect-missed-calendar-review--surface ()
  "Return the fresh WIP surface buffer for a missed-calendar-review walk.
Activates the mode and initializes the buffer-local counters before the
walk starts, so :render's own mode-activation guard never fires and the
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
  "End-of-walk: report the tally and clean up the surface buffer.
Runs in the surface buffer after the engine has cleared its session."
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)
    (message "Missed-calendar review complete. %s" summary)))

(defun org-gtd-reflect-missed-calendar-review--spec ()
  "Return the missed-calendar-review walk spec template."
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
```

Add a `;;;; Entry Point` section (place it after `;;;; Modes`):

```elisp
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
        (org-gtd-walk-start spec
                            (org-gtd-reflect-missed-calendar-review--surface))))))
```

Add a `;;;; Commands` section (place it after `;;;; Entry Point`), starting with just the
quit command (the disposition commands come in Tasks 4-8):

```elisp
;;;; Commands

(defun org-gtd-reflect-missed-calendar-review-quit ()
  "Abandon the review: report the tally, clean up, tear down the walk."
  (interactive)
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-walk-quit)
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)
    (message "Missed-calendar review complete. %s" summary)))
```

**Step 4: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS (registration, entry, empty-state, quit tests green; earlier tests green).

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: register overdue-calendar-review walk + entry/quit/empty-state"
```

---

## Task 4: `d` — done disposition

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing test**

Append to the test file:

```elisp
;;; Disposition: done

(deftest mcr/done-archives-and-advances ()
  "`d' marks the item done, archives it, and ends the walk on the last item."
  (let ((id (mcr-test--make-calendar "It happened" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-done))
    ;; Only item -> walk finished -> surface cleaned up.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))
    ;; The item is no longer detected as overdue (it was archived away).
    (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items)))
    (ignore id)))
```

**Step 2: Run test to verify it fails**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — `org-gtd-reflect-missed-calendar-review-done` is undefined.

**Step 3: Implement the done command**

In the `;;;; Commands` section, add:

```elisp
(defun org-gtd-reflect-missed-calendar-review-done ()
  "Mark the current item done and archive it (it happened), then advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker
           (org-todo (org-gtd-keywords--done))
           (org-gtd-archive-item-at-point)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :done)
       (org-gtd-walk-advance)))))
```

**Step 4: Run test to verify it passes**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add done disposition to overdue-calendar-review"
```

---

## Task 5: `m` — migrate to Next Action (with hook suppression)

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing tests**

Append to the test file:

```elisp
;;; Disposition: migrate

(deftest mcr/migrate-retypes-to-next-action ()
  "`m' migrates the item to a next action: ORG_GTD=Actions, NEXT state,
ORG_GTD_TIMESTAMP dropped."
  (let ((id (mcr-test--make-calendar "Still need to do this" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate))
    (let ((marker (org-id-find id 'marker)))
      (assert-true marker)
      (org-with-point-at marker
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
        (assert-equal (org-gtd-keywords--next) (org-get-todo-state))
        (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))))

(deftest mcr/migrate-suppresses-organize-hooks ()
  "Migrate binds `org-gtd-organize-hooks' off, so decoration hooks never fire."
  (mcr-test--make-calendar "No re-prompt" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let* ((fired nil)
         (org-gtd-organize-hooks (list (lambda () (setq fired t)))))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate))
    (assert-nil fired)))
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — `org-gtd-reflect-missed-calendar-review-migrate` is undefined.

**Step 3: Implement the migrate command**

In the `;;;; Commands` section, add:

```elisp
(defun org-gtd-reflect-missed-calendar-review-migrate ()
  "Migrate the current item to a Next Action (it still needs doing), then advance.
Runs the headless organize pipeline with the classic decoration hooks
bound off -- the item is already clarified, so it must not be re-prompted
for tags/effort/etc.  The pipeline auto-drops the Calendar-only
ORG_GTD_TIMESTAMP because next-action declares no properties."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'next-action)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :migrated)
       (org-gtd-walk-advance)))))
```

**Step 4: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add migrate-to-next-action disposition (hooks suppressed)"
```

---

## Task 6: `r` — reschedule (with past-date re-prompt guard)

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing tests**

Append to the test file:

```elisp
;;; Disposition: reschedule

(deftest mcr/read-future-date-rejects-past ()
  "The date reader re-prompts until the chosen date is today-or-later."
  (let ((answers (list "2000-01-01" "2999-01-01")))
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'sit-for) (lambda (&rest _) t)))
      (assert-equal "2999-01-01"
                    (org-gtd-reflect-missed-calendar-review--read-future-date))
      ;; both answers consumed => it looped past the in-the-past first answer.
      (assert-nil answers))))

(deftest mcr/reschedule-sets-new-future-timestamp ()
  "`r' keeps the item a Calendar item and writes the new (bracketed) timestamp."
  (let ((id (mcr-test--make-calendar "Needs new date" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) "2999-01-01")))
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-reflect-missed-calendar-review-reschedule)))
    (let ((marker (org-id-find id 'marker)))
      (assert-true marker)
      (org-with-point-at marker
        (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "<2999-01-01>"
                      (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))))
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — `--read-future-date` and the reschedule command are undefined.

**Step 3: Implement the date reader and the reschedule command**

In the `;;;; Commands` section, add both:

```elisp
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
written verbatim as a valid org timestamp."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker))
            (date (org-gtd-reflect-missed-calendar-review--read-future-date)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'calendar
                                    (list (cons :when (format "<%s>" date))))))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :rescheduled)
       (org-gtd-walk-advance)))))
```

**Step 4: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add reschedule disposition with past-date re-prompt guard"
```

---

## Task 7: `t` — trash disposition

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

**Step 1: Write the failing test**

Append to the test file:

```elisp
;;; Disposition: trash

(deftest mcr/trash-cancels-and-archives ()
  "`t' cancels + archives the item (irrelevant now) and ends the walk."
  (mcr-test--make-calendar "No longer relevant" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-reflect-missed-calendar-review-trash))
  (assert-equal 0 (length (org-gtd-wip--get-buffers)))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))
```

**Step 2: Run test to verify it fails**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — `org-gtd-reflect-missed-calendar-review-trash` is undefined.

**Step 3: Implement the trash command**

In the `;;;; Commands` section, add:

```elisp
(defun org-gtd-reflect-missed-calendar-review-trash ()
  "Trash the current item (irrelevant now: cancel + archive), then advance.
Reuses the `trash' type's cancel-and-archive disposition through the
headless pipeline, with decoration hooks bound off."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'trash)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :trashed)
       (org-gtd-walk-advance)))))
```

**Step 4: Run test to verify it passes**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add trash disposition to overdue-calendar-review"
```

---

## Task 8: `s` — skip and `c` — clarify

**Files:**
- Modify: `org-gtd-reflect-missed-calendar-review.el`
- Test: `test/unit/missed-calendar-review-test.el`

> **Product-decision caveat (surface to the maintainer, do not silently change).** The
> design says `c` runs `org-gtd-clarify-item` on the item and that "the review continues."
> Opening the full interactive clarify flow *while the walk's `:scope` lock is held over the
> same agenda files* is genuinely messy (clarify + organize would contend with the walk).
> This plan implements the least-messy version that satisfies the walk invariants: **advance
> the walk first, then open clarify on the item** (so on the last item the walk finishes and
> unlocks before clarify opens). If the maintainer wants clarify to instead *quit* the walk
> entirely first, that is a one-line change — flag it in review.

**Step 1: Write the failing tests**

Append to the test file:

```elisp
;;; Disposition: skip

(deftest mcr/skip-advances-without-changing-item ()
  "`s' advances without mutating the item; the item is still overdue."
  (let ((id (mcr-test--make-calendar "Decide later" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-skip))
    ;; Only item -> walk finished -> surface cleaned up, but item unchanged.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))
    (let ((marker (org-id-find id 'marker)))
      (org-with-point-at marker
        (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "<2020-01-01>"
                      (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))
    ;; Still detected on a fresh run (skip is "not now", not "never").
    (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items)))))

(deftest mcr/skip-counts-a-skip ()
  "`s' increments the skipped counter (checked mid-walk, two items)."
  (mcr-test--make-calendar "First" "<2020-01-01>")
  (mcr-test--make-calendar "Second" "<2020-01-02>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-skip)
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :skipped))
      (org-gtd-reflect-missed-calendar-review-quit))))

;;; Disposition: clarify

(deftest mcr/clarify-invokes-clarify-item-and-advances ()
  "`c' calls `org-gtd-clarify-item' on the item and advances the walk."
  (let ((clarified nil))
    (mcr-test--make-calendar "Rethink me" "<2020-01-01>")
    (org-gtd-reflect-missed-calendar-review)
    (cl-letf (((symbol-function 'org-gtd-clarify-item)
               (lambda (&rest _) (setq clarified t))))
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-reflect-missed-calendar-review-clarify)))
    (assert-true clarified)
    ;; Only item -> walk advanced off the end -> surface cleaned up.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))
```

**Step 2: Run tests to verify they fail**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: FAIL — the skip and clarify commands are undefined.

**Step 3: Implement the skip and clarify commands**

In the `;;;; Commands` section, add:

```elisp
(defun org-gtd-reflect-missed-calendar-review-skip ()
  "Skip the current item (decide later -- not a change) and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (org-gtd-reflect-missed-calendar-review--bump :reviewed)
     (org-gtd-reflect-missed-calendar-review--bump :skipped)
     (org-gtd-walk-advance))))

(defun org-gtd-reflect-missed-calendar-review-clarify ()
  "Clarify the current item fully (the heavy escape hatch), then advance.
Advances the walk first so that on the last item the walk finishes and
releases its scope lock before the interactive clarify flow opens."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-walk-advance)
       (when marker
         (org-gtd-clarify-item marker))))))
```

**Step 4: Run tests to verify they pass**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-reflect-missed-calendar-review.el test/unit/missed-calendar-review-test.el
git commit -m "feat: add skip and clarify dispositions to overdue-calendar-review"
```

---

## Task 9: Counters tally end-to-end + on-finish cleanup

**Files:**
- Test: `test/unit/missed-calendar-review-test.el` (test-only task — no production change
  expected; if a test fails, fix the production code)

**Step 1: Write the failing test**

Append to the test file:

```elisp
;;; Counters + finish

(deftest mcr/counters-tally-across-dispositions ()
  "Mixed dispositions across several items tally correctly on the surface."
  (mcr-test--make-calendar "One"   "<2020-01-01>")
  (mcr-test--make-calendar "Two"   "<2020-01-02>")
  (mcr-test--make-calendar "Three" "<2020-01-03>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    ;; item 1 -> skip, item 2 -> migrate, item 3 -> done (finishes the walk)
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-skip))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-migrate))
    ;; Read counters BEFORE the last disposition finishes+cleans up the surface.
    (with-current-buffer surface
      (assert-same 2 (plist-get org-gtd-reflect-missed-calendar-review--counters :reviewed))
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :skipped))
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :migrated))
      (org-gtd-reflect-missed-calendar-review-done))
    ;; Last disposition ran :on-finish, which cleaned up the surface buffer.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))
```

**Step 2: Run test to verify it fails (or passes immediately)**

Run the /test skill on `test/unit/missed-calendar-review-test.el`.
Expected: PASS if Tasks 3-8 are correct. If it fails, debug the counter/`on-finish`
wiring (do **not** weaken the test to make it pass). Use superpowers:systematic-debugging
if needed.

**Step 3: Commit**

```bash
git add test/unit/missed-calendar-review-test.el
git commit -m "test: cover counter tally + on-finish cleanup for overdue-calendar-review"
```

---

## Task 10: Lint, byte-compile clean, and full suite

**Files:**
- Possibly modify: `org-gtd-reflect-missed-calendar-review.el` (fix any lint/compile warnings)

**Step 1: Byte-compile the new module clean**

Run:

```bash
~/bin/eldev clean && ~/bin/eldev compile --warnings-as-errors
```

Expected: no errors, no warnings for `org-gtd-reflect-missed-calendar-review.el`. Fix any
undefined-function/unused-lexical warnings (e.g. add missing `declare-function` forms) until
clean. The autoload cookie on the entry command and on the mode must be present — confirm
`org-gtd-autoloads` regenerates without error.

**Step 2: Run the full test suite via the /test skill**

Run the /test skill on `test/unit/missed-calendar-review-test.el` one final time, then run
the /test skill across the whole suite (no file argument) to confirm no regressions in
sibling walk consumers (`someday-review-test.el`, `inbox-walk-test.el`) or the reflect view
tests.

Expected: all green.

**Step 3: Commit any fixups**

```bash
git add -A
git commit -m "chore: byte-compile-clean overdue-calendar-review module"
```

---

## Definition of done

- `M-x org-gtd-reflect-missed-calendar-review` opens a read-only console over overdue
  Calendar items, or messages the clean-landscape line when there are none.
- Keys `d m r t c s q` behave per the design's disposition table; mutating dispositions
  suppress `org-gtd-organize-hooks`; reschedule rejects past dates.
- Buffer-local counters tally `reviewed / done / migrated / rescheduled / trashed / skipped`
  and `:on-finish` reports them and cleans up the surface.
- The walk is registered under `'missed-calendar-review`, scoped to `(org-agenda-files)`
  (mutually exclusive with the someday review), `:resumable nil`.
- New module is `require`d from `org-gtd.el`; entry command and mode are autoloaded.
- Byte-compiles clean; the full test suite is green.

## Deferred (explicitly NOT in this plan — do not build)

- Command-center row and Weekly-Review-step embedding (design §10).
- Resume / back-with-undo (design §10 — the walk is `:resumable nil` by design).
- A general `org-gtd-retype` abstraction (design §10 — YAGNI; migrate rides
  `org-gtd-process-heading`).
