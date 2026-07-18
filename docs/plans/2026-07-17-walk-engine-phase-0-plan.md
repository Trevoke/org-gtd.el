# Walk Engine — Phase 0 (Engine Core) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the org-gtd walk engine's headless core — a pure serializable walk
model (Tier 1) plus a generic session driver, `org-gtd-walks` registry, scope
identity/locking, and opt-in checkpointing (Tier 2) — with **zero** consumers and
touching **zero** existing features. Everything is new files.

**Architecture:** Two new modules. `org-gtd-walk-model.el` is the pure model
(plain plist data + pure functions; no buffers, no org, no I/O). `org-gtd-walk.el`
is the driver + registry + scope lock + checkpoint file I/O; it owns the one place
walk state moves (render + checkpoint + transition) and renders into a
caller-provided surface. Consumers register a spec `(:name :find :render :actions
:on-finish :resumable :resolve :scope)` into the `org-gtd-walks` alist — Phase 0
defines and validates the spec/registry but registers **no real consumers**.

**Tech Stack:** Emacs Lisp (lexical-binding); e-unit test framework (`deftest`,
`assert-*`); Eldev build/test; `prin1`/`read` for serialization. No runtime
dependencies beyond built-in `seq`/`subr-x` and existing org-gtd modules.

**Source of truth:** `docs/plans/2026-07-17-walk-engine-design.md` (read §4–§10 and
§12 before starting). This plan implements the "Phase 0 — Engine core" bullet of §12.

---

## Conventions the implementer MUST follow

- **Run tests ONLY via** `.claude/skills/test/run-tests.sh <target>`. Never call
  `eldev etest` directly. Single-file form:
  `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
  The runner prints a clean pass/fail summary and the seed used.
- **Compile check:** `~/bin/eldev compile --warnings-as-errors` (run in the final
  task; must be warning-free).
- **e-unit assertions available:** `assert-equal`, `assert-same` (eq), `assert-true`,
  `assert-nil`, `assert-false`, `assert-match`, `assert-raises` (form:
  `(assert-raises 'error BODY...)`), `assert-same-items`, `assert-not-equal`.
- **Test files** live in `test/unit/` and are auto-discovered by Eldev. Each starts
  with the e-unit prelude (see Task 1) and ends with `(provide 'NAME)` +
  `;;; NAME.el ends here`.
- **Tier 1 model tests must NOT** `(require 'org-gtd)` or use
  `ogt-eunit-with-mock-gtd`, mock-fs, or any buffer/org setup. They require only
  `e-unit` and `org-gtd-walk-model`.
- **Module file boilerplate** (header + footer): copy the exact shape from
  `org-gtd-someday-review.el` lines 1–34 and 290–294. Every new `.el` needs:
  - first line `;;; NAME.el --- SHORT DESC -*- lexical-binding: t; coding: utf-8 -*-`
  - the copyright / license block (copy verbatim, update the `--- DESC`)
  - `;;; Commentary:` + `;;; Code:` sections
  - a `;;;; Footer` with `(provide 'NAME)` and `;;; NAME.el ends here`
- **Registry pattern** mirrors `org-gtd-types` / `org-gtd-customize-type` in
  `org-gtd-types.el` (an alist `(name . plist)` with a register/accessor API).
- **Commits:** one per green task, conventional-commit style
  (`feat:` / `test:` — prefer `feat:` since test+impl land together).
  End every commit message with the trailer:
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
- Work on branch `walk-engine-design` (already checked out).

## The walk model — exact semantics (design §4)

A model is a **plist**: `(:entries LIST :cursor INT :meta LIST)`.

- `:entries` — ordered list of opaque **serializable** handles (strings, symbols,
  or numbers — org-ids in practice). Never markers/buffers/functions.
- `:cursor` — index of the **current** item. `0 ≤ cursor ≤ (length entries)`.
  `cursor == (length entries)` means **done** (ran off the end).
- Items at indices `< cursor` are handled; index `cursor` is current; `> cursor`
  are pending.

Pure ops (take a model, return a **new** model; never mutate input):

| op | result |
|----|--------|
| `org-gtd-walk-model-current` | `(nth cursor entries)`, or `nil` when done |
| `org-gtd-walk-model-done-p` | `cursor ≥ (length entries)` |
| `org-gtd-walk-model-remaining` | `(max 0 (- (length entries) cursor))` |
| `org-gtd-walk-model-advance` | new model, `cursor+1` (never past length) |
| `org-gtd-walk-model-enqueue(m handle where)` | insert `handle`, see below |

**Enqueue insertion index** (clamped to `[0, (length entries)]`). Two positions,
**both insert after the current item** (into the remaining queue):

- `top` → insert at index `(1+ cursor)` (front of the remaining queue: handled
  **next**, right after the current item; the current item is unchanged).
- `bottom` → insert at index `(length entries)` (handled last).

Any other `where` → signal `error`. Both positions insert *after* the cursor
because the only real caller — inbox's duplicate-queue — enqueues a duplicate
*while the current item is still being processed* (before `advance`); inserting
*at* the cursor would make the duplicate current mid-process, and the subsequent
`advance` would land back on the just-handled item. So duplicates must land after
the cursor. This maps directly onto today's
`org-gtd-clarify-duplicate-queue-position` (`top`/`bottom`, default `bottom`).
"Skip but revisit later" = `advance` + `enqueue(handle, bottom)`.

---

# GROUP A — Walk model (Tier 1, pure)

Files for the whole group:
- Create: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

---

### Task 1: Create the model module + `org-gtd-walk-model-create`

**Files:**
- Create: `org-gtd-walk-model.el`
- Create: `test/unit/walk-model-test.el`

**Step 1: Write the failing test**

Create `test/unit/walk-model-test.el`:

```elisp
;;; walk-model-test.el --- Unit tests for the pure walk model -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier 1 tests for the pure walk model (org-gtd-walk-model).
;; These are pure unit tests on plain lists: NO org, NO buffers, NO mock-fs,
;; NO ogt-eunit-with-mock-gtd.  They must not be able to flake.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk-model)

(e-unit-initialize)

;;; org-gtd-walk-model-create

(deftest walk-model-create-builds-a-fresh-model ()
  "create seeds entries, cursor 0, and meta."
  (let ((m (org-gtd-walk-model-create '("a" "b" "c") '(:tag foo))))
    (assert-equal '("a" "b" "c") (plist-get m :entries))
    (assert-same 0 (plist-get m :cursor))
    (assert-equal '(:tag foo) (plist-get m :meta))))

(deftest walk-model-create-defaults-meta-to-nil ()
  "meta defaults to nil when omitted."
  (let ((m (org-gtd-walk-model-create '("a"))))
    (assert-nil (plist-get m :meta))))

(deftest walk-model-create-copies-entries ()
  "create does not retain the caller's list object."
  (let* ((src (list "a" "b"))
         (m (org-gtd-walk-model-create src)))
    (setcar src "MUTATED")
    (assert-equal '("a" "b") (plist-get m :entries))))

(provide 'walk-model-test)

;;; walk-model-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `Cannot open load file` / `org-gtd-walk-model` feature not
provided (the module doesn't exist yet).

**Step 3: Write minimal implementation**

Create `org-gtd-walk-model.el` with the standard header/footer and:

```elisp
;;; org-gtd-walk-model.el --- Pure headless walk model -*- lexical-binding: t; coding: utf-8 -*-
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

;;;; Footer

(provide 'org-gtd-walk-model)

;;; org-gtd-walk-model.el ends here
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS (3 tests).

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add pure walk model with create"
```

---

### Task 2: `org-gtd-walk-model-current` + `-done-p` + `-remaining`

**Files:**
- Modify: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test** (append before the `(provide ...)`):

```elisp
;;; cursor queries

(deftest walk-model-current-returns-handle-at-cursor ()
  "current returns the entry at the cursor."
  (let ((m (org-gtd-walk-model-create '("a" "b" "c"))))
    (assert-equal "a" (org-gtd-walk-model-current m))))

(deftest walk-model-current-is-nil-when-done ()
  "current returns nil once the cursor is past the last entry."
  (let ((m (list :entries '("a") :cursor 1 :meta nil)))
    (assert-nil (org-gtd-walk-model-current m))))

(deftest walk-model-done-p-false-mid-walk ()
  "done-p is nil while the cursor points at an entry."
  (assert-nil (org-gtd-walk-model-done-p
               (org-gtd-walk-model-create '("a" "b")))))

(deftest walk-model-done-p-true-past-end ()
  "done-p is non-nil when cursor equals the entry count."
  (assert-true (org-gtd-walk-model-done-p (list :entries '("a") :cursor 1))))

(deftest walk-model-done-p-true-for-empty-entries ()
  "an empty walk is done immediately."
  (assert-true (org-gtd-walk-model-done-p (org-gtd-walk-model-create '()))))

(deftest walk-model-remaining-counts-pending-inclusive-of-current ()
  "remaining counts the current item plus everything after it."
  (assert-same 3 (org-gtd-walk-model-remaining
                  (org-gtd-walk-model-create '("a" "b" "c"))))
  (assert-same 1 (org-gtd-walk-model-remaining
                  (list :entries '("a" "b" "c") :cursor 2)))
  (assert-same 0 (org-gtd-walk-model-remaining
                  (list :entries '("a") :cursor 1))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `void-function org-gtd-walk-model-current`.

**Step 3: Write minimal implementation** (add a `;;;; Queries` section after
Construction):

```elisp
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add walk model cursor queries (current/done-p/remaining)"
```

---

### Task 3: `org-gtd-walk-model-advance` (pure, non-mutating)

**Files:**
- Modify: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test:**

```elisp
;;; advance

(deftest walk-model-advance-moves-cursor-forward ()
  "advance returns a model whose cursor is one greater."
  (let* ((m0 (org-gtd-walk-model-create '("a" "b" "c")))
         (m1 (org-gtd-walk-model-advance m0)))
    (assert-same 1 (plist-get m1 :cursor))
    (assert-equal "b" (org-gtd-walk-model-current m1))))

(deftest walk-model-advance-does-not-mutate-input ()
  "advance is pure: the original model is unchanged."
  (let ((m0 (org-gtd-walk-model-create '("a" "b"))))
    (org-gtd-walk-model-advance m0)
    (assert-same 0 (plist-get m0 :cursor))))

(deftest walk-model-advance-past-last-item-is-done ()
  "advancing off the last item makes the walk done, not out of bounds."
  (let* ((m (list :entries '("a") :cursor 0))
         (m1 (org-gtd-walk-model-advance m)))
    (assert-true (org-gtd-walk-model-done-p m1))
    (assert-same 1 (plist-get m1 :cursor))))

(deftest walk-model-advance-when-done-stays-done ()
  "advance never pushes the cursor beyond (length entries)."
  (let* ((m (list :entries '("a") :cursor 1))
         (m1 (org-gtd-walk-model-advance m)))
    (assert-same 1 (plist-get m1 :cursor))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `void-function org-gtd-walk-model-advance`.

**Step 3: Write minimal implementation** (add a `;;;; Transitions` section):

```elisp
;;;; Transitions (pure — take a model, return a new model)

(defun org-gtd-walk-model-advance (model)
  "Return a copy of MODEL with the cursor advanced by one.
The cursor never moves past the entry count (the done position)."
  (let ((len (length (plist-get model :entries)))
        (cursor (plist-get model :cursor)))
    (list :entries (plist-get model :entries)
          :cursor (min len (1+ cursor))
          :meta (plist-get model :meta))))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add pure walk model advance"
```

---

### Task 4: `org-gtd-walk-model-enqueue` at `bottom`

**Files:**
- Modify: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test:**

```elisp
;;; enqueue

(deftest walk-model-enqueue-bottom-appends-to-end ()
  "enqueue bottom puts the new handle last; cursor is unchanged."
  (let* ((m0 (org-gtd-walk-model-create '("a" "b")))
         (m1 (org-gtd-walk-model-enqueue m0 "z" 'bottom)))
    (assert-equal '("a" "b" "z") (plist-get m1 :entries))
    (assert-same 0 (plist-get m1 :cursor))))

(deftest walk-model-enqueue-does-not-mutate-input ()
  "enqueue is pure."
  (let ((m0 (org-gtd-walk-model-create '("a" "b"))))
    (org-gtd-walk-model-enqueue m0 "z" 'bottom)
    (assert-equal '("a" "b") (plist-get m0 :entries))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `void-function org-gtd-walk-model-enqueue`.

**Step 3: Write minimal implementation** (add to `;;;; Transitions`; include the
private index helper now so later tasks only add cases):

```elisp
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add walk model enqueue at bottom"
```

---

### Task 5: `enqueue` at `top` (handled next, current unchanged)

**Files:**
- Test: `test/unit/walk-model-test.el` (impl already covers it — this task pins the
  contract with tests; if any assertion fails, fix the helper).

**Step 1: Write the failing test:**

```elisp
(deftest walk-model-enqueue-top-inserts-next-after-current ()
  "enqueue top puts the handle immediately after the current item, unchanged."
  (let* ((m0 (list :entries '("a" "b" "c") :cursor 1))
         (m1 (org-gtd-walk-model-enqueue m0 "z" 'top)))
    (assert-equal '("a" "b" "z" "c") (plist-get m1 :entries))
    ;; cursor still points at the current item "b"
    (assert-same 1 (plist-get m1 :cursor))
    (assert-equal "b" (org-gtd-walk-model-current m1))))

(deftest walk-model-enqueue-top-when-done-appends ()
  "top clamps to the end when the walk is already done."
  (let* ((m0 (list :entries '("a") :cursor 1))
         (m1 (org-gtd-walk-model-enqueue m0 "z" 'top)))
    (assert-equal '("a" "z") (plist-get m1 :entries))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS immediately (the Task 4 helper already implements `top` as
`cursor+1`). If red, correct `org-gtd-walk-model--insert-index`. This task exists
to lock the `top` = "handled next, current item unchanged" contract with explicit
tests — the duplicate-queue's critical invariant.

**Step 3: Commit**

```bash
git add test/unit/walk-model-test.el
git commit -m "test: pin walk model enqueue top-inserts-next contract"
```

---

### Task 6: `enqueue` at `top` from cursor 0 + unknown-position error

**Files:**
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test:**

```elisp
(deftest walk-model-enqueue-top-at-start-inserts-next-not-current ()
  "From a fresh walk, top inserts after the current item; current is unchanged."
  (let* ((m0 (org-gtd-walk-model-create '("a" "b")))
         (m1 (org-gtd-walk-model-enqueue m0 "z" 'top)))
    (assert-equal '("a" "z" "b") (plist-get m1 :entries))
    (assert-same 0 (plist-get m1 :cursor))
    (assert-equal "a" (org-gtd-walk-model-current m1))))

(deftest walk-model-enqueue-unknown-position-errors ()
  "enqueue signals an error for an unrecognized position."
  (assert-raises 'error
    (org-gtd-walk-model-enqueue (org-gtd-walk-model-create '("a")) "z" 'sideways)))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS immediately (helper already inserts `top` at cursor+1 and errors on
the `t` branch). If red, correct the helper.

**Step 3: Commit**

```bash
git add test/unit/walk-model-test.el
git commit -m "test: pin walk model enqueue top-from-start and unknown-position error"
```

---

### Task 7: `org-gtd-walk-model-valid-p`

**Files:**
- Modify: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test:**

```elisp
;;; valid-p

(deftest walk-model-valid-p-accepts-a-fresh-model ()
  (assert-true (org-gtd-walk-model-valid-p
                (org-gtd-walk-model-create '("a" "b")))))

(deftest walk-model-valid-p-accepts-done-cursor-at-length ()
  "cursor == (length entries) is the valid done position."
  (assert-true (org-gtd-walk-model-valid-p (list :entries '("a") :cursor 1))))

(deftest walk-model-valid-p-accepts-symbol-and-number-handles ()
  (assert-true (org-gtd-walk-model-valid-p
                (list :entries '(foo 42 "bar") :cursor 0))))

(deftest walk-model-valid-p-rejects-negative-cursor ()
  (assert-nil (org-gtd-walk-model-valid-p (list :entries '("a") :cursor -1))))

(deftest walk-model-valid-p-rejects-cursor-past-end ()
  (assert-nil (org-gtd-walk-model-valid-p (list :entries '("a") :cursor 2))))

(deftest walk-model-valid-p-rejects-non-integer-cursor ()
  (assert-nil (org-gtd-walk-model-valid-p (list :entries '("a") :cursor "0"))))

(deftest walk-model-valid-p-rejects-non-list-entries ()
  (assert-nil (org-gtd-walk-model-valid-p (list :entries "a" :cursor 0))))

(deftest walk-model-valid-p-rejects-non-serializable-handle ()
  "Live markers and other non-serializable handles are rejected."
  (assert-nil (org-gtd-walk-model-valid-p
               (list :entries (list (make-marker)) :cursor 0))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `void-function org-gtd-walk-model-valid-p`.

**Step 3: Write minimal implementation** (new `;;;; Validation` section):

```elisp
;;;; Validation

(defun org-gtd-walk-model--handle-serializable-p (handle)
  "Return non-nil when HANDLE is a `prin1'/`read'-safe walk handle.
Handles are strings, symbols, or numbers (org-ids in practice); live
markers and buffers are rejected so they can never be persisted."
  (or (stringp handle) (symbolp handle) (numberp handle)))

(defun org-gtd-walk-model-valid-p (model)
  "Return non-nil when MODEL is internally coherent and serializable.
Checks entries is a list of serializable handles and cursor is an
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add walk model valid-p guard"
```

---

### Task 8: `serialize` / `deserialize` round-trip + corruption fallback

**Files:**
- Modify: `org-gtd-walk-model.el`
- Test: `test/unit/walk-model-test.el`

**Step 1: Write the failing test:**

```elisp
;;; serialize / deserialize

(deftest walk-model-serialize-round-trips ()
  "deserialize of a serialized model reproduces it."
  (let* ((m (list :entries '("a" "b" "c") :cursor 1 :meta '(:tag foo)))
         (s (org-gtd-walk-model-serialize m)))
    (assert-true (stringp s))
    (assert-equal m (org-gtd-walk-model-deserialize s))))

(deftest walk-model-deserialize-garbage-returns-nil ()
  "unreadable text yields nil, not an error."
  (assert-nil (org-gtd-walk-model-deserialize "(:entries oops")))

(deftest walk-model-deserialize-readable-but-invalid-returns-nil ()
  "a readable but incoherent model (cursor out of range) yields nil."
  (assert-nil (org-gtd-walk-model-deserialize
               (prin1-to-string (list :entries '("a") :cursor 9)))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: FAIL — `void-function org-gtd-walk-model-serialize`.

**Step 3: Write minimal implementation** (new `;;;; Serialization` section):

```elisp
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
    (when (and model (org-gtd-walk-model-valid-p model))
      model)))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-model-test.el`
Expected: PASS. **Tier 1 is now complete and green.**

**Step 5: Commit**

```bash
git add org-gtd-walk-model.el test/unit/walk-model-test.el
git commit -m "feat: add walk model serialize/deserialize with corruption fallback"
```

---

# GROUP B — Consumer spec + `org-gtd-walks` registry

Files:
- Create: `org-gtd-walk.el` (driver/registry module — grows through Groups B–D)
- Test: `test/unit/walk-registry-test.el`

`org-gtd-walk.el` is the second (impure) module. It requires `org-gtd-walk-model`
and `org-gtd-core` (for `org-gtd-directory`, used later in checkpoints).

---

### Task 9: Create driver module + `org-gtd-walks` registry (register/get)

**Files:**
- Create: `org-gtd-walk.el`
- Create: `test/unit/walk-registry-test.el`

**Step 1: Write the failing test.** Create `test/unit/walk-registry-test.el`:

```elisp
;;; walk-registry-test.el --- Unit tests for the walk registry + spec -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the org-gtd-walks registry and spec validation.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

(deftest walks-registry-starts-empty ()
  "Phase 0 ships no registered consumers."
  (assert-nil org-gtd-walks))

(deftest walk-register-then-get-round-trips ()
  "register stores a spec that get returns."
  (let ((org-gtd-walks nil)
        (spec '(:name demo :find ignore :render ignore :scope "s")))
    (org-gtd-walk-register 'demo spec)
    (assert-equal spec (org-gtd-walk-get 'demo))))

(deftest walk-get-unknown-returns-nil ()
  (let ((org-gtd-walks nil))
    (assert-nil (org-gtd-walk-get 'nope))))

(deftest walk-register-replaces-existing-name ()
  "registering the same name twice replaces, not duplicates."
  (let ((org-gtd-walks nil))
    (org-gtd-walk-register 'demo '(:name demo :v 1))
    (org-gtd-walk-register 'demo '(:name demo :v 2))
    (assert-same 2 (plist-get (org-gtd-walk-get 'demo) :v))
    (assert-same 1 (length org-gtd-walks))))

(provide 'walk-registry-test)

;;; walk-registry-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-registry-test.el`
Expected: FAIL — `Cannot open load file` for `org-gtd-walk`.

**Step 3: Write minimal implementation.** Create `org-gtd-walk.el` with the full
header/footer and:

```elisp
;;; org-gtd-walk.el --- Generic item-walk session driver -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; [copy the license block verbatim from org-gtd-someday-review.el lines 8-19]

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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-registry-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-registry-test.el
git commit -m "feat: add org-gtd-walks registry with register/get"
```

---

### Task 10: `org-gtd-walk-spec-valid-p`

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-registry-test.el`

**Step 1: Write the failing test** (append before `(provide ...)`):

```elisp
;;; spec validation

(defun walk-registry-test--good-spec ()
  "A minimally valid spec."
  (list :name 'demo
        :find #'ignore
        :render #'ignore
        :actions nil
        :on-finish nil
        :resumable nil
        :resolve nil
        :scope "scope-x"))

(deftest walk-spec-valid-p-accepts-a-good-spec ()
  (assert-true (org-gtd-walk-spec-valid-p (walk-registry-test--good-spec))))

(deftest walk-spec-valid-p-requires-a-symbol-name ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :name "demo"))))

(deftest walk-spec-valid-p-requires-callable-find ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :find 42))))

(deftest walk-spec-valid-p-requires-callable-render ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :render "nope"))))

(deftest walk-spec-valid-p-requires-scope ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :scope nil))))

(deftest walk-spec-valid-p-allows-nil-optional-fields ()
  "actions/on-finish/resolve may be nil; a lambda resolve is fine too."
  (let ((spec (plist-put (walk-registry-test--good-spec)
                         :resolve (lambda (_h) t))))
    (assert-true (org-gtd-walk-spec-valid-p spec))))

(deftest walk-spec-valid-p-rejects-non-callable-resolve ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :resolve 7))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-registry-test.el`
Expected: FAIL — `void-function org-gtd-walk-spec-valid-p`.

**Step 3: Write minimal implementation** (add a `;;;; Spec validation` section in
`org-gtd-walk.el`):

```elisp
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-registry-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-registry-test.el
git commit -m "feat: add walk consumer spec validation"
```

---

# GROUP C — Scope identity + locking + checkpoint paths

Files:
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-scope-test.el`

Scope is the org container a walk reads from: a file path string, a list of file
paths, or a subtree org-id string (design §5). Two uses: **lock** (scope alone —
no two walks over the same container) and **checkpoint identity** (`name`+scope).

---

### Task 11: `org-gtd-walk--scope-key` canonicalization

**Files:**
- Modify: `org-gtd-walk.el`
- Create: `test/unit/walk-scope-test.el`

**Step 1: Write the failing test.** Create `test/unit/walk-scope-test.el`:

```elisp
;;; walk-scope-test.el --- Unit tests for walk scope identity + locking -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for scope canonicalization, the concurrency lock, and checkpoint
;; path derivation (design §5, §8).  These use plain strings and a temp
;; directory only -- no org, no mock-fs.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

;;; scope-key

(deftest scope-key-of-string-is-the-string ()
  (assert-equal "/x/inbox.org" (org-gtd-walk--scope-key "/x/inbox.org")))

(deftest scope-key-of-list-is-order-independent ()
  "A file-set scope keys the same regardless of listing order."
  (assert-equal (org-gtd-walk--scope-key '("a.org" "b.org"))
                (org-gtd-walk--scope-key '("b.org" "a.org"))))

(deftest scope-key-distinguishes-different-scopes ()
  (assert-not-equal (org-gtd-walk--scope-key "a.org")
                    (org-gtd-walk--scope-key "b.org")))

(provide 'walk-scope-test)

;;; walk-scope-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: FAIL — `void-function org-gtd-walk--scope-key`.

**Step 3: Write minimal implementation** (new `;;;; Scope and locking` section):

```elisp
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-scope-test.el
git commit -m "feat: add walk scope-key canonicalization"
```

---

### Task 12: scope lock — `locked-p` / `lock` / `unlock`

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-scope-test.el`

**Step 1: Write the failing test** (append):

```elisp
;;; locking

(deftest scope-lock-lifecycle ()
  "lock makes a scope locked; unlock releases it."
  (let ((org-gtd-walk--locked-scopes nil))
    (assert-nil (org-gtd-walk--scope-locked-p "s"))
    (org-gtd-walk--lock-scope "s")
    (assert-true (org-gtd-walk--scope-locked-p "s"))
    (org-gtd-walk--unlock-scope "s")
    (assert-nil (org-gtd-walk--scope-locked-p "s"))))

(deftest scope-lock-is-per-container ()
  "Different scopes lock independently."
  (let ((org-gtd-walk--locked-scopes nil))
    (org-gtd-walk--lock-scope "a")
    (assert-nil (org-gtd-walk--scope-locked-p "b"))))

(deftest scope-lock-matches-order-independent-file-sets ()
  "A file-set locked in one order is seen as locked in another."
  (let ((org-gtd-walk--locked-scopes nil))
    (org-gtd-walk--lock-scope '("a.org" "b.org"))
    (assert-true (org-gtd-walk--scope-locked-p '("b.org" "a.org")))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: FAIL — `void-variable org-gtd-walk--locked-scopes` /
`void-function org-gtd-walk--scope-locked-p`.

**Step 3: Write minimal implementation** (add to the Scope section):

```elisp
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
```

Add `(require 'cl-lib)` near the top of `org-gtd-walk.el` (for `cl-pushnew`).

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-scope-test.el
git commit -m "feat: add walk scope concurrency lock"
```

---

### Task 13: checkpoint path + save/load/delete file I/O

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-scope-test.el`

**Step 1: Write the failing test** (append). These use a real temp directory bound
via `org-gtd-directory` — deterministic, not mock-fs, no buffer visiting:

```elisp
;;; checkpoint file I/O

(deftest checkpoint-path-keys-on-name-and-scope ()
  "Different name or scope yields a different checkpoint path; same inputs match."
  (let ((org-gtd-directory "/tmp/gtd/"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'b "s"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'a "t"))
    (assert-equal (org-gtd-walk--checkpoint-path 'a "s")
                  (org-gtd-walk--checkpoint-path 'a "s"))))

(deftest checkpoint-save-load-delete-round-trip ()
  "A saved model reloads equal; delete removes the file."
  (let* ((org-gtd-directory (make-temp-file "walk-ckpt" t))
         (path (org-gtd-walk--checkpoint-path 'demo "scope"))
         (model (list :entries '("a" "b") :cursor 1 :meta nil)))
    (unwind-protect
        (progn
          (org-gtd-walk--save-checkpoint path model)
          (assert-true (file-exists-p path))
          (assert-equal model (org-gtd-walk--load-checkpoint path))
          (org-gtd-walk--delete-checkpoint path)
          (assert-nil (file-exists-p path)))
      (delete-directory org-gtd-directory t))))

(deftest checkpoint-load-missing-file-returns-nil ()
  (let ((org-gtd-directory (make-temp-file "walk-ckpt" t)))
    (unwind-protect
        (assert-nil (org-gtd-walk--load-checkpoint
                     (org-gtd-walk--checkpoint-path 'demo "scope")))
      (delete-directory org-gtd-directory t))))

(deftest checkpoint-load-corrupt-file-returns-nil ()
  "A garbage checkpoint file loads as nil (fresh-start fallback)."
  (let* ((org-gtd-directory (make-temp-file "walk-ckpt" t))
         (path (org-gtd-walk--checkpoint-path 'demo "scope")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "(:entries oops"))
          (assert-nil (org-gtd-walk--load-checkpoint path)))
      (delete-directory org-gtd-directory t))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: FAIL — `void-function org-gtd-walk--checkpoint-path`.

**Step 3: Write minimal implementation** (new `;;;; Checkpoint persistence` section).
The path is keyed by `name`+scope; `md5` of the scope key keeps the filename
filesystem-safe:

```elisp
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
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-scope-test.el
git commit -m "feat: add walk checkpoint path and file I/O"
```

---

# GROUP D — Session driver (Tier 2, integration against a stub spec)

Files:
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-driver-test.el`

These are headless integration tests. The stub spec has a fixed `:find`, a
`:render` that pushes the rendered handle onto a log list, a toggleable `:resolve`,
and an `:on-finish` counter. The surface is a throwaway buffer. Global state
(`org-gtd-walk--locked-scopes`) and the log/counter are reset per test via
dynamic `let`; `org-gtd-directory` is bound to a temp dir. Nothing visits a GTD
file, so `default-directory`/mock-fs never leak.

The session bundle is stored **buffer-local** on the surface buffer in
`org-gtd-walk--active` (plist `:model :spec :surface :checkpoint-path :skipped`).
Transitions operate on `org-gtd-walk--active` of the current buffer, so tests wrap
transition calls in `(with-current-buffer surface ...)`.

---

### Task 14: driver test harness + `org-gtd-walk-start` (non-empty find renders current)

**Files:**
- Modify: `org-gtd-walk.el`
- Create: `test/unit/walk-driver-test.el`

**Step 1: Write the failing test.** Create `test/unit/walk-driver-test.el`:

```elisp
;;; walk-driver-test.el --- Tier 2 integration tests for the walk driver -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier 2 driver tests against a stub spec: headless, deterministic, no org,
;; no mock-fs.  Proves the render/checkpoint/transition lifecycle exactly once
;; (design §6, §9, §10).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

;;; Stub spec + harness

(defvar walk-driver-test--render-log nil
  "Handles the stub :render has been called with, newest first.")
(defvar walk-driver-test--finish-count 0
  "How many times the stub :on-finish ran.")

(defun walk-driver-test--stub-spec (&rest overrides)
  "A minimal valid spec; OVERRIDES are applied as plist puts."
  (let ((spec (list :name 'stub
                    :find (lambda () (list "a" "b" "c"))
                    :render (lambda (handle _surface)
                              (push handle walk-driver-test--render-log))
                    :actions nil
                    :on-finish (lambda ()
                                 (setq walk-driver-test--finish-count
                                       (1+ walk-driver-test--finish-count)))
                    :resumable nil
                    :resolve nil
                    :scope "stub-scope")))
    (while overrides
      (setq spec (plist-put spec (pop overrides) (pop overrides))))
    spec))

(defmacro walk-driver-test--with-harness (surface-var &rest body)
  "Run BODY with fresh driver state and SURFACE-VAR bound to a temp buffer."
  (declare (indent 1))
  `(let ((walk-driver-test--render-log nil)
         (walk-driver-test--finish-count 0)
         (org-gtd-walk--locked-scopes nil)
         (org-gtd-directory (make-temp-file "walk-drv" t))
         (,surface-var (generate-new-buffer " *walk-test*")))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,surface-var) (kill-buffer ,surface-var))
       (delete-directory org-gtd-directory t))))

;;; start

(deftest walk-start-renders-first-item-and-activates ()
  "start with a non-empty find renders the first handle and stores a session."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (assert-equal '("a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-true org-gtd-walk--active)
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))))

(provide 'walk-driver-test)

;;; walk-driver-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: FAIL — `void-variable org-gtd-walk--active` /
`void-function org-gtd-walk-start`.

**Step 3: Write minimal implementation** (new `;;;; Session driver` section in
`org-gtd-walk.el`). Introduce the buffer-local session, the surface-buffer helper,
render/checkpoint/settle internals, and `start`:

```elisp
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
```

Note: `org-gtd-walk-finish` is defined now because `--settle` may call it (an
empty-after-resolve walk). `org-gtd-walk-advance`/`-enqueue`/`-quit` arrive in
later tasks.

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el
git commit -m "feat: add walk driver start + render/settle/finish"
```

---

### Task 15: `org-gtd-walk-advance` renders the next item

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-driver-test.el`

**Step 1: Write the failing test** (append before `(provide ...)`):

```elisp
;;; advance

(deftest walk-advance-renders-next-item ()
  "advance moves the cursor and re-renders the new current handle."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (with-current-buffer surface (org-gtd-walk-advance))
    ;; newest render first: "b" after "a"
    (assert-equal '("b" "a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-equal "b" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: FAIL — `void-function org-gtd-walk-advance`.

**Step 3: Write minimal implementation** (add to the Session driver section):

```elisp
(defun org-gtd-walk-advance ()
  "Advance the active walk to the next item and re-render (design §6).
Finishes when the walk runs off the end.  Runs in the surface buffer."
  (setf (plist-get org-gtd-walk--active :model)
        (org-gtd-walk-model-advance (plist-get org-gtd-walk--active :model)))
  (org-gtd-walk--settle))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el
git commit -m "feat: add walk driver advance transition"
```

---

### Task 16: advancing off the end finishes + runs `:on-finish`

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already present via `--settle`).

**Step 1: Write the failing test:**

```elisp
(deftest walk-advance-off-end-finishes-and-clears-session ()
  "Running past the last item finishes: on-finish runs, session cleared, scope unlocked."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface
      (org-gtd-walk-advance)   ; "b"
      (org-gtd-walk-advance)   ; "c"
      (org-gtd-walk-advance))  ; off end -> finish
    (assert-same 1 walk-driver-test--finish-count)
    (with-current-buffer surface (assert-nil org-gtd-walk--active))
    (assert-nil (org-gtd-walk--scope-locked-p "stub-scope"))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS (settle+finish already implement this). If red, revisit
`org-gtd-walk--settle`/`-finish`.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver finish-on-exhaustion"
```

---

### Task 17: empty `find` finishes immediately without activating

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already present in `start`).

**Step 1: Write the failing test:**

```elisp
;;; empty find

(deftest walk-empty-find-finishes-without-activating ()
  "An empty find runs on-finish, renders nothing, activates nothing, locks nothing."
  (walk-driver-test--with-harness surface
    (let ((result (org-gtd-walk-start
                   (walk-driver-test--stub-spec :find (lambda () '()))
                   surface)))
      (assert-nil result)
      (assert-nil walk-driver-test--render-log)
      (assert-same 1 walk-driver-test--finish-count)
      (with-current-buffer surface (assert-nil org-gtd-walk--active))
      (assert-nil (org-gtd-walk--scope-locked-p "stub-scope")))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS. If red, fix the empty-find branch in `org-gtd-walk-start`.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver empty-find immediate finish"
```

---

### Task 18: `org-gtd-walk-enqueue` re-renders current + updates model

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-driver-test.el`

**Step 1: Write the failing test:**

```elisp
;;; enqueue

(deftest walk-enqueue-bottom-extends-without-moving-cursor ()
  "enqueue bottom adds a pending item and re-renders the (unchanged) current."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface (org-gtd-walk-enqueue "z" 'bottom))
    (with-current-buffer surface
      (assert-equal '("a" "b" "c" "z")
                    (plist-get (plist-get org-gtd-walk--active :model) :entries))
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))
    ;; re-rendered current "a" again on top of the initial "a"
    (assert-equal '("a" "a") walk-driver-test--render-log)))

(deftest walk-enqueue-top-inserts-next-and-rerenders-current ()
  "enqueue top puts the handle right after the current item, which stays current."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface (org-gtd-walk-enqueue "z" 'top))
    (with-current-buffer surface
      (assert-equal '("a" "z" "b" "c")
                    (plist-get (plist-get org-gtd-walk--active :model) :entries))
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))
    ;; current "a" re-rendered on top of the initial "a"
    (assert-equal '("a" "a") walk-driver-test--render-log)))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: FAIL — `void-function org-gtd-walk-enqueue`.

**Step 3: Write minimal implementation:**

```elisp
(defun org-gtd-walk-enqueue (handle where)
  "Insert HANDLE into the active walk at WHERE and re-render (design §6).
WHERE is `top' (handled next) or `bottom' (handled last); both insert
after the current item.  Runs in the surface buffer."
  (setf (plist-get org-gtd-walk--active :model)
        (org-gtd-walk-model-enqueue
         (plist-get org-gtd-walk--active :model) handle where))
  (org-gtd-walk--render-current)
  (org-gtd-walk--checkpoint))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el
git commit -m "feat: add walk driver enqueue transition"
```

---

### Task 19: stale handle via `:resolve` auto-skips and counts

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already in `--settle`).

**Step 1: Write the failing test:**

```elisp
;;; stale-handle skipping

(deftest walk-resolve-skips-stale-handles-on-advance ()
  "A :resolve that rejects \"b\" auto-advances past it to \"c\" and counts the skip."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resolve (lambda (h) (not (equal h "b"))))
     surface) ; renders "a"
    (with-current-buffer surface (org-gtd-walk-advance)) ; would land "b", skips to "c"
    (assert-equal '("c" "a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-same 1 (plist-get org-gtd-walk--active :skipped)))))

(deftest walk-resolve-all-stale-finishes ()
  "If every remaining handle is stale, settling finishes the walk."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resolve (lambda (h) (equal h "a")))
     surface) ; "a" resolves, renders "a"
    (with-current-buffer surface (org-gtd-walk-advance)) ; "b","c" stale -> finish
    (assert-same 1 walk-driver-test--finish-count)
    (with-current-buffer surface (assert-nil org-gtd-walk--active))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS (settle implements resolve-skip). If red, fix `--settle`.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver stale-handle auto-skip"
```

---

### Task 20: resumable walk checkpoints after start and advance

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already in start/advance via `--checkpoint`).

**Step 1: Write the failing test:**

```elisp
;;; checkpointing

(deftest walk-resumable-checkpoints-after-start-and-advance ()
  "A resumable walk writes its model to disk, updated on each transition."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resumable t) surface)
    (let ((path (org-gtd-walk--checkpoint-path 'stub "stub-scope")))
      (assert-true (file-exists-p path))
      (assert-same 0 (plist-get (org-gtd-walk--load-checkpoint path) :cursor))
      (with-current-buffer surface (org-gtd-walk-advance))
      (assert-same 1 (plist-get (org-gtd-walk--load-checkpoint path) :cursor)))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS. If red, verify `--checkpoint` is called from settle/enqueue.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver resumable checkpointing"
```

---

### Task 21: `org-gtd-walk-quit` keeps checkpoint; finish deletes it

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-driver-test.el`

**Step 1: Write the failing test:**

```elisp
;;; quit vs finish

(deftest walk-quit-keeps-checkpoint-and-runs-no-on-finish ()
  "quit tears down but preserves a resumable checkpoint and skips on-finish."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec :resumable t) surface)
    (let ((path (org-gtd-walk--checkpoint-path 'stub "stub-scope")))
      (with-current-buffer surface (org-gtd-walk-quit))
      (assert-true (file-exists-p path))
      (assert-same 0 walk-driver-test--finish-count)
      (with-current-buffer surface (assert-nil org-gtd-walk--active))
      (assert-nil (org-gtd-walk--scope-locked-p "stub-scope")))))

(deftest walk-finish-deletes-checkpoint ()
  "Finishing a resumable walk removes its checkpoint file."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec :resumable t) surface)
    (let ((path (org-gtd-walk--checkpoint-path 'stub "stub-scope")))
      (assert-true (file-exists-p path))
      (with-current-buffer surface
        (org-gtd-walk-advance) (org-gtd-walk-advance) (org-gtd-walk-advance))
      (assert-nil (file-exists-p path)))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: FAIL — `void-function org-gtd-walk-quit` (the finish half already passes).

**Step 3: Write minimal implementation:**

```elisp
(defun org-gtd-walk-quit ()
  "Abandon the active walk (design §9).
Tears down and unlocks but keeps the checkpoint (if resumable) and runs
no :on-finish.  Runs in the surface buffer."
  (org-gtd-walk--unlock-scope
   (plist-get (plist-get org-gtd-walk--active :spec) :scope))
  (setq org-gtd-walk--active nil))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el
git commit -m "feat: add walk driver quit (keeps checkpoint)"
```

---

### Task 22: corrupt checkpoint falls back to a fresh find

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already in start via deserialize guard).

**Step 1: Write the failing test:**

```elisp
;;; corrupt checkpoint

(deftest walk-corrupt-checkpoint-starts-fresh ()
  "A garbage checkpoint on a resumable walk is discarded; find runs fresh."
  (walk-driver-test--with-harness surface
    (let ((path (org-gtd-walk--checkpoint-path 'stub "stub-scope")))
      (with-temp-file path (insert "(:entries oops :cursor"))
      (org-gtd-walk-start (walk-driver-test--stub-spec :resumable t) surface)
      (assert-equal '("a") walk-driver-test--render-log)
      (with-current-buffer surface
        (assert-same 0 (plist-get (plist-get org-gtd-walk--active :model) :cursor))
        (assert-equal '("a" "b" "c")
                      (plist-get (plist-get org-gtd-walk--active :model) :entries))))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS (deserialize returns nil for corrupt → start uses fresh find). If
red, check `org-gtd-walk--load-checkpoint`/`start` ordering.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver corrupt-checkpoint fresh start"
```

---

### Task 23: scope lock refuses a second concurrent walk

**Files:**
- Test: `test/unit/walk-driver-test.el` (impl already in start).

**Step 1: Write the failing test:**

```elisp
;;; scope lock

(deftest walk-second-walk-over-same-scope-is-refused ()
  "Starting a second walk over a locked scope errors; the first stays active."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (let ((other (generate-new-buffer " *walk-test-2*")))
      (unwind-protect
          (progn
            (assert-raises 'error
              (org-gtd-walk-start
               (walk-driver-test--stub-spec :name 'stub2) other))
            ;; first walk untouched
            (with-current-buffer surface (assert-true org-gtd-walk--active))
            (assert-true (org-gtd-walk--scope-locked-p "stub-scope")))
        (kill-buffer other)))))

(deftest walk-different-scopes-coexist ()
  "Two walks over different scopes run side by side."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (let ((other (generate-new-buffer " *walk-test-2*")))
      (unwind-protect
          (progn
            (org-gtd-walk-start
             (walk-driver-test--stub-spec :name 'stub2 :scope "other-scope")
             other)
            (with-current-buffer other (assert-true org-gtd-walk--active)))
        (kill-buffer other)))))
```

**Step 2: Run test to verify it fails, then passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS (start's lock check already refuses). If red, check
`org-gtd-walk--scope-locked-p` in `start`.

**Step 3: Commit**

```bash
git add test/unit/walk-driver-test.el
git commit -m "test: pin walk driver scope-lock refusal and coexistence"
```

---

### Task 24: action error before transition leaves state intact

**Files:**
- Modify: `org-gtd-walk.el`
- Test: `test/unit/walk-driver-test.el`

Design §9: actions do the org effect *then* call a transition; an error before the
transition must leave the walk on the current item. The driver provides a wrapper
that surfaces the error without advancing.

**Step 1: Write the failing test:**

```elisp
;;; action error handling

(deftest walk-action-error-before-transition-does-not-advance ()
  "An action that throws before its transition leaves cursor and session intact."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface
      (org-gtd-walk-call-action
       (lambda () (error "boom before transition")))
      ;; still on "a", session alive
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model)))
      (assert-true org-gtd-walk--active))
    ;; only the initial render happened
    (assert-equal '("a") walk-driver-test--render-log)))

(deftest walk-call-action-runs-transition-on-success ()
  "call-action runs the action; an action that advances does advance."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (with-current-buffer surface
      (org-gtd-walk-call-action (lambda () (org-gtd-walk-advance)))
      (assert-equal "b" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: FAIL — `void-function org-gtd-walk-call-action`.

**Step 3: Write minimal implementation:**

```elisp
(defun org-gtd-walk-call-action (fn)
  "Invoke action FN, surfacing any error without disturbing walk state.
Because an action does its org side-effect and only then calls a
transition, an error thrown before the transition leaves the walk on
the current item (design §9)."
  (condition-case err
      (funcall fn)
    (error (message "org-gtd walk action error: %s"
                    (error-message-string err)))))
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el
git commit -m "feat: add walk driver action-error wrapper"
```

---

# GROUP E — Wire-up, compile, full suite

### Task 25: load the new modules from `org-gtd.el` + compile clean + full suite

**Files:**
- Modify: `org-gtd.el` (add `(require 'org-gtd-walk)` alongside the other module
  requires — confirm the exact require block first by reading the file; the model
  is pulled transitively but you may add `(require 'org-gtd-walk-model)` too for
  explicitness).

**Step 1: Add the requires.** Read `org-gtd.el`, find the block of
`(require 'org-gtd-...)` module loads, and add:

```elisp
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)
```

(No autoloads/interactive entry points in Phase 0 — nothing user-facing yet.)

**Step 2: Compile with warnings as errors**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: clean compile, no warnings. Fix any docstring/lexical/unused-var
warnings the linter flags in the two new files.

**Step 3: Run the three new test files, then the whole unit suite**

Run:
```
.claude/skills/test/run-tests.sh test/unit/walk-model-test.el
.claude/skills/test/run-tests.sh test/unit/walk-registry-test.el
.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el
.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el
.claude/skills/test/run-tests.sh unit
```
Expected: all green; the full `unit` run shows no regressions (Phase 0 adds files
only, touches no existing behavior).

**Step 4: Commit**

```bash
git add org-gtd.el
git commit -m "feat: load walk engine modules from org-gtd.el"
```

---

## Done — Phase 0 deliverables

- `org-gtd-walk-model.el` — pure model: create, current, done-p, remaining,
  advance, enqueue (top/bottom, both after the cursor), valid-p,
  serialize/deserialize.
- `org-gtd-walk.el` — registry (`org-gtd-walks`, register/get), spec validation,
  scope-key + concurrency lock, checkpoint path + file I/O, session driver (start,
  advance, enqueue, quit, finish, settle/render/checkpoint, action wrapper).
- Tests: `test/unit/walk-model-test.el` (Tier 1, pure),
  `test/unit/walk-registry-test.el`, `test/unit/walk-scope-test.el`,
  `test/unit/walk-driver-test.el` (Tier 2, stub spec).

No consumers, no existing features touched. Phase 1 (someday-review migration)
begins from here.

## Execution Handoff

Plan complete and saved. Two execution options:

**1. Subagent-Driven (this session)** — dispatch a fresh subagent per task, review
between tasks (REQUIRED SUB-SKILL: superpowers:subagent-driven-development).

**2. Parallel Session (separate)** — open a new session in this worktree and batch
through with checkpoints (REQUIRED SUB-SKILL: superpowers:executing-plans).

Which approach?
