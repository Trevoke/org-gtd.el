# View Manager — Multi-Section Views Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Let a single saved GTD view hold multiple ordered **sections** (agenda
blocks), built/edited/reordered/previewed in the existing builder transient with
no elisp, while single-section views stay byte-for-byte backward compatible.

**Architecture:** Keep the builder's per-section editing exactly as it is today
(the infix layer keeps mutating `org-gtd-view-manager--build-state`, which now
means "the ACTIVE section's alist"). Add a thin section layer around it:
`--build-name` (the view name, hoisted out of the active alist),
`--build-sections` (ordered list of section alists, each WITHOUT `name`), and
`--build-active` (index). A "Sections" transient group adds/switches/deletes/
reorders sections. Compile splits into `--compile-section` (one section, no
name) and `--compile-view` (flat spec for one section, `((name) (blocks …))`
spec for many). The view DSL already renders both shapes — it is NOT touched.

**Tech Stack:** Emacs Lisp, transient.el, e-unit test framework, Eldev.

---

## Environment constraints (read before every task)

- **Work ONLY in** `/home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design`.
  Never `cd` elsewhere. All paths below are relative to that worktree root.
- **Run tests ONLY via** `.claude/skills/test/run-tests.sh <file-or-category>`.
  Do NOT invoke the `/test` skill and do NOT call `eldev etest` directly.
  - Single file: `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el`
  - Category: `.claude/skills/test/run-tests.sh unit`
  - The script prints `PASS: N tests in Xs` + `Seed: <N>` on success, or a
    summary line plus failure detail on failure. Capture the seed on any flake.
- **Compile check:** `~/bin/eldev compile --warnings-as-errors`, then
  `git checkout -- dir` (compile deletes the tracked Info `dir` file). **Never**
  `git add` the `dir` file or any `*.elc`.
- **Do NOT regress the fixes already on this branch:**
  - **P1** — `--compile` conses a FRESH cell per key (cache-safety); `--preview-now`
    takes a `force` arg; `--preview`/RET force a render.
  - **P2** — `--render-preview` binds `org-agenda-window-setup` to `current-window`;
    `--read-effort` blank-clears (returns nil).
  - **3+2 column layout** in the builder macro (`rows`: Type/Time/Structural on
    row 1, Metadata/Prefix on row 2).
  - **iv4b** — preview-on-open (`--preview-now t` before `transient-setup`) and the
    `transient-heading` "View:" summary line.
- **Out of scope:** yaks `z15n` (rename Render) and `lk0a` (sample-data message
  spam). Do NOT touch `org-gtd-view-language.el` (multi-block already works —
  verified: `org-gtd-view-show` renders a flat spec AND a `((name) (blocks …))`
  spec; see `org-gtd-view-language.el:1087-1158` and `--create-custom-commands`
  at `:343-366`).

---

## Test idioms (study these before writing tests)

Reference files:
- `test/unit/view-manager-compile-test.el` — pure `--compile` tests: `(require
  'e-unit)`, `(require 'org-gtd-view-manager)`, `(e-unit-initialize)`, then
  `deftest` + `assert-equal`/`assert-nil`.
- `test/unit/view-manager-badge-test.el` — same pure-test shape for badges.
- `test/unit/view-manager-build-test.el` and `.../view-manager-list-test.el` —
  transient/state tests: `(require 'ogt-eunit-prelude "test/helpers/prelude.el")`,
  `(e-unit-initialize)`, `(around-each (proceed context) (ogt-eunit-with-mock-gtd
  (funcall proceed context)))`, then `deftest`. Use `ogt--transient-suffix-plist`
  (from `test/helpers/utils.el:44`) for transient-key assertions,
  `cl-letf (((symbol-function 'foo) …))` to stub, and `assert-raises` for errors.
- `ogt-eunit-with-mock-gtd` (`test/helpers/setup.el:262`) gives a mock filesystem
  so the store round-trips on disk; use it for any test that reads/writes the store.

Assertion vocabulary: `assert-equal`, `assert-true`, `assert-nil`, `assert-raises`,
`assert-match`.

---

## Section-management keybindings (chosen)

The builder already binds these and MUST NOT collide: infix letters
`t w D C o O N H A e W G P x X` and actions `RET s C-c C-k`. Meta-chorded keys
live in a distinct transient namespace from plain letters, so the following are
collision-free against every filter-spec `:key` and every action key:

| Command             | Key        |
|---------------------|------------|
| Add section         | `M-a`      |
| Next section        | `M-n`      |
| Previous section    | `M-p`      |
| Delete section      | `M-k`      |
| Move section up     | `M-<up>`   |
| Move section down   | `M-<down>` |

**Uniqueness confirmed:** none of `M-a M-n M-p M-k M-<up> M-<down>` appears in
`org-gtd-view-manager--filter-specs` (`org-gtd-view-manager.el:126-171`, all plain
single letters) nor in the Actions group (`RET s C-c C-k`). They are also distinct
from the list transient's `<up>`/`<down>` (a *different* prefix). Task 6 adds a test
asserting each is bound via `ogt--transient-suffix-plist`.

---

## RISKY EDIT — the `name` relocation call-site inventory

`name` moves OUT of `--build-state` into a new `--build-name`. Every reader of
`(assq 'name --build-state)` / `(alist-get 'name --build-state)` must change.
Readers of `name` on a **stored spec** (not build-state) stay valid because
`name` remains at the top level of BOTH a flat spec and a `blocks` spec.

**MUST CHANGE (read `name` from `--build-state`):**

1. `--build-summary` — `org-gtd-view-manager.el:468`
   `(or (cdr (assq 'name org-gtd-view-manager--build-state)) "Untitled")`
   → read `org-gtd-view-manager--build-name`. (Task 5)
2. `--save` current-name — `org-gtd-view-manager.el:625`
   `(or (cdr (assq 'name org-gtd-view-manager--build-state)) "Untitled")`
   → `org-gtd-view-manager--build-name`. (Task 4)
3. `--save` spec assembly — `org-gtd-view-manager.el:628-631`
   `(cons (cons 'name name) (assq-delete-all 'name (--compile --build-state)))`
   → `(--compile-view name --build-sections)` (name already embedded by
   `--compile-view`; sync the active section first). (Task 4)
4. `--build` seed — `org-gtd-view-manager.el:750-753`
   `(setq --build-state (copy-alist (or starting-spec (list (cons 'name "Untitled")
   (cons 'type 'next-action)))))` → `(org-gtd-view-manager--build-load starting-spec)`
   which splits name out into `--build-name` and sets up `--build-sections`/
   `--build-active`/`--build-state`. (Task 3 defines `--build-load`; Task 6 wires it
   into `--build`.)
5. `--preview-now` compile — `org-gtd-view-manager.el:522`
   `(org-gtd-view-manager--compile org-gtd-view-manager--build-state)`
   → `(org-gtd-view-manager--compile-current-view)` (sync active + compile whole
   view). (Task 6)

**STAYS VALID (reads `name` from a stored/seed spec, not build-state):**

- `--build` original-name — `org-gtd-view-manager.el:748-749`
  `(and starting-spec (alist-get 'name starting-spec))` — stored spec, unchanged.
- `--list-duplicate` copy-name — `org-gtd-view-manager.el:852-854`
  `(alist-get 'name spec)` + `(assq-delete-all 'name (copy-alist spec))` — stored
  spec. Works for a flat spec; for a `blocks` spec it strips name and leaves
  `(blocks …)`, which `--build-load` (Task 3) handles. No change needed.
- `--migrate` — `org-gtd-view-manager.el:416,429` — legacy flat entries, unchanged.
- `--store-*` — name is the alist key throughout, unaffected.

---

### Task 1: Compile split — `--compile-section` + `--compile-view`

**Files:**
- Modify: `org-gtd-view-manager.el:365-382` (Compile section)
- Test: `test/unit/view-manager-compile-test.el`

Introduce the two new pure compilers. **Leave `org-gtd-view-manager--compile`
in place for now** — `--build-summary`, `--save`, and `--preview-now` still call
it and are migrated in Tasks 4/5/6; Task 6 deletes `--compile` once its last
caller is gone.

**Step 1: Write the failing tests**

Add to `test/unit/view-manager-compile-test.el` (keep the existing tests; add
these). Also **retarget the five existing tests** from
`org-gtd-view-manager--compile` to `org-gtd-view-manager--compile-section`
(behavior is identical for their assertions — none assert `name`):

```elisp
(deftest view-manager-compile-section/drops-name ()
  "A section spec carries no name (the view name lives at view level)."
  (let ((sec (org-gtd-view-manager--compile-section
              '((name . "x") (type . next-action)))))
    (assert-nil (assq 'name sec))
    (assert-equal 'next-action (cdr (assq 'type sec)))))

(deftest view-manager-compile-view/one-section-is-flat ()
  "One section compiles to a FLAT spec (name at top, no `blocks')."
  (let ((spec (org-gtd-view-manager--compile-view
               "My View" '(((type . next-action) (area-of-focus . "Work"))))))
    (assert-equal "My View" (cdr (assq 'name spec)))
    (assert-nil (assq 'blocks spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))
    (assert-equal "Work" (cdr (assq 'area-of-focus spec)))))

(deftest view-manager-compile-view/many-sections-use-blocks ()
  "Two+ sections compile to a `((name) (blocks (S0 S1 …)))' spec."
  (let* ((spec (org-gtd-view-manager--compile-view
                "Engage"
                '(((type . calendar))
                  ((type . next-action) (area-of-focus . "Work"))
                  ((type . delegated)))))
         (blocks (cdr (assq 'blocks spec))))
    (assert-equal "Engage" (cdr (assq 'name spec)))
    (assert-equal 3 (length blocks))
    (assert-equal 'calendar (cdr (assq 'type (nth 0 blocks))))
    (assert-equal "Work" (cdr (assq 'area-of-focus (nth 1 blocks))))
    (assert-equal 'delegated (cdr (assq 'type (nth 2 blocks))))
    ;; No section carries a name.
    (assert-nil (assq 'name (nth 0 blocks)))))
```

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el`
Expected: FAIL — `void-function org-gtd-view-manager--compile-section` /
`--compile-view`.

**Step 3: Minimal implementation**

In `org-gtd-view-manager.el`, in the `;;;; Compile` section (after the existing
`--compile` at line 382), add:

```elisp
(defun org-gtd-view-manager--compile-section (section)
  "Compile one SECTION alist (no name) into a filtered spec.
Keys whose value is nil are omitted so the DSL applies its own
defaults; only curated filter keys pass through, values already in DSL
shape.  `name' is NOT in the allow-list, so a stray name is dropped --
the view name lives at view level, never inside a section."
  (let ((allowed (mapcar #'car org-gtd-view-manager--filter-specs))
        result)
    (dolist (cell section)
      (when (and (memq (car cell) allowed)
                 (not (null (cdr cell))))
        ;; Fresh cons per key (P1): the compiled spec is cached and the
        ;; build state is mutated in place; sharing a cell would corrupt
        ;; the cache and defeat `--preview-changed-p'.
        (push (cons (car cell) (cdr cell)) result)))
    (nreverse result)))

(defun org-gtd-view-manager--compile-view (name sections)
  "Assemble a stored view spec from NAME and SECTIONS.
ONE section yields a FLAT spec identical to today's single-section
output (back-compat, no `blocks').  TWO OR MORE sections yield a
`((name . NAME) (blocks . (S0 S1 …)))' spec.  Each Sn is
`org-gtd-view-manager--compile-section' of the nth section."
  (let ((compiled (mapcar #'org-gtd-view-manager--compile-section sections)))
    (if (= (length compiled) 1)
        (cons (cons 'name name) (car compiled))
      (list (cons 'name name)
            (cons 'blocks compiled)))))
```

**Step 4: Run to verify green**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el`
Expected: PASS (all existing + 3 new).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-compile-test.el
git commit -m "feat(view-manager): add --compile-section and --compile-view"
```

---

### Task 2: Section state model + pure section ops

**Files:**
- Modify: `org-gtd-view-manager.el` — add defvars near the builder state block
  (`:450-460`) and the section-ops functions in a new `;;;; Section state`
  subsection just before `;;;; Builder transient` (`:443`).
- Test: `test/unit/view-manager-sections-test.el` (new file)

Add `--build-name`, `--build-sections`, `--build-active`, plus the pure ops:
`--build-sync-active`, `--build-switch-to`, `--build-add-section`,
`--build-next-section`, `--build-prev-section`, `--build-delete-section`
(min-one guard), `--build-move-section-up`, `--build-move-section-down`
(bounds). All operate on the state vars via direct calls — no live transient.

**Key invariant (why sync exists):** `--set-value` reassigns `--build-state` to a
NEW list when it unsets a key (`assq-delete-all`), so `--build-state` can diverge
from its section slot. Therefore every op that changes the active index or the
section list FIRST calls `--build-sync-active` (write `--build-state` back into
`(nth active sections)`), THEN mutates, THEN reloads `--build-state` from the new
active slot.

**Step 1: Write the failing tests**

Create `test/unit/view-manager-sections-test.el`:

```elisp
;;; view-manager-sections-test.el --- Tests for the section state model -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Pure unit tests for the multi-section builder state transitions.
;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

(e-unit-initialize)

(defun view-manager-sections-test--seed (sections active)
  "Seed the builder section state for a test."
  (setq org-gtd-view-manager--build-name "V")
  (setq org-gtd-view-manager--build-sections sections)
  (setq org-gtd-view-manager--build-active active)
  (setq org-gtd-view-manager--build-state (nth active sections)))

(deftest view-manager-sections/sync-writes-state-back ()
  "Sync copies the live --build-state into its section slot."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'next-action)) (list (cons 'type 'delegated))) 0)
  ;; Simulate --set-value reassigning --build-state to a new list.
  (setq org-gtd-view-manager--build-state
        (list (cons 'type 'next-action) (cons 'area-of-focus "Work")))
  (org-gtd-view-manager--build-sync-active)
  (assert-equal "Work"
                (cdr (assq 'area-of-focus
                           (nth 0 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/add-appends-and-activates ()
  "Add appends a default next-action section and makes it active."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))) 0)
  (org-gtd-view-manager--build-add-section)
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state))))

(deftest view-manager-sections/next-prev-switch-active ()
  "Next/prev move the active index and reload --build-state; clamped at ends."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))
         (list (cons 'type 'delegated))) 0)
  (org-gtd-view-manager--build-next-section)
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state)))
  (org-gtd-view-manager--build-prev-section)
  (assert-equal 0 org-gtd-view-manager--build-active)
  ;; prev at 0 is a no-op (clamped).
  (org-gtd-view-manager--build-prev-section)
  (assert-equal 0 org-gtd-view-manager--build-active)
  ;; next past the end is a no-op (clamped).
  (org-gtd-view-manager--build-next-section)
  (org-gtd-view-manager--build-next-section)
  (org-gtd-view-manager--build-next-section)
  (assert-equal 2 org-gtd-view-manager--build-active))

(deftest view-manager-sections/delete-refuses-last ()
  "Deleting the only section is refused; state is unchanged."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'next-action))) 0)
  (assert-nil (org-gtd-view-manager--build-delete-section))
  (assert-equal 1 (length org-gtd-view-manager--build-sections)))

(deftest view-manager-sections/delete-active-moves-to-neighbor ()
  "Deleting the active section drops it and clamps active to a neighbor."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))
         (list (cons 'type 'delegated))) 2)
  (assert-true (org-gtd-view-manager--build-delete-section))
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state))))

(deftest view-manager-sections/move-up-swaps-and-follows ()
  "Move-up swaps with the previous section; active follows the moved one."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 1)
  (org-gtd-view-manager--build-move-section-up)
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'next-action
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections))))
  (assert-equal 'calendar
                (cdr (assq 'type (nth 1 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/move-up-at-top-is-noop ()
  "Move-up at index 0 changes nothing."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 0)
  (org-gtd-view-manager--build-move-section-up)
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'calendar
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/move-down-at-bottom-is-noop ()
  "Move-down at the last index changes nothing."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 1)
  (org-gtd-view-manager--build-move-section-down)
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action
                (cdr (assq 'type (nth 1 org-gtd-view-manager--build-sections)))))

(provide 'view-manager-sections-test)
;;; view-manager-sections-test.el ends here
```

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el`
Expected: FAIL — `void-function org-gtd-view-manager--build-sync-active` etc.

**Step 3: Minimal implementation**

Add defvars alongside the existing builder state (after
`org-gtd-view-manager.el:456-460`):

```elisp
(defvar org-gtd-view-manager--build-name "Untitled"
  "The view name being built.  View-level, split out of the section alists.")
(defvar org-gtd-view-manager--build-sections nil
  "Ordered list of section alists (each a key -> value alist, NO name).")
(defvar org-gtd-view-manager--build-active 0
  "Index of the active section within `org-gtd-view-manager--build-sections'.")
```

Add a new `;;;; Section state` subsection just before `;;;; Builder transient`
(`org-gtd-view-manager.el:443`):

```elisp
;;;; Section state

;; The infix layer keeps editing `org-gtd-view-manager--build-state' (the ACTIVE
;; section's alist).  These ops manage the surrounding section list.  Because
;; `--set-value' can REASSIGN `--build-state' to a fresh list (assq-delete-all on
;; an unset), every op that changes the active index or the section list first
;; syncs `--build-state' back into its slot, then reloads it from the new slot.

(defun org-gtd-view-manager--build-sync-active ()
  "Write the live `--build-state' back into its section slot."
  (when (and org-gtd-view-manager--build-sections
             (integerp org-gtd-view-manager--build-active)
             (>= org-gtd-view-manager--build-active 0)
             (< org-gtd-view-manager--build-active
                (length org-gtd-view-manager--build-sections)))
    (setf (nth org-gtd-view-manager--build-active
               org-gtd-view-manager--build-sections)
          org-gtd-view-manager--build-state)))

(defun org-gtd-view-manager--build-switch-to (index)
  "Sync the active section, then make (clamped) INDEX active and load it."
  (org-gtd-view-manager--build-sync-active)
  (setq org-gtd-view-manager--build-active
        (max 0 (min index (1- (length org-gtd-view-manager--build-sections)))))
  (setq org-gtd-view-manager--build-state
        (nth org-gtd-view-manager--build-active
             org-gtd-view-manager--build-sections)))

(defun org-gtd-view-manager--build-add-section ()
  "Append a default next-action section and switch to it."
  (org-gtd-view-manager--build-sync-active)
  (setq org-gtd-view-manager--build-sections
        (append org-gtd-view-manager--build-sections
                (list (list (cons 'type 'next-action)))))
  (org-gtd-view-manager--build-switch-to
   (1- (length org-gtd-view-manager--build-sections))))

(defun org-gtd-view-manager--build-next-section ()
  "Switch to the next section (clamped at the last)."
  (org-gtd-view-manager--build-switch-to
   (1+ org-gtd-view-manager--build-active)))

(defun org-gtd-view-manager--build-prev-section ()
  "Switch to the previous section (clamped at the first)."
  (org-gtd-view-manager--build-switch-to
   (1- org-gtd-view-manager--build-active)))

(defun org-gtd-view-manager--build-delete-section ()
  "Delete the active section, refusing to delete the last one.
Returns non-nil when a section was deleted, nil when refused."
  (if (<= (length org-gtd-view-manager--build-sections) 1)
      (progn (message "A view needs at least one section") nil)
    (org-gtd-view-manager--build-sync-active)
    (let ((i org-gtd-view-manager--build-active))
      (setq org-gtd-view-manager--build-sections
            (append (seq-take org-gtd-view-manager--build-sections i)
                    (seq-drop org-gtd-view-manager--build-sections (1+ i))))
      (setq org-gtd-view-manager--build-active
            (min i (1- (length org-gtd-view-manager--build-sections))))
      (setq org-gtd-view-manager--build-state
            (nth org-gtd-view-manager--build-active
                 org-gtd-view-manager--build-sections))
      t)))

(defun org-gtd-view-manager--build-move-section-up ()
  "Swap the active section with the one before it; active follows it.
No-op at index 0."
  (when (> org-gtd-view-manager--build-active 0)
    (org-gtd-view-manager--build-sync-active)
    (let* ((i org-gtd-view-manager--build-active)
           (secs (copy-sequence org-gtd-view-manager--build-sections))
           (above (nth (1- i) secs))
           (here (nth i secs)))
      (setf (nth (1- i) secs) here)
      (setf (nth i secs) above)
      (setq org-gtd-view-manager--build-sections secs)
      (setq org-gtd-view-manager--build-active (1- i))
      (setq org-gtd-view-manager--build-state
            (nth org-gtd-view-manager--build-active secs))
      t)))

(defun org-gtd-view-manager--build-move-section-down ()
  "Swap the active section with the one after it; active follows it.
No-op at the last index."
  (when (< org-gtd-view-manager--build-active
           (1- (length org-gtd-view-manager--build-sections)))
    (org-gtd-view-manager--build-sync-active)
    (let* ((i org-gtd-view-manager--build-active)
           (secs (copy-sequence org-gtd-view-manager--build-sections))
           (here (nth i secs))
           (below (nth (1+ i) secs)))
      (setf (nth i secs) below)
      (setf (nth (1+ i) secs) here)
      (setq org-gtd-view-manager--build-sections secs)
      (setq org-gtd-view-manager--build-active (1+ i))
      (setq org-gtd-view-manager--build-state
            (nth org-gtd-view-manager--build-active secs))
      t)))
```

**Step 4: Run to verify green**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-sections-test.el
git commit -m "feat(view-manager): add section state model and section ops"
```

---

### Task 3: Load — seed section state from a spec (or fresh)

**Files:**
- Modify: `org-gtd-view-manager.el` — add `--build-load` to the `;;;; Section
  state` subsection (after the ops from Task 2).
- Test: `test/unit/view-manager-sections-test.el` (append)

**Step 1: Write the failing tests**

Append to `test/unit/view-manager-sections-test.el`:

```elisp
(deftest view-manager-load/fresh-is-one-default-section ()
  "A nil spec seeds one Untitled next-action section."
  (org-gtd-view-manager--build-load nil)
  (assert-equal "Untitled" org-gtd-view-manager--build-name)
  (assert-equal 1 (length org-gtd-view-manager--build-sections))
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-nil (assq 'name org-gtd-view-manager--build-state)))

(deftest view-manager-load/flat-spec-is-one-section ()
  "A flat spec loads name + one section (spec minus name)."
  (org-gtd-view-manager--build-load
   '((name . "Saved") (type . delegated) (who . "Sam")))
  (assert-equal "Saved" org-gtd-view-manager--build-name)
  (assert-equal 1 (length org-gtd-view-manager--build-sections))
  (assert-nil (assq 'name (nth 0 org-gtd-view-manager--build-sections)))
  (assert-equal 'delegated (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-equal "Sam" (cdr (assq 'who org-gtd-view-manager--build-state))))

(deftest view-manager-load/blocks-spec-loads-section-list ()
  "A blocks spec loads name + the section list, active at 0."
  (org-gtd-view-manager--build-load
   '((name . "Engage")
     (blocks . (((type . calendar))
                ((type . next-action) (area-of-focus . "Work"))))))
  (assert-equal "Engage" org-gtd-view-manager--build-name)
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'calendar (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-equal "Work"
                (cdr (assq 'area-of-focus
                           (nth 1 org-gtd-view-manager--build-sections)))))
```

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el`
Expected: FAIL — `void-function org-gtd-view-manager--build-load`.

**Step 3: Minimal implementation**

Add to the `;;;; Section state` subsection:

```elisp
(defun org-gtd-view-manager--build-load (starting-spec)
  "Seed the builder section state from STARTING-SPEC (or fresh when nil).
Flat spec -> one section (spec minus name).  `blocks' spec -> its
section list.  nil -> one default Untitled next-action section.
Sets `--build-name', `--build-sections', `--build-active' (0) and loads
`--build-state' from the first section."
  (cond
   ((null starting-spec)
    (setq org-gtd-view-manager--build-name "Untitled")
    (setq org-gtd-view-manager--build-sections
          (list (list (cons 'type 'next-action)))))
   ((assq 'blocks starting-spec)
    (setq org-gtd-view-manager--build-name
          (or (alist-get 'name starting-spec) "Untitled"))
    (setq org-gtd-view-manager--build-sections
          (mapcar #'copy-alist (alist-get 'blocks starting-spec))))
   (t
    (setq org-gtd-view-manager--build-name
          (or (alist-get 'name starting-spec) "Untitled"))
    (setq org-gtd-view-manager--build-sections
          (list (assq-delete-all 'name (copy-alist starting-spec))))))
  (setq org-gtd-view-manager--build-active 0)
  (setq org-gtd-view-manager--build-state
        (nth 0 org-gtd-view-manager--build-sections)))
```

**Step 4: Run to verify green**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-sections-test.el
git commit -m "feat(view-manager): load section state from flat/blocks/fresh spec"
```

---

### Task 4: `--save` — relocate name reads, round-trip multi-section

**Files:**
- Modify: `org-gtd-view-manager.el:615-657` (`--save`)
- Test: `test/unit/view-manager-list-test.el` (existing save tests) +
  `test/unit/view-manager-build-test.el` (`save-rejects-blank-name`) + a new
  multi-section round-trip test.

Rewrite `--save` to read the name from `--build-name`, compile the whole view via
`--compile-view` (after syncing the active section), and keep every existing
guard (blank-name, overwrite-different, rename-move, edit-save-same-name).

**Step 1: Write the failing test**

The existing save tests (`view-manager-list/rename-is-a-move-not-a-copy`,
`.../edit-save-same-name-no-overwrite-prompt`,
`view-manager-build/save-rejects-blank-name`) currently seed `--build-state` with
a `name`. They MUST be updated to also seed `--build-name`/`--build-sections`/
`--build-active`. Update them AND add the round-trip.

In `test/unit/view-manager-list-test.el`, change the three seed blocks. For
`rename-is-a-move-not-a-copy`, replace the seed with:

```elisp
  (org-gtd-view-manager--store-upsert
   "Old" (list (cons 'name "Old") (cons 'type 'next-action)))
  (org-gtd-view-manager--build-load '((name . "Old") (type . next-action)))
  (setq org-gtd-view-manager--build-original-name "Old")
  (setq org-gtd-view-manager--build-dirty t)
```

For `edit-save-same-name-no-overwrite-prompt`, replace its seed with:

```elisp
    (org-gtd-view-manager--store-upsert
     "A" (list (cons 'name "A") (cons 'type 'next-action)))
    (org-gtd-view-manager--build-load '((name . "A") (type . single-action)))
    (setq org-gtd-view-manager--build-original-name "A")
    (setq org-gtd-view-manager--build-dirty t)
```

Add a new round-trip test to `test/unit/view-manager-list-test.el`:

```elisp
(deftest view-manager-list/multi-section-save-round-trips-to-blocks ()
  "Saving a two-section view stores a blocks spec that reloads verbatim."
  (org-gtd-view-manager--build-load '((name . "Engage") (type . calendar)))
  (org-gtd-view-manager--build-add-section) ;; section 2 = next-action
  (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Work")
  (setq org-gtd-view-manager--build-original-name "Engage")
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Engage"))
            ((symbol-function 'org-gtd-view-manager--build-restore-windows)
             #'ignore))
    (org-gtd-view-manager--save))
  (let ((spec (org-gtd-view-manager--store-get "Engage")))
    (assert-equal 2 (length (cdr (assq 'blocks spec))))
    (assert-equal 'calendar
                  (cdr (assq 'type (nth 0 (cdr (assq 'blocks spec))))))
    (assert-equal "Work"
                  (cdr (assq 'area-of-focus
                             (nth 1 (cdr (assq 'blocks spec))))))))
```

In `test/unit/view-manager-build-test.el`, update `save-rejects-blank-name`'s
seed:

```elisp
  (org-gtd-view-manager--build-load '((name . "Untitled") (type . next-action)))
  (setq org-gtd-view-manager--build-dirty t)
```

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: FAIL — the updated tests still call the OLD `--save`, which reads
`(assq 'name --build-state)` (now nil) and cons-name-wraps `--compile`, so the
round-trip has no `blocks` and the current-name defaults to "Untitled" not the
loaded name.

**Step 3: Minimal implementation**

Replace `--save` (`org-gtd-view-manager.el:615-657`) with:

```elisp
(defun org-gtd-view-manager--save ()
  "Persist the built view, guarding against blank names and silent overwrite.
Prompts for a name (defaulting to `org-gtd-view-manager--build-name').
The active section is synced, then the whole view is compiled via
`org-gtd-view-manager--compile-view' -- a FLAT spec for one section,
a `blocks' spec for many.  Blank names are rejected (fail-soft: builder
reopened, nothing written); overwriting a DIFFERENT existing view
prompts; a save under a changed name is a rename-move (old entry
deleted); saving back to the same name is silent."
  (interactive)
  (let* ((name (read-string "Name this view: " org-gtd-view-manager--build-name))
         (spec (progn
                 (org-gtd-view-manager--build-sync-active)
                 (org-gtd-view-manager--compile-view
                  name org-gtd-view-manager--build-sections))))
    (cond
     ((string-blank-p name)
      (org-gtd-view-manager--build-resume)
      (user-error "A view needs a name"))
     ((and (org-gtd-view-manager--store-get name)
           (not (equal name org-gtd-view-manager--build-original-name))
           (not (y-or-n-p (format "A view named '%s' exists — overwrite? " name))))
      (message "Save cancelled")
      (org-gtd-view-manager--build-resume))
     (t
      (org-gtd-view-manager--store-upsert name spec)
      (when (and org-gtd-view-manager--build-original-name
                 (not (equal org-gtd-view-manager--build-original-name name)))
        (org-gtd-view-manager--store-delete
         org-gtd-view-manager--build-original-name))
      (setq org-gtd-view-manager--build-name name)
      (setq org-gtd-view-manager--build-original-name nil)
      (setq org-gtd-view-manager--build-dirty nil)
      (message "Saved view '%s'" name)
      (org-gtd-view-manager--build-restore-windows)))))
```

**Step 4: Run to verify green**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
then `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
Expected: PASS (note: `view-manager-build-test.el`'s summary/preview-on-open
tests still reference the OLD summary/preview and may fail here — those are fixed
in Tasks 5/6. If they fail now, that is expected; re-run after Task 6. To keep
each task green in isolation, run only the specific tests touched here, or accept
that `build-test` fully greens at Task 6.)

> **Sequencing note:** `view-manager-build-test.el` mixes save-tests (this task)
> with summary/preview-on-open tests (Tasks 5/6). It becomes fully green at Task 6.
> Verify the save tests specifically here (they are independent of summary/preview).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-list-test.el \
        test/unit/view-manager-build-test.el
git commit -m "feat(view-manager): --save compiles whole view; name from --build-name"
```

---

### Task 5: Badge summarizes `blocks`; summary is view-aware

**Files:**
- Modify: `org-gtd-view-manager.el:347-363` (`--badge`) — split into a
  `--badge-section` (today's body) + a dispatcher `--badge`.
- Modify: `org-gtd-view-manager.el:462-471` (`--build-summary`).
- Test: `test/unit/view-manager-badge-test.el` (append) +
  `test/unit/view-manager-build-test.el` (`summary-label-is-a-heading`).

**Step 1: Write the failing tests**

Append to `test/unit/view-manager-badge-test.el`:

```elisp
(deftest view-manager-badge/blocks-spec-summarized ()
  "A blocks spec summarizes as `N sections: b0 · b1 · …', not an empty badge."
  (assert-equal
   "2 sections: calendar · next-action · Work"
   (org-gtd-view-manager--badge
    '((name . "Engage")
      (blocks . (((type . calendar))
                 ((type . next-action) (area-of-focus . "Work"))))))))

(deftest view-manager-badge/flat-spec-unchanged ()
  "A flat spec still badges via the per-section formatter."
  (assert-equal "next-action · Home"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . next-action)
                   (area-of-focus . "Home")))))
```

Rewrite `summary-label-is-a-heading` in `test/unit/view-manager-build-test.el`
to seed the new state and assert the view-aware content:

```elisp
(deftest view-manager-build/summary-label-is-a-heading ()
  "The summary's `View:' label carries `transient-heading' and shows the
view name plus a section marker."
  (org-gtd-view-manager--build-load '((name . "Untitled") (type . next-action)))
  (let ((summary (org-gtd-view-manager--build-summary)))
    (assert-equal 'transient-heading (get-text-property 0 'face summary))
    (assert-true (string-prefix-p "View: " (substring-no-properties summary)))
    (assert-true (string-match-p "Untitled" (substring-no-properties summary)))
    (assert-true (string-match-p "Section 1/1"
                                 (substring-no-properties summary)))))
```

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-badge-test.el`
Expected: FAIL — `blocks-spec-summarized` gets `""` (current `--badge` finds no
filter keys at the top level of a blocks spec).

**Step 3: Minimal implementation**

Split `--badge` (`org-gtd-view-manager.el:347-363`). Rename today's body to
`--badge-section` and add a dispatcher:

```elisp
(defun org-gtd-view-manager--badge-section (spec)
  "Return a compact one-line summary of a SINGLE section/flat SPEC (name excluded)."
  (let (parts)
    (dolist (entry org-gtd-view-manager--filter-specs)
      (let* ((key (car entry))
             (cell (assq key spec)))
        (when cell
          (let* ((val (cdr cell))
                 (fmt (funcall (plist-get (cdr entry) :formatter) val)))
            (push (cond
                   ((memq key '(not-done not-habit)) (symbol-name key))
                   ((memq key '(who tags priority)) (format "%s=%s" key fmt))
                   (t fmt))
                  parts)))))
    (string-join (nreverse parts) " · ")))

(defun org-gtd-view-manager--badge (spec)
  "Return a compact one-line summary of SPEC (name excluded).
A multi-section `blocks' spec summarizes as `N sections: b0 · b1 · …';
a flat/single spec lists its filter values."
  (let ((blocks (alist-get 'blocks spec)))
    (if blocks
        (format "%d sections: %s"
                (length blocks)
                (string-join
                 (mapcar #'org-gtd-view-manager--badge-section blocks)
                 " · "))
      (org-gtd-view-manager--badge-section spec))))
```

Rewrite `--build-summary` (`org-gtd-view-manager.el:462-471`):

```elisp
(defun org-gtd-view-manager--build-summary ()
  "Return the view-aware summary header for the builder.
Shows the view name as a `transient-heading', the active-section
marker (`Section i/N'), and a compact badge per section with `▸' on
the active one."
  (org-gtd-view-manager--build-sync-active)
  (let* ((n (length org-gtd-view-manager--build-sections))
         (active org-gtd-view-manager--build-active)
         (badges
          (string-join
           (seq-map-indexed
            (lambda (sec i)
              (let ((b (org-gtd-view-manager--badge-section sec)))
                (if (= i active) (concat "▸ " b) b)))
            org-gtd-view-manager--build-sections)
           " | ")))
    (concat
     (propertize "View: " 'face 'transient-heading)
     org-gtd-view-manager--build-name
     "  —  "
     (format "Section %d/%d" (1+ active) n)
     "   [ " badges " ]")))
```

**Step 4: Run to verify green**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-badge-test.el`
Expected: PASS (existing flat-spec badge tests still pass via `--badge-section`).
Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: PASS (list `--rows` uses `--badge`, now blocks-aware).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-badge-test.el \
        test/unit/view-manager-build-test.el
git commit -m "feat(view-manager): blocks-aware badge and view-aware summary"
```

---

### Task 6: Builder — Sections group, whole-view preview, wire load

**Files:**
- Modify: `org-gtd-view-manager.el` — interactive section commands (new, before
  the builder macro at `:670`); `--compile-current-view` (new, near preview);
  `--preview-now` (`:514-529`); the builder macro's prefix layout (`:729-764`);
  the `--build` interactive body seed (`:750-753` → `--build-load`); remove the
  now-unused `--compile` (`:367-382`).
- Test: `test/unit/view-manager-build-test.el` (section keys, preview-on-open),
  `test/unit/view-manager-preview-test.el` (retarget `--compile`, composite
  preview).

**Step 1: Write the failing tests**

Add to `test/unit/view-manager-build-test.el`:

```elisp
(deftest view-manager-build/section-keys-are-bound ()
  "The Sections group binds all six section-management keys."
  (dolist (key '("M-a" "M-n" "M-p" "M-k" "M-<up>" "M-<down>"))
    (let ((plist (ogt--transient-suffix-plist
                  'org-gtd-view-manager--build key)))
      (assert-equal key (plist-get plist :key)))))

(deftest view-manager-build/preview-on-open-compiles-whole-view ()
  "Opening a builder on a blocks spec previews the composite (blocks) spec."
  (let ((captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (setq captured spec))))
      (org-gtd-view-manager--build
       '((name . "Engage")
         (blocks . (((type . calendar)) ((type . next-action)))))))
    (assert-equal 2 (length (cdr (assq 'blocks captured))))
    (assert-equal "Engage" (cdr (assq 'name captured)))))
```

In `test/unit/view-manager-preview-test.el`:
- Retarget `view-manager-preview/compile-does-not-alias-build-state` (line ~46)
  from `org-gtd-view-manager--compile` to `org-gtd-view-manager--compile-section`.
- Rewrite `ret-forces-render-when-cache-current`, `debounce-still-skips-unchanged`
  and `re-renders-on-value-change` to set up the section state and seed
  `--preview-last` via `--compile-current-view` (the exact path `--preview-now`
  uses). Example for `debounce-still-skips-unchanged`:

```elisp
(deftest view-manager-preview/debounce-still-skips-unchanged ()
  "The debounced (non-forced) path still skips a genuinely unchanged spec."
  (let ((count 0))
    (org-gtd-view-manager--build-load '((name . "x") (type . next-action)))
    (setq org-gtd-view-manager--preview-last
          (org-gtd-view-manager--compile-current-view))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview-now))
    (assert-equal 0 count)))
```

`ret-forces-render-when-cache-current` — same setup, call `--preview`, expect
count 1. `re-renders-on-value-change` — `--build-load` a single-section view,
render, then `(setf (alist-get 'area-of-focus --build-state) "Home")`, render
again, expect count 2 (`--compile-current-view` syncs the mutated state).

**Step 2: Run to verify red**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
Expected: FAIL — section keys unbound; preview-on-open still flat/void
`--compile-current-view`.

**Step 3: Minimal implementation**

(a) Add interactive section commands before the builder macro
(`org-gtd-view-manager.el:670`). Next/prev only switch (transient auto-redraws
the summary); add/delete/move change the composite, so they mark dirty and
schedule a preview:

```elisp
(defun org-gtd-view-manager--section-add ()
  "Add a section, mark dirty, refresh preview."
  (interactive)
  (org-gtd-view-manager--build-add-section)
  (setq org-gtd-view-manager--build-dirty t)
  (org-gtd-view-manager--preview-schedule))

(defun org-gtd-view-manager--section-next ()
  "Switch to the next section."
  (interactive)
  (org-gtd-view-manager--build-next-section))

(defun org-gtd-view-manager--section-prev ()
  "Switch to the previous section."
  (interactive)
  (org-gtd-view-manager--build-prev-section))

(defun org-gtd-view-manager--section-delete ()
  "Delete the active section (refused if it is the last)."
  (interactive)
  (when (org-gtd-view-manager--build-delete-section)
    (setq org-gtd-view-manager--build-dirty t)
    (org-gtd-view-manager--preview-schedule)))

(defun org-gtd-view-manager--section-move-up ()
  "Move the active section up."
  (interactive)
  (when (org-gtd-view-manager--build-move-section-up)
    (setq org-gtd-view-manager--build-dirty t)
    (org-gtd-view-manager--preview-schedule)))

(defun org-gtd-view-manager--section-move-down ()
  "Move the active section down."
  (interactive)
  (when (org-gtd-view-manager--build-move-section-down)
    (setq org-gtd-view-manager--build-dirty t)
    (org-gtd-view-manager--preview-schedule)))
```

(b) Add `--compile-current-view` in the `;;;; Live preview` section (before
`--preview-now`, `org-gtd-view-manager.el:514`):

```elisp
(defun org-gtd-view-manager--compile-current-view ()
  "Sync the active section, then compile the whole view to a stored spec."
  (org-gtd-view-manager--build-sync-active)
  (org-gtd-view-manager--compile-view
   org-gtd-view-manager--build-name
   org-gtd-view-manager--build-sections))
```

(c) Change `--preview-now` (`org-gtd-view-manager.el:522`) to compile the
composite (keep P1 force + changed-p, P2 lives in `--render-preview`):

```elisp
  (let ((spec (org-gtd-view-manager--compile-current-view)))
```

(d) In the builder macro, add a Sections group to the prefix layout. After
`,@rows` and before the `["Actions" …]` group (`org-gtd-view-manager.el:738`),
insert:

```elisp
         ["Sections"
          ("M-a"      "Add"       org-gtd-view-manager--section-add       :transient t)
          ("M-n"      "Next"      org-gtd-view-manager--section-next      :transient t)
          ("M-p"      "Prev"      org-gtd-view-manager--section-prev      :transient t)
          ("M-k"      "Delete"    org-gtd-view-manager--section-delete    :transient t)
          ("M-<up>"   "Move up"   org-gtd-view-manager--section-move-up   :transient t)
          ("M-<down>" "Move down" org-gtd-view-manager--section-move-down :transient t)]
```

(e) In the `--build` interactive body, replace the seed
(`org-gtd-view-manager.el:750-753`) with:

```elisp
         (org-gtd-view-manager--build-load starting-spec)
```

Leave the `--build-original-name` line (`:748-749`), the dirty reset, the
`--preview-last nil` reset, and the iv4b `(org-gtd-view-manager--preview-now t)`
before `transient-setup` intact. `--preview-now` now compiles the composite via
`--compile-current-view`, which reads the just-loaded section state.

(f) Delete the now-unused `--compile` (`org-gtd-view-manager.el:367-382`) — its
last callers (summary, save, preview) were migrated in Tasks 4/5/6.

**Step 4: Run to verify green**

Run in order:
- `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
- `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
- `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el`
Expected: PASS. Confirm `every-infix-and-action-key-is-bound` (layout regression
guard) and the two iv4b on-open tests still pass — single-section views stay flat,
so `(alist-get 'type captured)`/`(alist-get 'name captured)` still resolve.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el \
        test/unit/view-manager-preview-test.el
git commit -m "feat(view-manager): Sections transient group and whole-view preview"
```

---

### Task 7: Regression sweep, compile, and manual re-QA

**Files:** none (verification only), plus any fix commits the sweep reveals.

**Step 1: Full unit suite**

Run: `.claude/skills/test/run-tests.sh unit`
Expected: `PASS: <N> tests`. If a flake appears, re-run with the printed
`--seed=<N>` to reproduce, and check `around-each`/mock-fs setup per MEMORY.

**Step 2: Byte-compile clean**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: no warnings/errors. Then restore the Info dir file the compile deletes:

Run: `git checkout -- dir`
Never `git add` `dir` or any `*.elc`.

**Step 3: Full suite (all categories)**

Run: `.claude/skills/test/run-tests.sh`
Expected: `PASS`.

**Step 4: MANUAL re-QA (interactive/visual — design §9)**

These cannot be headless. Perform each in a live Emacs on the worktree:

1. `M-x org-gtd-view-manager` → `c` (Create). Confirm the builder opens with a
   live agenda preview, the `View: Untitled` heading, and `Section 1/1` with one
   `▸ next-action` badge.
2. Press `M-a` (Add). Confirm `Section 2/2`, a second `next-action` badge, and the
   preview redraws to a TWO-block agenda.
3. Edit section 2 (e.g. `A` area-of-focus = Work). Confirm the badge shows `▸ …
   Work` and the second agenda block filters accordingly.
4. `M-p`/`M-n` — confirm the `▸` marker and `Section i/N` move; the composite
   agenda is unchanged by a mere switch.
5. `M-<up>`/`M-<down>` — confirm the two agenda BLOCKS swap order on screen and
   the active marker follows the moved section.
6. `M-k` on one section — confirm it deletes and the preview drops to one block;
   `M-k` on the last remaining section — confirm it is refused with "A view needs
   at least one section".
7. `s` (Save) as "QA Multi". Reopen `M-x org-gtd-view-manager`; confirm the row
   badge reads `N sections: … · …`. `e` (Edit) it; confirm both sections reload.
8. **Back-compat round-trip:** Create a ONE-section view, save, and inspect
   `~/gtd/views.eld` (or the configured `org-gtd-directory`): confirm it is a FLAT
   spec (no `blocks` key). Edit it, `M-a` to add a section, save: confirm it is now
   a `blocks` spec. `M-k` back to one section, save: confirm it is FLAT again.
9. **Fail-soft:** give one section a filter that matches nothing; confirm that
   block shows org-agenda's "no matches", the other blocks still render, and no
   stack trace appears.

**Step 5: Commit any sweep fixes**

```bash
git add -A -- ':!dir' ':!*.elc'
git commit -m "test(view-manager): regression sweep for multi-section views"
```

---

## Under-specified / follow-up decisions (flagged, not guessed)

These were resolved with the least-surprising default; each is a candidate for a
follow-up decision if the maintainer disagrees:

1. **Add-section insert position.** Chosen: append at END and switch to it (Task 2
   `--build-add-section`). The design §3 says "add" without a position. Alternative:
   insert AFTER the active section. Flagged — trivial to change if "insert here" is
   preferred.
2. **Next/prev wrap vs clamp.** Chosen: CLAMP (no wrap), matching the list
   transient's Up/Down clamping. Design §8 only specifies reorder bounds, not
   switch bounds. Flag if wrap-around is wanted.
3. **Summary section-badge format.** Chosen:
   `View: <name>  —  Section i/N   [ b0 | ▸ b1 | b2 ]` (design §6 gives an
   example, not a spec). Confirm the exact glyphs/separators read well in the live
   transient during manual QA (Step 4.1).
4. **`--compile` removal.** Chosen: REMOVE it in Task 6 once its last caller is
   migrated (DRY — `--compile-section`/`--compile-view` fully replace it). The
   task brief allowed "keep or replace call sites"; removal is cleaner. If any
   out-of-tree caller depends on `--compile`, keep it as
   `(defalias 'org-gtd-view-manager--compile 'org-gtd-view-manager--compile-section)`
   instead — but note the semantic difference (the alias drops `name`).
5. **`view-manager-build-test.el` greens fully only at Task 6.** That file mixes
   save-tests (Task 4) with summary/preview tests (Tasks 5/6). This is called out
   in Task 4's Step 4 note; not a defect, just a cross-task sequencing artifact.
6. **Empty-section compile.** Design §4 asserts a section always has at least a
   `type`. `--build-add-section` guarantees this (default `next-action`), so
   `--compile-view` never emits an empty block. No dropping logic added (YAGNI).
   Flag if a user could ever reach a truly empty section (e.g. unsetting `type`
   via a future infix change) — today `type` has no unset path in the readers.

---

## Execution handoff

Plan complete. Recommended: **Subagent-Driven** (REQUIRED SUB-SKILL:
superpowers:subagent-driven-development) — one fresh subagent per task with a code
review between tasks, since the tasks are sequential and each is small and
independently verifiable.
