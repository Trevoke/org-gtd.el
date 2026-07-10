# View Manager Polish Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Land three small, self-contained UX polish fixes in the org-gtd.el View Manager (infix label/value spacing, a clearer LIST action verb, and a once-per-session sample-data banner).

**Architecture:** Each fix is an independent change to a single function/label in `org-gtd-view-manager.el`, each backed by a real headless e-unit test. The three fixes are unrelated, so each gets its own red-green TDD cycle and its own commit.

**Tech Stack:** Emacs Lisp (Emacs 28.1+), transient.el, e-unit test framework, Eldev.

---

## Environment & Guardrails (READ FIRST)

- **Work ONLY in** `/home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design`. Do NOT `cd` elsewhere. Stay on branch `view-manager-design`.
- **Run tests** with the repo's test runner (NOT the `/test` skill):
  - Single file: `.claude/skills/test/run-tests.sh test/unit/<file>.el`
  - Category: `.claude/skills/test/run-tests.sh unit`
- **Compile check** (optional per task, required before the final task's commit):
  `~/bin/eldev compile --warnings-as-errors` then immediately `git checkout -- dir`.
  **Never `git add` the `dir/` autoloads or any `.elc` file.**
- **Do NOT regress prior branch work**: P1/P2/iv4b/x3jq preview logic, the 3+2 side-by-side infix layout, and the multi-section panel + rename. Keep the current-value-or-`—` behavior and the badge/value formatting intact.
- **Do NOT touch** `org-gtd-view-language.el`.
- All three fixes live in `org-gtd-view-manager.el`. Line numbers below are anchors as of this plan; re-confirm with a `grep`/`Read` before each edit since earlier tasks do not shift later anchors (edits are in disjoint regions, but verify anyway).

---

## Fix 1 background (yak 4sly) — per-group label alignment

`org-gtd-view-manager--infix-description` (currently lines 699-709) formats every builder row as
`(format "%-13s %s" label value)`. The `13` is the width of the longest label in the *entire* table (`area-of-focus`). So short labels get a huge gap: `t type            next-action`, `w when              —`. Reported as distracting.

**Chosen scheme: Option A — pad each label to the max label width WITHIN ITS `:group`.**

Rationale: the builder already lays the infixes out as separate per-group columns (Type / Time / Structural / Metadata / Prefix), and `org-gtd-view-manager--filter-specs` carries a `:group` on every entry. Values only ever need to align *within* a column, never across columns, so a global pad is strictly wrong. Per-group padding removes the gap for short labels while keeping values tidily aligned inside each column. It is cheap and clean to compute from the single source of truth, so Option A dominates B (still gappy) and C (loses in-column alignment).

Per-group max label widths (from `--filter-specs`):

| group        | labels                                              | max width |
|--------------|-----------------------------------------------------|-----------|
| `type`       | type(4)                                              | 4         |
| `time`       | when(4), deadline(8), scheduled(9)                   | 9         |
| `structural` | todo(4), done(4), not-done(8), not-habit(9)          | 9         |
| `metadata`   | area-of-focus(13), effort(6), who(3), tags(4), priority(8) | 13  |
| `prefix`     | prefix(6), prefix-width(12)                          | 12        |

So `type` pads to 4 → `"type next-action"` (single space); `area-of-focus` pads to 13 → unchanged. Values still align down each column.

**Implementation note (verified in this environment):** Emacs Lisp `format` does NOT support `%-*s` dynamic width (`(format "%-*s" 9 "when")` errors `Invalid format operation %*`). Use `string-pad` (available since Emacs 28.1, min supported version) which left-justifies/pads on the right exactly like `%-Ns`.

---

### Task 1: Per-group infix label alignment (Fix 1, yak 4sly)

**Files:**
- Modify: `org-gtd-view-manager.el:699-709` (`org-gtd-view-manager--infix-description`) and add a small helper just above it.
- Test: `test/unit/view-manager-build-test.el` (append new tests).

**Step 1: Write the failing tests**

Append these two tests to `test/unit/view-manager-build-test.el`, just before the final `(provide 'view-manager-build-test)` line:

```elisp
(deftest view-manager-build/infix-description-tight-for-short-label ()
  "A short label pads only to its own group's max width, not a global 13.
`type' is the sole `type'-group key, so its value follows after ONE space."
  (let ((org-gtd-view-manager--build-state nil))
    (assert-equal
     "type —"
     (org-gtd-view-manager--infix-description 'type "type"))))

(deftest view-manager-build/infix-description-aligns-longest-label ()
  "The longest `metadata' label pads to the group max (13) with one trailing space."
  (let ((org-gtd-view-manager--build-state nil))
    (assert-equal
     "area-of-focus —"
     (org-gtd-view-manager--infix-description 'area-of-focus "area-of-focus"))))
```

**Step 2: Run tests to verify they fail**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
Expected: FAIL. The old `"%-13s %s"` produces `"type          —"` (13-wide pad), so
`infix-description-tight-for-short-label` fails with actual `"type          —"` vs expected `"type —"`. (`area-of-focus` may already pass since 13 == 13, but the short-label test must be red.)

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el`, replace the whole `org-gtd-view-manager--infix-description` defun (lines 699-709) with a per-group helper plus an updated formatter:

```elisp
(defun org-gtd-view-manager--group-label-width (group)
  "Return the widest label (symbol-name) width among specs in GROUP.
Labels only ever align within their own builder column, so the pad
width is the per-group max, never the global table max."
  (apply #'max
         (mapcar (lambda (entry) (length (symbol-name (car entry))))
                 (seq-filter
                  (lambda (entry) (eq (plist-get (cdr entry) :group) group))
                  org-gtd-view-manager--filter-specs))))

(defun org-gtd-view-manager--infix-description (dsl-key label)
  "Return the builder row description for DSL-KEY.
LABEL is the human-readable prefix, padded to the max label width of
DSL-KEY's `:group' so values align down each column without a global
gap.  The current value (from `org-gtd-view-manager--build-state',
rendered through the key's formatter) follows, or `—' when unset."
  (let* ((entry (assq dsl-key org-gtd-view-manager--filter-specs))
         (group (plist-get (cdr entry) :group))
         (width (org-gtd-view-manager--group-label-width group))
         (val (cdr (assq dsl-key org-gtd-view-manager--build-state)))
         (formatter (plist-get (cdr entry) :formatter)))
    (concat (string-pad label width)
            " "
            (if (null val) "—" (or (funcall formatter val) "on")))))
```

**Step 4: Run tests to verify they pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
Expected: PASS (all build-test tests, including the two new ones).

**Step 5: Manual-QA note (visual, not asserted)**

The alignment actually *looking* better is a visual nicety and is NOT unit-tested. Optional manual check: `M-x org-gtd-view-manager` → Create; confirm each column's values sit one space after the widest label in that column (e.g. `type next-action`, and Time's `when`/`deadline`/`scheduled` values line up).

**Step 6: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit -m "fix(view-manager): align infix values per group, not to a global pad"
```

---

## Fix 2 background (yak z15n) — rename LIST "Render" to "Open"

The `org-gtd-view-manager` LIST transient (lines 1214-1224) binds RET as
`("RET" "Render" org-gtd-view-manager--list-render)`. "Render" is jargon; on an empty store RET opens Engage, and the empty-state line already says "...or RET to open Engage". Rename the **display label only** to `"Open"`. Do NOT rename the command function `org-gtd-view-manager--list-render`.

Grep confirms **no test asserts the literal string "Render"** (`grep -rn "Render" test/` returns nothing). So no test needs updating — but the new test below locks the label in.

---

### Task 2: Rename LIST action label to "Open" (Fix 2, yak z15n)

**Files:**
- Modify: `org-gtd-view-manager.el:1217` (the RET suffix in `org-gtd-view-manager`).
- Test: `test/unit/view-manager-list-test.el` (append new test).

**Step 1: Write the failing test**

Append to `test/unit/view-manager-list-test.el`, just before `(provide 'view-manager-list-test)`:

```elisp
(deftest view-manager-list/ret-action-labeled-open ()
  "The RET action reads as `Open', matching the empty-state hint, not `Render'."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "RET")))
    (assert-equal "Open" (plist-get plist :description))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: FAIL — `:description` is currently `"Render"`, so assert-equal reports expected `"Open"` vs actual `"Render"`.

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el` line 1217, change only the label string:

```elisp
   [("RET" "Open"      org-gtd-view-manager--list-render)
```

(Leave the command symbol `org-gtd-view-manager--list-render` and all other suffixes untouched.)

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-list-test.el
git commit -m "fix(view-manager): label the list RET action Open, not Render"
```

---

## Fix 3 background (yak lk0a) — sample-data banner spams every render

When `org-agenda-files` is empty, `org-gtd-view-manager--render-preview` (lines 814-831) `message`s
`"sample data · your agenda-files are empty — previewing org-gtd's built-in set"` on **every** render. With the live preview firing on every filter change, rapid edits spam `*Messages*`/the echo area.

**Chosen mechanism:** a session flag `org-gtd-view-manager--sample-banner-shown`. `--render-preview` emits the banner only when the sample branch is taken AND the flag is nil, then sets it. The flag is RESET to nil in the `--build` interactive body, right where `--build-dirty`/`--preview-last` are already reset (lines 1078-1079, inside the `--define-builder-transient` macro template — these are literal `setq` forms in the generated defun's body, so a third `setq` slots in cleanly). This shows the banner at least once per builder-open when agenda-files are empty, and never in the real-agenda-files branch.

Note the reset lives in the builder-open path only; the two existing `view-manager-sample-test.el` tests each bind a fresh context but do not reset the flag between the two `--render-preview` calls — that is exactly what the new test exercises.

---

### Task 3: Show the sample-data banner once per builder session (Fix 3, yak lk0a)

**Files:**
- Modify: `org-gtd-view-manager.el` — add a defvar near the sample section (just above `org-gtd-view-manager--render-preview`, ~line 813), gate the `message` inside `--render-preview` (lines 826-831), and add a reset `setq` in the `--build` body (after line 1079).
- Test: `test/unit/view-manager-sample-test.el` (append new test).

**Step 1: Write the failing test**

Append to `test/unit/view-manager-sample-test.el`, just before `(provide 'view-manager-sample-test)`:

```elisp
(deftest view-manager-sample/banner-shows-once-per-session ()
  "With empty agenda-files, the sample banner is messaged once, not per render.
Resetting the session flag re-arms it for the next builder-open."
  (let ((org-agenda-files nil)
        (org-gtd-view-manager--sample-banner-shown nil)
        (banner-count 0))
    (cl-letf (((symbol-function 'org-gtd-view-show) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest _)
                 (when (and (stringp fmt) (string-prefix-p "sample data" fmt))
                   (setq banner-count (1+ banner-count))))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (assert-equal 1 banner-count)
      ;; Re-arm as a fresh builder-open would, and confirm it shows again.
      (setq org-gtd-view-manager--sample-banner-shown nil)
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (assert-equal 2 banner-count))))
```

**Step 2: Run test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el`
Expected: FAIL — either a void-variable error for `org-gtd-view-manager--sample-banner-shown` (defvar not yet added) or, once bound, `banner-count` is 2 after the first two renders (banner fires every time), so `(assert-equal 1 banner-count)` fails.

**Step 3: Write the minimal implementation**

3a. Add the defvar immediately above `org-gtd-view-manager--render-preview` (before line 814):

```elisp
(defvar org-gtd-view-manager--sample-banner-shown nil
  "Non-nil once the sample-data banner has been shown this builder session.
Reset when the builder opens so the banner appears at most once per
session instead of on every debounced preview render (yaks lk0a).")
```

3b. In `org-gtd-view-manager--render-preview`, gate the `message` (replace the sample branch, current lines 829-831):

```elisp
      (let ((org-agenda-files (list (org-gtd-view-manager--sample-file))))
        (unless org-gtd-view-manager--sample-banner-shown
          (message "sample data · your agenda-files are empty — previewing org-gtd's built-in set")
          (setq org-gtd-view-manager--sample-banner-shown t))
        (org-gtd-view-show spec)))))
```

(Leave `--sample-file` writing and the `org-gtd-view-show` render unchanged.)

3c. In the `--build` interactive body, add the reset right after the existing `--preview-last` reset (after line 1079, inside the macro template):

```elisp
         (setq org-gtd-view-manager--build-dirty nil)
         (setq org-gtd-view-manager--preview-last nil)
         (setq org-gtd-view-manager--sample-banner-shown nil)
```

**Step 4: Run test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el`
Expected: PASS (all three sample-test tests, including the new one). The existing two `--render-preview` tests remain green because they only inspect which files `org-gtd-view-show` sees, not the message.

**Step 5: Compile check (do this on the final task)**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: no warnings/errors. Then discard generated autoloads: `git checkout -- dir`.
**Do NOT `git add` `dir/` or any `.elc`.**

**Step 6: Manual-QA note (behavior, not asserted here)**

Optional manual check: with `org-agenda-files` empty, `M-x org-gtd-view-manager` → Create, then change several filters rapidly — the "sample data ·" banner appears once, not on every keystroke. Abort and reopen the builder — it appears once more.

**Step 7: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-sample-test.el
git commit -m "fix(view-manager): show the sample-data banner once per builder session"
```

---

## Final verification

Run the full unit category to confirm no regressions across the three fixes:

Run: `.claude/skills/test/run-tests.sh unit`
Expected: all unit tests PASS. Capture the reported seed; if anything fails, re-run with `--seed=N` to reproduce.

Confirm the working tree has no staged `dir/` or `.elc` artifacts (`git status`), and that exactly three commits were added on `view-manager-design`, one per fix.
