# View Manager: Preview Window-Steal & Effort-Clear Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix two independent View Manager builder bugs: (A) the effort filter cannot be cleared with empty input, and (B) the live agenda preview tears down the transient panel on every refresh.

**Architecture:** Bug A is a one-line reader fix in `org-gtd-view-manager--read-effort` mirroring the blank-input handling already present in the other readers; it is fully unit-testable. Bug B is a one-form `let`-binding of `org-agenda-window-setup` around the preview render in `org-gtd-view-manager--render-preview`, so the agenda renders without reorganizing the frame and destroying the transient's window; the binding is unit-testable, the visible window behavior is verified by a manual re-QA step.

**Tech Stack:** Emacs Lisp, org-mode / org-agenda, transient.el, e-unit test framework.

---

## Environment constraints (READ BEFORE STARTING — these are hard rules)

- **Work ONLY in** `/home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design`. Do NOT `cd` anywhere else. All paths below are relative to this worktree root.
- **Run tests ONLY via the runner script**, never the `/test` skill and never `eldev etest` directly:
  ```bash
  .claude/skills/test/run-tests.sh test/unit/<file>-test.el
  ```
  e-unit does NOT support single-test selection — tests run at file granularity. The runner prints a `Using seed: N` line; if a run fails, re-run with `--seed=N` to reproduce.
- **e-unit idioms** (see `test/unit/view-manager-readers-test.el` and `test/unit/view-manager-preview-test.el`): `(e-unit-initialize)` after requires; `deftest`; `assert-equal` / `assert-nil` / `assert-true`; NO `should-error` — assert on a `user-error` by wrapping in `condition-case` and matching `error-message-string`; stub functions with `cl-letf (((symbol-function '...) (lambda ...)))`.
- **Compile check** (run after each bug's code change):
  ```bash
  ~/bin/eldev compile --warnings-as-errors
  git checkout -- dir
  ```
  `compile` deletes the tracked Info `dir` file — restore it with `git checkout -- dir`. **NEVER `git add` `dir` or any `.elc` file.**
- **Do NOT regress** the just-landed P1 preview-refresh fix (commits e1e29e2, d17bbd1: `--compile` conses fresh cells; `--preview` forces a render via `--preview-now`'s `force` arg), the 0.25s debounce, the fail-soft `condition-case` error handling (design §8), or the sample-data fallback in `--render-preview`.
- **Do NOT re-open scope** into P1, the multi-section/layout yaks, or anything beyond these two bugs.
- **Keep the two bugs in separate commits.**

---

## Background: what was verified during investigation

- **Bug A reproduces headlessly.** Calling `org-gtd-view-manager--read-effort` with `read-string` stubbed to return `""` raises `user-error: "Effort needs a duration like 30m (e.g. <30m, >1h)"`. All other readers already treat blank as "unset": `--read-time` (via `--time->dsl`'s `((string-blank-p input) nil)`), `--read-prefix` (via `--parse-prefix`'s blank guard), `--read-string`, and `--read-area` (`(if (string-blank-p v) nil v)`). Only `--read-effort` lacks the guard because it passes raw input straight into `--effort->dsl`, which `user-error`s on a non-matching (blank) string. **No other reader has the same trap** — Bug A's fix is isolated to `--read-effort`.
- **Bug B's visible symptom does NOT reproduce headlessly** (it needs a live interactive frame/transient), but the *cause* is empirically confirmed: `org-agenda-window-setup` defaults to `reorganize-frame`, which deletes/rearranges the frame's windows when `org-gtd-view-show` calls `(org-agenda nil key)` — destroying the transient's window. A stub of `org-gtd-view-show` confirms `--render-preview` currently runs the render under `org-agenda-window-setup = reorganize-frame`. The fix makes it run under `current-window`, which is directly assertable via the same stub.

---

## Task 1: Bug A — clear the effort filter with empty input

**yaks:** `view-manager-builder-effort-filter-cannot-be-cleared-with-empty-input-bq02`

**Files:**
- Modify: `org-gtd-view-manager.el` (function `org-gtd-view-manager--read-effort`, ~lines 285-288)
- Test: `test/unit/view-manager-readers-test.el`

**Step 1: Write the failing tests**

Add these two `deftest`s to `test/unit/view-manager-readers-test.el`, after the existing `view-manager-reader/effort-rejects-garbage` test (around line 34). They stub `read-string` via `cl-letf`, exactly like the reader is called at runtime.

```elisp
(deftest view-manager-reader/effort-blank-unsets ()
  "A blank effort entry returns nil (unset), not a teaching error.
Regression: `--read-effort' passed raw input to `--effort->dsl',
which `user-error's on a blank string, so `e' + RET could not clear
the filter the way every other reader can."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (assert-nil (org-gtd-view-manager--read-effort)))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (assert-nil (org-gtd-view-manager--read-effort))))

(deftest view-manager-reader/effort-parses-through-reader ()
  "A non-blank effort still parses into the DSL shape via the reader."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "<30m")))
    (assert-equal '(< "30m") (org-gtd-view-manager--read-effort))))
```

Note: `view-manager-readers-test.el` already `(require 'org-gtd-view-manager)` but does NOT `(require 'cl-lib)`. Add `(require 'cl-lib)` next to the existing requires at the top of the file (the preview test already does this) so `cl-letf` is available.

**Step 2: Run the tests to verify they fail**

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-readers-test.el
```
Expected: FAIL — `view-manager-reader/effort-blank-unsets` errors/fails because `--read-effort` raises `user-error: "Effort needs a duration like 30m ..."` on blank input. (`effort-parses-through-reader` should already pass.)

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el`, change `org-gtd-view-manager--read-effort` to treat blank input as nil BEFORE calling `--effort->dsl`, mirroring `--read-area`:

```elisp
(defun org-gtd-view-manager--read-effort (&rest _)
  "Read a comparison effort and parse it into the DSL shape.
A blank entry returns nil (unset), mirroring the other readers, so the
effort filter can be cleared with an empty input rather than erroring."
  (let ((v (read-string "Effort (e.g. <30m, >1h): ")))
    (if (string-blank-p v) nil
      (org-gtd-view-manager--effort->dsl v))))
```

**Step 4: Run the tests to verify they pass**

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-readers-test.el
```
Expected: PASS — all effort/time/prefix reader tests pass, including the two new ones.

**Step 5: Compile-check**

Run:
```bash
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
```
Expected: compiles with no warnings/errors. (`git checkout -- dir` restores the Info `dir` file that `compile` deletes.) Do NOT `git add` `dir` or `.elc`.

**Step 6: Commit (Bug A only)**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-readers-test.el
git commit -m "fix(view-manager): clear effort filter on blank input

--read-effort passed raw input to --effort->dsl, which user-errors on a
blank string, so 'e' + empty RET could not unset the effort filter the
way every other reader can. Treat blank input as nil (unset) before
parsing, mirroring --read-area / --read-time / --read-prefix.

Fixes yaks view-manager-builder-effort-filter-cannot-be-cleared-with-empty-input-bq02"
```
(End the commit message with the repo's required `Co-Authored-By` trailer per CLAUDE.md.)

---

## Task 2: Bug B — keep the transient visible across preview refreshes

**yaks:** `view-manager-builder-transient-panel-disappears-when-the-agenda-preview-refreshes-k65z`

**Chosen fix (recommended): `let`-bind `org-agenda-window-setup` to `current-window` around the render in `--render-preview`.**

Rationale:
- **Smallest robust fix** — one `let` form wrapping the two existing render branches; no new display-buffer rules, no advice, no window bookkeeping.
- **Root cause is exactly this variable.** `org-gtd-view-show` calls `(org-agenda nil key)`, and org-agenda honours `org-agenda-window-setup`. The default `reorganize-frame` (confirmed empirically) deletes/rearranges the frame's windows, destroying the transient's window — so the panel vanishes until a later command redraws it. `current-window` displays the agenda buffer in the selected window WITHOUT deleting or splitting any other window, so the transient's window survives every refresh.
- **Does not regress P1 / debounce / fail-soft / sample-data.** The binding is purely cosmetic (which window the agenda lands in); the render still fires on every change (P1), still coalesces via the debounce, still runs inside `--preview-now`'s `condition-case` (fail-soft), and still wraps BOTH branches of `--render-preview` so the sample-data fallback path is bound identically.

**Discarded alternative:** render into a DEDICATED side/preview window via `display-buffer` rules (a `display-buffer-alist` entry keyed on the agenda buffer). Rejected because org-agenda does its top-level window setup through `org-agenda-window-setup`, not `display-buffer` — so this approach would STILL have to neutralize `org-agenda-window-setup` and additionally maintain a side-window rule, more surface for a strictly cosmetic gain. If a future yaks wants a persistent side-by-side preview layout, revisit then; it is out of scope here.

**Files:**
- Modify: `org-gtd-view-manager.el` (function `org-gtd-view-manager--render-preview`, ~lines 572-582)
- Test: `test/unit/view-manager-preview-test.el`

**Step 1: Write the failing test**

Add this `deftest` to `test/unit/view-manager-preview-test.el` (it already `(require 'cl-lib)`). Place it after the existing preview tests, before the `(provide ...)`.

```elisp
(deftest view-manager-preview/render-binds-nondisruptive-window-setup ()
  "The preview render runs under a non-window-tearing `org-agenda-window-setup'.
Regression: the default `reorganize-frame' deletes/rearranges the frame's
windows when the preview calls `org-agenda', destroying the builder's
transient panel on every refresh.  `--render-preview' must bind
`org-agenda-window-setup' to `current-window' so the agenda lands in the
selected window without touching the transient's window."
  (let ((captured 'unset)
        (org-agenda-files nil)) ;; force the sample-data branch too
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-window-setup))))
      (org-gtd-view-manager--render-preview
       '((name . "x") (type . next-action))))
    (assert-equal 'current-window captured)))
```

Note: with `org-agenda-files` nil this exercises the sample-data branch (proving that branch is bound too). The stub replaces `org-gtd-view-show`, so no real agenda/window work happens — the test observes only the dynamic value of `org-agenda-window-setup` in effect during the render.

**Step 2: Run the test to verify it fails**

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el
```
Expected: FAIL — `captured` is `reorganize-frame` (the confirmed default), not `current-window`.

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el`, wrap the render in a `let` binding `org-agenda-window-setup`. Keep both branches and the sample-data banner intact:

```elisp
(defun org-gtd-view-manager--render-preview (spec)
  "Render SPEC via `org-gtd-view-show', using sample data if needed.
When `org-agenda-files' is empty, a tiny built-in sample file is
`let'-bound into `org-agenda-files' FOR THE RENDER ONLY, with a
banner explaining the substitution.  Real agenda-files are left
untouched.

`org-agenda-window-setup' is bound to `current-window' for the render
so `org-agenda' lands the preview in the selected window WITHOUT
reorganizing the frame -- the frame's default `reorganize-frame' would
delete the builder transient's window on every refresh, making the
panel vanish until a later command redrew it (yaks -k65z)."
  (let ((org-agenda-window-setup 'current-window))
    (if org-agenda-files
        (org-gtd-view-show spec)
      (let ((org-agenda-files (list (org-gtd-view-manager--sample-file))))
        (message "sample data · your agenda-files are empty — previewing org-gtd's built-in set")
        (org-gtd-view-show spec)))))
```

**Step 4: Run the test to verify it passes**

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el
```
Expected: PASS — including the existing P1/debounce/force tests (they stub `--render-preview` itself, so this change does not affect them) and the new binding test.

**Step 5: Run the full view-manager unit set to confirm no regression**

Run each in turn (the runner takes one target):
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-readers-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-run-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el
```
Expected: all PASS. (Optionally run the whole `unit` category: `.claude/skills/test/run-tests.sh unit`.)

**Step 6: Compile-check**

Run:
```bash
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
```
Expected: no warnings/errors. Restore `dir`. Do NOT `git add` `dir` or `.elc`.

**Step 7: Commit (Bug B only — separate from Bug A)**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-preview-test.el
git commit -m "fix(view-manager): keep transient visible across preview refreshes

The builder's live preview calls org-gtd-view-show -> (org-agenda nil
\"g\"), which honours org-agenda-window-setup. The default
reorganize-frame deletes/rearranges the frame's windows, destroying the
builder transient's window, so the panel vanished on every refresh (made
worse now that P1 fires the preview on every change). Bind
org-agenda-window-setup to current-window around the render so the
agenda lands in the selected window without tearing down the transient.

Fixes yaks view-manager-builder-transient-panel-disappears-when-the-agenda-preview-refreshes-k65z"
```
(End with the required `Co-Authored-By` trailer.)

**Step 8: MANUAL re-QA by the user (required — the visible behavior is interactive and does NOT reproduce headlessly)**

The unit test proves the binding is in effect; it cannot prove the transient stays visible, because that needs a live frame + transient. Ask the user to run these exact steps in a real interactive Emacs (a canonical org-gtd setup: keywords + keyword-mapping + org-agenda-files, per MEMORY "QA needs a proper org-gtd setup"):

1. `M-x org-gtd-view-manager` — the view list opens.
2. Press `c` to open the builder — the transient panel appears at the bottom.
3. **As the FIRST action**, set a filter that changes the spec, e.g. press `a` (area-of-focus) and enter a value, or `t` (type) and pick one. (Filtering first is the repro trigger from the yaks.)
4. Wait for the ~0.25s debounced preview to render the agenda.
5. **Confirm BOTH:** (a) the transient panel is STILL visible (it must NOT vanish / require clicking the agenda window to reappear), AND (b) the agenda preview updated to reflect the new filter.
6. Change another filter (e.g. press `e`, enter `<30m`) and confirm again that the transient stays visible AND the preview updates.
7. Press `e` then empty RET — confirm the effort filter clears without an error (this also re-verifies Bug A interactively).
8. Press `RET` (explicit Preview) — confirm it still forces a render (P1 not regressed).

Expected result: the transient panel remains visible through every refresh and the preview updates each time. If the panel still vanishes, the chosen `current-window` value was insufficient for this frame layout — fall back to evaluating `other-window`, and only then reconsider the discarded dedicated-side-window approach.

---

## Done criteria

- [ ] Bug A: `--read-effort` returns nil on blank input; two new reader tests pass; committed separately.
- [ ] Bug B: `--render-preview` binds `org-agenda-window-setup` to `current-window`; binding unit test passes; committed separately.
- [ ] `~/bin/eldev compile --warnings-as-errors` clean for both changes; `dir` restored; no `.elc`/`dir` staged.
- [ ] Full `unit` category green.
- [ ] P1 preview-refresh, debounce, fail-soft, and sample-data behavior all still pass their existing tests.
- [ ] User completed the manual re-QA for Bug B and confirmed the transient stays visible with the preview updating.
