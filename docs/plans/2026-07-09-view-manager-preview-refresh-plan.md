# View Manager Live-Preview Refresh Fix — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the View Manager builder's live preview so that changing a filter's value (e.g. area-of-focus Work → Home, effort <0:30 → >1h) re-renders the *Org Agenda* preview and brings previously-hidden items back.

**Architecture:** The bug has a single headlessly-proven root cause plus one defensive gap. Root cause: `org-gtd-view-manager--compile` returns a spec that *shares cons cells* with the live `--build-state`; `--preview-last` is set to that shared spec, so when `--set-value` mutates a filter value **in place** (`setf (alist-get …)`, the value→value case), it silently mutates `--preview-last` too — `--preview-changed-p` then reports "unchanged" and the debounced render is **skipped entirely**. We fix `--compile` to return fresh (unaliased) cons cells so the changed-check is honest. Defensive gap: the explicit `RET` preview shares the same changed-p guard, so once the cache is stale there is no recovery; we give `--preview-now` an optional `force` argument and have `RET` force a render unconditionally. Both fixes are unit-testable. The interactive agenda-buffer rebuild itself does not reproduce headlessly, so its final confirmation is a manual re-QA step.

**Tech Stack:** Emacs Lisp, transient.el, org-agenda, e-unit test framework (`deftest`, `assert-true`, `assert-nil`, `assert-equal`).

**Scope boundaries (read before starting):**
- This plan owns: the changed-p cache correctness (`--compile` aliasing) and the `RET` force-render recovery path.
- This plan does **NOT** own: window/redisplay handling while the transient is visible (sibling yak `…transient-panel-disappears…k65z`) or the effort-clear behavior (`…bq02`). We do **not** kill/erase `*Org Agenda*`, rebind `org-agenda-window-setup`, or otherwise touch org-agenda's window/buffer plumbing. Investigation showed the render was being *skipped*, not mis-rendered, so forcing a buffer rebuild is unnecessary and would collide with k65z. If manual QA (Task 3) shows the buffer *still* fails to rebuild after these fixes, that residual is the k65z window interaction and is handled by that separate plan.

---

## Investigation summary (why this is the fix)

Confirmed headlessly with a stub harness (render calls counted via `cl-letf` on `org-gtd-view-manager--render-preview`):

1. `--compile` shares cons cells with `--build-state`: `(eq (assq 'area-of-focus build-state) (assq 'area-of-focus (compile build-state)))` ⇒ `t`.
2. Setting `--preview-last` to that spec, then doing `(setf (alist-get 'area-of-focus build-state) "Home")` (exactly what `--set-value` does for an existing key) mutates `--preview-last` in place. `--preview-changed-p` then returns `nil` for the Home spec — the render is skipped. This reproduces the value→value failure **headlessly**, at the changed-p layer, before org-agenda is ever involved.
3. `nil→value` prepends a new cons (unshared) and `value→nil` deletes the key, so both produce a genuinely different spec — matching the field report that adding and clearing *do* work.
4. Fix verified headlessly: making `--compile` cons fresh cells makes `--preview-changed-p` return `t` for the Home spec and `--preview-now` re-renders (render count 1→2). The debounce's skip-if-truly-unchanged behavior is preserved.
5. `RET` (`--preview`) currently just calls `--preview-now`, so it inherits the same guard; with the cache stale it renders 0 times. Adding a `force` argument makes it render unconditionally.

This revises the field report's fault #1 ("org-agenda buffer doesn't rebuild on re-invocation"): the buffer looked un-rebuilt because the render was never dispatched for the new value. The field report's own QA UPDATE narrowed the fault to "the live-preview path, NOT `org-gtd-view-show`," which is consistent with this cause.

**Baseline (already green before any change):**
```
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el   # PASS: 2 tests
.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el    # PASS: 2 tests
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el     # PASS: 5 tests
```

---

## Task 1: Stop `--compile` from aliasing `--build-state` (honest changed-p)

This is the root-cause fix. A pure unit proves the aliasing; an integration-style unit proves the debounced preview now re-renders on a value→value change.

**Files:**
- Modify: `org-gtd-view-manager.el:364-375` (function `org-gtd-view-manager--compile`)
- Test: `test/unit/view-manager-preview-test.el`

**Step 1: Write the failing pure test (no aliasing)**

Add to `test/unit/view-manager-preview-test.el`, before the `(provide …)` line:

```elisp
(deftest view-manager-preview/compile-does-not-alias-build-state ()
  "The compiled spec must not share mutable cons cells with the build state.
Regression: `--set-value' mutates an existing key in place via
`setf (alist-get ...)'.  When the compiled spec shared that cell,
`--preview-last' was silently corrupted and the value->value change
was reported as `unchanged', skipping the live-preview render."
  (let ((org-gtd-view-manager--build-state
         (list (cons 'name "x")
               (cons 'type 'next-action)
               (cons 'area-of-focus "Work"))))
    (let ((spec (org-gtd-view-manager--compile
                 org-gtd-view-manager--build-state)))
      ;; Mutate the live state exactly like `--set-value' does for an
      ;; existing key.  The already-compiled snapshot must NOT change.
      (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Home")
      (assert-equal "Work" (alist-get 'area-of-focus spec)))))
```

**Step 2: Run the test to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
Expected: FAIL — the assertion sees `"Home"` (the snapshot was aliased and mutated), reported as an assertion failure on `view-manager-preview/compile-does-not-alias-build-state`.

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el`, in `org-gtd-view-manager--compile`, change the push of the shared cell to a push of a fresh cell. Replace:

```elisp
    (dolist (cell state)
      (when (and (memq (car cell) allowed)
                 (not (null (cdr cell))))
        (push cell result)))
```

with:

```elisp
    (dolist (cell state)
      (when (and (memq (car cell) allowed)
                 (not (null (cdr cell))))
        ;; Cons a FRESH cell: the compiled spec is cached in
        ;; `--preview-last', and `--set-value' mutates existing keys in
        ;; place.  Sharing the cell would let that in-place mutation
        ;; corrupt the cache and defeat `--preview-changed-p'.
        (push (cons (car cell) (cdr cell)) result)))
```

**Step 4: Run the test to verify it passes**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
Expected: PASS — 3 tests (the 2 existing changed-p tests plus the new one).

**Step 5: Write the failing integration test (debounced render re-fires)**

Add to `test/unit/view-manager-preview-test.el`, before `(provide …)`. This needs `cl-lib` for `cl-incf`; add `(require 'cl-lib)` near the top of the file (after `(require 'org-gtd-view-manager)`) if it is not already present.

```elisp
(deftest view-manager-preview/re-renders-on-value-change ()
  "A value->value filter change re-renders the live preview.
Simulates the debounced path: render Work, then mutate the value in
place to Home (as `--set-value' does) and fire `--preview-now' again.
Both renders must happen -- the second must not be skipped by a stale
`--preview-last' cache."
  (let ((count 0)
        (org-gtd-view-manager--preview-last nil)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x")
               (cons 'type 'next-action)
               (cons 'area-of-focus "Work"))))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview-now)
      (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Home")
      (org-gtd-view-manager--preview-now))
    (assert-equal 2 count)))
```

**Step 6: Run it to verify it passes (already fixed by Step 3)**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
Expected: PASS — 4 tests. (This test would have been RED before Step 3; verify by temporarily reverting Step 3 if you want to see the red, then restore. Not required.)

**Step 7: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-preview-test.el
git commit -m "fix(view-manager): stop --compile aliasing build-state so preview re-renders

The compiled spec shared cons cells with --build-state and was cached in
--preview-last.  --set-value mutates an existing key in place, which
corrupted the cache and made --preview-changed-p report a value->value
change as unchanged, skipping the live-preview render (the value->value
refresh bug).  Cons fresh cells in --compile so the change-check is honest.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: `RET` forces a render (recovery hatch, bypasses changed-p)

Even with an honest cache, if any residual interactive condition ever leaves the preview stale, the explicit `RET` must always be able to force a fresh render. We add a `force` argument rather than a second code path (DRY), keeping the debounce's skip-if-unchanged behavior for the timer path.

**Files:**
- Modify: `org-gtd-view-manager.el:504-521` (functions `org-gtd-view-manager--preview-now` and `org-gtd-view-manager--preview`)
- Test: `test/unit/view-manager-preview-test.el`

**Step 1: Write the failing test**

Add to `test/unit/view-manager-preview-test.el`, before `(provide …)`:

```elisp
(deftest view-manager-preview/ret-forces-render-when-cache-current ()
  "Explicit RET preview renders even when the cache equals the current spec.
RET is the user's recovery hatch: it must never be silenced by
`--preview-changed-p', otherwise a stale preview is unrecoverable."
  (let ((count 0)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x") (cons 'type 'next-action))))
    (setq org-gtd-view-manager--preview-last
          (org-gtd-view-manager--compile org-gtd-view-manager--build-state))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview))
    (assert-equal 1 count)))

(deftest view-manager-preview/debounce-still-skips-unchanged ()
  "The debounced (non-forced) path still skips a genuinely unchanged spec.
Guards the DRY refactor: adding `force' to `--preview-now' must not
break the debounce's coalescing (design: at most one render per idle
window)."
  (let ((count 0)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x") (cons 'type 'next-action))))
    (setq org-gtd-view-manager--preview-last
          (org-gtd-view-manager--compile org-gtd-view-manager--build-state))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview-now))
    (assert-equal 0 count)))
```

**Step 2: Run to verify the first test fails, the second passes**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
Expected: FAIL on `view-manager-preview/ret-forces-render-when-cache-current` (count is 0 — RET is still guarded). `view-manager-preview/debounce-still-skips-unchanged` PASSES already (documents the behavior we must preserve).

**Step 3: Write the minimal implementation**

In `org-gtd-view-manager.el`, replace `org-gtd-view-manager--preview-now` and `org-gtd-view-manager--preview` with:

```elisp
(defun org-gtd-view-manager--preview-now (&optional force)
  "Render the current build state immediately, fail-soft.
Unless FORCE is non-nil, skips the render when the compiled spec is
unchanged from the last one previewed (debounce coalescing).  FORCE
bypasses that guard -- the explicit `RET' preview passes it so a stale
view is always recoverable.  Any `org-gtd-view-show' error is caught
and surfaced as a one-line teaching message, never a stack trace
(design §8)."
  (let ((spec (org-gtd-view-manager--compile org-gtd-view-manager--build-state)))
    (when (or force (org-gtd-view-manager--preview-changed-p spec))
      (condition-case err
          (progn
            (org-gtd-view-manager--render-preview spec)
            (setq org-gtd-view-manager--preview-last spec))
        (error (message "org-gtd view preview: %s"
                        (error-message-string err)))))))

(defun org-gtd-view-manager--preview (&rest _)
  "Explicit `RET' preview -- bypass the debounce and force a render."
  (interactive)
  (org-gtd-view-manager--preview-now t))
```

Note: `--preview-schedule` still passes `#'org-gtd-view-manager--preview-now` to `run-with-idle-timer` (no `force`), so the debounced timer keeps its skip-if-unchanged coalescing. Do not change `--preview-schedule`.

**Step 4: Run to verify all pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el`
Expected: PASS — 6 tests.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-preview-test.el
git commit -m "fix(view-manager): RET preview forces a render, bypassing changed-p

Give --preview-now an optional FORCE arg; --preview (RET) passes it so an
explicit preview always renders and a stale view is recoverable.  The
debounced timer path stays non-forced, preserving its skip-if-unchanged
coalescing.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Regression sweep, compile check, and manual re-QA

The unit-testable faults are fixed. The remaining confirmation — that the *Org Agenda* buffer visibly updates during a live interactive builder session — **cannot be automated** (it does not reproduce in batch: no window, no transient, no idle timer contention). This task runs the full regression net and hands the user an exact manual reproduction to confirm the interactive behavior.

**Files:** none modified (verification only).

**Step 1: Run the view-manager test suite**

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```
Expected: preview PASS 6 tests; sample PASS 2 tests; build PASS 5 tests. The sample-data fallback and fail-soft error handling paths are unchanged and must stay green.

**Step 2: Run the full unit category (no unrelated regressions)**

Run: `.claude/skills/test/run-tests.sh unit`
Expected: all unit tests PASS. If any unrelated failure appears, capture the reported seed and re-run with `--seed=N` before investigating.

**Step 3: Byte-compile clean (warnings as errors)**

Run:
```bash
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
```
Expected: compiles with no warnings/errors. `git checkout -- dir` restores the tracked Info `dir` file that compile deletes. **Never** `git add` `dir` or any `.elc` file.

**Step 4: MANUAL RE-QA (user must perform — interactive, not automatable)**

State to the user that the following must be verified by hand in a real Emacs session, because the agenda-buffer/window behavior does not reproduce in batch:

1. In a proper org-gtd setup with real agenda-files (keywords + keyword-mapping + `org-agenda-files`), open the View Manager builder (`M-x org-gtd-view-run` → create, or however the builder is entered).
2. Set the type to `next-action`. Set area-of-focus to **Work**. Wait ~0.3s for the debounced preview. Confirm the *Org Agenda* preview shows only Work items.
3. **Change area-of-focus to Home** (the value→value case). Wait for the debounce. **Expected (the fix):** the preview now shows Home items — including items the Work filter had hidden. Before this fix it kept showing Work.
4. Repeat with effort: set effort `<0:30`, confirm narrowing, then change to `>1h` and confirm the preview updates to the new set.
5. Press `RET` (Preview) at any point and confirm it always re-renders (recovery hatch).
6. Regression checks by hand: adding a filter from nothing (nil→value) still narrows; clearing a filter (value→nil) still widens; rapid edits still coalesce into a single render (debounce); with empty `org-agenda-files` the sample-data banner still appears.
7. Optional cross-check of the cache: `(with-current-buffer "*Org Agenda*" (buffer-string))` after step 3 should contain the Home result, not the Work result.

If step 3 or 4 still shows a stale buffer *despite* the render now firing (you can confirm the render fired via `M-x` or a temporary `message` in `--render-preview`), the residual is the transient/window interaction owned by yak `…k65z` — stop and report it against that yak rather than expanding this plan.

**Step 5: Final commit (if any verification-only artifacts, otherwise skip)**

No code change in this task. If Tasks 1 and 2 were committed separately, nothing to commit here. Confirm `git status` shows only the intended source + test changes (no `dir`, no `.elc`).

---

## Notes for the executor

- Keep Tasks 1 and 2 as separate commits — they are independent fixes with independent tests.
- Do not touch `org-gtd-view-language.el` / `org-gtd-view-show`; investigation cleared it (repeated standalone calls rebuild correctly, headlessly and interactively).
- Do not add kill-buffer / `org-agenda-redo` / `org-agenda-window-setup` logic — out of scope (k65z) and unnecessary given the root cause is a skipped render, not a mis-render.
- The two new integration tests rely on `cl-lib` (`cl-incf`) and `cl-letf`; ensure `(require 'cl-lib)` is present in `test/unit/view-manager-preview-test.el`.
