# Walk Engine Phase 2 — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fold the review console's bespoke checklist-string walk onto the Phase 0 walk engine (a migration, characterize-first), and add a generic `walk` review step type that drives any registered walk hosted in the review buffer.

**Architecture:** The review console (`org-gtd-review.el`) currently runs a hand-rolled walk *inside* a `checklist` step — `:walk-items` (checklist strings) + `:walk-pos` in `org-gtd-review--state`, advanced by `org-gtd-review--walk-next`, checkpointed inside the review's single `review-state.eld`. Phase 2 replaces that mechanism with the engine: a walk spec whose handles are the checklist strings, **hosted** in the review buffer via the engine's caller-provided-surface mechanism (`org-gtd-walk--surface-buffer` already accepts a `(:buffer … :region …)` plist and passes the whole surface to `:render` untouched), running `:resumable nil` with the walk **model embedded in the review's own checkpoint**. The same hosted-composition helper then backs a new `(:type walk :walk SYMBOL)` step that looks `SYMBOL` up in `org-gtd-walks` and advances the session on finish. No user-visible change to the checklist step; the existing review tests are the contract.

**Tech Stack:** Emacs Lisp, e-unit (`deftest`/`assert-*`), Eldev, `org-gtd-walk.el` + `org-gtd-walk-model.el` (Phase 0 engine), `org-gtd-review.el`, `org-gtd-checklist.el`.

---

## Conventions (apply to every task)

- **Run tests only via the `/test` skill** (Skill tool). Never call `eldev etest` or `run-tests.sh` directly. The review characterization file is `test/unit/review-test.el`; the schedule file is `test/unit/review-schedule-test.el`; new Phase-2 tests go in `test/unit/review-walk-test.el` (create).
- **Compile clean:** `~/bin/eldev compile --warnings-as-errors` must pass before every commit.
- **e-unit only.** Mirror the existing review tests' shape (`around-each` + `ogt-eunit-with-mock-gtd`, `org-gtd-review-profiles` let-bound to a tiny profile). Model/driver-level assertions can run headless like `test/unit/walk-driver-test.el`.
- **New files carry `Copyright © 2026 Aldric Giacomoni`** in the header (see `org-gtd-walk.el`). This plan creates one new test file; edits to existing files need no header change.
- **Commit trailer** on every commit:
  ```
  Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
  ```
- **Characterize-first for Deliverable A.** The behavioral review tests must stay green at every commit. Only tests that assert the *deleted internals* (`:walk-items`/`:walk-pos`) are rewritten — and only in the single cutover task (Task A5), never before.
- **Phase-0/1 engine gotchas that bite here:**
  - `org-gtd-walk--active` is `permanent-local` — it survives `kill-all-local-variables`. It is buffer-local on the surface buffer; when the surface is the review buffer it lands on `*GTD Review*`.
  - Wrap any subtree copy/cut in `org-gtd--without-kill-merge` (not needed for the string-checklist walk, which pastes no subtree — relevant only if a future heading walk renders one).
  - A `:render` that pastes an org subtree must bind `org-id-track-globally nil` (again: not the checklist walk; keep in mind for heading walks — deferred).
  - The test harness resets `org-gtd-walk--locked-scopes` between tests (`test/helpers/setup.el`), so a leaked lock will not cascade *in tests* — but production teardown must still unlock (see Decision 4).

---

## Design decisions to rule on BEFORE executing

These are surfaced for the maintainer. Do not start Task A1 until they are decided; the tasks below assume the **recommended** answers.

> **RULED (orchestrator) — all seven confirmed AS RECOMMENDED, proceed.** One
> addition to Decision 4: the build MUST include an explicit test that the
> synthetic scope lock is released on **every** teardown path (kill-buffer,
> quit, complete-step, reset-session) — a missed path leaks the lock in
> production. Decision 7 stands: `stuck-projects`/heading-walk steps are
> deferred net-new UX; validate Deliverable B with a tiny test-only walk only.

### Decision 1 — Does hosting a walk on the review buffer collide with the review's own state?
**Finding:** No variable collision. `org-gtd-review--state` is a **global** `defvar` (not buffer-local); `org-gtd-walk--active` is buffer-local (permanent-local) on the surface buffer. Hosting the walk sets `org-gtd-walk--active` buffer-local on `*GTD Review*`; the two never alias. The review's `kill-buffer-hook` (`org-gtd-review--on-buffer-kill`) and `header-line-format` are unaffected.
**The real coupling** is the *render*: `org-gtd-review--render` does `(erase-buffer)` and redraws the whole console on every transition. There is no persistent sub-region with its own text or keymap — "the step region" is simply the current-item line the console prints while rendering a walk step (today: `org-gtd-review--render` lines ~320-325). So the hosted walk's `:render` does not own a live region; it triggers a full console re-render that reads the current handle out of `org-gtd-walk--active`'s model.
**Recommendation:** Accept this. The surface passed to the engine is the plist `(:buffer <*GTD Review*> :region console)`; the hosted `:render` ignores the region detail and calls a console re-render. Confirm this is acceptable (vs. building a genuinely independent editable region — rejected as over-engineering, and unnecessary for strings).

### Decision 2 — How do walk action keys become active during a walk step?
**Finding:** The review buffer stays in `org-gtd-review-mode` (keymap `n`/`s`/`c`/`p`/`q`) for the whole session. The checklist walk needs **no** per-item action beyond advance — the console's existing `n` drives it. So for Deliverable A **no keymap change is needed**: `org-gtd-review-next` on a walk step calls the engine transition `org-gtd-walk-advance`; `s` still exits the whole step; `c` still captures.
A registered *heading* walk (e.g. someday-review) carries its own `d`/`c`/`q` keymap and its `:render` pops its own WIP buffer — that keymap-layering-into-the-console problem is exactly the **deferred net-new heading-walk-step UX** (see "Out of scope"). Deliverable B therefore validates the generic `walk` step with a **tiny test-only walk** whose only interaction is advance/finish, *not* by wiring someday-review as a real console step.
**Recommendation:** Deliverable A and B both drive advancement through the console's `n`; no `org-gtd-review-mode-map` change, no overriding keymap. Confirm we are **not** attempting per-item action keys inside the console in Phase 2.

### Decision 3 — Exactly how the walk model embeds in `--save-state` / `--state-valid-p`.
**Finding:** `org-gtd-review--state` is `prin1`'d wholesale by `--save-state`. A walk model is plain serializable data (`(:entries … :cursor … :meta …)`), so replacing the two keys `:walk-items`/`:walk-pos` with a single `:walk-model` key serializes for free.
**Plan:**
- Replace `:walk-items`/`:walk-pos` everywhere with `:walk-model` (default `nil`).
- `--state-valid-p`: drop the `:walk-items`/`:walk-pos` clause; require `(let ((m (plist-get state :walk-model))) (or (null m) (org-gtd-walk-model-valid-p m)))`.
- The active source of truth during walking is `org-gtd-walk--active`; the review **mirrors** its model into `:walk-model` and calls `--save-state` on every transition, via the hosted `:render` hook (Decision 5).
**Recommendation:** Single `:walk-model` key validated by `org-gtd-walk-model-valid-p`. Confirm.

### Decision 4 — Start path & scope lock for a hosted walk (leak risk).
**Finding:** The engine's only entry point, `org-gtd-walk-start`, **locks the scope** (`org-gtd-walk--locked-scopes`, process-global) and refuses a second walk over the same scope. A hosted checklist walk mutates nothing (strings), so the lock buys little, and a review buffer killed/paused mid-walk would **leak the lock** unless teardown releases it.
**Two options:**
- **(a, recommended)** Reuse `org-gtd-walk-start` with a **synthetic scope** derived from the step (e.g. `(list "review-hosted" profile-name step-title)`), and make **every** review teardown path (`--complete-step`, `--teardown`, `--reset-session`/`--on-buffer-kill`, pause/quit) call `org-gtd-walk-quit` first when `org-gtd-walk--active` is set on the console buffer — quit unlocks and clears the session but (resumable-nil) writes nothing, so the review's own checkpoint remains the resume source. The review's single-session entry guard already prevents a genuine second concurrent session in-process.
- **(b)** Add a lock-free `org-gtd-walk-start-hosted` to the engine. More engine surface; avoids all leak reasoning.
**Recommendation:** (a). It exercises the real, Tier-2-covered start path and keeps engine surface unchanged; the teardown-unlock is a small, testable invariant. Confirm — if (b) is preferred, Task A2 changes to add the engine entry point instead.

### Decision 5 — Sync + checkpoint hook.
**Finding:** The engine calls the spec's `:render` after **every** settle (start, advance, enqueue). That is the natural single hook to (1) re-render the console and (2) mirror `org-gtd-walk--active`'s model into `org-gtd-review--state :walk-model` and `--save-state`. `:on-finish` runs *after* the engine clears `org-gtd-walk--active`, so finish must not read it.
**Recommendation:** Hosted `:render` = a review function that syncs the model into `:walk-model`, saves state, and re-renders the console. Hosted `:on-finish` = `org-gtd-review--complete-step` (which clears `:walk-model` and advances). Confirm.

### Decision 6 — Preserve resume-mid-checklist across restart?
**Finding:** Today `review/kill-mid-walk-resumes-at-item` proves killing the console mid-walk resumes at `(2/8)`. With the fold, resume must rehydrate `org-gtd-walk--active` from the checkpoint's `:walk-model` when a resumed session lands on an in-progress walk step. `org-gtd-review--begin-session` installs the saved state and renders; it must additionally, when the current step is a walk step **and** `:walk-model` is non-nil, rebuild the hosted session (`org-gtd-walk--active`) from the saved model + the step's spec, then let `--render` draw it. This preserves the exact behavior.
**Recommendation:** Rehydrate in `--begin-session`. Confirm the behavior contract (resume lands on the same item, `(2/8)`) is preserved.

### Decision 7 — First-`n`-loads vs. auto-start on entry.
**Finding:** The checklist step loads its walk on the **first `n`** (`:acted` gates it), then advances per `n`. To preserve behavior exactly (`review/checklist-step-walks-items-one-at-a-time`: one `n` = "load, show item 1"), Deliverable A keeps first-`n`-loads. Deliverable B's generic `walk` step **mirrors** this (first `n` starts, subsequent `n` advance) so no new UX is introduced.
**Recommendation:** Mirror first-`n`-loads for both. Flag "auto-start the first item on step entry" as a *possible* future UX refinement, out of Phase 2 scope.

---

## Deliverable A — Fold the checklist step's walk onto the engine (characterize-first)

### Task A0: Establish the green baseline

**Files:** none (verification only).

1. Run the `/test` skill over `test/unit/review-test.el` and `test/unit/review-schedule-test.el`. Confirm **all** pass. Record the pass count — this is the safety net for the whole of Deliverable A.
2. Run `~/bin/eldev compile --warnings-as-errors`. Confirm clean.
3. No commit (baseline only). If anything is red here, stop and report — do not build on a red baseline.

---

### Task A1: Build the checklist walk spec (headless, no console wiring yet)

Add a constructor that turns a checklist step into an engine spec. No behavior change yet: nothing calls it.

**Files:**
- Modify: `org-gtd-review.el` (add `require 'org-gtd-walk`; new private fn near the checklist code)
- Test: `test/unit/review-walk-test.el` (create)

**Step 1 — Failing test.** In the new file (mirror the header/prelude of `test/unit/review-test.el`):

```elisp
(deftest review-walk/checklist-spec-finds-template-items ()
  "The checklist walk spec's :find yields the template's item strings."
  (let* ((step '(:title "Sweep" :type checklist :checklist "Mind sweep prompts"))
         (spec (org-gtd-review--checklist-walk-spec step)))
    (assert-true (org-gtd-walk-spec-valid-p spec))
    (assert-equal (org-gtd-checklist-template--items "Mind sweep prompts")
                  (funcall (plist-get spec :find)))
    (assert-nil (plist-get spec :resumable))))
```

Wrap the file in `around-each` + `ogt-eunit-with-mock-gtd` like `review-test.el` (the checklist templates file is seeded under `org-gtd-directory`).

**Step 2 — Run, expect FAIL** (`org-gtd-review--checklist-walk-spec` undefined).

**Step 3 — Implement.** Add `(require 'org-gtd-walk)` to the Requirements block. Add:

```elisp
(defun org-gtd-review--checklist-walk-spec (step)
  "Return an engine walk spec for the checklist STEP.
Handles are the template's checkbox strings; the walk is hosted in
the console (`:resumable' nil — the review owns persistence)."
  (let ((name (plist-get step :checklist)))
    (list :name (intern (format "review-checklist-%s" name))
          :find (lambda () (org-gtd-checklist-template--items name))
          :render #'org-gtd-review--hosted-render      ; defined in A2
          :actions nil
          :on-finish #'org-gtd-review--complete-step
          :resumable nil
          :resolve nil
          :scope (list "review-hosted" name))))
```

(`org-gtd-review--hosted-render` does not exist yet; A1's test only touches `:find`/`:resumable`/validity, so a forward reference is fine for spec construction. If `org-gtd-walk-spec-valid-p` rejects a `:render` whose symbol is unbound, define a stub `org-gtd-review--hosted-render` now returning nil and flesh it out in A2 — `--callable-p` checks `fboundp`, so the symbol must be bound.)

**Step 4 — Run, expect PASS.**

**Step 5 — Compile clean, then commit** (`feat: add checklist walk spec constructor (unwired)`).

---

### Task A2: Build the hosted render + sync helper

The single hook (Decision 5): re-render the console, mirror the active model into `:walk-model`, checkpoint.

**Files:**
- Modify: `org-gtd-review.el`
- Test: `test/unit/review-walk-test.el`

**Step 1 — Failing test.** Drive it through the real engine so the render fires:

```elisp
(deftest review-walk/hosted-render-mirrors-model-and-renders ()
  "Starting a hosted walk renders the current item and mirrors the model."
  (let ((org-gtd-review-profiles review-walk-test--tiny-checklist-profile))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                 ; first n: start hosted walk
      (assert-true (plist-get org-gtd-review--state :walk-model))
      (assert-match "(1/" (buffer-string)))))
```

Define `review-walk-test--tiny-checklist-profile` as a one-phase profile with one checklist step (`:checklist "Mind sweep prompts"`) then a prompt step.

**Step 2 — Run, expect FAIL.**

**Step 3 — Implement** `org-gtd-review--hosted-render` and a start helper:

```elisp
(defun org-gtd-review--sync-walk-model ()
  "Mirror the active hosted walk's model into the session and checkpoint."
  (when org-gtd-walk--active
    (plist-put org-gtd-review--state :walk-model
               (plist-get org-gtd-walk--active :model))
    (org-gtd-review--save-state)))

(defun org-gtd-review--hosted-render (_handle _surface)
  "Hosted-walk :render: sync the model, checkpoint, redraw the console.
Runs in the console buffer after every engine transition (Decision 5)."
  (org-gtd-review--sync-walk-model)
  (org-gtd-review--render))

(defun org-gtd-review--start-hosted-walk (spec)
  "Start SPEC hosted in the console buffer's step region."
  (org-gtd-walk-start
   spec
   (list :buffer (get-buffer-create org-gtd-review--buffer-name)
         :region 'console)))
```

Make `org-gtd-review--render`'s current-item line read the model from `org-gtd-walk--active` instead of `:walk-items`/`:walk-pos` — add a *new* branch that coexists with the old one for now (do not delete the old branch until A5):

```elisp
;; new branch, checked first
(when (and (memq (plist-get step :type) '(checklist walk))
           org-gtd-walk--active)
  (let* ((model (plist-get org-gtd-walk--active :model))
         (items (plist-get model :entries))
         (pos (plist-get model :cursor)))
    (when (< pos (length items))
      (insert (format "\n    → %s   (%d/%d)\n"
                      (nth pos items) (1+ pos) (length items))))))
```

Note: `org-gtd-review--render` runs `(with-current-buffer (get-buffer-create org-gtd-review--buffer-name) …)`, so `org-gtd-walk--active` (buffer-local on that buffer) is read in the right buffer. Confirm the `let*` reads it inside that `with-current-buffer`.

**Step 4 — Run, expect PASS.** Also re-run `review-test.el` — still green (old checklist path untouched; new render branch only fires when `org-gtd-walk--active` is set, which the old path never sets).

**Step 5 — Compile clean, commit** (`feat: hosted-walk render + model sync for review console`).

---

### Task A3: Route checklist advancement through the engine (behind first-n-loads)

Wire `org-gtd-review-next`'s `checklist` branch to start/advance the hosted walk **in addition to** proving finish advances the session — but keep the old `--walk-next` reachable is NOT the goal; instead introduce the new path and make the checklist branch use it. Behavioral tests must stay green.

**Files:**
- Modify: `org-gtd-review.el`
- Test: `test/unit/review-walk-test.el`

**Step 1 — Failing test.** Prove item-by-item advance + exit through the engine path:

```elisp
(deftest review-walk/checklist-hosted-walk-advances-and-exits ()
  "n loads the hosted walk, advances item by item, then leaves the step."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile)) ; 8-item "Mind sweep prompts" + prompt
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                       ; load, item 1
      (assert-match "(1/8)" (buffer-string))
      (dotimes (_ 8) (org-gtd-review-next))       ; through 8 and out
      (assert-match "After" (buffer-string)))))
```

**Step 2 — Run, expect FAIL** (checklist branch still uses `--walk-next`, which the new render branch does not track — or vice versa; either way this asserts the engine path).

**Step 3 — Implement.** Add a hosted-walk advance entry and switch the `checklist` pcase branch:

```elisp
(defun org-gtd-review--walk-step-next (step)
  "Do the walk STEP: start on first n, else advance the hosted walk."
  (if (plist-get org-gtd-review--state :acted)
      (with-current-buffer org-gtd-review--buffer-name
        (org-gtd-walk-advance))
    (plist-put org-gtd-review--state :acted t)
    (org-gtd-review--start-hosted-walk
     (org-gtd-review--checklist-walk-spec step))))
```

Empty-template behavior: `org-gtd-walk-start` on an empty `:find` runs `:on-finish` (`--complete-step`) without activating — the step self-satisfies. Preserve the user message ("Nothing in checklist … moving on") by checking items before start:

```elisp
;; inside --walk-step-next, before start, when not acted and items are empty:
(let ((items (org-gtd-checklist-template--items (plist-get step :checklist))))
  (when (null items)
    (message "Nothing in checklist '%s' — moving on.  (Edit %s to add items.)"
             (plist-get step :checklist)
             (org-gtd-checklist-template--file-path))))
```

Point the `checklist` pcase branch at `org-gtd-review--walk-step-next` **but keep `--walk-next` defined** for this task (do not delete yet — deletion is A5). The branch now calls the new fn.

**Step 4 — Run, expect PASS.** Re-run `review-test.el`. The behavioral checklist tests (`checklist-step-walks-items-one-at-a-time`, `checklist-step-missing-template-auto-advances`, `default-weekly-profile-runs-end-to-end`) must stay green. The internal-asserting tests (`skip-mid-walk-exits-whole-step`, `capture-mid-walk-keeps-position`, `kill-mid-walk-resumes-at-item`, `mangled-state-file-starts-fresh`, `state-valid-p-rejects-corrupt-fields`) **may now fail** because state uses `:walk-model`, not `:walk-items`/`:walk-pos`. That is expected — **do not** fix them here; they are rewritten in A5. If keeping them green in the interim is required by the reviewer, gate A3 so both representations are written (mirror `:walk-model` and keep `:walk-pos` in sync) — but the recommended path is the clean cutover in A5. Note in the commit which internal tests are temporarily red.

> Reviewer note: if the discipline "green throughout, thin only at cutover" must hold literally at *every* commit, merge A3+A4+A5 into one cutover commit. The split here is for reviewability; the maintainer chooses.

**Step 5 — Compile clean, commit** (`feat: route review checklist step through the walk engine`).

---

### Task A4: Resume + teardown for the hosted walk

Rehydrate `org-gtd-walk--active` on resume (Decision 6); unlock on every teardown path (Decision 4).

**Files:**
- Modify: `org-gtd-review.el`
- Test: `test/unit/review-walk-test.el`

**Step 1 — Failing tests.**

```elisp
(deftest review-walk/kill-mid-hosted-walk-resumes-at-item ()
  "Killing the console mid-walk resumes at the checkpointed item."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)   ; item 1
      (org-gtd-review-next))  ; item 2
    (kill-buffer org-gtd-review--buffer-name)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "(2/8)" (buffer-string)))))

(deftest review-walk/kill-mid-hosted-walk-releases-scope-lock ()
  "Killing the console mid-walk unlocks the hosted walk's scope."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next))
    (kill-buffer org-gtd-review--buffer-name)
    (assert-nil org-gtd-walk--locked-scopes)))
```

**Step 2 — Run, expect FAIL.**

**Step 3 — Implement.**
- In `org-gtd-review--begin-session`, after installing state and **before/at** render, rehydrate when resuming into an in-progress walk step:

```elisp
(let ((step (org-gtd-review--current-step)))
  (when (and (memq (plist-get step :type) '(checklist walk))
             (plist-get step :acted-noop nil)      ; see note
             (plist-get org-gtd-review--state :walk-model))
    (with-current-buffer (get-buffer-create org-gtd-review--buffer-name)
      (setq org-gtd-walk--active
            (list :model (plist-get org-gtd-review--state :walk-model)
                  :spec (org-gtd-review--spec-for-step step)
                  :surface (list :buffer (current-buffer) :region 'console)
                  :checkpoint-path nil :skipped 0))
      (org-gtd-walk--lock-scope (plist-get (plist-get org-gtd-walk--active :spec) :scope)))))
```

Add `org-gtd-review--spec-for-step` returning the checklist spec for a `checklist` step (and, in Deliverable B, the registered spec for a `walk` step). The `:acted` flag distinguishes "walk in progress" (rehydrate) from "not yet started" (fresh). Use `(plist-get org-gtd-review--state :acted)` as the gate — drop the placeholder `:acted-noop`.

- Add unlock-on-teardown. In `org-gtd-review--reset-session` (called by `--teardown` and `--on-buffer-kill`), before clearing `org-gtd-review--state`:

```elisp
(let ((buf (get-buffer org-gtd-review--buffer-name)))
  (when (and buf (buffer-local-value 'org-gtd-walk--active buf))
    (with-current-buffer buf (org-gtd-walk-quit))))   ; unlock, keep checkpoint
```

- In `org-gtd-review--complete-step`, before advancing, tear down any live hosted walk cleanly (normal finish path already ran `org-gtd-walk-finish` via `:on-finish`, which unlocked and cleared — but the `s`/skip path leaves it active). So on skip mid-walk, `--complete-step` must also quit the hosted walk. Add the same quit guard at the top of `--complete-step`, then clear `:walk-model`.

**Step 4 — Run, expect PASS.** Re-run `review-test.el` behavioral tests — green.

**Step 5 — Compile clean, commit** (`feat: resume + scope-unlock for hosted review walks`).

---

### Task A5: Cutover — delete the bespoke walk mechanics and rewrite internal tests

Remove `:walk-items`/`:walk-pos`, `--walk-next`, the old render branch, and the walk-specific `--state-valid-p` clause; rewrite the tests that asserted those internals. Behavior identical.

**Files:**
- Modify: `org-gtd-review.el`
- Modify: `test/unit/review-test.el`

**Step 1 — Delete in `org-gtd-review.el`:**
- `org-gtd-review--walk-next` (whole function).
- The old checklist item-line branch in `--render` (the `(plist-get state :walk-items)` block) — keep only the model-driven branch from A2.
- In `--complete-step`: replace `(plist-put state :walk-items nil)` + `(plist-put state :walk-pos 0)` with `(plist-put state :walk-model nil)`.
- In `--start-fresh`: replace `:walk-items nil :walk-pos 0` in the initial state with `:walk-model nil`.
- In `--state-valid-p`: delete the `walk-items`/`walk-pos` `let` + clauses; add `(let ((m (plist-get state :walk-model))) (or (null m) (org-gtd-walk-model-valid-p m)))`.
- Update the `org-gtd-review--state` docstring (drop `:walk-items :walk-pos`, add `:walk-model`).

**Step 2 — Rewrite the internal-asserting tests in `review-test.el`:**
- `review/skip-mid-walk-exits-whole-step`: replace `(assert-nil (plist-get … :walk-items))` with `(assert-nil (plist-get org-gtd-review--state :walk-model))`.
- `review/capture-mid-walk-keeps-position`: replace the `:walk-pos` assertion with a behavioral one — `(assert-match "(2/8)" (buffer-string))` — plus, if desired, assert the model cursor: `(assert-equal 1 (plist-get (plist-get org-gtd-review--state :walk-model) :cursor))`.
- `review/kill-mid-walk-resumes-at-item`: already behavioral (`(2/8)`) — keep; verify it passes through the engine path (it now exercises A4 rehydrate).
- `review/mangled-state-file-starts-fresh`: change the fixture from `:walk-items ("a" "b") :walk-pos 9` to an invalid `:walk-model`, e.g. `:walk-model (:entries ("a" "b") :cursor 9 :meta nil)` (cursor out of range → `org-gtd-walk-model-valid-p` nil → fresh). Keep the assertions (`Step one`, fresh checkpoint valid). Change the trailing `:walk-pos` assertion to check the fresh model or drop it.
- `review/state-valid-p-rejects-corrupt-fields`: change `base` to use `:walk-model nil`; replace the `:walk-items`/`:walk-pos` mangles with `:walk-model` mangles: a non-list model, and a model with an out-of-range cursor, both `assert-nil`; a valid model `assert-true`.

**Step 3 — Run** `/test` over `review-test.el`, `review-schedule-test.el`, `review-walk-test.el`. **All green.**

**Step 4 — Compile clean** (`--warnings-as-errors`; a now-unused function would warn).

**Step 5 — Commit** (`refactor: delete bespoke review walk mechanics; fold onto engine`).

---

## Deliverable B — Generic `walk` review step type

Reuses the hosted-composition helper from Deliverable A; a `walk` step differs only in that its spec comes from the `org-gtd-walks` registry instead of being built from a checklist template.

### Task B1: Validate `walk` steps in `--check-profile`

**Files:**
- Modify: `org-gtd-review.el` (`org-gtd-review--check-profile`)
- Test: `test/unit/review-walk-test.el`

**Step 1 — Failing tests.**

```elisp
(deftest review-walk/walk-step-missing-walk-errors-cleanly ()
  "A walk step without :walk is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Walk what?" :type walk))))))
    (let ((err (condition-case e (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":walk" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review-walk/walk-step-unknown-walk-errors-cleanly ()
  "A walk step naming an unregistered walk is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Ghost" :type walk :walk no-such-walk))))))
    (let ((err (condition-case e (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "no-such-walk" (cadr err)))
    (assert-nil org-gtd-review--state)))
```

**Step 2 — Run, expect FAIL.**

**Step 3 — Implement.** In `--check-profile`, after the `checklist` clause add:

```elisp
(when (eq (plist-get step :type) 'walk)
  (let ((wname (plist-get step :walk)))
    (unless wname
      (user-error
       "Review profile '%s', phase '%s': walk step \"%s\" is missing :walk — name a registered walk, like :walk stuck-projects"
       name (car phase) (plist-get step :title)))
    (unless (org-gtd-walk-get wname)
      (user-error
       "Review profile '%s', phase '%s': walk step \"%s\" names :walk %s, which is not registered in org-gtd-walks"
       name (car phase) (plist-get step :title) wname))))
```

**Step 4 — Run, expect PASS.**

**Step 5 — Compile clean, commit** (`feat: validate walk review step type`).

---

### Task B2: Dispatch, render, and finish for `walk` steps

**Files:**
- Modify: `org-gtd-review.el` (`org-gtd-review-next` pcase, `--spec-for-step`, optional `--step-guidance`)
- Test: `test/unit/review-walk-test.el`

**Step 1 — Failing test** using a **tiny test-only registered walk** (Decision 2 — do not use someday-review):

```elisp
(defvar review-walk-test--handles '("alpha" "beta" "gamma"))

(defun review-walk-test--register ()
  (org-gtd-walk-register
   'review-test-walk
   (list :name 'review-test-walk
         :find (lambda () review-walk-test--handles)
         :render #'org-gtd-review--hosted-render
         :actions nil
         :on-finish #'org-gtd-review--complete-step
         :resumable nil
         :resolve nil
         :scope (list "review-test-walk"))))

(deftest review-walk/walk-step-drives-registered-walk-and-advances ()
  "A :type walk step walks the registered handles, then advances the session."
  (review-walk-test--register)
  (let ((org-gtd-review-profiles
         '(("W" ("P" (:title "Walk it" :type walk :walk review-test-walk)
                     (:title "After" :type prompt))))))
    (org-gtd-review "W")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                 ; start, alpha
      (assert-match "alpha" (buffer-string))
      (assert-match "(1/3)" (buffer-string))
      (org-gtd-review-next)                 ; beta
      (assert-match "(2/3)" (buffer-string))
      (org-gtd-review-next)                 ; gamma
      (org-gtd-review-next)                 ; off end -> complete-step
      (assert-match "After" (buffer-string)))))
```

> Note: this test-only walk's `:scope` (`"review-test-walk"`) differs from the console's per-step synthetic scope in A. Since B reuses the **registered** spec's scope directly (not a per-step synthetic one), confirm the registered scope is what locks. Acceptable for a test walk; a real hosted heading walk's scope semantics are part of the deferred design.

**Step 2 — Run, expect FAIL.**

**Step 3 — Implement.**
- Add `org-gtd-review--spec-for-step`:

```elisp
(defun org-gtd-review--spec-for-step (step)
  "Return the hosted walk spec for STEP (checklist or walk)."
  (pcase (plist-get step :type)
    ('checklist (org-gtd-review--checklist-walk-spec step))
    ('walk (org-gtd-walk-get (plist-get step :walk)))))
```

- Generalize `org-gtd-review--walk-step-next` (from A3) to use `--spec-for-step` instead of hardcoding the checklist spec, so both `checklist` and `walk` share it. Keep the empty-find message specific to checklist (`walk` steps with empty finds simply self-satisfy — no message, matching the engine's empty-find-finishes).
- In `org-gtd-review-next`'s pcase, add a `('walk (org-gtd-review--walk-step-next step))` branch alongside `('checklist …)` — or collapse both into one branch matching `(or 'checklist 'walk)`.
- Optionally add a `walk` case to `org-gtd-review--step-guidance` (a one-liner; not asserted by tests — keep minimal or omit).

Crucially: a registered walk's spec has its own `:on-finish` (e.g. someday-review reports a summary). When hosted, finish must advance the **session**, not run the walk's standalone `:on-finish`. So `--walk-step-next` must **override** `:on-finish` (and `:render`) on the looked-up spec before starting:

```elisp
(let ((spec (copy-sequence (org-gtd-review--spec-for-step step))))
  (setq spec (plist-put spec :on-finish #'org-gtd-review--complete-step))
  (setq spec (plist-put spec :render #'org-gtd-review--hosted-render))
  (setq spec (plist-put spec :resumable nil))
  (org-gtd-review--start-hosted-walk spec))
```

This is the design's "Composition when hosted": surface = console region, `:on-finish` = advance the session, `:resumable nil`, model embedded in the review checkpoint. The checklist spec already sets these, so the override is a no-op for it and the real work for registry walks. (The registered walk's own `:render` — e.g. someday-review's buffer-popping render — is deliberately replaced; a real hosted heading walk that needs its own per-item render/keymap is the deferred net-new design.)

**Step 4 — Run, expect PASS.** Re-run `review-test.el` + `review-walk-test.el` — green.

**Step 5 — Compile clean, commit** (`feat: generic walk review step type driven by the registry`).

---

### Task B3: Resume a `walk` step across restart

`--begin-session` rehydrate (A4) already keys on `(memq type '(checklist walk))` and `--spec-for-step`. Add explicit coverage.

**Files:**
- Test: `test/unit/review-walk-test.el`
- Modify (only if the test reveals a gap): `org-gtd-review.el`

**Step 1 — Failing/confirming test:** start the registered-walk profile, advance to `beta`, kill the console, resume, assert `(2/3)` and `beta`. Also assert `org-gtd-walk--locked-scopes` is released after the kill (teardown unlock) and re-acquired after resume.

**Step 2 — Run.** If green, the A4 machinery already generalizes — record and move on. If red, extend `--begin-session` rehydrate / `--spec-for-step` to cover the `walk` case and the override of `:on-finish`/`:render` on rehydrate (same overrides as B2).

**Step 3 — Compile clean, commit** (`test: walk review step resumes across restart`).

---

### Task B4: Full-suite regression sweep

**Files:** none (verification).

1. Run the `/test` skill over the whole suite (not just review files) — the fold touches shared session/checkpoint code; confirm no collateral breakage (someday-review, walk-driver, walk-model, checklist tests).
2. `~/bin/eldev compile --warnings-as-errors` — clean.
3. If green, Deliverable B is complete. Commit any final doc/comment touch-ups separately (`docs:`), keeping generated artifacts out of source commits per project convention.

---

## Explicitly OUT of scope — flag, do not build

**A real net-new heading-walk review step** (e.g. turning "Review stuck projects" from today's read-only `:type view` step into an actionable `:type walk :walk stuck-projects` that walks each stuck project and acts on it in place). This is **net-new UX** the design (§12, "Two kinds of phase") says must be **designed collaboratively first** — a short brainstorm agreeing the exact interaction before any code:

- How per-item action keys (give-a-next-action / defer / skip) become active inside the console buffer (Decision 2 shows the console keymap does not currently host them — this is the crux of the deferred design).
- Whether such a step renders the project subtree into the console region or pops a WIP buffer (someday-review's render pops a buffer and is **incompatible** with the console region surface as written — evidence this needs its own design).
- What "act on a stuck project" does (edit in place, jump to clarify, etc.) and how it composes with the session's checkpoint.

Phase 2 delivers only the **mechanism** (the `walk` step type + hosted composition), validated with a tiny test-only walk. The `stuck-projects` step in the default profile **remains a `:type view` step**. Do not add it to any shipped profile as a walk. Track the heading-walk-step UX as a separate follow-on design item (brainstorm → design doc → its own plan), per the v5 discipline for net-new behavior.

---

## Task outline (11 tasks)

**Deliverable A — checklist fold (characterize-first):**
- A0: Establish the green baseline
- A1: Build the checklist walk spec (unwired)
- A2: Build the hosted render + sync helper
- A3: Route checklist advancement through the engine
- A4: Resume + teardown (scope-unlock) for hosted walks
- A5: Cutover — delete bespoke mechanics, rewrite internal tests

**Deliverable B — generic `walk` step type:**
- B1: Validate `walk` steps in `--check-profile`
- B2: Dispatch, render, finish for `walk` steps (test-only registered walk)
- B3: Resume a `walk` step across restart
- B4: Full-suite regression sweep
