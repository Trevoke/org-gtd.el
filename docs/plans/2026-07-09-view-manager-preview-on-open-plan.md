# View Manager builder: preview-on-open (and header clarity) — TDD plan

Yak: `view-manager-builder-no-summarypreview-feedback-on-open-iv4b`
Date: 2026-07-09
Scope: `org-gtd-view-manager.el` builder transient UX fix. Two sub-points from manual QA.

## Problem (from QA, 2026-07-09 steps 3.1–3.2)

1. **PRIMARY — no preview on open.** Opening the builder (`c` Create, or `e` Edit
   in the manager) does NOT render a preview. Nothing agenda-shaped appears until
   the user presses `RET` or changes an infix. If a stale agenda from a prior
   action is still on screen, the builder looks like it is previewing stale data.
   **Fix:** auto-render the preview ONCE when the builder opens, so the current
   spec's agenda shows immediately.

2. **SECONDARY — summary not obviously a header.** The top summary line
   (`View: <name>  —  <badge>`, from `org-gtd-view-manager--build-summary`, shown
   via the transient `[:description ...]`) did not stand out; the tester could not
   tell where the "header" was ("What header? I don't know where that is supposed
   to be"). Keep any tweak minimal.

## Environment & constraints (READ FIRST — do not deviate)

- Work ONLY in `/home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design`.
  Do NOT `cd` elsewhere. Use absolute paths.
- Run tests ONLY via the repo script (NOT the `/test` skill):
  - Single file: `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el`
  - Category: `.claude/skills/test/run-tests.sh unit`
- e-unit framework. Study `test/unit/view-manager-preview-test.el` and
  `test/unit/view-manager-build-test.el` for idioms: `cl-letf` stubs,
  `ogt-eunit-with-mock-gtd`, `around-each`, `assert-equal`/`assert-true`.
- Compile check: `~/bin/eldev compile --warnings-as-errors` then
  `git checkout -- dir` (compile deletes the tracked Info `dir`). NEVER `git add`
  `dir` or any `.elc`.
- Do NOT regress prior work on this branch:
  - **P1** preview-refresh: `--compile` conses fresh cells; `--preview-now` has an
    optional `force` arg; `--preview`/`RET` forces.
  - **P2** render window: `--render-preview` let-binds
    `org-agenda-window-setup` to `current-window` (keeps the transient panel).
  - **Layout**: the 3+2 side-by-side column layout in the builder macro.
  - Debounce (`--preview-schedule`), fail-soft `condition-case`, sample-data branch.
- Do NOT fix the known message-spam yak (lk0a), z15n, or eneg here. A single
  render on open emits at most one message — acceptable, does not worsen lk0a.

## Chosen mechanism (and why)

**Render immediately and forcibly in the interactive body, BEFORE `transient-setup`:**

```elisp
(setq org-gtd-view-manager--preview-last nil)
(org-gtd-view-manager--preview-now t)   ;; <-- NEW: render once on open
(transient-setup 'org-gtd-view-manager--build)
```

Rationale, weighed against the alternatives:

- **Immediate call vs `--preview-schedule` (idle timer).** An immediate call is
  deterministic and directly unit-testable. `--preview-schedule` fires on the next
  idle tick without `force`, is timing-sensitive, and would collide with test
  determinism. The debounced path exists for *rapid infix edits*, not for a single
  guaranteed render on open. Choose the immediate call.
- **Before vs after `transient-setup`.** The whole interactive body runs to
  completion first; the transient's event loop only takes over afterward, so code
  placed after `transient-setup` would still run before any key is read. Placing
  the render **before** `transient-setup` is nonetheless preferred because it makes
  the target window deterministic: `--render-preview` binds
  `org-agenda-window-setup` to `current-window`, i.e. it lands the agenda in the
  *selected* window. Before `transient-setup`, that is unambiguously the window the
  builder was invoked from (the same window the config snapshot just captured).
  After `transient-setup`, there is a chance the transient's own popup window is
  selected/current and the agenda would render into that tiny panel. Rendering
  first avoids that class of bug entirely, and the window-config snapshot is
  already taken at the top of the body, so abort/save restore-on-exit still works.
- **`force` argument.** `--preview-last` was just reset to `nil`, so the
  changed-check would fire anyway; passing `t` is defensive and self-documenting
  ("always render on open"). `--preview-now` sets `--preview-last` on success, so a
  subsequent identical `RET` still force-renders (P1, unchanged) while a subsequent
  identical *debounce* correctly no-ops — no pointless double render.
- **No new helper.** `--preview-now` is already the tested seam. The on-open
  behaviour is verified by driving the real prefix body with `transient-setup`
  stubbed (below), which also proves the call is placed in the body — stronger than
  testing an extracted helper in isolation.

Edge cases handled for free: empty `org-agenda-files` routes through
`--render-preview`'s sample-data branch (banner shown) on open too; editing (`e`)
previews the stored spec because the body seeds `--build-state` from
`starting-spec` before the render; a fresh create previews the default
`((name . "Untitled") (type . next-action))` spec.

## Headlessly testable vs manual

- **Headless (unit):**
  - Opening the builder renders exactly once, with the *starting* spec — for both
    a fresh create (default `next-action`) and an edit (stored spec). Drive
    `org-gtd-view-manager--build` with `transient-setup` → `ignore` and
    `--render-preview` → a counter/capture stub.
  - The render populates `--preview-last` with the compiled starting spec.
  - (Point 2) `--build-summary` string content + the `transient-heading` face on
    the label.
- **Manual QA only:** the actual agenda appearing on screen; the panel staying
  visible (P2); the header *reading* as a header; the sample-data banner on empty
  `org-agenda-files`.

---

## Commit 1 — auto-render the preview on builder open (PRIMARY)

### Step 1.1 (RED) — add the on-open render tests

Append to `test/unit/view-manager-build-test.el` (before the final `;;;` line).
These live with the other builder-integration tests and reuse that file's
`ogt-eunit-with-mock-gtd` `around-each`.

```elisp
(deftest view-manager-build/renders-preview-on-open-fresh ()
  "Opening a fresh builder renders the preview once for the default spec.
Regression: the builder used to show nothing until the first RET/infix,
so a stale agenda from a prior action looked like the builder's preview."
  (let ((count 0)
        (captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (cl-incf count) (setq captured spec))))
      (org-gtd-view-manager--build))
    (assert-equal 1 count)
    (assert-equal 'next-action (alist-get 'type captured))
    (assert-equal "Untitled" (alist-get 'name captured))))

(deftest view-manager-build/renders-preview-on-open-edit ()
  "Editing an existing view renders that view's stored spec once on open."
  (let ((count 0)
        (captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (cl-incf count) (setq captured spec))))
      (org-gtd-view-manager--build '((name . "Saved") (type . delegated))))
    (assert-equal 1 count)
    (assert-equal 'delegated (alist-get 'type captured))
    (assert-equal "Saved" (alist-get 'name captured))))

(deftest view-manager-build/render-on-open-populates-cache ()
  "The on-open render seeds `--preview-last', so an identical debounce no-ops.
RET still force-renders (covered in view-manager-preview-test); this only
guards that the open render is not itself skipped and does update the cache."
  (let ((org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview) #'ignore))
      (org-gtd-view-manager--build '((name . "Saved") (type . delegated))))
    (assert-equal 'delegated (alist-get 'type org-gtd-view-manager--preview-last))))
```

Notes:
- `transient-setup` is stubbed to `ignore` so no live transient is set up —
  `org-gtd-view-manager--build`'s body otherwise runs verbatim (it is an ordinary
  command; the prefix macro only stores the layout separately). This is why the
  test can assert placement/behaviour of the real body.
- `--render-preview` is stubbed, so no `org-agenda`, no sample-data, no message,
  no timer — the test is hermetic and cannot worsen lk0a.
- `cl-lib` is already required transitively via the prelude; `cl-incf` is used the
  same way as in `view-manager-preview-test.el`.

**Run (expect RED):**

```
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```

Expected: the two `renders-preview-on-open-*` tests FAIL with
`assert-equal 1 count` seeing `0` (the current body never renders on open); the
cache test FAILS seeing `--preview-last` still `nil`. All previously-passing tests
in the file still pass.

### Step 1.2 (GREEN) — render once in the builder body

In `org-gtd-view-manager.el`, inside the `transient-define-prefix
org-gtd-view-manager--build` body generated by
`org-gtd-view-manager--define-builder-transient` (around lines 747–753), insert the
render call between the `--preview-last` reset and `transient-setup`:

```elisp
         (setq org-gtd-view-manager--build-dirty nil)
         (setq org-gtd-view-manager--preview-last nil)
         ;; Render the starting spec's agenda immediately so the builder shows a
         ;; live preview the moment it opens, instead of a stale agenda from a
         ;; prior action until the first RET/infix.  Forced (`--preview-last' was
         ;; just cleared) and placed BEFORE `transient-setup' so the agenda lands
         ;; in the invoking window (the one the config snapshot captured), never
         ;; the transient's own popup window.  Sets `--preview-last', so a later
         ;; identical debounce no-ops while RET still force-renders (P1).
         (org-gtd-view-manager--preview-now t)
         (transient-setup 'org-gtd-view-manager--build)))))
```

Leave everything else in the body untouched (window-config snapshot,
original-name, state seeding).

**Run (expect GREEN):**

```
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```

Expected: all tests in the file PASS, including the three new ones.

### Step 1.3 — full unit suite + compile

```
.claude/skills/test/run-tests.sh unit
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
```

Expected: unit suite green (no regression in `view-manager-preview-test.el` or
elsewhere); compile finishes with zero warnings/errors. Confirm `git status`
shows no `dir`/`.elc` staged.

### Step 1.4 — commit

```
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit
```

Message:

```
fix(view-manager): render preview once when the builder opens

Opening the builder (Create/Edit) now renders the current spec's
agenda immediately instead of leaving a stale agenda from a prior
action on screen until the first RET/infix.  Forced render placed
before `transient-setup' so the agenda lands in the invoking window,
not the transient popup; it seeds `--preview-last' so a later
identical debounce no-ops while RET still force-renders.

Yak: view-manager-builder-no-summarypreview-feedback-on-open-iv4b

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

---

## Commit 2 — make the summary read as a header (SECONDARY, recommended)

**Recommendation: DO this — it is a one-line, low-risk, idiomatic change and it
directly answers point 2.** Point 1 (auto-preview) already removes the "nothing
happens on open" feeling, but the tester also literally could not locate the
summary line. The cleanest fix is to render the `View:` label in transient's own
`transient-heading` face, so the summary visually matches the column headings
("Type", "Time", …) right below it and unmistakably reads as the panel's title.
No layout change, no new line, no decoration.

### Step 2.1 (RED) — summary header test

Append to `test/unit/view-manager-build-test.el`:

```elisp
(deftest view-manager-build/summary-label-is-a-heading ()
  "The summary's `View:' label carries the `transient-heading' face and the
visible text still reads `View: <name>  —  <badge>'."
  (setq org-gtd-view-manager--build-state
        (list (cons 'name "Untitled") (cons 'type 'next-action)))
  (let ((summary (org-gtd-view-manager--build-summary)))
    (assert-equal 'transient-heading (get-text-property 0 'face summary))
    (assert-true (string-prefix-p "View: "
                                  (substring-no-properties summary)))
    (assert-true (string-match-p "Untitled"
                                 (substring-no-properties summary)))))
```

**Run (expect RED):**

```
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```

Expected: FAILS on `get-text-property` returning `nil` (the current label has no
face).

### Step 2.2 (GREEN) — propertize the label

In `org-gtd-view-manager.el`, update `org-gtd-view-manager--build-summary`
(lines 462–468):

```elisp
(defun org-gtd-view-manager--build-summary ()
  "Return the summary header line for the builder.
The leading `View:' label is rendered in `transient-heading' so it
reads as the builder's title, matching the column headings below it."
  (concat
   (propertize "View: " 'face 'transient-heading)
   (or (cdr (assq 'name org-gtd-view-manager--build-state)) "Untitled")
   "  —  "
   (org-gtd-view-manager--badge
    (org-gtd-view-manager--compile org-gtd-view-manager--build-state))))
```

Only the `"View: "` literal is wrapped; the name, separator, and badge are
unchanged. `transient-heading` is a standard transient face (no new require).

**Run (expect GREEN):**

```
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
```

Expected: all tests PASS; compile clean.

### Step 2.3 — commit

```
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit
```

Message:

```
fix(view-manager): render the builder summary as a heading

Wrap the summary's `View:' label in the `transient-heading' face so
it visually matches the column headings and reads clearly as the
builder's header, addressing QA feedback that the header could not be
located.

Yak: view-manager-builder-no-summarypreview-feedback-on-open-iv4b

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

---

## Manual re-QA (after both commits)

Run in a real Emacs (`M-x org-gtd-view-manager`):

1. **Fresh create.** Press `c`. The default next-action agenda previews
   IMMEDIATELY (no `RET` needed). The summary line reads clearly as a header.
2. **Edit existing.** Highlight a saved view, press `e`. That view's stored spec
   previews immediately on open.
3. **Empty agenda-files.** With `org-agenda-files` empty, open the builder: the
   sample-data preview and its banner show on open (single message — acceptable).
4. **No regressions:** changing an infix still updates the preview (P1); pressing
   `RET` still force-refreshes; the transient panel stays visible across refreshes
   (P2); the 3+2 column layout is intact; rapid infix edits still coalesce
   (debounce); abort/save restore the entry window layout.

## Regression guardrails / out of scope

- Do NOT touch P1 (`--compile` fresh cells, `--preview-now` force arg,
  `--preview`/RET), P2 (`--render-preview` `current-window` binding), the column
  layout, the debounce timer, the fail-soft `condition-case`, or the sample-data
  branch.
- Do NOT attempt to fix lk0a (message spam), z15n, or eneg here.
- Do NOT `git add` `dir` or `.elc`; always `git checkout -- dir` after compiling.
