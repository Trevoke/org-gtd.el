# Walk Engine Phase 1 — Migrate someday-review — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Re-express `org-gtd-someday-review` as a walk **spec** registered in `org-gtd-walks` and driven by the Phase-0 engine (`org-gtd-walk.el` / `org-gtd-walk-model.el`), deleting the bespoke state machine while preserving every user-visible behavior (same command, same `d`/`c`/`q` keys, same UX).

**Architecture:** someday-review becomes a **consumer** of the generic engine. The registered spec is a *template* (`:find` = all someday items, plus `:render`/`:actions`/`:on-finish`/`:resolve`/`:scope`); the interactive entry command specializes `:find` (closing over the chosen list filter and the items it already found) and `:scope` (the mutation surface), then calls `org-gtd-walk-start`. The engine owns the queue, cursor, render/checkpoint/transition loop, scope lock, and empty/finish handling; someday-review keeps only its org effects (`--find-items`, `--add-reviewed-entry`, reactivate), its WIP-buffer render, its major mode + keymap, and the three action commands (rewired to do org effect → engine transition).

**Tech Stack:** Emacs Lisp (lexical binding), org-mode, the Phase-0 walk engine, e-unit tests, Eldev. Run tests **only** via `.claude/skills/test/run-tests.sh`.

---

## READ BEFORE STARTING — decisions the maintainer must rule on

This is a **migration of a shipped feature**. Nine points below change the shape
of the work; several need a maintainer call. **Do not start Task 1 until items
1–3 are confirmed.** They are also summarized back to the maintainer in the
handoff report.

> **RULED (orchestrator) — all three confirmed as recommended, proceed:**
> (1) characterize = behavior-subset stays green throughout + internal-state
> subset rewritten to Tier-3 adapter tests at cutover (no state-machine shims);
> (2) `:scope = (org-agenda-files)` — the real mutation surface;
> (3) `:resumable nil` — behavior parity (resumability is a later feature call).

1. **"Existing tests pass unchanged" is not literally achievable.** Of the 25
   tests in `test/unit/someday-review-test.el`, roughly **nine assert deleted
   internals** — `org-gtd-someday-review--state` (`:queue`/`:position`/`:reviewed`/
   `:clarified`/`:list-name`), `--start-session`/`--end-session`, and the
   `--advance` cursor mechanics the engine now owns. Once those symbols are
   deleted the tests **error** (void-variable / void-function), so they cannot
   stay green "unchanged." The migration discipline is therefore realized as:
   **characterize first** = capture the current 25-green baseline and split it
   into a *behavior safety-net subset* (stays green through the whole migration)
   and an *internal-state subset* (rewritten to Tier-3 adapter tests **at
   cutover**, because the old symbols vanish then). **Recommendation:** adopt
   this split (categorized in Task 1); do **not** keep the old state machine
   alive as shims purely to satisfy the internal tests — that would defeat the
   migration. **Maintainer: confirm this reading of "characterize then thin."**

2. **Scope choice (design §5).** The engine (Phase 0) treats `:scope` as a
   literal value — `org-gtd-walk-start` never `funcall`s it; it is fed straight
   to `--scope-key` (`md5`/`member`), which accepts a string or a list of
   strings. someday's `--find-items` scans **all** `(org-agenda-files)`, but
   design §5 nominally names "the tasks file." The lock must cover the **actual
   mutation surface**. **Recommendation:** scope = the file-set
   `(org-agenda-files)`, computed in the entry command and injected into the
   spec (the engine keys a list order-independently). **Maintainer: confirm
   file-set scope vs. the narrower single tasks-file scope.**

3. **`:resumable nil`.** someday-review was in-memory only; keeping it
   non-resumable is behavior parity, minimizes change, and sidesteps the
   quit-vs-finish checkpoint distinction. Resumability is a later, separate
   decision. **Recommendation: `nil`.** **Maintainer: confirm.**

4. **Single reused surface buffer, not per-item WIP temp files.** The engine has
   **one** surface for the whole walk; the old code created a fresh
   `org-gtd-wip--get-buffer <item-id>` per item and cleaned up the previous one.
   The migration uses one WIP buffer keyed by a fixed session key
   (`"someday-review"`), and `:render` **erases and refills** it each item.
   User-visible behavior is identical (one review buffer, current item shown,
   `org-gtd-wip--get-buffers` non-empty during the walk, empty after). Internal
   temp-file semantics change (one file reused vs. many). *Informational.*

5. **Progress header needs the model.** The old header showed `(pos/total)`. The
   engine's `:render` signature is `(handle surface)` with no model argument, but
   render runs **in the surface buffer**, where `org-gtd-walk--active` is
   buffer-local and holds the model. Render reads cursor+length from there for
   the header. Minor, accepted coupling. *Informational.*

6. **The registry entry is a template; the entry command specializes it.**
   `:find` must close over the prompted list filter (and reuse the items already
   found to avoid a second agenda scan), and `:scope` is computed at
   invocation. The registered `org-gtd-walks` entry defaults `:find` to *all*
   someday items so the Phase-2 `walk` step type can use it declaratively.
   *Informational.*

7. **`:on-finish` runs after `org-gtd-walk--active` is cleared.** `walk-finish`
   sets `org-gtd-walk--active` to nil *before* calling `:on-finish`, so the
   summary counts cannot come from the model there. Counts are held in a
   **buffer-local** counters var on the surface, bumped by the actions, read by
   `:on-finish`. *Informational.*

8. **quit preserves the old summary + cleanup.** The old `quit` ran
   `--cleanup-and-end` → `--end-session`, which **cleaned the WIP buffer and
   printed the summary**. Engine `walk-quit` runs no `:on-finish`. So the quit
   *command* prints the summary and cleans the surface itself, then calls
   `org-gtd-walk-quit`. Behavior preserved. *Informational.*

9. **`:actions` is inert in the Phase-0 engine.** The driver never reads
   `:actions`; a standalone walk gets its keys from its major-mode map (which
   still exists). `:actions` is set to the same keymap for completeness and for
   the Phase-2 host (which will install it into a step region). *Informational.*

---

## Files

- **Modify:** `org-gtd-someday-review.el` (the whole guts; keep header, mode,
  keymap, `--find-items`, `--item-matches-filter-p`, `--add-reviewed-entry`,
  `--initialize-buffer` logic; delete `--state`/`--advance`/`--session-active`/
  `--start-session`/`--end-session`/`--cleanup-and-end`/`--display-current-item`
  old form; rewire commands + entry point; register the spec).
- **Modify:** `test/unit/someday-review-test.el` (rewrite the internal-state
  subset to Tier-3 adapter tests at cutover; keep the behavior subset).
- **Reference (do not modify):** `org-gtd-walk.el`, `org-gtd-walk-model.el`,
  `org-gtd-wip.el`, `org-gtd-core.el` (`org-gtd--without-kill-merge`,
  `org-gtd-someday`, `org-gtd-prop-someday-list`), `org-gtd-files.el`,
  `org-gtd-reactivate.el`, `org-gtd-someday.el` (`org-gtd-someday-lists`).
- **Reference tests (do not modify):** `test/unit/someday-dispatcher-test.el`
  (must stay green — it exercises `org-gtd-someday`, untouched by this work),
  `test/unit/walk-*-test.el` (the engine contract).

---

## Task 1: Characterize — capture the baseline and split the tests

No code. Establish the safety net before touching anything.

**Step 1: Run the existing someday-review suite and record it green.**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected: all 25 pass. Record the count + seed in the task notes. If not
green on a clean `org-gtd-5`+Phase-0 tree, STOP and report — the baseline is
the contract.

**Step 2: Categorize each test (write the categorization into the task notes).**

*Behavior safety-net (must stay green through the entire migration; adapt only
their setup to the public API if they currently call deleted internal helpers):*
- `adds-reviewed-entry-to-logbook`, `preserves-existing-logbook-entries`
  (`--add-reviewed-entry`, kept real)
- `finds-all-someday-items`, `filters-by-list-property`, `finds-unassigned-items`
  (`--find-items`, kept real)
- `creates-wip-buffer-with-review-mode`, `shows-keybindings-in-header-line`
  (WIP buffer + mode + read-only + content + header — real behavior, currently
  driven through internal helpers; re-express via the engine entry path)
- `mode-has-defer-keybinding`, `mode-has-clarify-keybinding`,
  `mode-has-quit-keybinding`, `mode-is-derived-from-org-mode`,
  `mode-has-essential-keybindings` (keymap + mode, kept)
- `defer-adds-logbook-entry`, `defer-ends-session-when-done` (org effect + finish)
- `clarify-increments-clarified-count` (runs clarify without error)
- `quit-ends-session`, `quit-cleans-up-wip-buffer` (teardown behavior)
- `entry-point-starts-session`, `entry-point-shows-message-when-no-items`
  (public command behavior)
- `evil-integration-registered` (after-load hook, unaffected)

*Internal-state subset (rewritten to Tier-3 adapter tests at cutover — Task 6):*
- `initializes-session-state` (`--state :queue`/`:position`)
- `tracks-statistics` (`--state :reviewed`/`:clarified`)
- `defer-advances-to-next-item` (`:position`/`:reviewed`)
- `clarify-advances-to-next-item` (`:position`/`:clarified`)
- `entry-point-accepts-list-argument` (`--state :list-name`)

**Step 3: Commit the categorization note (no source change).**

```bash
git add docs/plans/2026-07-17-walk-engine-phase-1-plan.md
git commit -m "docs: record someday-review characterization baseline for walk migration

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Add the `:find` builder (adapter over `--find-items`)

Build the engine-facing find as a closure factory. `--find-items` already
returns org-ids for a filter; wrap it so the entry command can close over a
filter and (later) over pre-found items.

**Files:** Modify `org-gtd-someday-review.el`; Test `test/unit/someday-review-test.el`.

**Step 1: Write the failing Tier-3 test.**

```elisp
(deftest someday-review/find-builder-returns-filtered-ids ()
  "The :find builder yields exactly the ids matching its filter."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    (with-suppressed-warnings ((obsolete org-gtd-someday-create))
      (with-simulated-input "Work RET" (org-gtd-someday-create "Work idea"))
      (with-simulated-input "Personal RET" (org-gtd-someday-create "Personal idea")))
    (let ((find (org-gtd-someday-review--make-find "Work")))
      (assert-equal 1 (length (funcall find))))))
```

This test needs `(around-each ... ogt-eunit-with-mock-gtd ...)`, already present
in the file.

**Step 2: Run to verify it fails.**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected: FAIL — `org-gtd-someday-review--make-find` undefined.

**Step 3: Implement.**

```elisp
(defun org-gtd-someday-review--make-find (list-filter)
  "Return a nullary :find closure yielding someday ids for LIST-FILTER."
  (lambda () (org-gtd-someday-review--find-items list-filter)))
```

**Step 4: Run to verify it passes.** Expected: PASS.

**Step 5: Commit.**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "feat: add someday-review :find builder for walk engine

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Add the `:resolve` predicate

Stale-handle skipping for robustness (an id whose heading vanished mid-walk).
Cheap; matches the engine's `--settle` contract.

**Files:** Modify `org-gtd-someday-review.el`; Test same test file.

**Step 1: Write the failing test.**

```elisp
(deftest someday-review/resolve-rejects-missing-id ()
  "The :resolve predicate is nil for an unknown id, non-nil for a real one."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Real item"))
  (let ((id (car (org-gtd-someday-review--find-items nil))))
    (assert-true (org-gtd-someday-review--resolve id))
    (assert-nil (org-gtd-someday-review--resolve "no-such-id-xyz"))))
```

**Step 2: Run — FAIL** (`--resolve` undefined).

**Step 3: Implement.**

```elisp
(defun org-gtd-someday-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))
```

**Step 4: Run — PASS.**

**Step 5: Commit** (`feat: add someday-review :resolve predicate`).

---

## Task 4: Add the `:render` function (`(handle surface)` contract)

Adapt `--display-current-item` + `--initialize-buffer` to the engine: resolve
id→marker, **erase and refill** the surface with the item's subtree (using
`org-gtd--without-kill-merge` for the copy/paste), set the header-line progress
from the buffer-local `org-gtd-walk--active` model, ensure the major mode +
read-only, and display the buffer.

**Files:** Modify `org-gtd-someday-review.el`; Test same test file.

**Step 1: Write the failing test.** Drive render directly against a surface
buffer that already carries a minimal active bundle (so the header can read the
model), mirroring how the driver calls it.

```elisp
(deftest someday-review/render-fills-surface-with-current-item ()
  "Render draws the item, activates review mode read-only, and shows progress."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Render me"))
  (let* ((id (car (org-gtd-someday-review--find-items nil)))
         (surface (org-gtd-wip--get-buffer "someday-review")))
    (with-current-buffer surface
      (setq-local org-gtd-walk--active
                  (list :model (org-gtd-walk-model-create (list id))))
      (org-gtd-someday-review--render id surface)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Render me" (buffer-string))
      (assert-match "\\[d\\]" header-line-format)
      (assert-match "(1/1)" header-line-format))
    (org-gtd-wip--cleanup-temp-file "someday-review")))
```

Add `(require 'org-gtd-walk)` to the test file's requires if not already loaded
transitively.

**Step 2: Run — FAIL** (`--render` undefined).

**Step 3: Implement.** Reuse the subtree-copy logic, wrapped in the Phase-0
kill-merge guard; read progress from the active model.

```elisp
(defun org-gtd-someday-review--render (id surface)
  "Render the someday item ID into SURFACE (the walk :render contract).
Resolves ID to a marker, refills SURFACE with the subtree, and sets the
review mode, read-only state, header-line, and display."
  (let ((marker (org-id-find id 'marker)))
    (when marker
      (with-current-buffer surface
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-gtd--without-kill-merge
            (org-with-point-at marker (org-copy-subtree)))
          (org-paste-subtree)
          (goto-char (point-min)))
        (unless (eq major-mode 'org-gtd-someday-review-mode)
          (org-gtd-someday-review-mode))
        (setq buffer-read-only t)
        (let* ((model (plist-get org-gtd-walk--active :model))
               (pos (1+ (plist-get model :cursor)))
               (total (length (plist-get model :entries))))
          (setq header-line-format
                (format "[d] Defer  [c] Clarify  [q] Quit  (%d/%d)" pos total)))
        (pop-to-buffer surface)))))
```

*Note:* `org-copy-subtree`/`org-paste-subtree` must both run without the kill
guard tripping; keep the copy inside `org-gtd--without-kill-merge` exactly as the
archive/organize modules do. The paste reads the same kill entry immediately, so
wrapping the copy is sufficient (mirrors the old `--initialize-buffer`, which
bound `last-command` to nil around the copy only).

**Step 4: Run — PASS.**

**Step 5: Commit** (`feat: add someday-review :render for walk engine`).

---

## Task 5: Cutover — register the spec, rewire the entry command + action commands, delete the state machine

This is the atomic behavior-preserving cutover. After it, the **behavior
safety-net subset** must be green; the **internal-state subset** is rewritten in
Task 6 (do the two together if executing in one session — they are one logical
change split for reviewability). Break into fine steps.

**Files:** Modify `org-gtd-someday-review.el`.

**Step 1: Add the surface constructor + buffer-local counters.**

```elisp
(defconst org-gtd-someday-review--surface-key "someday-review"
  "Fixed WIP key for the single someday-review surface buffer.")

(defvar-local org-gtd-someday-review--counters nil
  "Buffer-local plist (:reviewed N :clarified N) for the active surface.")

(defun org-gtd-someday-review--surface ()
  "Return the fresh single WIP surface buffer for a someday-review walk."
  (let ((buf (org-gtd-wip--get-buffer org-gtd-someday-review--surface-key)))
    (with-current-buffer buf
      (setq-local org-gtd-someday-review--counters (list :reviewed 0 :clarified 0)))
    buf))

(defun org-gtd-someday-review--bump (key)
  "Increment counter KEY (:reviewed or :clarified) on the surface buffer."
  (setq org-gtd-someday-review--counters
        (plist-put org-gtd-someday-review--counters key
                   (1+ (plist-get org-gtd-someday-review--counters key)))))
```

**Step 2: Add `:on-finish` (summary + surface cleanup).**

```elisp
(defun org-gtd-someday-review--on-finish ()
  "End-of-walk: report the summary and clean up the surface buffer.
Runs in the surface buffer after the engine has cleared its session."
  (let ((reviewed (plist-get org-gtd-someday-review--counters :reviewed))
        (clarified (plist-get org-gtd-someday-review--counters :clarified)))
    (org-gtd-wip--cleanup-temp-file org-gtd-someday-review--surface-key)
    (message "Review complete. %d items reviewed, %d clarified."
             reviewed clarified)))
```

*Note:* `org-gtd-wip--cleanup-temp-file` kills the surface buffer it is called
from; that is safe (it is the tail of the transition). The `message` reads the
counters captured before the kill.

**Step 3: Register the spec template in `org-gtd-walks`.**

```elisp
(defun org-gtd-someday-review--spec ()
  "Return the someday-review walk spec template (default :find = all items)."
  (list :name 'someday-review
        :find (org-gtd-someday-review--make-find nil)
        :render #'org-gtd-someday-review--render
        :actions org-gtd-someday-review-mode-map
        :on-finish #'org-gtd-someday-review--on-finish
        :resumable nil
        :resolve #'org-gtd-someday-review--resolve
        :scope (org-agenda-files)))

(org-gtd-walk-register 'someday-review (org-gtd-someday-review--spec))
```

Add `(require 'org-gtd-walk)` to the module's Requirements block.

**Step 4: Rewire the entry command to the engine.**

```elisp
;;;###autoload
(defun org-gtd-reflect-someday-review (&optional list)
  "Review someday/maybe items one at a time.
With optional LIST argument, review only items in that list.
When `org-gtd-someday-lists' is configured, prompts for list selection.
Adds \\='Unassigned\\=' option for items without a list."
  (interactive
   (list (when org-gtd-someday-lists
           (completing-read "Review which list? "
                            (append org-gtd-someday-lists '("Unassigned"))
                            nil t))))
  (let* ((list-filter (cond
                       ((equal list "Unassigned") 'unassigned)
                       ((and list (not (string-empty-p list))) list)
                       (t nil)))
         (items (org-gtd-someday-review--find-items list-filter)))
    (if (null items)
        (message "No someday items to review.")
      (let ((spec (org-gtd-someday-review--spec)))
        (setq spec (plist-put spec :find (lambda () items)))
        (setq spec (plist-put spec :scope (org-agenda-files)))
        (org-gtd-walk-start spec (org-gtd-someday-review--surface))))))
```

*Rationale:* find runs **once**; the empty case messages without creating a
buffer or locking scope (preserving the old "No someday items" behavior and
avoiding a stray buffer); the non-empty case closes `:find` over the found items
so the engine does not re-scan.

**Step 5: Rewire the three action commands to org-effect → transition.**

```elisp
(defun org-gtd-someday-review-defer ()
  "Defer the current item (log a review) and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker
           (org-gtd-someday-review--add-reviewed-entry)
           (save-buffer)))
       (org-gtd-someday-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-someday-review-clarify ()
  "Reactivate the current item and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker (org-gtd-reactivate)))
       (org-gtd-someday-review--bump :clarified)
       (org-gtd-walk-advance)))))

(defun org-gtd-someday-review-quit ()
  "Abandon the review: report the summary, clean up, tear down the walk."
  (interactive)
  (let ((reviewed (plist-get org-gtd-someday-review--counters :reviewed))
        (clarified (plist-get org-gtd-someday-review--counters :clarified)))
    (org-gtd-walk-quit)
    (org-gtd-wip--cleanup-temp-file org-gtd-someday-review--surface-key)
    (message "Review complete. %d items reviewed, %d clarified." reviewed clarified)))
```

*Note:* `defer`/`clarify` run in the surface buffer (keymap dispatch), so
`org-gtd-walk--active` and the buffer-local counters are in scope. `quit` reads
counters before `walk-quit` (which nils the session but leaves the buffer-local
counter var intact until cleanup).

**Step 6: Delete the bespoke state machine.** Remove:
`org-gtd-someday-review--session-active`, `--state`,
`org-gtd-someday-review--current-item-id` (defvar-local),
`--start-session`, `--end-session`, `--advance`, `--cleanup-and-end`,
`--display-current-item`, `--cleanup-current-buffer`, `--initialize-buffer`
(its copy/paste logic now lives inline in `--render`).
Keep: `--find-items`, `--item-matches-filter-p`, `--add-reviewed-entry`, the
major mode, the keymap, the evil integration.

**Step 7: Compile clean.**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: no warnings/errors. Fix any (unused vars from deletions, missing
requires, docstring/checkdoc nits).

**Step 8: Run the behavior safety-net subset.**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected: the behavior subset passes; the internal-state subset **errors**
(deleted symbols) — that is expected and fixed in Task 6. If any *behavior*
test fails, debug with superpowers:systematic-debugging until it passes; behavior
parity is the gate.

**Step 9: Commit the source cutover** (do not commit red tests — Task 6 lands
with this if executing together; if committing separately, note the known-red
internal subset in the message).

```bash
git add org-gtd-someday-review.el
git commit -m "refactor: drive someday-review through the walk engine

Register a someday-review walk spec in org-gtd-walks and rewire the entry
command and d/c/q commands onto org-gtd-walk-start/-advance/-quit; delete the
bespoke queue/cursor state machine. No user-visible behavior change.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Thin — rewrite the internal-state tests as Tier-3 adapter tests

Replace the five internal-state tests (which asserted `--state`/`--advance`
mechanics the engine now owns and Tier-2 driver tests already cover) with thin
adapter tests of someday's own wiring; adapt any behavior test whose *setup*
called a deleted helper to use the public entry path.

**Files:** Modify `test/unit/someday-review-test.el`.

**Step 1: Delete the internal-state tests** `initializes-session-state`,
`tracks-statistics`, `defer-advances-to-next-item`,
`clarify-advances-to-next-item`, `entry-point-accepts-list-argument`.

**Step 2: Adapt behavior tests that used deleted helpers.** Where a test called
`--start-session` + `--display-current-item` for setup, replace with the public
`org-gtd-reflect-someday-review` (which now starts + renders). For example:

```elisp
(deftest someday-review/creates-wip-buffer-with-review-mode ()
  "The walk shows the current item in a read-only review-mode WIP buffer."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Review me"))
  (org-gtd-reflect-someday-review)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))))
  (org-gtd-someday-review-quit))
```

Apply the same pattern to `shows-keybindings-in-header-line`,
`defer-adds-logbook-entry`, `defer-ends-session-when-done` (assert via
`org-gtd-wip--get-buffers` emptying / summary message rather than
`--session-active`), `clarify-increments-clarified-count`, `quit-ends-session`
(assert the WIP buffer is gone rather than `--session-active`), and
`entry-point-starts-session` / `entry-point-shows-message-when-no-items` (assert
buffer presence/absence rather than `--session-active`).

*Guidance for the executor:* prefer asserting **observable** facts — the WIP
buffer exists / is gone (`org-gtd-wip--get-buffers`), the source item gained a
`:LOGBOOK:` "Reviewed" line, the header-line content — over private session
vars. `--session-active` no longer exists; use buffer presence as the proxy.

**Step 3: Add two thin Tier-3 adapter tests** (the "does the adapter wire
correctly" replacements for the deleted mechanics):

```elisp
(deftest someday-review/find-returns-only-someday-ids ()
  "The adapter :find yields ids for someday items and nothing else."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "One")
    (org-gtd-someday-create "Two"))
  (assert-equal 2 (length (funcall (org-gtd-someday-review--make-find nil)))))

(deftest someday-review/defer-logs-review-then-advances ()
  "defer writes a Reviewed logbook line on the source item and moves on."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "First")
    (org-gtd-someday-create "Second"))
  (org-gtd-reflect-someday-review)
  (let* ((surface (car (org-gtd-wip--get-buffers)))
         (id (with-current-buffer surface
               (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))))
    (with-current-buffer surface (org-gtd-someday-review-defer))
    ;; still walking (a second item remains) and the first item got its log line
    (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
    (let ((marker (org-id-find id 'marker)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker) (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t))))
          (assert-match ":LOGBOOK:" (buffer-substring (point) end))))))
  (org-gtd-someday-review-quit))
```

**Step 4: Run the full someday-review file green.**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected: all green. Debug any red with superpowers:systematic-debugging.

**Step 5: Commit.**

```bash
git add test/unit/someday-review-test.el
git commit -m "test: thin someday-review tests to the walk adapter

Delete tests of the deleted queue/cursor internals (now owned by the engine and
covered by walk-driver-test), re-express behavior tests via the public entry
path, add thin Tier-3 adapter tests.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Full-suite regression check

**Step 1: Compile clean.**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: clean.

**Step 2: Run the unit category.**

Run: `.claude/skills/test/run-tests.sh unit`
Expected: green. Pay attention to `someday-dispatcher-test.el` and the
`walk-*-test.el` engine tests — none should regress.

**Step 3: Run the whole suite.**

Run: `.claude/skills/test/run-tests.sh all`
Expected: green. If a flake appears, re-run with the printed `--seed=N` to
distinguish a real regression from ordering noise (see MEMORY: flakes are
usually setup issues — check `around-each` / `ogt-eunit-with-mock-gtd` and
`default-directory` leakage, and that the surface buffer + scope lock are torn
down on every exit path).

**Step 4: Final verification (superpowers:verification-before-completion).**
Confirm: (a) `~/bin/eldev compile --warnings-as-errors` clean; (b) `all` green
with its seed recorded; (c) `org-gtd-walks` contains `someday-review`; (d) a
manual smoke check that `M-x org-gtd-reflect-someday-review` opens the review
buffer, `d`/`c`/`q` behave, and the scope lock releases on finish and on quit
(no lingering entry in `org-gtd-walk--locked-scopes`).

**Step 5: Commit any final fixes**, then the branch `walk-engine-phase-1` is
ready to merge back to `org-gtd-5` green.

---

## Notes for the executor

- **Do not run `eldev etest` directly** — always the `/test` skill script.
- **`org-gtd--without-kill-merge`** (org-gtd-core) must wrap every subtree
  copy/cut in the render path (MEMORY + Phase-0 convention); the archive and
  organize modules are the reference call sites.
- **Scope lock hygiene:** every exit path (finish, quit, start-failure) must
  release the scope. The engine handles finish/quit/start-failure; someday's
  commands only add org effects and cleanup around the engine transitions —
  never bypass them.
- **on-finish vs quit:** finish is engine-driven (`--settle` → `walk-finish`);
  quit is command-driven (summary + cleanup, then `walk-quit`). Both must leave
  zero WIP buffers and an unlocked scope.
- Reference the design record throughout: `docs/plans/2026-07-17-walk-engine-design.md`
  §4–§9 (contract) and §12 (Phase 1).
