# Walk Engine Phase 4 — Inbox Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.
> Tests run ONLY via the `test` skill (Skill tool), never `eldev etest` directly. Compile with `~/bin/eldev compile --warnings-as-errors`. New files carry `Copyright © 2026 Aldric Giacomoni`. Commit trailer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.

**Goal:** Reimplement inbox processing (`org-gtd-process.el` + the queue/continuation machinery in `org-gtd-clarify.el` and `org-gtd-organize-core.el`) as a consumer of the generic walk engine, collapsing the three stacked ad-hoc queues (pending-inboxes, per-inbox iteration, duplicate-queue) into one walk model, with no user-visible change to the processing UX.

**Architecture:** Register an `inbox` walk spec. `:find` scans the main inbox + `org-gtd-additional-inbox-files` (multi-source) and returns a list of **synthetic string tokens**, seeding the model's `:meta` with token→marker for each inbox heading. `:render` resolves the current token → marker (or, for a duplicate token, → `(:title :content)`) and sets up the editable clarify WIP surface. The organize transient stays exactly as-is; the async seam is `org-gtd-organize--call`, whose post-organize continuation becomes `org-gtd-walk-advance` (gated on `org-gtd-walk--active`). The duplicate-queue becomes model `:meta` entries enqueued via direct `org-gtd-walk-model-enqueue` + one advance. Resume is **deferred** (`:resumable nil` this phase), which is what lets `:meta` hold live markers (the model is never serialized).

**Tech Stack:** Emacs Lisp, org-mode, transient.el, e-unit test framework, the walk engine already on `org-gtd-5` (`org-gtd-walk.el`, `org-gtd-walk-model.el`).

---

## ✅ Decisions — RULED (2026-07-17)

All six gating decisions are ruled. Summary, then detail below.

| # | Ruling |
| --- | --- |
| **D1** | **Confirmed.** Discriminate organize→advance on `org-gtd-walk--active`; advance strictly in the no-error branch. |
| **D2** | **Changed — token + `:meta`-marker model** (both original options rejected; see D2). ALL handles are synthetic string tokens; `:meta` maps token → live **marker** (inbox item) or `(:title :content)` (duplicate). No serialization (resume deferred). Lazy id semantics preserved. |
| **D3** | **D3a confirmed.** One reused editable clarify surface (erase/refill + rekey/rename per item). |
| **D4** | **D4a + D4c confirmed.** Synthetic-token duplicate handles, content in `:meta`; enqueue by mutating the model then one advance; position from `org-gtd-clarify-duplicate-queue-position`. |
| **D5** | **D5b — DEFER resume.** Ship `:resumable nil` parity. **Task 11 dropped** (separate follow-up branch). Task 12 carries no resume/`:resolve` tests. |
| **D6** | **D6a confirmed.** Multi-source `:find` over `(cons (org-gtd-inbox-path) org-gtd-additional-inbox-files)`; **keep** the `*Org GTD Duplicate Queue*` side-window and kill-buffer/kill-emacs "save to inbox" safety, re-backed off the walk model. |

### D1 — Async organize→advance rewiring (the crux) — RULED: confirmed

`org-gtd-organize--call` (in `org-gtd-organize-core.el`) is the single completion point for *every* organize action, and it is used in **two** modes:

- **Inbox / walk-driven clarify**: today it ends by calling `org-gtd-clarify--continuation` (= `#'org-gtd-process-inbox`, which recurses to the next item).
- **One-off clarify** (`org-gtd-clarify-item` from an agenda line, `C-u` skip-refile, project reclassify): `continuation` is nil; it just restores the window config and cleans up.

The rewire replaces the `(when continuation (funcall continuation))` branch with `(org-gtd-walk-advance)` — **but only on the walk-driven path**. The one-off path (no active walk) must be untouched.

- **Detection mechanism (decide):** How does `org-gtd-organize--call` know it is inside a walk? Options: (a) check `org-gtd-walk--active` in the surface buffer; (b) keep a boolean/marker buffer-local (`org-gtd-clarify--walk-p`) set by render. Recommendation: **(a)** — the surface buffer is where `org-gtd-walk--active` lives and where organize runs, so `(and org-gtd-walk--active t)` is the discriminator, and `org-gtd-clarify--continuation` is deleted.
- **Error path parity:** organize wraps the real work in `(catch 'org-gtd-error …)`; on a thrown `org-gtd-error` (e.g. `processing/rejects-project-with-no-tasks`) it must **not** advance — the walk stays on the current item (design §9). The advance call belongs strictly in the no-error branch, mirroring today's continuation placement. Confirm this stays true.
- **Risk:** `org-gtd-walk-advance` re-renders in the surface buffer via `org-gtd-walk--settle`. After organize has refiled/cut the current item, advance moves the cursor and render draws the *next* item — good. But if the surface buffer was killed/replaced by organize, advance runs against a dead buffer. The reused-single-surface model (D3) is what keeps the surface alive across advance; this decision is coupled to D3.

**Rule on:** detection mechanism (a) vs (b); confirm advance lives only in the no-error branch.

### D2 — Handle representation — RULED: token + `:meta`-marker model (both original options rejected)

Both originally-tabled options are wrong for the parity phase and are **rejected**:

- **D2b (`"file::pos"` position handles) is broken, not merely fragile.** Inbox items are cut/removed as they are organized *during* the walk, so a character position captured at `:find` time points at the wrong heading (or into empty space) once earlier items are gone. Rejected.
- **D2a (eager `org-id-get-create` at find) breaks lazy-id parity.** It stamps an `:ID:` into every inbox heading the instant you start processing — including items you *skip* and never organize — which contradicts "no behavior change" (today an unprocessed/skipped inbox item has no id). Rejected for the parity phase.

**Ruled model — unify handles with D4a:** ALL inbox handles are **synthetic string tokens**. The walk model's `:meta` maps each token to either:
- a live **marker** — for an original inbox heading (created at `:find` time), or
- a `(:title :content)` plist — for a duplicate (created at enqueue time, D4a).

`:find` scans the multi-source inbox and, per heading, creates a token and stores its marker in `:meta` (it does **not** assign any id). `:render` resolves the current token → marker via `:meta` and sets up the clarify surface; the org-id is assigned **lazily at render** exactly as today (`org-gtd-id-get-create` on the source marker, `org-gtd-clarify.el:254`), so a skipped/never-reached item is never stamped. Organize cuts the subtree at that marker (via `org-gtd-clarify--source-heading-marker`).

**Why markers, not ids or positions:** Emacs markers move with buffer edits, so they stay pointing at the correct heading across the in-session cuts that break positions — and because **resume is deferred (D5b) we never serialize the model**, so the handle-must-be-scalar constraint (`walk-model-valid-p`) never applies to a live inbox walk. (`:meta` holding markers is never round-tripped through `prin1`.) This preserves today's exact lazy-id semantics and is uniform with duplicate handling.

**Durability caveat (flagged):** a marker dies if its inbox **buffer** is killed mid-session. Today's recursive re-find-from-file was immune to that; the marker model is not. Mitigation: the walk holds the inbox buffers open (`find-file-noselect`) for the session, and `:render` defensively checks `(marker-buffer m)` / `(marker-position m)` before use — a dead marker auto-skips (advance) rather than erroring. See end-to-end coherence note at the bottom of this section list.

**Rule on:** none — ruled. Update D2, Task 1 (find → tokens + `:meta` markers), Task 2, and Task 3 accordingly.

### D3 — Editable clarify surface vs the engine's one-surface render model — RULED: D3a

someday-review and the checklist walk render **read-only** displays into a single reused surface (erase + refill). Inbox is different: the clarify buffer is **editable** (the user edits before organizing) and is temp-file-backed for crash protection, and today each item gets its **own** WIP buffer keyed by clarify-id (`org-gtd-wip--get-buffer`).

- **Option D3a — one reused surface (engine-native):** a single clarify surface buffer, erased and refilled per item, `org-gtd-walk--active` buffer-local on it, rekeyed (`org-gtd-wip--rekey`) + renamed per item so the buffer name still carries the item slug and each item still gets a fresh temp file. This is exactly what today's duplicate-queue path already does (`org-gtd-clarify--process-next-queued-item` reuses one buffer and rekeys) — so it is proven behavior, just extended to *all* items instead of only duplicates. Tests key off `ogt-get-wip-buffer` (any buffer matching the prefix) and buffer contents, so one surface satisfies them, including the "readable buffer name"/"fresh id" duplicate tests.
- **Option D3b — per-item WIP buffers (status quo):** keep spawning a buffer per item; the walk surface is "whichever buffer is current." This fights the engine (one `org-gtd-walk--active` per surface buffer; advancing would need to move the session between buffers) and re-introduces the multi-buffer lifecycle the engine exists to kill.

**Ruled: D3a.** It is the only option that fits `org-gtd-walk--active`'s one-surface model, and it matches the already-shipping duplicate-reuse mechanic. Accepted behavior change: only one clarify buffer exists at a time (today, too, only one is normally open; multi-buffer clarify is an edge the kill-safety code handles — see D6).

### D4 — Duplicate-queue → `walk-enqueue`, and the heterogeneous-handle problem — RULED: D4a + D4c

Duplicates are **not** file headings: `org-gtd-clarify-duplicate` builds an in-memory plist `(:title :content)` and appends it to the buffer-local `org-gtd-clarify--duplicate-queue`. They have no org-id and no source marker (they refile normally with `--source-heading-marker nil`). Two problems mapping this onto the walk:

1. **Handle representation.** The model rejects non-scalar handles (`walk-valid-p` requires string/symbol/number), so a duplicate's `(:content …)` plist cannot be an entry directly. Options:
   - **D4a — content in `meta`:** the entry handle is a synthetic string id (e.g. `(org-id-new)` or a `dup-N` token); the duplicate's content string is stored in the model's `:meta` plist keyed by that token. `:render` checks meta first (insert content, generate fresh id, no source marker), else resolves the handle as an org-id (inbox item). Serializable (string handle + string content in meta) → survives checkpoint. **Recommended.**
   - **D4b — spill duplicates to a holding file:** write each duplicate as a real heading with an id into a scratch org file included in the walk scope. Heavier, touches disk, but makes duplicates first-class ids. Rejected unless D4a proves unworkable.
2. **Enqueue timing + the render-of-current wrinkle.** Enqueue happens **mid-organize**, before the advance. The engine's public `org-gtd-walk-enqueue` inserts after the cursor **and re-renders the current item**. But by the time we enqueue (inside `org-gtd-organize--call`, after refiling/cutting), the current item's source is gone — re-rendering it would fail. So we must **not** use the rendering `walk-enqueue` here. Options:
   - **D4c — model-level enqueue then single advance:** during organize, mutate the active model directly (`org-gtd-walk-model-enqueue` on `(plist-get org-gtd-walk--active :model)`, once per queued duplicate, at the position mapped from `org-gtd-clarify-duplicate-queue-position`), then call `org-gtd-walk-advance` once (which renders the next item). No intermediate render. **Recommended.**
   - **D4d — add a non-rendering transition to the engine** (`org-gtd-walk-enqueue-quiet`). Only if D4c's direct model mutation is considered too intimate with engine internals. Note: `org-gtd-clarify-duplicate` (invoked while the user is still editing, *before* organize) DOES want a visible update — but it updates the *side-window duplicate display* (D6), not the walk render, so it can call model-enqueue + refresh-display without an advance.

**Ruled: D4a + D4c.** Position mapping: `top`/`bottom` from `org-gtd-clarify-duplicate-queue-position` pass straight through to `org-gtd-walk-model-enqueue`'s `where` (design §4 says they map directly). Note D4a is now the *general* handle model (D2), not a special case — inbox items carry markers in `:meta`, duplicates carry `(:title :content)`; render distinguishes by which meta shape the token maps to.

### D5 — Is `:resumable t` in scope, or deferred? — RULED: D5b (DEFER)

Design §12 lists "inbox becomes resumable" as a Phase 4 deliverable, but it is genuinely subtle:

- The checkpoint stores only the model (ids + cursor + meta). An item **mid-clarify with unsaved WIP edits** is not in the checkpoint — on resume, render re-copies the *pristine* inbox heading, silently discarding those edits. (The item is not lost — it is still in the inbox — but in-progress clarification work is.)
- Requires D2a (eager, real ids) so handles are stable and re-resolvable. With D2b (position handles) resume is unsafe.
- Needs `:resolve` (stale-handle skip) for items cut/removed since the checkpoint, and the "N no longer present" report (design §9).
- Concurrency/parity: today processing is purely in-memory; a crash loses session position entirely. Resume is strictly *new capability*, not parity — so it cannot break any existing test, but it must be justified as desirable.

**Options:**
- **D5a — ship resumable now** (with D2a): add `:resumable t`, `:resolve`, Tier-3 resume + stale-skip tests, and document the "unsaved WIP edits are lost on resume" caveat.
- **D5b — defer resume** (`:resumable nil` for now): land the structural migration first (three queues → one model), ship resume as a fast follow once the migration is proven. Keeps this already-huge task focused; matches the "characterize first, no behavior change" discipline (resume is behavior change).

**Ruled: D5b (DEFER).** Ship `:resumable nil` (parity). **Task 11 is dropped from this plan** (resume ships on a separate follow-up branch); Task 12 carries no resume/`:resolve` tests. This is what makes the D2 marker model sound: because the model is never serialized, `:meta` can hold live markers. When resume is picked up later, that branch must revisit the handle model (markers can't be serialized — it will need id-or-`:resolve` handles and must re-confront the "unsaved WIP edits lost on resume" caveat).

### D6 — `--pending-inboxes` multi-file iteration → single multi-source `:find`, and the duplicate side-window / kill-safety fate — RULED: D6a

- **Multi-file collapse:** `org-gtd-process--pending-inboxes` + `--next-inbox` + `--try-next-inbox` (iterate files, find first heading each) collapse into one `:find` that scans `(cons (org-gtd-inbox-path) org-gtd-additional-inbox-files)`, skipping missing/empty files, collecting all headings in file-then-document order. `:scope` = that file list (locks them all together). The "All inboxes are empty" message becomes the empty-find path (design §9). This is mechanical and low-risk. **No decision needed beyond confirming file ordering (main first, additional in listed order) is preserved.**
- **Duplicate side-window (`*Org GTD Duplicate Queue*`) + kill-safety:** a real decision. Today `org-gtd-clarify--duplicate-queue` (buffer-local list) backs (i) the side-window display of pending duplicates, and (ii) the kill-buffer / kill-emacs "you have N pending duplicates, save to inbox?" safety prompts. Once duplicates become walk entries (D4a), that buffer-local list is gone. Options:
  - **D6a — drive the side-window off the walk model:** the "pending duplicates" display reads the remaining duplicate entries from the walk model's remaining-queue + meta. Kill-safety reads the same. Preserves the exact UX. More rework.
  - **D6b — drop the duplicate side-window and kill-safety prompts:** duplicates that are already walk entries are, if `:resumable t`, recoverable from checkpoint (so the "save to inbox on exit" prompt becomes moot). If `:resumable nil` (D5b), abandoning the walk with pending duplicates loses them unless we keep *some* safety. Risky to drop with D5b.
  - **D6c — minimal: keep a thin display of remaining duplicate entries, drop the disk-spill safety** if resume covers it.

**Ruled: D6a** (consistent with D5b). File ordering confirmed: main inbox first, then `org-gtd-additional-inbox-files` in listed order. **Keep** the `*Org GTD Duplicate Queue*` side-window and the kill-buffer / kill-emacs "save to inbox" safety, re-backed off the walk model's remaining duplicate entries (`:meta` `(:title :content)` shapes) — not simplified away, so no duplicate is silently lost on quit while resume is absent.

---

## Characterization contract (the safety net)

These tests encode the behavior that MUST stay green through the migration (rewrite only the ones asserting deleted internals, and only at cutover — Task 10):

| File | What it pins |
| --- | --- |
| `test/unit/processing-test.el` | full 7-item process loop; decorations; agenda display; valid/invalid project (error → stay in WIP, no advance) |
| `test/unit/clarify-test.el` | source-marker, skip-refile, duplicate add/rename/exact, queue processing (organize processes queue before continuation), fresh-id + readable-name on reuse, stop-with-queue skips to next, kill-buffer/kill-emacs safety |
| `test/acceptance/additional-inboxes-test.el` | additional inbox after main; skip empty/missing; multiple in order; **session-state-on-cancel (asserts deleted internals → rewrite at cutover)** |
| `test/integration/end-to-end-test.el` | multi-file project processed via inbox |
| `test/unit/true-end-to-end-test.el` | keymap wiring (`C-c c` → organize), command availability |
| `test/unit/organizing-test.el`, `organize-core-test.el`, `project-clarify-test.el` | organize pipeline invariants (must be untouched by the rewire) |

**Internals that will be deleted (their assertions get rewritten):** `org-gtd-process--pending-inboxes`, `org-gtd-process--session-active`, `org-gtd-clarify--continuation`, `org-gtd-clarify--duplicate-queue` (as a buffer-local list) and its `--queue-add/--queue-pop/--queue-display/--queue-empty-p` helpers (reshaped, see D6).

---

## Task 0: Baseline — capture the green safety net

**Files:** none (verification only).

**Step 1:** Run the full characterization subset via the `test` skill:
- `test/unit/processing-test.el`, `test/unit/clarify-test.el`, `test/acceptance/additional-inboxes-test.el`, `test/integration/end-to-end-test.el`, `test/unit/true-end-to-end-test.el`, `test/unit/organizing-test.el`, `test/unit/organize-core-test.el`, `test/unit/project-clarify-test.el`.

**Step 2:** Record the pass counts and any seed. This is the contract; every task re-runs this subset and keeps it green (except the internals-asserting tests, which change only at Task 10).

**Step 3:** `~/bin/eldev compile --warnings-as-errors` — confirm a clean baseline.

**Step 4:** Commit nothing (baseline only). Note the numbers in the branch's working notes.

---

## Task 1: Inbox `:find` — multi-source scan → tokens + `:meta` markers (Tier-3)

**Files:**
- Create: `org-gtd-inbox-walk.el` (new module; the inbox consumer adapter, mirroring `org-gtd-someday-review.el`'s structure).
- Test: `test/unit/inbox-walk-test.el` (new).

**Design (D2 ruled):** `:find` does not return a bare list — it must both list the tokens (the model's `entries`) **and** seed `:meta` with token→marker. Since `org-gtd-walk-model-create` takes `(entries &optional meta)`, implement `:find` as the list of tokens and provide a separate `org-gtd-inbox-walk--build-model` (or a `:find` that closes over a freshly-built meta table) that the entry command uses to construct the model with meta. Simplest concrete shape: a builder `org-gtd-inbox-walk--scan` returns `(cons tokens meta-plist)`; the entry command (Task 5) calls `org-gtd-walk-model-create` with both. Keep `:find` itself a nullary fn returning the token list, and stash the meta on the model at start. (Finalize the exact seam in Task 5; Task 1 delivers the scan + token/marker construction.)

**Step 1 (failing test):** In `inbox-walk-test.el`, under `ogt-eunit-with-mock-gtd`, capture two items in the main inbox and one in an additional inbox file (mirror `additional-inboxes-test.el` setup). Assert `(org-gtd-inbox-walk--scan …)` returns 3 tokens (main-inbox items first, then the additional-inbox item), each token a string; and that each token maps (in the returned meta) to a **live marker** whose heading text matches the captured item. Assert every token satisfies `org-gtd-walk-model--handle-serializable-p` (string), and that the constructed model passes `org-gtd-walk-model-valid-p` (meta-with-markers does not break validity — validity only inspects `entries` + `cursor`).

**Step 2:** Run it — FAIL (function undefined).

**Step 3 (implement):** Write the scan: build the file list `(cons (org-gtd-inbox-path) org-gtd-additional-inbox-files)`, skip non-existent/empty files (`file-exists-p`), scan each with the `org-gtd-someday-review--find-items` pattern (`with-current-buffer (find-file-noselect …)`, `org-with-wide-buffer`, `re-search-forward "^\\*+ "`). Per heading: mint a token (`org-gtd-inbox-walk--token`, e.g. `(format "inbox-%s" (org-id-uuid))` — any unique string; it is never persisted), and store `(cons token (point-marker))` into the meta's token→marker map. **Do NOT call `org-gtd-id-get-create`** — no id is assigned at find (lazy-id parity, D2). Keep the source inbox buffers alive (they hold the markers). Return tokens in scan order + the meta.

**Step 4:** Run it — PASS.

**Step 5:** Add tests for skip-empty-file and skip-missing-file (scan returns only present items) and multi-file ordering (main first). Run — PASS.

**Step 6:** Commit: `feat: inbox walk scan → tokens + :meta markers (multi-source)`.

> Marker-durability note (from D2): a test should assert that after cutting the *first* item's heading from the inbox buffer, the *second* token's marker still resolves to the correct heading — the property that makes markers correct where positions are broken.

---

## Task 2: Uniform `:meta` accessors — marker entries and duplicate entries (D2 + D4a)

**Files:**
- Modify: `org-gtd-inbox-walk.el`.
- Test: `test/unit/inbox-walk-test.el`.

**Design:** One meta store, two value shapes under a token: a **marker** (original inbox item) or a `(:title :content)` plist (duplicate). `:render` (Task 3) dispatches on the shape. Provide accessors so both find (Task 1) and enqueue (Task 6) write through the same path.

**Step 1 (failing test):** Assert `org-gtd-inbox-walk--token` returns a fresh unique string; `org-gtd-inbox-walk--meta-put-marker` / `--meta-put-dup` store under a token in a model's `:meta`; `--meta-get` returns the stored value; `--meta-dup-p` distinguishes a `(:title :content)` value from a marker. Assert a model carrying marker + dup meta still passes `org-gtd-walk-model-valid-p` (validity ignores meta). **No serialization round-trip** (resume deferred; markers are intentionally non-serializable).

**Step 2:** Run — FAIL.

**Step 3 (implement):** Add the token generator + meta accessors (put-marker, put-dup, get, dup-p) operating on the model's `:meta` plist keyed by token string.

**Step 4:** Run — PASS.

**Step 5:** Commit: `feat: uniform :meta accessors for inbox-item and duplicate tokens`.

---

## Task 3: Inbox `:render` — editable clarify surface (D3a)

**Files:**
- Modify: `org-gtd-inbox-walk.el`.
- Test: `test/unit/inbox-walk-test.el`.

**Step 1 (failing test):** With one captured inbox item, create the single clarify surface (`org-gtd-inbox-walk--surface`), build a model over the scan (Task 1), call `org-gtd-inbox-walk--render (current-token) surface` with the model current. Assert: surface buffer contains the item heading; is in `org-gtd-clarify-mode`; `org-gtd-clarify--source-heading-marker` points at the real inbox heading (the marker from `:meta`); `org-gtd-clarify--clarify-id` is set (id assigned **now**, lazily, via `org-gtd-id-get-create` on the source marker); state properties (timestamp/who/style/project) are stripped (reuse `org-gtd-clarify--initialize-buffer-contents`'s stripping).

**Step 2:** Run — FAIL.

**Step 3 (implement):** Write `org-gtd-inbox-walk--render (token surface)`. Resolve `token` → meta value via `org-gtd-inbox-walk--meta-get` on the active model. Bind `org-id-track-globally nil` and wrap subtree copy in `org-gtd--without-kill-merge` (engine gotcha — renders that paste org subtrees must do both, see someday render).
- **Duplicate token (`--meta-dup-p`):** `inhibit-read-only`, erase surface, insert `:content`, generate a fresh id (`org-gtd-id-get-create`), rekey the surface (`org-gtd-wip--rekey`) + rename, set `--source-heading-marker nil`, `--skip-refile nil`.
- **Marker token (inbox item):** the value is a marker. **Defensive durability check (D2):** if `(not (and (markerp m) (marker-buffer m) (marker-position m)))` the source is gone (buffer killed / already cut) → skip via `org-gtd-walk-advance` and return (auto-skip, no error). Otherwise: erase surface; copy the source subtree in and strip state props (reuse `org-gtd-clarify--initialize-buffer-contents` logic); set `--source-heading-marker` to that marker; assign the clarify-id lazily now (`org-gtd-id-get-create` on the source marker, as `org-gtd-clarify.el:254` does today) and rekey/rename the surface to it.
- Ensure `org-gtd-clarify-mode` is active; set header-line to the existing clarify header.
- `org-gtd-clarify-setup-windows` / `pop-to-buffer` as today.

**Step 4:** Run — PASS. Add a second test for the duplicate-token branch (render inserts content, assigns fresh readable id, no source marker) — mirrors `clarify/queued-duplicate-gets-fresh-id`. Add a third: a marker token whose buffer was killed auto-skips instead of erroring (durability guard).

**Step 5:** Commit: `feat: inbox walk :render into single clarify surface`.

---

## Task 4: The async organize→advance seam (D1) — the crux

**Files:**
- Modify: `org-gtd-organize-core.el` (`org-gtd-organize--call`).
- Modify: `org-gtd-clarify.el` (mark the walk-driven path; remove reliance on `--continuation` on this path).
- Test: `test/unit/inbox-walk-test.el`.

**Step 1 (failing test):** Start an inbox walk over one item (via the Task 5 entry once it exists — for now drive the model+surface directly), then invoke `organize-as-single-action` in the surface. Assert: the item is refiled/cut, and because the walk had one entry, the walk finishes (`org-gtd-walk--active` nil, scope unlocked). Add a companion test: a two-item walk, organize the first, assert render now shows the second (advance happened).

**Step 2:** Run — FAIL (advance not wired).

**Step 3 (implement):** In `org-gtd-organize--call`, in the no-error branch:
- Replace `(when continuation (funcall continuation))` with: if `org-gtd-walk--active` (D1a discriminator) → call the walk transition (see Task 6 for the duplicate-enqueue-then-advance form); else → the existing one-off cleanup (window restore, horizons cleanup). Keep the error branch (`catch 'org-gtd-error`) exactly as-is so an organize error does **not** advance.
- Keep source-cut, `org-gtd-save-buffers`, and temp-file cleanup semantics. Under D3a the surface is reused, so do **not** kill the surface buffer on advance (advance re-renders into it); only finish/quit tears it down.

**Step 4:** Run — PASS. Re-run `organize-core-test.el` + `organizing-test.el` (one-off path must be unaffected) — PASS.

**Step 5:** Commit: `feat: organize completion advances the inbox walk`.

---

## Task 5: Inbox walk spec + `org-gtd-process-inbox` entry

**Files:**
- Modify: `org-gtd-inbox-walk.el` (spec, register, `:on-finish`, `:scope`).
- Modify: `org-gtd-process.el` (rewrite `org-gtd-process-inbox` to drive `org-gtd-walk-start`; delete `--pending-inboxes`/`--session-active`/`--next-inbox`/`--try-next-inbox`/`--stop` — **staged**, see Task 10 for full removal; here, reimplement the entry).
- Test: `test/acceptance/additional-inboxes-test.el`, `test/unit/processing-test.el` (kept green).

**Step 1 (failing test):** Rely on the existing `processing/organizes-all-items-leaving-inbox-empty` and `additional-inboxes/*` as the drivers.

**Step 2 (implement):**
- **Model-with-meta seam (from Task 1):** `org-gtd-walk-start` builds its model from `:find` alone (`(org-gtd-walk-model-create (funcall find))`) and cannot seed `:meta`. Two clean options — pick one during impl: **(i)** make `:find` a closure that runs the scan, stashes the resulting token→marker meta in a lexical it also injects — i.e. after `walk-start` returns, the entry command sets `(plist-put (plist-get org-gtd-walk--active :model) :meta <meta>)` in the surface buffer and re-renders; or **(ii, preferred)** the entry command itself builds the full model (`org-gtd-walk-model-create tokens meta`) and calls a thin `org-gtd-walk-start`-with-model path. If the engine only accepts `:find`, prefer (i) but keep it in *one* place (the entry command), documented. Flag: this is the one spot the engine's `find→model` assumption meets inbox's need to seed meta — keep the workaround minimal and localized, do not change the engine in this phase.
- `org-gtd-inbox-walk--spec`: `:name 'inbox`, `:find` = the token-scan closure, `:render #'org-gtd-inbox-walk--render`, `:actions` (organize is invoked from the clarify keymap's `C-c c` → `org-gtd-organize`; the walk's `:actions` keymap can be `org-gtd-clarify-mode-map` or nil since organize routes through the shared seam — decide during impl, but the transient invocation path is unchanged), `:on-finish` = report + `whitespace-cleanup` + `org-gtd-clarify--cleanup-horizons-view` + `org-gtd-save-buffers` (fold in today's `org-gtd-process--stop`), **`:resumable nil` (D5b — resume deferred)**, `:scope` = the inbox file list.
- `org-gtd-walk-register 'inbox (org-gtd-inbox-walk--spec)`.
- Rewrite `org-gtd-process-inbox` to build the spec fresh (like `org-gtd-reflect-someday-review` does), compute the file list once for both scan/`:scope`, and start the walk against the single surface. Empty-find → the engine finishes immediately; preserve the "All inboxes are empty. No items to process." message (emit from the entry when the scan is empty, matching design §9).

**Step 3:** Run `processing-test.el` + `additional-inboxes/processes-*` + `end-to-end-test.el` — PASS. (`additional-inboxes/clears-session-state-on-cancel` still asserts deleted internals — expect it RED here; it is rewritten in Task 10.)

**Step 4:** Compile `--warnings-as-errors`.

**Step 5:** Commit: `feat: register inbox walk; drive org-gtd-process-inbox through the engine`.

---

## Task 6: Duplicate commands → walk-enqueue (D4c)

**Files:**
- Modify: `org-gtd-clarify.el` (`org-gtd-clarify-duplicate`, `org-gtd-clarify-duplicate-exact`), `org-gtd-organize-core.el` (enqueue-then-advance in the walk branch).
- Test: `test/unit/clarify-test.el` (the duplicate-workflow tests are the contract).

**Step 1 (failing test):** Keep `clarify/duplicate-exact-adds-to-queue`, `clarify/organize-processes-queue-before-continuation`, `clarify/duplicate-full-workflow`, `clarify/stop-with-queue-skips-to-next`, `clarify/queued-duplicate-gets-fresh-id`, `clarify/queued-duplicate-has-readable-buffer-name` as drivers. The ones asserting the raw `--duplicate-queue` list length get reshaped to assert the walk model's remaining count (Task 10).

**Step 2 (implement):**
- `org-gtd-clarify-duplicate[-exact]`: instead of `org-gtd-clarify--queue-add`, store content in the active model's meta (`--meta-put-dup`, D4a) and `org-gtd-walk-model-enqueue` the token at `org-gtd-clarify-duplicate-queue-position` (D4c, model-level, no render), then refresh the pending-duplicates side-window display (D6a — kept, model-backed). This runs while the user is still editing, *before* organize, so it enqueues without advancing (the advance comes from organize later).
- In `org-gtd-organize--call`'s walk branch: the pre-organize duplicate collection is gone (duplicates already live in the model). Post-organize, just `org-gtd-walk-advance` — the model already carries the enqueued duplicates in the remaining queue, so advance lands on the first `top` duplicate (or the next inbox item for `bottom`), exactly reproducing the old queue semantics.

**Step 3:** Run the duplicate-workflow tests — PASS. Verify position mapping with a `top`-position test (duplicate handled next) and a `bottom`-position test (duplicate handled last).

**Step 4:** Commit: `feat: inbox duplicates enqueue into the walk model`.

---

## Task 7: `org-gtd-clarify-stop` / quit → `org-gtd-walk-quit` (or skip-current)

**Files:**
- Modify: `org-gtd-clarify.el` (`org-gtd-clarify-stop`).
- Test: `test/unit/clarify-test.el` (`stop-with-queue-skips-to-next`, `stop-without-queue-restores-window-config`, `stop-continues-queue-with-next-item`).

**Step 1 (failing test):** These existing tests are the contract: stop with remaining entries skips the current item and continues to the next; stop with none tears down and restores the window config.

**Step 2 (implement):** `org-gtd-clarify-stop` on the walk path:
- If the walk has remaining entries beyond the current: `org-gtd-walk-advance` (skip current, render next) — mirrors "discard current, process next queued item."
- If none remain: `org-gtd-walk-quit` (tear down, unlock — no checkpoint exists, resume deferred), clean up the surface + horizons, restore window config, message "Stopped clarifying."
- Delete the old `--session-active`/`--pending-inboxes` reset (those vars are gone in Task 10).

**Step 3:** Run the stop tests — PASS.

**Step 4:** Commit: `feat: inbox clarify-stop maps to walk skip/quit`.

---

## Task 8: Kill-safety + duplicate side-window fate (D6)

**Files:**
- Modify: `org-gtd-clarify.el` (`--queue-display`, `--queue-cleanup`, `--kill-buffer-query`, `--kill-emacs-query`, `--queue-save-to-inbox`, `--pending-duplicates-all-buffers`).
- Test: `test/unit/clarify-test.el` (kill-buffer/kill-emacs/side-window tests).

**Step 1 (D6a ruled):** Reimplement the pending-duplicates display + kill-safety to read the remaining **duplicate** entries from the walk model — the tokens at/after the cursor whose `:meta` value is a `(:title :content)` shape (`--meta-dup-p`) — instead of the deleted buffer-local `org-gtd-clarify--duplicate-queue`. `*Org GTD Duplicate Queue*` still lists them at `org-gtd-clarify-duplicate-queue-position`; `--queue-save-to-inbox` still appends their `:content` to the inbox on kill-buffer/kill-emacs "save". Keep the tests, adapted to seed the model rather than the raw list.

**Step 2:** Implement; run the affected clarify-test subset (`queue-display-*`, `kill-buffer-query-*`, `kill-emacs-*`, `pending-duplicates-all-buffers`) — PASS.

**Step 3:** Commit: `refactor: duplicate side-window + kill-safety off the walk model`.

---

## Task 9: `org-gtd-clarify-inbox-item` / `org-gtd-clarify-item` reconciliation

**Files:**
- Modify: `org-gtd-clarify.el` (delete `org-gtd-clarify-inbox-item`, `--inbox-p`, `--continuation`; keep `org-gtd-clarify-item` for the one-off/agenda path unchanged).
- Test: `test/unit/clarify-test.el` (agenda clarify tests — one-off path must stay green), `test/unit/true-end-to-end-test.el`.

**Step 1:** The one-off `org-gtd-clarify-item` (agenda line, skip-refile) path is NOT a walk — it must keep working exactly as today (no `org-gtd-walk--active`, so the D1 discriminator routes to the old cleanup). Confirm the agenda clarify tests (`clarify/agenda-converts-*`, `clarify/agenda-sets-skip-refile-*`) stay green.

**Step 2 (implement):** Remove `org-gtd-clarify-inbox-item` (the inbox entry now goes through the walk, not this shim) and the `--inbox-p`/`--continuation` buffer-locals and their references (transient `:if (not org-gtd-clarify--inbox-p)` gate — decide replacement: the skip-refile option was hidden during inbox processing; under the walk, gate on `org-gtd-walk--active` instead).

**Step 3:** Run clarify-test agenda subset + true-end-to-end — PASS.

**Step 4:** Commit: `refactor: drop inbox-item shim and continuation/inbox-p locals`.

---

## Task 10: Cutover — delete dead internals, rewrite internals-asserting tests

**Files:**
- Modify: `org-gtd-process.el` (delete `--pending-inboxes`, `--session-active`, `--next-inbox`, `--try-next-inbox`, `--stop`, the `defvar`s in `org-gtd-clarify.el` at lines 173–176).
- Modify: `org-gtd-clarify.el` (delete `--duplicate-queue` defvar + `--queue-add`/`--queue-pop`/`--queue-empty-p`, now superseded by the walk model + `:meta`; the D6a display/save helpers stay but read from the model, not the deleted list).
- Rewrite: `test/acceptance/additional-inboxes-test.el` (`clears-session-state-on-cancel`), and the `clarify-test.el` tests that poke `--duplicate-queue` directly (`duplicate-queue-variable-exists`, `queue-empty-p-*`, `queue-add-*`, `queue-pop-*`, `queue-display-*`, `queue-cleanup-*`).

**Step 1:** Rewrite `additional-inboxes/clears-session-state-on-cancel` to assert walk-level equivalents: after `org-gtd-clarify-stop` (quit), `org-gtd-walk--active` is nil and the inbox scope is unlocked (`org-gtd-walk--locked-scopes` does not contain the inbox scope key); re-running `org-gtd-process-inbox` re-finds all items.

**Step 2:** Reshape the raw-queue clarify tests to the new representation: seed a model with duplicate entries in meta and assert remaining-count / display / cleanup against the model-backed helpers (D6a). Preserve the *behavioral* duplicate tests untouched (`duplicate-full-workflow`, `stop-with-queue-skips-to-next`, etc. — those already pass via Tasks 6–7).

**Step 3:** Delete the dead functions/vars. Compile `--warnings-as-errors` (catches stragglers).

**Step 4:** Run the FULL characterization subset (Task 0 list) — all green.

**Step 5:** Commit: `refactor: remove pre-engine inbox queues; rewrite internals tests`.

---

## Task 11: Defer resume — file the follow-up (D5b)

**No code.** Resume (`:resumable t` + `:resolve` + the serializable-handle rework it forces) is out of scope for this branch (D5b).

**Step 1:** File a follow-up yak: `yx add "inbox walk resumable (Phase 4 follow-up)"`. In the yak note the two things the follow-up must confront: (a) markers in `:meta` cannot be serialized — resume needs id-or-position handles + a `:resolve` fn; (b) the "unsaved WIP edits lost on resume" caveat (an item mid-clarify is not in any checkpoint). No commit (tracking only).

---

## Task 12: Thin to Tier-3, final verification

**Files:** `test/unit/inbox-walk-test.el`, and prune redundant coverage now guaranteed by the engine (iteration/skip are trusted from Phases 0–2).

**Step 1:** Remove inbox-walk tests that merely re-prove engine mechanics (cursor advance, enqueue positions) — those live in the Tier-1/Tier-2 engine tests. Keep only inbox's own scan/`render`/`actions`/duplicate wiring, the marker-durability test, and the auto-skip-on-dead-marker test (Tier-3, design §10). **No resume / `:resolve` tests** (deferred).

**Step 2:** Run the FULL suite via the `test` skill (not just the subset) to catch cross-module regressions. Capture the seed.

**Step 3:** `~/bin/eldev compile --warnings-as-errors` and `~/bin/eldev lint --file="org-gtd-inbox-walk.el"`.

**Step 4:** Update `org-gtd.el` requires (load `org-gtd-inbox-walk`) and the `CHANGELOG.org`/design-doc Phase 4 status if appropriate (separate commit — generated/doc changes split from source per project convention).

**Step 5:** Commit: `test: thin inbox walk to Tier-3 adapter tests`.

---

## Cutover sequencing summary (why this order)

1. Tasks 1–3 build the new adapter pieces **in isolation** (new module, new tests) — zero risk to the live path.
2. Task 4 wires the async seam behind the `org-gtd-walk--active` discriminator — the one-off path is untouched because it has no active walk.
3. Task 5 flips `org-gtd-process-inbox` onto the engine while the behavior tests (not internals) stay green.
4. Tasks 6–9 move duplicates, stop, and kill-safety over, one behavior cluster at a time.
5. Task 10 is the only step that deletes internals and rewrites their tests — done last, atomically, after behavior is proven.
6. Task 11 files the deferred-resume follow-up (D5b); Task 12 thins the tests. Resume ships on a later branch.

The crux (Task 4 + Task 6) is the async organize→advance rewiring with mid-organize enqueue; it is isolated behind the walk discriminator so the shared `org-gtd-organize--call` never regresses the one-off/agenda organize paths.
