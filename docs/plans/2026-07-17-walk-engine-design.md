# A1 — Generalized Item-Walk Engine — Design

> Status: design agreed (2026-07-17). Target branch: `org-gtd-5` (v5).
> This is a design record from a brainstorming session, not an implementation
> plan. An implementation plan (phased tasks) is written separately per phase.

## 1. Motivation

org-gtd has grown several **hand-rolled walk state machines** — an ordered
collection of items, a cursor, and a "show current → act → advance" loop —
none of which share code:

- **someday-review** (`org-gtd-someday-review.el`, ~294 lines): walks *headings*
  (`:queue` of org-ids + `:position`), shows each in a WIP buffer via its own
  major mode (d/c/q → defer/clarify/quit). In-memory only; `--find-items`
  hardcodes `ORG_GTD == someday`.
- **review console** (inside `org-gtd-review.el`, ~657 lines): a profile-driven
  session (phases → steps, each `:type` prompt/command/view/checklist). The
  "walk" is a sub-mechanism *inside* a `checklist` step — `:walk-items` (checklist
  **strings**) + `:walk-pos` — and the whole session is **checkpointed** to
  `review-state.eld`.
- **inbox processing** (`org-gtd-process.el` + `org-gtd-clarify.el`, ~930 lines):
  the biggest and most hand-rolled — **three stacked ad-hoc queues**:
  `org-gtd-process--pending-inboxes` (a queue of inbox *files*), per-inbox heading
  iteration, and `org-gtd-clarify--duplicate-queue` (items added *mid-walk* when a
  clarified item splits into duplicates).

They diverge on three axes: *what they walk* (headings vs strings), *persistence*
(ephemeral vs checkpointed), and *shape* (standalone major-mode vs step-in-session).
This duplication is also the home of the project's worst test flakiness — the
session + WIP-buffer + checkpoint lifecycle is where `default-directory` /
mock-fs / kill-ring state leaks between tests.

**A1 builds one item-walk engine** these all become consumers of. It is a
*foundation* in the "registration-seam" sense: build the spine once, and new
walks slot in as data. Three co-equal drivers:

- **(a) Reusable seam** — surface a `walk` review step type so any review profile
  can walk headings (stuck projects, overdue calendar) in one profile line.
- **(b) Deduplication** — extract the common stateful skeleton so every walk
  shares it and differs only by its spec.
- **(c) Testability** — a headless, plain-data core so most walk logic is tested
  with zero buffers/mock-fs, shrinking the flaky test surface.

## 2. Goals / non-goals

**Goals**
- One engine; every existing walk (someday-review, review console's checklist
  walk, inbox) plus new walks (guided-review heading walks, UI-04) become
  consumers.
- Item-agnostic: the engine never inspects what an item *is*.
- Opt-in cross-restart resume, centralized (replacing review.el's bespoke
  checkpoint).
- Dependency-free (no dash/generator/stream at runtime — org-gtd stays lean).

**Non-goals**
- Not a general sequence/iterator library (see §11 — the reusable part is
  trivial; the valuable part is interaction + serializable resume + dynamic
  queue, which no iterator library provides).
- Not re-platforming inbox first — inbox is the north-star that validates the
  design but is migrated **last** (§10).

## 3. Architecture — three layers

1. **Walk model (pure, headless).** Plain serializable data + pure functions.
   No buffers, no org, no I/O. Unit-tested with plain lists. (§4)
2. **Session driver (generic UI).** Owns the loop, the keymap, and the
   render/checkpoint/transition lifecycle — the one place cursor state moves and
   the one place the (previously flaky) buffer/point lifecycle lives. (§6)
3. **Consumer spec.** `(:name :find :render :actions :on-finish :resumable
   :resolve :scope)` — plain data each consumer registers. (§7)

The driver renders into a **caller-provided surface** (a buffer + region), not a
hard-owned buffer: a standalone walk gets its own buffer; a `walk` step inside
the review console renders into the review buffer's step region.

*Rejected alternative:* a pure model only, each consumer building its own loop —
re-duplicates the flaky buffer-loop and loses (b)+(c).

## 4. The walk model

A single serializable structure (`cl-defstruct` or plist):

- **`entries`** — an ordered list of opaque, serializable handles (org-ids or
  strings). Filtering is upstream (`find` decides what is in the list); the walk
  never re-filters. **No per-entry status** — the cursor alone separates handled
  (behind) from pending (ahead), and an item's "done-ness" is its own org state,
  not the walk's.
- **`cursor`** — current index.
- **`meta`** — free-form serializable plist for the consumer.

Pure operations (take a model, return a model; no side effects):

- `walk-current` → handle at cursor
- `walk-advance` → cursor + 1
- `walk-enqueue(handle, top|bottom)` → insert into the **remaining** queue (after
  the cursor): `top` = front of the remaining queue (handled next, current item
  unchanged), `bottom` = end (handled last). Both insert *after* the current item
  because the **dynamic queue** inbox's duplicate-queue enqueues while the current
  item is still being processed — inserting *at* the cursor would make the
  duplicate current mid-process. `where` maps directly onto today's
  `org-gtd-clarify-duplicate-queue-position` (`top`/`bottom`, default `bottom`).
- `walk-remaining`, `walk-done-p`

"Skip but revisit later," if ever needed, is not a new concept: `advance` +
`enqueue(handle, bottom)`.

The model never performs work: `walk-advance` just moves the cursor; the actual
org effect is the consumer's action, run *before* it calls the transition.

## 5. Scope & identity

A walk is scoped to the **org container it reads from**, mirroring org's own
"functions target a buffer/subtree" model:

- **a subtree** (a heading's org-id) — e.g. a checklist walk under one heading, or
- **a file / file-set** ("level 0") — inbox files, someday, stuck-projects (they
  touch scattered headings *within* the file, so the file is the unit).

Two uses of scope:

- **Concurrency lock = scope alone.** No two walks over the same container at
  once (refuse or refocus). This prevents concurrent *mutation* of the same
  file/subtree — the real integrity risk. Different scopes coexist freely (inbox
  on inbox.org and a review walk on tasks.org run side by side).
- **Checkpoint identity = `name` + scope.** "someday walk over tasks.org" and
  "stuck walk over tasks.org" are distinct resumable sessions (never concurrent,
  each independently resumable).

Consequence (accepted, a feature): a filtered whole-file walk locks the whole
file, so two *different* filtered walks can't run over the same file at once —
they would otherwise interleave mutations.

Per-walk mechanics are **buffer-local** on the surface buffer (`defvar-local`);
scope is the identity/lock layer on top. Two walks of the *same* name+scope
(e.g. two inbox processes) are not forcibly prevented; they race their checkpoint
last-write-wins — the same stance `review.el`'s `--state-file` already documents.

## 6. Session driver + action contract

**What it owns:** a per-surface active-walk bundle — the model, the consumer
spec, the surface `(buffer + region)`, and the checkpoint path (if resumable) —
held buffer-local.

**Transition API — the one place state moves:** `walk-advance`,
`walk-enqueue(handle, where)`, `walk-quit`, `walk-finish`. Each mutates the
model, re-renders, and checkpoints in one tested place.

**Action contract (imperative transitions).** The spec's `:actions` is a keymap;
each bound command (1) does its org side-effect on `walk-current`, then (2) calls
a transition. The transition call **need not be synchronous** — this is the key
to the inbox case:

- someday-review's `defer` does its work and calls `walk-advance` immediately.
- inbox's `organize` opens the **transient**; only when the chosen category
  finishes does its continuation call `walk-advance`, many keystrokes later.

Same API; the driver doesn't care. No callback machinery in the engine. (Chosen
over "actions return a signal the driver interprets" precisely because the async
organize case has no single return point.)

**Render + surface.** `:render` is `(handle surface) → void`: draw the current
item into the region. The driver calls it after every transition. Headless tests
pass a stub `:render` and loop `walk-advance` — no buffer.

## 7. Consumer spec, registry, and the `walk` step type

**Spec** (plain data):

```elisp
(:name      symbol      ; identity; checkpoint filename for standalone walks
 :find      fn          ; () -> handles   (multi-source is this fn's job)
 :render    fn          ; (handle surface) -> void
 :actions   keymap      ; keys -> commands (org effect, then a transition)
 :on-finish fn          ; () -> void
 :resumable bool        ; opt-in checkpoint
 :resolve   fn          ; optional (handle -> non-nil?); stale handles auto-skip
 :scope     fn/spec)    ; the org container: file-set or subtree org-id
```

**Registry.** `org-gtd-walks` — an alist `(name . spec)`, exactly like
`org-gtd-types`. A walk is a first-class, named, reusable thing (someday,
`overdue-calendar`, `stuck-projects`, eventually `inbox`).

**The step type (the (a) seam).** Add `walk` to the review step `:type` set;
reference a registered walk *by symbol*, matching how `:view
org-gtd-show-all-next` and `:command org-gtd-process-inbox` already work:

```elisp
(:title "Clear stuck projects" :type walk :walk stuck-projects)
```

Profiles stay declarative (`sexp`-clean, no lambdas). Once the step type exists,
adding a walk to any review is one profile line.

**Composition when hosted.** The console starts the walk with `surface` = the
review buffer's step region and `:on-finish` = advance the session. Persistence
composes because the model is plain data: a standalone walk (`:resumable t`)
checkpoints to its own file; a walk hosted in a resumable review runs
`:resumable nil`, and the review embeds `(walk-model)` in its **single**
checkpoint — resuming straight back into the walk at its cursor.

## 8. Persistence / resume

Opt-in, engine-owned. `:resumable t` → the driver `prin1`s the model to a path
keyed by `name`+scope and `read`s it back on start, guarded by `walk-valid-p`
(cursor in range, entries a list, handles serializable) which falls back to a
fresh `find` on corruption — exactly review.el's `--state-valid-p`, promoted.

Because handles must be serializable, **live markers are never persisted** — a
heading walk carries org-ids and resolves id→marker at render time; a checklist
walk carries the strings. Markers stay an internal, render-time detail.

## 9. Error & edge handling

- **Empty `find`** → don't open a buffer; finish immediately (a hosted step just
  advances the review). Promotes someday-review's `(zerop (length queue))` check.
- **Stale handle** (persisted id no longer resolves) → `:resolve` yields nil →
  driver auto-advances, counting skips and reporting "N no longer present."
  Mostly bites resumable walks (inbox on resume).
- **Corrupt checkpoint** → `walk-valid-p` fails → discard file, start fresh.
- **Quit vs finish** — *finish* (ran off the end) deletes the checkpoint + runs
  `:on-finish`; *quit* (abandoned) tears down but **keeps** the checkpoint if
  `:resumable`, runs no `:on-finish`. Two distinct exits.
- **Action error** — because actions do the effect *then* transition, a throw
  before the transition means **no advance**; the walk stays on the current item.
  The driver wraps action invocation to surface the error and leave state intact.

## 10. Testing strategy

Three tiers, each stricter about what it may touch:

- **Tier 1 — model:** pure unit tests on plain lists (advance, enqueue at each
  position, done-p, `walk-valid-p` on corrupt data, serialize→`read` round-trip).
  No org, no buffers, no mock-fs — cannot flake. Most logic lives here.
- **Tier 2 — driver:** integration tests against a **stub spec** (fixed `find`,
  string-appending `render`, toggleable `resolve`). Asserts render-on-advance,
  stale-skip, finish-deletes / quit-keeps checkpoint, empty-find, corrupt→fresh,
  scope-lock. **Proves the flaky lifecycle exactly once, deterministically.**
- **Tier 3 — consumers:** thin real-org acceptance tests per consumer — only the
  consumer's own `find`/`render`/`actions` wiring, since iteration/resume/skip are
  already trusted. Real mock-fs but few tests.

Net effect: the buffer-and-mock-fs-dependent test surface shrinks to thin
per-consumer adapters. (Optional: a `walk-conformance` helper asserting any
registered spec meets the contract.)

## 11. Alternatives considered

- **Heading-only engine** (not item-agnostic) — tighter, but leaves (b) on the
  table (the checklist-string walk stays separate). Rejected given (b)/(c) are
  co-equal drivers.
- **Ephemeral engine; consumers persist** — simpler core, but leaves the
  gnarliest, flakiest code (checkpoint) un-deduped. Rejected.
- **An iterator/sequence library** (`dash`, `generator.el`, `stream.el`, `s`,
  threading macros) — the iteration surface (cursor over a list) is ~10 trivial
  lines; the hard parts are interaction-driven advance (map/reduce are
  synchronous), **serializable** resume (a generator continuation can't be
  `prin1`'d — a generator lib would make resume *worse*), and dynamic enqueue.
  `s` is strings. Keep the engine dependency-free; libraries help only the
  consumer's `find` step, which built-in `seq.el` covers.
- **Per-`name` or global-singleton identity** — replaced by per-scope (org
  container) identity, which matches org's model and precisely captures the
  concurrent-mutation risk.
- **Per-entry status in the model** — dropped; the cursor + upstream filtering +
  the item's own org state already carry it.

## 12. Migration sequencing

Each phase is a feature branch off `org-gtd-5`, merges back green.

**Two kinds of phase, two disciplines:**

- *Migrations* of existing behavior (Phase 1 someday-review, Phase 2's review.el
  checklist fold, Phase 4 inbox) follow **characterize first** — the engine-backed
  version must pass the consumer's existing tests as a safety net — **then thin**
  to Tier-3.
- *Net-new behavior* (Phase 2's `stuck-projects` step, **Phase 3 UI-04**) has no
  existing behavior to characterize against, so it must be **designed
  collaboratively first** — a short brainstorm agreeing the exact UX before any
  code. Do not build "something reasonable" for these; define them the way this
  document was defined.

- **Phase 0 — Engine core (zero risk):** model (Tier 1) + driver (Tier 2),
  registry, scope lock, checkpoint. Touches no existing feature.
- **Phase 1 — someday-review:** migrate the smallest real heading-walk; it has
  existing tests to characterize against. Validates the WIP-render + action
  pattern.
- **Phase 2 — `walk` step type + fold review.el:** add `walk` to the step
  `:type` set and a first heading-walk step (`stuck-projects`, new capability);
  move review.el's checklist-string walk + bespoke checkpoint onto the engine.
  Delivers (a) and the biggest (b)/(c).
- **Phase 3 — UI-04 overdue-calendar (design-gated, net-new):** a fresh walk
  (migrate-to-next-actions with veto), standalone command *and* review step.
  **Its UX must be agreed collaboratively before implementation** — e.g. what
  counts as "overdue," per-item consent vs. batch veto, what "migrate" does to
  the timestamp/state, and how it relates to the calendar view. Ships a journal
  BUILD-V5 item. Do not implement from a one-line description.
- **Phase 4 — inbox (north-star, last):** reimplement process/clarify —
  multi-source find, clarify-buffer render, organize-transient async action,
  duplicate-queue → `enqueue`. Three stacked queues collapse; inbox becomes
  resumable. Highest stakes, most characterization, done only after Phases 1–3
  harden the engine.
