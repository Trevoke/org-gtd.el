# Walk resume identity — split `:scope`, make walks resumable by selection — Design

**Status:** Designed 2026-07-22 (co-designed with maintainer). Ready to plan/build.
**Target:** v5 (`org-gtd-5`).
**Supersedes yak:** *"Refresh load-time :scope on registered walk specs when hosted as
review step."* The premise ("refresh the frozen scope") is discarded — the real problem is
that `:scope` conflates two identities. See §1.
**Depends on:** the walk engine (`org-gtd-walk.el`, `org-gtd-walk-model.el`, built). No new
lifecycle code — only an identity change plus opting two consumers into resume.

---

## 1. Problem & reframe

The originating yak asked to "refresh the load-time `:scope`" because a walk spec captures
`(org-agenda-files)` at registration time and can go stale when the spec is later hosted as
a Weekly-Review step. Investigating the code showed the framing was wrong.

`:scope` currently serves **two masters with one value** (the agenda-file list):

1. **Concurrency lock** — `--scope-key` → `--lock-scope` / `--scope-locked-p`: "no two
   walks mutate the same data at once."
2. **Resume checkpoint identity** — `--checkpoint-path` is `walk-<name>-<md5(scope)>.eld`:
   "which review am I resuming."

The moment someday-review wants to be resumable, these two want **different
granularities**. someday-review has a `list` filter ("review list A" vs. "Unassigned").
The file set is identical for every list, so a scope-keyed checkpoint **cannot tell list A
from list B** — reviewing A, quitting, then reviewing B would collide on one file. The
thing that identifies "the review I'm resuming" is the **selection**, not the file set.

**Reframe:** don't refresh `:scope` — *split* it. The lock stays coarse (file set); resume
gets its own fine-grained key built from the selection the consumer already holds at start
time. "Frozen vs. fresh" then evaporates: there is nothing captured at load time to go
stale, because the resume key is constructed at each standalone start.

## 2. Decisions (settled with maintainer)

- **Split into two keys**, not one overloaded value.
- **Resume identity = walk + selection**, not walk + file scope.
- **Resumable set:** `inbox` (already resumable) and `someday-review` (newly resumable).
  **`missed-calendar-review` stays `:resumable nil`** — a deliberate reversal *avoided*: its
  design (2026-07-19 §2) chose disposability so skipped items resurface on every run ("a
  veto is 'not now,' not 'never'"). Resume would defeat that, so it keeps no checkpoint.
- **Resumability is set at the standalone start-site, not baked into the registry
  template** — this is what keeps the hosted (review-step) path from double-persisting
  (§4).

## 3. The two identities

### 3.1 `:scope` — unchanged, lock only

Keeps its current meaning and plumbing: the coarse "what data might I mutate" identity,
still the file list, still feeding `--scope-key`/`--lock-scope`/`--scope-locked-p`. After
this change it does **nothing else**. Two reviews touching `someday.org` still mutually
exclude; the split does not loosen the lock.

### 3.2 `:resume-key` — new, optional, checkpoint identity

A new optional spec field naming the **selection**. `--checkpoint-path` changes:

```
;; before
(org-gtd-walk--checkpoint-path name scope)   ; walk-<name>-<md5(scope-key)>.eld
;; after
(org-gtd-walk--checkpoint-path name resume-key)
```

`--checkpoint-path` md5's the resume-key string, so any selection value (spaces, slashes,
symbols) is filename-safe. **Fallback:** a resumable walk that sets no `:resume-key` keys on
the **walk name alone** (correct for singletons like inbox).

Per consumer:

| walk | `:resumable` | `:resume-key` | checkpoint file |
|------|-------------|---------------|-----------------|
| `someday-review` | **t** (new) | the list filter — `"work"`, `"Unassigned"`, `"all"` | `walk-someday-review-<md5(list)>.eld` |
| `inbox` | t (unchanged) | constant (one inbox review) — rename only, no behavior change | `walk-inbox-<md5(const)>.eld` |
| `missed-calendar-review` | **nil** (unchanged) | — (moot; no checkpoint path built) | none |

Reviewing list A and list B now get **independent** checkpoints — the collision is gone by
construction.

## 4. Lifecycle & the hosted-path rule

**No new lifecycle code.** The engine already: checkpoints after *every* item in
`--settle` (crash-safe, not only on quit); `walk-quit` keeps the checkpoint; `walk-finish`
deletes it; `walk-start` loads it when `:resumable` and one exists. Making someday-review
resumable is just setting `:resumable t` + `:resume-key` on the spec it starts with.

**Where resumability is set resolves the double-persist collision.** Each walk has two spec
instances:

- the **registered template** in `org-gtd-walks` (what `org-gtd-review.el` fetches via
  `org-gtd-walk-get` when hosting a `walk` step), and
- the **fresh spec** the standalone entry point builds (someday-review already rebuilds it
  and `plist-put`s `:find`/`:scope` at call time).

**Rule: add `:resumable t` + `:resume-key` at the standalone start-site, keep the
registered template `:resumable nil`.** Consequences:

- **Standalone someday-review** → fresh spec is resumable → engine `.eld` persistence.
- **Hosted someday-review** → review.el fetches the `:resumable nil` template → the engine
  never self-persists → review-state's own `:walk-model` stays the sole store.

No collision, and no "am I hosted?" flag inside the engine — the distinction falls out of
which spec instance starts the walk.

**Resume trusts the checkpoint; it does not re-scan.** On resume, `:resolve`
(`org-id-find`) auto-skips items that moved or were disposed elsewhere (the `--settle`
skip loop) — the "a lot won't have moved" case: unmoved items resume in place, the few that
changed silently drop. **Documented trade-off:** someday items added *since* you started do
**not** appear until you finish a pass (a completed pass deletes the checkpoint, so the
next run re-scans fresh).

## 5. Edge cases

- **Stale/foreign/corrupt checkpoint** — `--load-checkpoint` / `walk-model-deserialize`
  must degrade to a fresh `:find` (return nil), never crash `walk-start`. Precedent:
  review.el already guards pre-engine `:walk-items`/`:walk-pos` checkpoints.
- **Selection changed between runs** — different `:resume-key` ⇒ different file ⇒
  independent. Reviewing A halfway then starting B never clobbers A. Cost: a
  quit-and-never-resumed selection leaves an orphan `.eld` (a completed pass would have
  deleted it). Minor disk cruft; **not** worth a sweeper in v1 — note and move on.
- **Concurrency unchanged** — scope-lock stays coarse (file set); a standalone and a hosted
  someday-review over the same files still refuse to overlap.
- **Serialization** — someday-review handles are org-id strings (clean round-trip); inbox's
  already-working resume is untouched by the rename.

## 6. Testing (adapter-tier — the someday-review / inbox harness)

- **Round-trip:** filter to list A, advance a few, `quit` → assert checkpoint exists keyed
  by list A; restart → resumes at the same cursor with unmoved items still queued;
  `finish` → checkpoint deleted.
- **Two-list independence:** quit mid-A, start B → B is fresh, A's checkpoint intact.
- **Stale item:** checkpoint holds an id that has since moved → resume auto-skips it
  (`:resolve`).
- **Hosted no-double-persist:** host someday-review as a review `walk` step → assert **no
  engine `.eld`** is written (template stays `:resumable nil`); review-state `:walk-model`
  carries the resume point.
- **missed-calendar stays disposable:** assert it writes no checkpoint.
- **Corrupt checkpoint:** a garbage `.eld` ⇒ `walk-start` falls back to a fresh `:find`,
  no error.

## 7. Deferred

- Orphan-checkpoint cleanup (a sweeper for quit-and-never-resumed selections).
- Making any *hosted* walk resumable via the engine's own `.eld` (today the review session
  owns hosted persistence; there is no demand to change that).
- A first-class "how many resumed" telemetry counter.

## 8. Provenance

Yak: *"Refresh load-time :scope on registered walk specs when hosted as review step"*
(2026-07-19, contextless one-liner). Reframed here from "refresh the frozen value" to
"split the conflated identity." Walk engine design: `docs/plans/2026-07-17-walk-engine-design.md`
(§5 scope & identity, §8 persistence/resume). Sibling consumers:
`docs/plans/2026-07-19-overdue-calendar-review-design.md` (§2 disposability decision that
this design preserves).
