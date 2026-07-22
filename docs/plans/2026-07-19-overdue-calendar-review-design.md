# Overdue Calendar Review (REC-UI-04) — Design

**Status:** Designed 2026-07-19 (co-designed with maintainer). Ready to plan/build.
**Target:** v5 (`org-gtd-5`).
**Depends on:** the generalized walk engine (`org-gtd-walk.el`, built) — this is its
first *net-new actionable* consumer.
**Supersedes:** the corpus sketch `docs/feature-analysis/ux-workflows/REC-UI-04.md`
(written pre-engine; expansive). Where they differ, this document wins.

---

## 1. Goal & framing

When a calendar appointment's date passes and the item is still open, it is no longer a
hard-landscape commitment — it is undecided work masquerading as a past appointment. This
feature is the **actionable counterpart of the read-only `org-gtd-reflect-missed-calendar`
view**: it walks each overdue calendar item one at a time and lets the user decide, with
consent, what each one *becomes now*. Nothing moves without a keypress.

**This is a review operation** (the GTD "Get Current → review previous calendar data"
pass), not a bulk "migrate." Migration to a next action is merely one of the dispositions
offered per item. Naming and structure therefore join the `org-gtd-reflect-*` review
family, alongside `org-gtd-reflect-someday-review`.

## 2. Interaction model

A **walk consumer**, structurally identical to `org-gtd-someday-review`:

- A disposable, re-runnable, full-frame console showing **one overdue item at a time,
  read-only**, with a header-line action bar.
- The user presses a disposition key; the walk auto-advances to the next item.
- `:resumable nil` — no checkpoint, no back-with-undo. Skipped/undecided items simply
  reappear on the next run (a veto is "not now," not "never").
- Concurrency: `:scope` = `(org-agenda-files)` (same as someday-review), so this review
  and the someday review are mutually exclusive — you don't run both at once.
- **No implicit mutating default on `RET`.** Every disposition is consequential (it
  archives/refiles/retypes), so each requires its own explicit key. `s` skips, `q` stops.

## 3. Disposition set (the action bar)

Reviewing a passed appointment, the item either happened, still needs doing, needs a new
date, is irrelevant, or needs rethinking:

| key | disposition | meaning | mechanism |
|-----|-------------|---------|-----------|
| `d` | **done** | it happened — I just didn't check it off | `(org-todo (org-gtd-keywords--done))` + `(org-gtd-archive-item-at-point)` (the `done-and-archive` path) |
| `m` | **migrate → Next Action** | didn't happen, still needs doing | `(org-gtd-process-heading marker 'next-action)` |
| `r` | **reschedule** | didn't happen, needs a new date | `(org-gtd-process-heading marker 'calendar '((:when . "<NEW>")))` |
| `t` | **trash** | didn't happen, irrelevant now | `(org-gtd-process-heading marker 'trash)` (its `cancel-and-archive` disposition) |
| `c` | **clarify** | rethink it fully (project? delegate? etc.) | `org-gtd-clarify-item` on the item — the heavy escape hatch |
| `s` | **skip** | decide later (not a change) | `org-gtd-walk-advance` |
| `q` | **quit** | stop the review | `org-gtd-walk-quit` + teardown |

The corpus's `keep` ("leave it overdue as-is") is intentionally **dropped**: an overdue
hard-landscape item is by definition no longer a valid calendar entry, so "keep it
rotting" is not a real disposition — it collapses into `skip`.

### 3.1 Action mechanics — the key decision

`m`, `r`, and `t` all reuse the **existing, headless, full-fidelity** organize pipeline
`org-gtd-process-heading` (organize-core.el), which is exactly "clean up an item + set it
as a type + file it":

1. `org-gtd--clear-foreign-properties TYPE` — deletes the old type's properties that the
   new type doesn't declare. For `migrate`, this **auto-drops the dead
   `ORG_GTD_TIMESTAMP`** (a Calendar-only property; next-action declares none).
2. the type's organize-fn (sets `ORG_GTD` + TODO state; reads CONFIG non-interactively).
3. `org-gtd--run-disposition` — refiles to the type's home (Calendar → Actions for
   migrate) or archives (trash).

**Suppress the decoration hooks.** The item is *already clarified* — it carries its tags,
effort, and area-of-focus. So each mutating action runs the pipeline with the user's
classic `org-gtd-organize-hooks` bound off, to avoid re-prompting (`org-set-tags-command`
etc.):

```elisp
(let ((org-gtd-organize-hooks nil))
  (org-gtd-process-heading marker TYPE CONFIG))
```

The structural pipeline (property reconcile, organize-fn, disposition/refile, and the
internal `:before/after-organize`/`:before/after-file` extension hooks — empty by
default) still runs, so a migrated item is indistinguishable from a normally-organized
next action *except* that it isn't re-decorated.

`d` (done) does not go through `process-heading` because "done" is a disposition, not a
type; it calls the two `done-and-archive` steps directly (no organize hooks are involved,
so nothing to suppress). `c` (clarify) is the one action that opens the full interactive
flow.

Each action is wrapped in `org-gtd-walk-call-action` and calls `org-gtd-walk-advance` on
success (the `someday-review--defer` template): resolve the current id → marker → act →
bump counter → advance.

## 4. Detection (`:find`)

Mirrors `org-gtd-someday-review--find-items`: scan `org-agenda-files`, collect the
`org-id` of every heading that satisfies the **same predicate the missed-calendar view
uses**, composed from `org-gtd-skip.el` factory predicates:

- `ORG_GTD = "Calendar"`, **and**
- not done (`org-gtd-pred--not-done`), **and**
- `ORG_GTD_TIMESTAMP` strictly before today (`org-gtd-pred--property-before-date` with
  today as reference — a *today* appointment hasn't lapsed yet), **and**
- **not an org-habit** (the view language's `not-habit` filter / `STYLE: habit`).

Reusing the view predicate means the console shows exactly what the read-only
`org-gtd-reflect-missed-calendar` view shows — one shared definition of "overdue calendar."

**Repeaters:** repeating *Calendar* items (`<… +1w>`) **are included** (parity with the
view). They are an edge case — genuinely recurring commitments belong to the Habit type,
which the `not-habit` filter excludes. If one surfaces, the user handles it with
`reschedule`/`clarify` rather than the `migrate` default; `migrate` is never forced.

`:resolve` = `org-id-find`.

## 5. Render (`:render`)

Read-only detail pane (the `someday-review--render` pattern — read-only buffer, header-line
action bar), showing per item:

- the heading;
- the lapsed date, humanized: `was: 2026-06-12 (37 days ago)`;
- the body;
- the area-of-focus, if any;
- a one-line teaching framing (`This date has passed — decide what it is now.`).

The header-line advertises the disposition keys. A dead/already-handled marker (e.g. the
item was migrated in another buffer) **auto-skips** — the engine's stale-handle guard
already does this via `org-gtd-walk-advance`.

## 6. Counters, completion, empty state

- Live counters (buffer-local, `someday-review--counters` pattern):
  `reviewed N · done N · migrated N · rescheduled N · trashed N · skipped N`.
- `:on-finish` — clean up the surface temp file and report the tally.
- **Empty state**: if `:find` returns nothing, the console never opens; message
  `No overdue calendar items — your hard landscape is clean.`

## 7. Entry point & naming

- **v1 = one standalone command: `org-gtd-reflect-missed-calendar-review`** (autoloaded).
  The actionable review counterpart of the read-only `org-gtd-reflect-missed-calendar`.
- Major mode `org-gtd-reflect-missed-calendar-review-mode` with its own keymap (the `d m r
  t c s q` bindings); buffer `*Org GTD Missed Calendar Review*`.
- Spec registered into `org-gtd-walks` under `'missed-calendar-review`.

## 8. Edge cases & error handling

- **Item changed under us** — `:resolve`/render re-check at act time; a dead marker
  auto-skips (engine guard) rather than erroring.
- **Reschedule to a past date** — `org-read-date`, then **re-prompt until the chosen date
  is today-or-later** (`That date is also in the past — pick today or later.`). A past
  reschedule is rejected, not silently accepted.
- **Action fails soft** — a migration whose target is malformed leaves the item untouched
  and reports to the header-line in org-gtd's teaching-error voice; the review continues.
  Actions run inside `org-gtd-walk-call-action`, so an error releases the walk cleanly.
- **Concurrency** — `:scope` lock refuses a second overlapping walk with the engine's
  standard "a walk is already active over scope …" message.

## 9. Testing plan

Tier-3 adapter tests (the `inbox-walk-test.el` / `someday-review` harness), driving
`:find`/`:render`/actions directly and via `org-gtd-walk-start`:

- detection: includes overdue calendar; excludes today/future, done, non-calendar, and
  org-habit items; **includes** a repeating calendar item.
- each disposition at a marker produces the right end-state: `d` → archived done; `m` →
  `ORG_GTD=Actions`, NEXT, timestamp dropped, filed under Actions; `r` → stays Calendar
  with the new timestamp; `t` → archived canceled; `s` → advances, no change.
- **hooks suppressed**: a migrate with a prompting `org-gtd-organize-hooks` value does not
  fire it (bind a hook that would error/record, assert it didn't run).
- reschedule rejects a past date (re-prompt loop).
- empty state opens no console.
- counters tally; `:on-finish` cleans up.

## 10. Deferred (not in v1)

- **Command-center row** and **embedding as a Weekly-Review step** — both are trivial once
  the walk exists (register/point at the same spec); deferred to follow-ups.
- **Resume / back-with-undo** — out of scope (disposable, re-runnable by design).
- A first-class general `org-gtd-retype` abstraction — **not built** (YAGNI): `migrate`
  rides the existing `org-gtd-process-heading`; extract a named retype primitive only if a
  second real caller appears.

## 11. Provenance

REC-UI-04 (journal foundational consumer; walk-engine dependency now BUILT). GTD "review
previous calendar data" (Get Current). Sibling of the read-only
`org-gtd-reflect-missed-calendar` view and of `org-gtd-reflect-someday-review`.
