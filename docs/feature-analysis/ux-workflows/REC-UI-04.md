# UX Workflow — Auto-migrate overdue calendar items → Next Actions (with consent)

`REC-UI-04` · An event's date passed and it didn't happen — decide, one at a time, what each stale calendar item *becomes* now, without anything moving behind your back · cluster: `B — Flag → decide → consent`

---

## 1. The need (what & why)

- In GTD, the calendar holds *hard landscape*: things that must happen on a specific day. When that day passes and the item is still open, it is no longer a calendar commitment — it is undecided work masquerading as a past appointment. It rots silently until a Weekly Review catches it. The user needs org-gtd to **surface every overdue calendar item and walk them one at a time**, proposing the obvious move (it's now just an action) but **never acting without a yes**.
- Hit during the **Get Current** phase of the Weekly Review (`reflect.feature` WF: "review previous calendar data … for remaining or emergent actions"), and any time the user opens *Missed items → Calendar*.
- Source: `REC-UI-04` (Not-implemented; missed views IMPL-074/081 surface past-due items but nothing acts) · GTD "review previous calendar data" (DA94-41/W08).

## 2. Entry points & discovery

- **Invoke**
  - `M-x org-gtd-migrate-overdue-calendar` — standalone triage session over all past-due `type . calendar` items.
  - From **`org-gtd-command-center`** [R]: the existing `M` *Missed items…* transient (`org-gtd-command-center--missed`) gains a row — `m` *Migrate overdue calendar…* — right beside the read-only `c` *Calendar only* view. Seeing the missed list is one key from acting on it.
  - **Embedded** as a phase inside the Weekly Review session (Cluster A engine): the *Get Current → previous calendar* step *is* this walk.
- **Discover** — a user who runs the read-only *Missed → Calendar only* [R] view sees a header-line hint: `n past-due calendar items — press m to triage them`. The count also feeds the [U] proactive integrity flag (REC-UI-02).

## 3. Full-lifecycle walkthrough

This feature's "object" is a **triage decision per item**, not a saved artifact. Lifecycle verbs map to the decision, not to a stored file.

- **Create / start** — `org-gtd-migrate-overdue-calendar` runs a detection pass (`org-map-entries` over the calendar type, `ORG_GTD_TIMESTAMP` earlier than today, not DONE), snapshots the window config, and opens the full-frame `*GTD Migrate Overdue Calendar*` console — the same three-widget session frame as Cluster A (phase/step tracker, running stats, header-line key advert). It stops on item 1.

  1. Item 1 is shown in a read-only detail pane: heading, its lapsed date (`was: 2026-06-12, 22 days ago`), body, area-of-focus.
  2. The **action bar** presents the decision with the migrate action as the default: `[m]igrate → Next Action  [r]eschedule  [c]larify  [k]eep  [s]kip`.
  3. Press `m` (or `RET`/`n` on the default) → the item is re-dispatched through the organize router as a single-action: `ORG_GTD` Calendar→Actions, the dead `ORG_GTD_TIMESTAMP` is dropped (recorded in a logbook note `migrated from calendar 2026-06-12`), TODO set to `NEXT`, filed to the actions list. Stats redraw (`migrated 1`), auto-advance to item 2 (process-loop continuation idiom [R]).
- **See / preview** — the stats block redraws live after every decision: `reviewed 3/9 · migrated 2 · rescheduled 0 · kept 1 · skipped 0`. Nothing changes on disk until the user presses a decision key for that item.
- **Edit / reconfigure** — the decision *is* the interaction. `r` **reschedule** prompts for a new date (org date picker) and the item **stays** a calendar item with a fresh timestamp. `c` **clarify** routes into the organize dispatch / WIP buffer [R] for full re-decision (make it a project, delegate it, trash it) — the escape hatch when "just an action" is wrong.
- **Save / name / recall** — no named object to save. **Pause** (`p`) persists session state (which items decided, which remain) to the shared Cluster-A resume file; re-invoking offers **Resume**. A completion-log line records the run.
- **Delete / undo / back out** — `b` steps **back** to the previous item and re-opens its decision (the last migration is reverted: type restored, timestamp restored). `k` **keep** is the explicit veto — leave the item exactly as-is (still overdue on the calendar) and move on. `q` quits with a Pause/Abandon prompt; on abandon, decisions already committed stand (they were each a consented action), window config is restored.
- **Repeat / recur** — re-run any time; already-migrated items no longer match the detection pass, so the list naturally shrinks. Kept items reappear next run (they're still overdue) — by design: a veto is "not now," not "never ask."

## 4. Interaction sketch

```
┌─ *GTD Migrate Overdue Calendar* ──────────────────────────────────┐
│ Phase 1 of 1  Triage overdue calendar        Item 4 of 9   →      │
│ reviewed 3/9 · migrated 2 · rescheduled 0 · kept 1 · skipped 0    │
├───────────────────────────────────────────────────────────────────┤
│  ● Dentist — 6-month cleaning                                     │
│    was scheduled: 2026-06-12  (22 days ago)                       │
│    Area of focus: Health                                          │
│                                                                   │
│    This calendar date has passed. It's now undecided work.        │
│    Default: make it a Next Action you'll do when you can.         │
├───────────────────────────────────────────────────────────────────┤
│ [m]igrate→Next Action  [r]eschedule  [c]larify  [k]eep  [s]kip    │
│ b back · p pause · q quit · , customize                           │
└───────────────────────────────────────────────────────────────────┘
```

**Keymap** (the action bar is generated from the step's `:allowed-actions`, not hand-authored)

| key | action |
|---|---|
| `m` / `RET` / `n` / `SPC` | **accept default** — migrate item to a Next Action, auto-advance |
| `r` | reschedule — pick a new date, item stays on the calendar |
| `c` | clarify — open organize dispatch / WIP for a full re-decision |
| `k` | **veto** — keep as-is (leave overdue), advance |
| `s` | **skip** this item this run (undecided; not a change) |
| `b` | back to previous item (reverts its committed decision) |
| `p` | pause — persist state, restore windows |
| `q` | quit — Pause/Abandon prompt |
| `,` | customize the migration profile (default action, drop-vs-keep timestamp) |

**Live preview** — before `m`: heading is `Calendar`, timestamp `2026-06-12`. The instant `m` is pressed the stats line increments `migrated →`, the item vanishes from the list, and the console advances — the only "preview" is the read-only detail pane; the mutation is deferred to the keypress, never speculative.

## 5. Fit with org-gtd

- **Extends** — the **reflect** missed-items surface (`org-gtd-reflect-missed-calendar`, IMPL-074/081 [R]) turns the read-only view into an actionable walk; reuses the **organize router** (`org-gtd--dispatch 'single-action`) [R] to perform each migration, the **process-loop continuation** [R] for one-at-a-time advance, **clarify + WIP** [R] for the `c` escape, and the **command-center** [R] `M` transient for its home.
- **Shared surface / cluster** — Cluster **B (flag → decide → consent)** with `REC-CLA-10`. The **detection-pass → walk → per-item accept/veto/skip** spine must feel **identical** to CLA-10's vague-phrasing triage and, per the inherited contract, to Cluster A's `someday-review` action bar. Concretely: same console chrome (three widgets), same `accept / veto / skip` semantics on the same relative keys, same teaching-voice framing, same Pause/Resume. The three-widget frame, session key vocabulary (`n/SPC b s p q ,`), and the walk-step action bar are the shared guided-session-engine contract from REC-REF-02 — I conform rather than reinvent. **Break from contract:** the *primary* action key is item-semantic (`m` migrate) rather than a generic "defer," because the whole point is a specific proposed disposition; the generic accept keys (`RET`/`n`/`SPC`) still fire it, preserving muscle memory.
- **Reuse vs. new** — reused: detection (`org-map-entries`), organize dispatch, continuation, WIP, session engine, command-center row. New: the overdue-calendar *step profile* fed to the engine, and its per-item detail pane + reschedule-vs-migrate-vs-keep decision.
- **Release tag** — leans almost entirely on `[R]` (reflect views, organize router, clarify/WIP, command-center) plus the `[U]` guided-session engine (Cluster A / REF-02). No `[R]` rework needed — the missed view is *added to*, not changed.

### Type / extension-UX opportunities

- The migration is a **type transition** (`Calendar → Actions`) that today is expressed as ad-hoc property rewriting. This argues for a first-class **`org-gtd-retype` / type-transition helper** on the registry: "re-dispatch this item as type X, reconciling its property descriptors" — dropping the now-invalid `ORG_GTD_TIMESTAMP` (calendar's `:required` timestamp descriptor) and satisfying the target type's descriptors. That helper would also serve any "convert X to Y" flow and is a clean addition to the [U] `customize-type` family.
- Whether the default action is "migrate" vs "reschedule" is a **per-type policy** — a natural slot on the calendar type (`:overdue-default`), reinforcing the descriptor-driven model rather than hard-coding behavior in the session.

## 6. Edge cases & failure modes

- **Empty state** — no overdue calendar items: the console never opens; a teaching message: `No overdue calendar items — your hard landscape is clean.` (invoked from the missed view, the `m` hint is simply absent).
- **Bad input** — reschedule to a past date: re-prompt with `That date is also in the past — pick today or later, or press k to keep it as a reminder.` Item already migrated in a concurrent buffer: detection re-checks at decision time; if it no longer matches, skip with a soft note rather than error.
- **Large sets** — dozens of overdue items (common on first run): the stats readout and Pause/Resume make a long sweep survivable; `s` lets the user triage the obvious ones and defer the rest without losing place.
- **When it goes wrong** — a migration whose target list is missing or a malformed heading fails **soft**: the item is left untouched, `Couldn't migrate "…": <reason>. Left it on the calendar — press c to clarify it by hand.` prints to the header-line, the session continues. No stack trace, matching org-gtd's teaching-error voice.

## 7. Open questions & maintainer decisions

- **Dead timestamp: drop, or keep as a note?** Default proposed = drop the `ORG_GTD_TIMESTAMP`, log the old date in the logbook. Alternative = keep it as an inactive timestamp in the body. Customizable via `,`; which is the *default*?
- **Should a `k` (kept) item be nag-suppressible** for N days, or always reappear next run? (Contract-consistent answer: always reappear; confirm.)
- **Recurring calendar items** (org repeaters): a lapsed instance of a repeater should probably be **excluded** (org already advanced it) — confirm the detection pass filters `+`/`.+`/`++` timestamps.

## 8. Provenance & links

- `REC-UI-04` · deliverable-#3 status **Not-implemented** · build route: `gap-implementation-strategies.md` §4 / §9 (org-native `org-map-entries` + bespoke consent; IMPL-074/081 missed views + IMPL-068/069 date-setters) · workflow `reflect.feature` WF-20 "Get Current — review previous calendar data" · cluster **B** sibling `REC-CLA-10`; inherits the Wave-0 guided-session-engine contract from `REC-REF-02`.
