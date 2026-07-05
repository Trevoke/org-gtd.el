# UX Workflow — Two-stage delegation

`REC-DEL-04` · *"I've decided to delegate this, but handing it off is itself a thing I still have to do"* · cluster: `C — Delegate-flow enrichments`

---

## 1. The need (what & why)

In GTD, *deciding to delegate* and *having delegated* are two different states. Between them sits a next action that is still on **my** plate: draft the email, catch Bob at standup, write the note. Today org-gtd collapses both into one step — pressing `d` immediately stamps `WAIT`/`DELEGATED_TO` and drops the item onto the **Waiting-For** list, as if the hand-off already happened. The user is then "waiting on Bob" for something Bob doesn't yet know exists. The item rots: it never appears on a next-action list, so nothing prompts the user to actually make the hand-off.

REC-DEL-04 adds a **plan-to-delegate pre-state**: stage 1 is a *next action for me* ("hand this off to Bob"); stage 2, after I've done that, is the familiar `WAIT`/`DELEGATED_TO` + check-in date. Users hit this during **clarify/organize** whenever the hand-off isn't instantaneous — which is most real delegations.

Source: `REC-DEL-04` (deliverable #3, **Not-implemented**, `tool/may`, DA94-39) · build route: schema addition (`gap-implementation-strategies.md` §7, line 264) — a plan-to-delegate pre-state layered on WAIT→delegate conversion (IMPL-027) + delegate state (IMPL-025).

## 2. Entry points & discovery

- **Invoke** — the *same* `d` in the organize transient `[R]` (`org-gtd-organize` → `org-gtd-delegate` → `org-gtd--dispatch 'delegated`). No new key, no new disposition row. Pressing `d` now opens a one-question stage prompt (hand-off done yet?) before the who/when prompts. The DWIM entry `org-gtd-delegate` (also reachable on an agenda line) gains the same branch.
- **Advance to stage 2** — re-run delegate on the item (`C-c C-o d` in its WIP buffer, or `M-x org-gtd-delegate` on the agenda line). The command detects the `planned` stage and completes the hand-off instead of starting a new one.
- **Discover** — three in-flow affordances, no manual needed: (1) the stage prompt itself names the distinction; (2) plan-to-delegate items show in **engage → All actions ready** prefixed `→ hand off to Bob` `[R]/[U]`, so they nag from the next-action list the way GTD intends; (3) the delegate prompt's help line teaches the two stages.

## 3. Full-lifecycle walkthrough

**Create / start (stage 1 — plan to delegate).**
1. Clarify an inbox item, `C-c C-o` → transient → `d`.
2. New stage prompt appears: `Have you handed this off yet?  [h] Not yet  [w] Already`.
3. Press `h`. Prompt: `Who will do this?` → `Bob`. Then (DEL-05 fold-in) `Hand off via:` → `email`.
4. org-gtd writes a **NEXT** action (my ball), tagged so engage labels it, with `DELEGATED_TO: Bob`, `ORG_GTD_DELEGATE_STAGE: planned`, `ORG_GTD_DELEGATE_CHANNEL: email`. **No check-in date yet** — there's nothing to check in on. Item lands on the next-action list, not Waiting-For.

**See / preview.** In `org-gtd-engage`, the item appears in *All actions ready* as `→ hand off to Bob — Review Q3 budget`. It is NOT in the *Delegated to check in on* block (correct: the ball is mine).

**Advance (stage 2 — record the hand-off).**
5. Once I've emailed Bob, I run delegate again on the item (`C-c C-o d`).
6. It detects `stage=planned` and shortcuts: `Completing hand-off to Bob — When to check in?` → `2026-07-11`.
7. Keyword flips **NEXT→WAIT**, `ORG_GTD_TIMESTAMP` set, `ORG_GTD_DELEGATE_STAGE: delegated`. Item leaves my next-action list and enters Waiting-For — now identical to a one-step delegation.

**Edit / reconfigure.** Re-running delegate at either stage re-prompts (change assignee/channel/date). Free org editing in the WIP buffer edits the properties directly.

**Save / name / recall.** Persisted as ordinary org properties on the heading; recall is structural — stage 1 via engage next-actions ("my ball"), stage 2 via *Delegated to check in on* / `org-gtd-reflect-upcoming-delegated` ("their ball"). No separate store.

**Delete / undo / back out.** Abandon a plan-to-delegate with the normal `t` (trash) disposition, or re-organize it as any other type — it's a plain heading. `C-c C-k` aborts the organize session and restores window config `[R]`.

**Repeat / recur.** N/A — a one-way two-stage progression, not recurring. (Recurrence is the Habit type's job.)

## 4. Interaction sketch

**Mock — stage 1 (plan to delegate):**
```
┌─ Organize ─────────────────────────────┐
│ Actionable                             │
│  q quick   s next-action   d delegate  │   ← press d
│  c calendar   h habit                  │
└────────────────────────────────────────┘
        │
        ▼  d
  Delegate "Review Q3 budget"
  Have you handed this off yet?
    [h] Not yet — I still need to hand it off
    [w] Already — now waiting on them
        │  h
        ▼
  Who will do this?           Bob
  Hand off via (channel):     email        ← DEL-05, optional
        │
        ▼   result in the WIP buffer
  * NEXT → hand off to Bob: Review Q3 budget      :@waiting:
    :PROPERTIES:
    :DELEGATED_TO:            Bob
    :ORG_GTD_DELEGATE_STAGE:  planned
    :ORG_GTD_DELEGATE_CHANNEL: email
    :END:
```

**Mock — stage 2 (complete hand-off), triggered by `d` on a `planned` item:**
```
  Delegate "Review Q3 budget" — completing hand-off to Bob
  When to check in?           2026-07-11
        │
        ▼
  * WAIT Review Q3 budget
    :DELEGATED_TO: Bob   :ORG_GTD_TIMESTAMP: <2026-07-11>
    :ORG_GTD_DELEGATE_STAGE: delegated
```

**Keymap:**

| Key | Context | Action |
|-----|---------|--------|
| `d` | organize transient | Delegate disposition → stage prompt |
| `h` | stage prompt | Plan to delegate (stage 1, NEXT, no date) |
| `w` | stage prompt | Already delegated (current one-step flow) |
| `d` | `C-c C-o` on a `planned` item | Complete hand-off (stage 2, → WAIT) |
| `t` | organize transient | Trash / abandon the plan |
| `C-c C-k` | clarify buffer | Abort, restore windows |

**Live preview.** No dedicated preview pane (this isn't a Cluster-E CRUD manager); the "preview" is the item's own transition between the engage *next-actions* block (stage 1) and the *delegated* block (stage 2) — the structural feedback the prefix-format DSL already provides.

## 5. Fit with org-gtd

- **Extends** — the **organize transient** delegate disposition `[R]` (`org-gtd-delegate`, `org-gtd-delegate--organize`, IMPL-025/027); the **delegated type** in `org-gtd-types.el` (adds an intra-type two-stage state machine keyed on `ORG_GTD_DELEGATE_STAGE`); **clarify + WIP** `[R]`; and **engage** `[R]` / the prefix-format **view DSL** `[R]` to label stage-1 items.
- **Shared surface / cluster** — Cluster **C** (Delegate-flow enrichments) with `REC-DEL-03` (delegate a whole project) and `REC-DEL-05` (channel guidance). All three enter through `d` and share the **who / when / channel** prompt sequence and the `DELEGATED_TO` + WAIT vocabulary. Must feel identical: the assignee prompt wording, the channel sub-prompt (this doc places DEL-05's channel question *inside* stage 1, so DEL-05 gets it "for free"), and the follow-up/Waiting-For view. DEL-03's project-collapse produces a stage-2 Waiting-For; DEL-04's stage 2 must produce the byte-identical property shape so both land in the same review. The build-route split these three ways (§7/§3#9/§5) — from a UX standpoint they are **one flow**; confirmed, not broken.
- **Reuse vs. new** — Reused as-is: the `d` entry, who/when prompts, `org-gtd--dispatch`, keyword mapping (`:state :next`/`:wait` → NEXT/WAIT), WIP buffer, engage blocks. Genuinely new: the *stage prompt*, the `ORG_GTD_DELEGATE_STAGE` marker, and the stage-detecting branch in `org-gtd-delegate--organize`.
- **Release tag** — leans on `[R]` (delegate disposition, clarify, engage, keyword mapping) and `[U]` (view-DSL prefix labeling, `:organize-fn` slot for the branch). No `[R]` rework beyond adding a branch to `org-gtd-delegate--organize`, justified on GTD fidelity: the current one-step flow mislabels un-handed-off items as Waiting-For.

### Type / extension-UX opportunities

Two real ones. **(1) An enum/choice property descriptor.** `ORG_GTD_DELEGATE_STAGE` is a fixed small set (`planned`/`delegated`) — today's descriptor `:type` supports only `text`/`timestamp`/`repeating-timestamp`, so this feature hand-rolls the choice prompt. Adding a `:type choice :options (...)` descriptor `[U]` would let this, X-04's `ENERGY`, and future enums reuse one prompt idiom. **(2) Intra-type state progression as a first-class type concept.** DEL-04 reveals that a type may want an ordered `:states` progression (planned → delegated) with a per-stage keyword and per-stage prompt subset, rather than the single `:state :wait`. Formalizing `:states` on the type descriptor — and generating the "advance" behavior from it — would make this a declarative type feature instead of bespoke logic, and would feed the "generate the transient/flow from the registry" goal the primer flags as a prime type-UX target.

## 6. Edge cases & failure modes

- **Empty state** — none applies; every plan-to-delegate starts from a real heading.
- **Re-running `d` on a `delegated` item** — treat as edit: re-prompt who/when (no stage regression). Re-running on a `planned` item → stage-2 completion. Never silently create a duplicate.
- **Marking the stage-1 NEXT "done" via plain `C-c C-t`/agenda** (user bypasses stage 2) — item would archive as if completed. Guard: `org-gtd-delegate--organize` leaves the `planned` marker so an `:before-file` hook `[U]` can warn — *"This delegation hasn't been handed off yet; complete the hand-off (`d`) or trash it"* — in the teaching-error voice, non-fatal.
- **Stale plans** — a `planned` item sitting for weeks is a legitimate stuck signal; it already surfaces on the next-action list and is a natural candidate for a future `reflect-stuck-*` extension. No crash, just visibility.
- **Bad channel input** — free-text with completion; unknown channel is accepted (DEL-05 is guidance, not a gate).

## 7. Open questions & maintainer decisions

- **Stage-1 keyword**: NEXT (my recommendation — the hand-off is a genuine next action) vs. a dedicated `PLAN`/`TODO` keyword. NEXT reuses existing machinery and the next-action list; a new keyword needs `org-gtd-keyword-mapping` surgery `[R]`. Maintainer call.
- **Stage-2 trigger**: re-run `d` (my choice, keeps one key) vs. a dedicated `org-gtd-delegate-handoff` command surfaced as an agenda action. The latter is more discoverable but adds a command.
- **Should stage 1 record a "requested/planned on" date** (parallel to WF-14's "date recorded") for stuck-detection? Likely yes via `ORG_GTD_CAPTURED_AT`-style stamp.
- **Property name**: `ORG_GTD_DELEGATE_STAGE` vs. folding into the existing `:state` slot — depends on whether the maintainer adopts the `:states` progression (opportunity #2).

## 8. Provenance & links

`REC-DEL-04` · deliverable-#3 status **Not-implemented** (`tool/may`, DA94-39) · `gap-implementation-strategies.md` §7 line 264 (build route: plan-to-delegate pre-state on IMPL-027 + IMPL-025) · workflows: `organize.feature` WF-14 (Waiting-For list management) · siblings: `REC-DEL-03`, `REC-DEL-05` (Cluster C); channel prompt shared with DEL-05 · type-UX ties to `REC-X-04` (enum descriptor).
