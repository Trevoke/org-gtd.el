# UX Workflow — Delegate a whole project

`REC-DEL-03` · *"Someone else now owns this entire multi-step project — I want to hand it off and stop steering it, without losing the plan I built."* · cluster: `C — delegate-flow enrichments`

---

## 1. The need (what & why)

- In GTD, delegating means the outcome is still yours to *track* but the *doing* is someone else's. Today org-gtd only delegates a **single action**. When you've already broken an outcome into a project (purpose, next actions, an edna chain), and then hand the whole thing to a colleague, you're forced to either leave a live project cluttering your engage view or manually tear the breakdown down. Neither is right: you want **one Waiting-For line to check in on**, with the **full breakdown preserved losslessly** for the day it bounces back.
- Hit during **clarify/organize** (a captured "have Dana run the Q3 migration" that's really a project) or later during **reflect** (a project you were driving that you now offload).
- Source: `REC-DEL-03`, deliverable-#3 **Not-implemented**, **Adjudicated 2026-06-05 #6: IMPLEMENT (reframed)** — the missing seam between item-delegation (IMPL-025) and project state-snapshot (IMPL-059/064).

## 2. Entry points & discovery

- **Invoke** — the organize transient's existing **`d` (delegate)** key `[U]`, pressed while the item at point is a **project heading** (`ORG_GTD=Projects`). Reached the same three ways delegate always is: inside the clarify WIP buffer (`C-c C-o` → `d`), from `org-gtd-organize` standalone, or on a project row in an agenda/engage view. No new command, no new key.
- **Discover** — *automatic*, because the dispatcher already routes on project-ness. Today, pressing `d` on a project raises `"Type delegated does not support project-level handling"` (it has no `:organize-project-fn`). Filling that slot turns that dead end into the feature — the affordance the user already reaches for simply starts working. The transient row label gains the hint *"delegate (whole project if on a project)"*.

## 3. Full-lifecycle walkthrough

**Create / start.** Point on the project heading, `d`:
1. org-gtd detects `ORG_GTD=Projects` and routes to `org-gtd-process-project` with the `delegated` type (the same router that already sends someday/tickler to their project handlers).
2. The **shared Cluster-C prompt sequence** runs against the *project heading*: **Who will do this?** (`DELEGATED_TO`) → **When to check in?** (`ORG_GTD_TIMESTAMP`) → *(DEL-05)* **channel** hint line.
3. A **lossless snapshot** is taken: `org-gtd-project--save-state` stamps every heading + task (`ORG_GTD_STATE`, todo keyword, cookies), exactly as project-someday does.
4. **Collapse:** the project heading becomes the single Waiting-For — `ORG_GTD=Delegated`, state `WAIT`, `DELEGATED_TO`/`ORG_GTD_TIMESTAMP` set, and a logged note *"delegated to Dana"*. Every child task is frozen out of the engage flow (its live NEXT/TODO cleared, snapshot retained), so exactly **one** line surfaces.

**See / preview.** Inside the clarify WIP buffer the collapse happens in view: the breakdown folds under the now-Waiting-For heading. Header-line confirms *"Delegated whole project to Dana — check in <2026-07-18>. Breakdown preserved."* On file/close the window config is restored and the item lands via the delegated refile target ("Actions").

**Edit / reconfigure.** Change owner or check-in date by re-running `d` on the (still one-line) delegated project, or edit `DELEGATED_TO`/`ORG_GTD_TIMESTAMP` directly. The snapshot is untouched.

**Save / name / recall.** Persists as a normal delegated item — so it appears in the **delegated follow-up block of `org-gtd-engage`** and `org-gtd-reflect-upcoming-delegated`, identical to a delegated single action. *Recall the plan* = the snapshot lives in the subtree; *recall to active* = below.

**Delete / undo / back out.** When the project bounces back (or you take it over again), **`org-gtd-reactivate`** `[R]` restores every heading's saved state via `org-gtd-project--restore-state`, rebuilds the NEXT/TODO chain, and drops the delegation — the full breakdown returns exactly as it was. Mid-clarify, `C-c C-k` aborts with no change to source.

**Repeat / recur.** N/A — a one-time hand-off; recurrence is a habit concern.

## 4. Interaction sketch

```
 Clarify: ★ Q3 data migration            [ORG_GTD=Projects]
   ├─ NEXT  Export legacy tables
   ├─ TODO  Transform schema
   └─ TODO  Cut over & verify
 ───────────────────────────────────────────────────────────
 C-c C-o →  Organize this item
   Actionable   q quick  s single-action  [d] delegate  c calendar  h habit
   Project      p new project             a add-to-existing
   Non-action   i tickler  y someday  k knowledge  t trash
 ───────────────────────────────────────────────────────────
 d →  Who will do this?          Dana⏎
      When to check in?          2026-07-18⏎
      Track via (email>note>voice, C-h for why):  email⏎    ← DEL-05
 ───────────────────────────────────────────────────────────
 WAIT ★ Q3 data migration   :DELEGATED_TO: Dana  <2026-07-18>
        :LOGBOOK:  delegated to Dana
        (3-task breakdown snapshotted, frozen)
```

| key | action |
|-----|--------|
| `C-c C-o` | open organize transient (from clarify WIP) `[U]` |
| `d` | delegate — **whole project when on a project heading** `[U]` |
| `C-h` (at channel prompt) | why trackable channels matter (DEL-05) |
| `C-c C-k` | abort clarify, source untouched `[R]` |
| `M-x org-gtd-reactivate` | restore the frozen breakdown, un-delegate `[R]` |

**Live preview:** as the collapse commits, the WIP buffer visibly folds the three tasks under the single `WAIT` line and the header-line swaps to the delegation confirmation — the before/after is the fold itself.

## 5. Fit with org-gtd

- **Extends** — the **organize transient delegate disposition** (`org-gtd-delegate`, `:organize-fn org-gtd-delegate--organize`) by adding the missing **`:organize-project-fn`** to the `delegated` type; the **project state-snapshot** machinery (`org-gtd-project--save-state`/`--restore-state`, `org-gtd-project-someday` as the template, IMPL-059/064); and the **clarify + WIP** surface. Recall rides the existing `org-gtd-project-reactivate` / `org-gtd-reactivate`.
- **Shared surface / cluster** — Cluster **C** with **REC-DEL-04** (plan-to-delegate pre-state) and **REC-DEL-05** (channel guidance). This must feel **identical** to delegating a single action: same `d` key, same *who → when → channel* prompt order, same `DELEGATED_TO`+`WAIT` vocabulary, same delegated follow-up view. The *only* difference a user should perceive is the collapse+snapshot — everything framing it is the one delegate flow. I **confirm** the C clustering from a UX standpoint: the whole-project variant is not a separate command, just the project branch of the same disposition.
- **Reuse vs. new** — Reused as-is: the delegate prompts, the snapshot save/restore, reactivate, the dispatch router (`org-gtd-process-project`), the delegated refile/engage surfaces. Genuinely new: one `:organize-project-fn` (`org-gtd-delegate--organize-project`) that wires the who/when prompt to the collapse — the "seam."
- **Release tag** — Leans on `[U]` `:organize-project-fn` slots + organize transient (HEAD) and `[R]` snapshot/reactivate (4.6.1). No `[R]` rework: the `d` key and reactivate keep their exact current semantics; we only fill an empty slot.

### Type / extension-UX opportunities

This feature is a **clean proof of the slot model**: adding a whole new project-level disposition required **zero transient edits and zero help-text edits** — the `d` key and dispatcher already exist; the type just lacked `:organize-project-fn`. That's the type UX working as intended, and worth documenting as the canonical example. It does surface one refinement: **reactivate is written around tickler/someday assumptions** yet the snapshot format is type-agnostic. v5 should make `org-gtd-reactivate` **restore any snapshotted project regardless of which disposition froze it** (delegated included), and consider a type slot `:reactivate-fn` / a shared `:snapshot t` marker so "freeze a project losslessly, restore it later" is **one reusable capability** three dispositions (someday, tickler, delegated) opt into — rather than each re-implementing the collapse. That unification is the type-UX win this feature reveals.

## 6. Edge cases & failure modes

- **Empty state** — heading isn't actually a project (no `ORG_GTD=Projects`, no subtasks): `d` behaves as ordinary single-action delegation, silently. No snapshot, no ceremony.
- **External dependencies** — another live project depends on a task here. Reuse `org-gtd-project--check-external-dependencies`'s teaching prompt: *"External tasks depend on this project: … Continue delegating the whole thing?"* — `yes-or-no-p`, default safe, `user-error "Delegate cancelled"` on veto (identical voice to project-someday).
- **Multi-project tasks** — a task shared with another *active* project is **not** frozen (same skip rule as someday): *"Skipping multi-project task (other active projects): …"* via `message`, so the other project keeps running.
- **Missing owner** — `DELEGATED_TO` is `:required`; empty input re-prompts, never commits a nameless delegation.
- **When it goes wrong** — all fail-soft: consent prompts, logged `message`s, header-line confirmation. No stack traces; an abort leaves the source project fully intact because the collapse only commits after the prompts succeed.

## 7. Open questions & maintainer decisions

- **Frozen-task representation:** set child tasks to `ORG_GTD=Delegated` too (uniform, but pollutes delegated counts), or a neutral frozen marker like someday's? Recommend the **someday-style freeze** so only the project heading counts as one Waiting-For.
- **Re-delegate vs. reactivate-then-redelegate** when the owner changes — is editing `DELEGATED_TO` in place enough, or should owner-change re-log? Recommend in-place edit + a fresh log note.
- **DEL-04 interaction:** should a *plan-to-delegate* project (DEL-04 pre-state) be collapsible the same way, or only committed delegations? Defer to the C synthesis pass.

## 8. Provenance & links

`REC-DEL-03` · deliverable-#3 **Not-implemented** · adjudicated 2026-06-05 #6 IMPLEMENT (reframed) · build route: `gap-implementation-strategies.md` §3#9 "Add a delegate↔projects seam" (reuse IMPL-059/064 snapshot + IMPL-025 delegation) · siblings `REC-DEL-04`, `REC-DEL-05` (Cluster C) · pattern source `org-gtd-project-someday` / `org-gtd-project-reactivate` (`org-gtd-projects.el:1228`, `:1279`).
