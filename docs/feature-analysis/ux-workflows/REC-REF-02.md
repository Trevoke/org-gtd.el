# UX Workflow — REC-REF-02 · Guided three-phase Weekly Review

`REC-REF-02` · "Walk me through my whole Weekly Review — Get Clear, Get Current, Get Creative — one step at a time, so I trust the system again." · cluster: `A — guided review & sweep sessions`

> **Wave-0 note.** This doc defines the **reusable guided-session-engine contract** that REF-06, CAP-09 and X-15 (Cluster A) — and the recovery/mind-sweep flows — inherit. Design the engine here; describe REF-02 as its first profile.

---

## Implementation status (2026-07-08) — IMPLEMENTED (lean)

REC-REF-02 was implemented in the checklists + guided-review work (PR #294,
unmerged at time of writing; not yet in a release). The adjudicated design is
`docs/plans/2026-07-06-checklists-and-guided-review-design.md` — **read its §8**
for exactly what shipped. A **lean** engine landed; the full console described in
§3–§6 below is mostly deferred (see "Still open").

- **Implemented (lean):** a guided multi-phase session (`M-x org-gtd-review`,
  autoloaded, `org-gtd-review.el`); configurable **profiles**
  (`org-gtd-review-profiles` defcustom, with the Weekly Review as the default
  value); step types **`prompt` / `command` / `view` / `checklist`**; session
  keys **`n s p q`**; phase checkpoint messages; **pause/resume**; simple
  completion counts (steps done/skipped); REF-01's recurring-reminder rider via
  `org-gtd-review-schedule` (creates a Weekly Review habit). Home: a
  command-center `w · Weekly Review (guided…)` row.
  - *Step-type divergence from this doc:* this doc names types
    `prompt`/`action`/`view`/`walk`; shipped types are
    `prompt`/**`command`**/`view`/**`checklist`**. `command` is this doc's
    `action`. The `checklist` step walks item **strings** from a `checklists.org`
    template (CHK-01) — **not** org headings; the org-heading `walk` step is
    deferred (see below).
- **Rejected — do not re-litigate (design doc §8):** org-edna involvement in the
  engine; a *hidden* `.review-state.el` — the shipped state file is the **visible
  `review-state.eld`** in `org-gtd-directory`.
- **Still open (deferred, valid pulls — everything in §3–§6 beyond the lean cut):**
  - the **`walk` step type** — iterating *org headings* (projects, someday items)
    one at a time in WIP buffers with per-item actions (`c`/`x`/`d`) and the
    no-next-action **invariant guard** (§3 walk step, §4 mock, §6 guard). The
    shipped `checklist` step walks item *strings*, not headings; the org-item walk
    is the missing piece REF-06 and the someday-review generalization both need.
  - the **stats block** (the X-15 completeness readout, §4), the
    **review-completion log**, the **back-step `b`**, and in-session **`,`
    customize**.
  - generalizing **`org-gtd-someday-review`** into a profile (it is untouched).
  - **cadence-ladder profiles** (daily/monthly/quarterly/annual — REF-05/WF-22);
    ship as documented examples first.
  - action bars generated from **`:allowed-actions`** (the §5 registry-parity
    idea); steps currently declare behavior via `:type` only, and there is no
    per-step action bar yet.
- **Contract for Cluster A siblings** (REC-REF-06, REC-CAP-09, REC-X-15): the
  engine they inherit is **profiles + typed steps + `n s p q` + pause/resume** —
  *not* the full console (stats block, review log, back-step, generalized
  someday-review) this doc describes. REF-06/CAP-09 become "just another profile"
  only once the `walk` step type lands; X-15 still needs the stats block.

---

## 1. The need (what & why)

- The Weekly Review is GTD's keystone ("an unused system is not a system"), yet org-gtd today only offers a scatter of independent `org-gtd-reflect-*` views. The user must *remember the whole ritual* and run each piece by hand — exactly the mental overhead the system is meant to remove. This feature turns the ritual into one guided session ending in the honest claim: *"I know everything I'm not doing but could be."*
- Hit **weekly** (WF-21), at a 1–2h executive session; the engine also backs the daily/monthly/quarterly/annual cadence ladder (WF-22).
- Source: `REC-REF-02` (Not-implemented; adjudicated 2026-06-05 #1 IMPLEMENT — configurable phases, pluggable trigger-list, named cadence profiles) — "the single biggest UX gap vs. the books."

## 2. Entry points & discovery

- **Invoke** — `M-x org-gtd-review` (optional `PROFILE` arg). From the command center: a new **Reflect** row `w · Guided review…` opens a profile picker [R surface: `org-gtd-command-center`].
- **Resume** — if a session was paused, `org-gtd-review` detects saved state and offers *Resume where you left off / Start over*.
- **Discover** — the command-center Reflect group is where users already look for review actions; the `w` mnemonic sits beside the existing per-view rows (`a y d r R`). REF-01's recurring reminder (rider) can drop a "Weekly Review due" tickler that, when engaged, calls the same command — discovery via the agenda they already read.

## 3. Full-lifecycle walkthrough

> **Partly superseded (2026-07-08).** The lean session shipped, but much of this walkthrough is **deferred, not built** (see the Implementation status block + design doc §8): the running stats block, `walk` steps over org headings with per-item `c`/`x`/`d` actions, the back-step `b`, in-session `,` customize, and the review-completion log. The pause state file shipped as the **visible `review-state.eld`**, not the hidden `.review-state.el` named here (org-edna is not involved).

**Primary path (Weekly profile):**

1. `org-gtd-review` → profile picker (only if >1 profile configured); Enter picks **Weekly**. Window config is snapshotted.
2. The **`*GTD Review*` console** opens full-frame: profile name, a **phase tracker** (Get Clear ▸ Get Current ▸ Get Creative), a **step tracker** (Step 1/12), and a **running stats block** (see §4). First step is shown with its instruction.
3. Each step declares a *type* and its allowed actions; the user acts, then `n` advances:
   - **prompt step** ("Gather loose papers, receipts, cards into the inbox") → checkbox; `n` marks done.
   - **action step** ("Empty inbox to zero") → `n` launches `org-gtd-process-inbox`; on completion control returns to the console, inbox-count stat now 0.
   - **view step** ("Review Waiting-For") → embeds the matching `org-gtd-reflect-*` view read-only; browse, then `n`.
   - **walk step** ("Projects, one by one") → walks each active project in a WIP buffer with the shared per-item action bar; the **invariant guard** (§6) blocks advance until each project has a next action.
4. Phase boundaries show a checkpoint ("Get Clear complete — 3 items captured, inbox at zero. Continue to Get Current?").
5. **Get Creative** ends with the someday sweep (reuses `org-gtd-reflect-someday-review` as an embedded walk) and a creative-capture prompt. Final screen: the completeness readout (X-15 stat) + "Review complete: 14 reviewed, 4 clarified, 2 captured." Completion is logged; window config restored.

- **Create / start** — `org-gtd-review`; profile picker. A *profile* is a named phase→step sequence (defcustom, §5).
- **See / preview** — the console; stats and trackers update **live** after every action (§4).
- **Edit / reconfigure** — `,` in-console opens `customize` on the current profile's step list; or edit `org-gtd-review-profiles`. Skip a step this run with `s`; the profile is untouched.
- **Save / name / recall** — `p` **pauses**: session state (position, stats) is written to `org-gtd-directory/.review-state.el`; `org-gtd-review` later offers Resume. Completed runs append to a **review log** (date, profile, stats), recalled via a completed-items-style view. Named profiles *are* the saved sequences.
- **Delete / undo / back out** — `b` steps back (re-do a step); `q` quits with a choice *Pause & keep / Abandon*; Abandon discards session state and restores windows. No destructive edits happen without the per-step action the user chose.
- **Repeat / recur** — each profile carries a `:cadence` (weekly, daily, monthly…); REF-01's rider schedules the reminder. The cadence ladder = several profiles, each run at its interval.

## 4. Interaction sketch

> **Partly superseded (2026-07-08).** Shipped session keys are `n s p q` only. The `b` back-step, `,` customize, the stats block, and the `walk`-step action bar (`c`/`x`/`d`) shown below are **deferred** — see design doc §8.

**Console mock (a walk step, mid-session):**
```
┌ *GTD Review* — Weekly Review ───────────────────────────────────────┐
│ Phase:  [✓ Get Clear]  ▸ Get Current ◂  [ Get Creative ]            │
│ Step 8/12 · Projects, one by one                                    │
│ ── Stats ──────────────────────────────────────────────────────    │
│  reviewed 9   clarified 3   captured 2   inbox 0                     │
│  next-actions in system: 42   ⚠ below 50 — system may be thin       │
│ ─────────────────────────────────────────────────────────────────  │
│  Ensure every active project has a next action.                     │
│                                                                     │
│  ** Ship v5 release notes            [Project]                      │
│     - [ ] (no NEXT action found)  ⚠ add one before continuing       │
│                                                                     │
│  Project 3 of 7                                                     │
├─────────────────────────────────────────────────────────────────────┤
│ [c] Clarify  [x] Add next action  [s] Skip  [b] Back  [p] Pause  [q]│
└─────────────────────────────────────────────────────────────────────┘
```
Header-line advertises the step's keys live, exactly as `org-gtd-someday-review` does.

**Keymap (the shared session vocabulary):**

| key | action | scope |
|-----|--------|-------|
| `n` / `SPC` | do this step / advance | every step |
| `b` | back to previous step | every step |
| `s` | skip this step (this run only) | every step |
| `p` | pause — persist state, restore windows | session |
| `q` | quit — Pause or Abandon prompt | session |
| `,` | customize this profile | session |
| `c` | clarify current item (→ WIP + organize) | walk steps |
| `x` | add next action to current item | walk steps |
| `d` | defer current item | walk steps |

**Live preview** — before: stats show `inbox 12`; after the "empty inbox" action step returns, the block redraws `inbox 0` and `captured` ticks up; the phase tracker checkmark flips when the last step of a phase is done.

## 5. Fit with org-gtd

- **Extends** — the **reflect guided session engine** `org-gtd-someday-review.el` [R] (IMPL-084): its `--state` plist (`:queue/:position/:reviewed/:clarified`), per-item WIP buffer [R] (`org-gtd-wip--get-buffer`), read-only mode + header-line idiom, and defer/clarify/quit action set. Generalized to a **sequence of phases, each a sequence of typed steps** (prompt/action/view/walk), each step carrying `:allowed-actions`. Content pulls existing `org-gtd-reflect-*` views [R] and `org-gtd-process-inbox` [R]. Home: `org-gtd-command-center` [R].
- **Shared surface / cluster** — Cluster A siblings **inherit this engine**: REF-06 (maintenance step-list) and CAP-09 (trigger-list prompt-walk) are just *other profiles*; X-15 is the completeness stat rendered in the **stats block** above. What must feel **identical**: the console layout (phase/step trackers + stats block), the session key vocabulary (`n b s p q`), the per-item walk action bar (shared with Cluster B's accept/veto/skip and with `someday-review`), and window-config restore on finish/abort. The stats-block format is the single canonical widget.
- **Reuse vs. new** — *Reused as-is:* WIP buffer, reflect views, process-inbox, someday-review walk, command-center. *Genuinely new:* the phase/step orchestration layer, the console with trackers, pause/resume persistence, the review-completion log, and the profile defcustom.
- **Release tag** — everything leaned on is **[R]** (4.6.1). The one **rework of [R]**: generalizing `org-gtd-someday-review` into `org-gtd-review` (engine) with someday-review re-expressed as a one-phase profile. Justification: **GTD-fidelity** (the book's ritual is inherently multi-phase and org-gtd has no orchestration) plus **UX** (removes duplicate loops — every guided gap is this loop with different content). Keep `org-gtd-reflect-someday-review` as a thin alias for back-compat.

### Type / extension-UX opportunities

> **Deferred (2026-07-08).** Action bars generated from `:allowed-actions` did **not** ship; steps declare behavior via `:type` only, and there is no per-step action bar yet (it belongs with the deferred `walk` step). See design doc §8.

- **A step-type registry parallel to the GTD type registry.** Steps have a `:type` (prompt/action/view/walk) and `:allowed-actions`; the action bar should be **generated from `:allowed-actions`**, not hand-authored — directly addressing the primer's "three-places-kept-in-sync-by-hand" smell that also afflicts the organize transient. Building this generator here proves the pattern the organize transient should later adopt.
- **Profiles are a named-object collection** — same shape as Cluster E's view/checklist managers. Ship the profile store as a plain `defcustom` now, but flag it so E's CRUD-manager idiom can later wrap it (`list → create → preview → edit → save → delete`) without rework.
- **Walk-step defer/clarify should route into the organize transient / `:organize-fn` slot [U]**, so "clarify from a review" and "clarify from the inbox" are the same disposition surface — one dispatch, not two.

## 6. Edge cases & failure modes

- **Empty state** — inbox already at zero / no projects: the step shows "Nothing here — nice" and auto-satisfies; `n` skips forward. A brand-new user with an empty system can still run the ritual and see every step.
- **Invariant guard** — a project with no next action **blocks `n`** (teaching message, not error) until the user adds one via `x` or explicitly `s`-skips with an acknowledgment — enforcing WF-21's "every active project leaves with a next action."
- **Large data** — many projects/someday items: the walk paginates one-at-a-time (already the engine's model); the step tracker shows `Project 3/40` so scope is honest.
- **Interrupted / crash** — WIP buffers are file-backed and auto-saved; `.review-state.el` persists position, so a crash mid-review resumes cleanly. Abandoning is always explicit.
- **When it goes wrong** — stays in the teaching voice: header-line hints, a checkpoint message per phase, never a stack trace; a step whose backing view errors logs via `message` and lets the user `s`-skip rather than aborting the whole session.

## 7. Open questions & maintainer decisions

- **Console vs. transient** for the session shell: a dedicated `*GTD Review*` buffer (proposed, matches someday-review) vs. a persistent transient. Buffer wins for embedding views/WIP; confirm.
- **Bundled vs. authored profiles:** the Weekly profile ships built-in; do daily/monthly/quarterly/annual ship as ready profiles or as documented examples? (Ties to REF-05 cadence ladder.)
- **Completion-tracking depth** (REF-01 rider): a simple date log vs. a per-phase completeness record feeding a "streak"/coverage view.
- **Trigger-list step** depends on CAP-09/CHK-01 (checklist type). Until then, the Get-Clear mind-sweep step is a plain prompt; wire it to trigger-lists when CHK-01 lands.

## 8. Provenance & links

- `REC-REF-02` · Not-implemented · adjudicated 2026-06-05 #1 IMPLEMENT (+REF-01/04/07/08 riders) · build route: `gap-implementation-strategies.md` §3 #1 (generalize `org-gtd-someday-review` IMPL-084 into a phase-driven engine), §2 (command-center IMPL-135 as hub), phase content = reflect views IMPL-074–082.
- Workflows: `WF-21` (Weekly Review: Get Clear/Current/Creative), `WF-20` (daily orientation profile), `WF-22` (cadence ladder).
- Cluster A siblings: `REC-REF-06`, `REC-CAP-09`, `REC-X-15` (this doc sets their contract). Adjacent: Cluster B (`REC-CLA-10`, `REC-UI-04`) shares the per-item accept/veto/skip action bar; Cluster E owns the future profile-CRUD manager.
