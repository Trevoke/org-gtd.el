# UX Workflow — Concreteness Check (anti-degradation)

`REC-CLA-10` · catch next-actions & projects that have decayed into vague "stuff" and prompt to re-concretize them · cluster: `B — flag → decide → consent`

---

## 1. The need (what & why)

GTD's core discipline is that every open loop is worded as a concrete outcome or a physical next action ("Call plumber re: kitchen leak"), not a topic ("kitchen", "plumber", "stuff"). Entries decay: a crisp action gets edited down to a noun, a project is renamed to a theme, or a hastily-clarified item was never concrete to begin with. Nothing in org-gtd catches this — stuck/metadata views (IMPL-075–077) flag missing *metadata*, never vague *phrasing*. The user hits it during the Weekly Review, when a list of "actions" turns out to be a list of subjects they can't actually *do*. This feature is the wording-quality sweep: detect degraded entries, walk them one at a time, and prompt the user to re-concretize — never rewriting anything without consent.

Source: `REC-CLA-10` (Not-implemented, `methodology/should`) · GTD "next action must be the physical, visible activity" · natural rider on the REF-02 guided review.

## 2. Entry points & discovery

- **Invoke (standalone)** — `M-x org-gtd-reflect-vague-items`. Runs the flag→decide→consent session over the whole GTD file.
- **Invoke (in-review)** — appears as a **phase inside `org-gtd-reflect` (REF-02)**: after "Get Current," a "Concreteness" phase walks the same flagged list with the same action bar. No separate launch.
- **Invoke (command-center)** — `org-gtd-command-center` → Review System → **`V` Vague / degraded items**. Sits beside `S` stuck and `M` missed. `[R]`
- **Discover (opportunistic warn)** — an **[U] `:after-organize` hook** (`org-gtd-concreteness-warn`) runs the same lint on the item just organized; if the title reads as vague it drops a *header-line* nudge — "Heads up: 'taxes' has no verb — `M-x org-gtd-reflect-vague-items` to re-concretize." Observe-only (per the hook contract: warn/annotate, never gate), so the user meets the concept in the flow they already run, no manual required.

## 3. Full-lifecycle walkthrough

Primary path — **standalone concreteness sweep**:

- **Create / start** — `M-x org-gtd-reflect-vague-items`. A **detection pass** scans every active next-action and project heading and runs the concreteness lint (`org-gtd-concreteness-lint`): each heading returns either `nil` (concrete) or a *reason* string ("no leading verb", "single bare word", "matches vague pattern: look into"). Flagged headings become the session's walk list. If the list is empty the session ends immediately with "Every action reads as concrete — nice." and restores your windows.
- **See / preview** — the session opens **full-frame in the *GTD Review* console** with the three fixed Cluster-A widgets: the phase/step tracker (*Concreteness · item 3/11*), the running stats block (*flagged 11 · re-concretized 2 · kept 1 · skipped 0*, redrawn after every action), and the header-line advertising the current keys. The body shows the flagged heading, **why it was flagged**, and a suggested reframing where one can be inferred.
- **Edit / reconfigure** — the per-item **action bar** (identical to someday-review and to sibling UI-04): **`r` re-concretize** accepts the prompt and drops you into a one-line minibuffer pre-filled with the suggested wording — edit and `RET` renames the heading in place; **`c` clarify** routes the item into the full **clarify WIP buffer + organize dispatch** for a real rework (rename, split into a project, add a next action); **`k` keep** vetoes the flag (see below); **`s` skip** leaves it for this run only.
- **Save / name / recall** — `k` (keep-as-is) sets **`ORG_GTD_CONCRETE: kept`** on the heading, so the lint suppresses it on future runs — the user's judgement is remembered, not re-litigated every week. `p` **pauses**: session progress persists to the shared state file so a later Resume picks up mid-list. There is no named object to save — the "collection" is derived fresh each run.
- **Delete / undo / back out** — `b` steps **back** to revisit the prior decision; a re-concretize rename is an ordinary org edit (`undo` works). To *un-keep* an item (make it eligible again), clear its `ORG_GTD_CONCRETE` property — surfaced via `,` (customize) → "Reset kept items." `q` **quits** (Pause/Abandon prompt); the window configuration snapshotted at start is **restored** on finish or abort. Nothing is ever renamed or deleted without the user pressing `r`/`c`.
- **Repeat / recur** — designed to run every Weekly Review, standalone or as the REF-02 phase. `kept` annotations and the vague-pattern defcustom make each run quieter and better-tuned over time.

## 4. Interaction sketch

```
┌─ *GTD Review* ───────────────────────────────────────────────┐
│ Phase 4/6  Concreteness            Step 3 of 11               │  ← tracker widget
│ flagged 11 · re-concretized 2 · kept 1 · skipped 0           │  ← stats widget (live)
├──────────────────────────────────────────────────────────────┤
│  ⚠ This entry reads as "stuff", not a doable action:         │
│                                                               │
│      * NEXT  Mom                                              │
│      why:  no leading verb · single bare word                 │
│                                                               │
│  Suggested outcome:                                           │
│      "Plan Mom's 60th birthday party"                         │
│                                                               │
│  What's the very next physical action?                        │
├──────────────────────────────────────────────────────────────┤
│ r re-concretize   c clarify (full)   k keep as-is   s skip     │  ← action bar
│ n/SPC next  b back  p pause  q quit  , customize             │  ← header-line
└──────────────────────────────────────────────────────────────┘

  press r ─▶  Re-word action: Plan Mom's 60th birthday party▎   (minibuffer, editable)
  press c ─▶  opens clarify WIP buffer → C-c C-o organize dispatch
  press k ─▶  sets ORG_GTD_CONCRETE: kept  (won't flag again)
```

**Keymap**

| key | action |
|---|---|
| `r` | re-concretize — accept prompt, edit suggested wording in minibuffer, rename in place |
| `c` | clarify — route item into clarify WIP + organize dispatch for full rework |
| `k` | keep as-is — set `ORG_GTD_CONCRETE: kept`, suppress future flags (the veto) |
| `s` | skip this run — leave unchanged, re-eligible next run |
| `n` / `SPC` | advance to next flagged item |
| `b` | back to previous item |
| `p` | pause — persist session state, restore windows |
| `q` | quit — Pause/Abandon prompt, restore windows |
| `,` | customize profile — edit vague-pattern list, reset kept items |

**Live preview** — after `r`, the stats block re-renders (`re-concretized 3`) and the tracker advances; the renamed heading is visible in place on the next `n`. No standing preview pane (this is a triage loop, not a CRUD manager).

## 5. Fit with org-gtd

- **Extends** — the **[U] guided session engine** (inherited from REF-02) as its console and action-bar host; the **[R] reflect family** (`org-gtd-reflect-*`) for naming and command-center placement; the **[R] clarify WIP + organize dispatch** (IMPL-007/018) as the `c` deep-rework path; **[R] property descriptors** for the `ORG_GTD_CONCRETE` suppression flag; an **[U] `:after-organize` hook** for the opportunistic warn. Detection is a new pure predicate (`org-gtd-concreteness-lint`) modeled on the stuck-view detectors (IMPL-075–077).
- **Shared surface / cluster** — **Cluster B (flag → decide → consent)**, with sibling **REC-UI-04** (overdue calendar items). The **detection-pass → one-at-a-time walk → accept/veto/skip** shape must be *identical* to UI-04, to Cluster A's `someday-review`, and to the REF-02 walk-step action bar — same console, same three widgets, same `s`/`p`/`q`/`b` vocabulary, same "never auto-act, always teach" voice. Only the flag reason (vague phrasing vs. overdue date) and the accept verb (`r` re-concretize vs. UI-04's re-schedule) differ. This **confirms** the build-route hypothesis that scattered CLA-10 (Clarify §2) under a single triage surface.
- **Reuse vs. new** — reused: the session engine, WIP/organize dispatch, property idiom, window restore, pause/resume state file. Genuinely new: the concreteness lint predicate and its `org-gtd-concreteness-vague-patterns` / `org-gtd-concreteness-min-words` defcustoms.
- **Release tag** — leans mainly on **[U]** (session engine, command-center, hooks) — fair for v5. The one **[R]** surface reused as-is (clarify WIP) is not reworked, only entered.

### Type / extension-UX opportunities

1. **A computed `vague` filter type in the view DSL** — expose the lint as a first-class computed type (alongside `stuck-*`) in `org-gtd-view-lang--known-filter-keys`, so users can build their own "vague actions" block without this session. Cheap, and it makes the lint composable.
2. **Per-type `:concreteness-fn` slot** — a project's concreteness rule differs from a next-action's ("Look into X" and "R&D X" are *valid* project names but vague action names). A per-type slot lets each type define its own lint, and cleanly prevents the false-positive on legitimate research-project names. This is a small, natural extension of the per-type `:organize-fn` slot pattern.
3. `ORG_GTD_CONCRETE` demonstrates a reusable **"user-judged suppression" property descriptor** other flag-loops (UI-04) can adopt — worth promoting to a shared descriptor.

## 6. Edge cases & failure modes

- **Empty state** — nothing flagged: session ends at once with "Every action reads as concrete — nice." and restores windows. No blank console.
- **False positives** — legitimate GTD names ("R&D new CRM", "Resolve situation with Carolyn Jones") can trip a naive verb check. Mitigations: the vague-pattern list is a tunable defcustom; the per-type `:concreteness-fn` exempts research/process projects; and `k` (keep) permanently silences any single entry. The lint biases toward *under*-flagging.
- **Bad input / large data** — thousands of headings: detection runs once up front over the file; the walk is bounded by the flagged count, not the file size. Renames are ordinary org edits.
- **When it goes wrong** — a lint error on one heading is logged via `message` and that heading is skipped, never aborting the session (fail-soft). A rename collision or empty new title re-prompts in the minibuffer rather than erroring. Every flag explains *why* in plain English ("no leading verb"), never a regex dump — teaching, not scolding.

## 7. Open questions & maintainer decisions

- **Default vague-pattern set** — ship an opinionated list ("look into" *as action*, "stuff", "misc", bare single nouns, trailing "…"/"?") or start conservative and let users grow it? Recommend conservative + documented examples.
- **Verb detection** — heuristic leading-verb check (word-list) vs. no verb check at all (patterns only)? A verb list risks locale/false-positive noise; patterns are safer but miss bare nouns. Leaning pattern-first with an *optional* verb check off by default.
- **Should `c` (full clarify) re-run the lint on the reworked title before advancing**, closing the loop immediately? Nice, but adds a mid-walk detour — defer unless cheap.
- **Command-center key** — `V` proposed; confirm no collision.

## 8. Provenance & links

`REC-CLA-10` · deliverable-#3 status **Not-implemented** (`methodology/should`) · `gap-implementation-strategies.md` §build-route: "Stuck/metadata views (IMPL-075–077) as detection scaffold; add a concreteness prompt" · workflow `WF-10` (outcome reframing / project identification, `clarify.feature`) · Cluster **B** sibling `REC-UI-04`; inherits the Cluster **A** guided-session-engine contract from `REC-REF-02`, into which it also embeds as a phase.
