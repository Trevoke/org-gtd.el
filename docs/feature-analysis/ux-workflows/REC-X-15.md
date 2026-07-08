# UX Workflow — ≥50-Actions Completeness Heuristic

`REC-X-15` · "Does my system actually hold everything, or is my head still doing the remembering?" · cluster: `A — guided review & sweep sessions`

> **Scope note (demoted).** This is *not* a session of its own. It is the **completeness readout** — one line in the running-stats widget the Cluster-A session engine already draws. Everything below designs that stat: how it's computed, phrased, thresholded, and surfaced. No standalone walkthrough, because there is no standalone flow.

---

> **Corpus note (2026-07-08).** REC-REF-02's engine shipped **lean** (PR #294) and **does not yet include the running stats block** this readout lives in — the stats block is **deferred**, and completion shows simple step counts only. X-15 remains unimplemented and still needs the stats block built first. See `docs/plans/2026-07-06-checklists-and-guided-review-design.md` §8.

## 1. The need (what & why)

- GTD's rule of thumb: a working system for an engaged adult holds **roughly 30–100 discrete next actions** (Allen's "if you have fewer than ~50, you're probably not writing everything down"). A low count is a *smell*, not a failure — it usually means commitments are still living in your head instead of in the system.
- The user hits this **during a review** (Cluster-A sessions) and at **onboarding** — the two moments they're asking "is my inventory trustworthy yet?" A raw inbox count (IMPL-132) answers "what's unprocessed," never "is the whole thing complete."
- Source: `REC-X-15` (Not-implemented; `methodology/may`; audit add) — "docs-only candidate or trivial REF-02 stat." We build the trivial REF-02 stat, done right.

## 2. Entry points & discovery

- **Invoke (primary):** none — it *appears automatically* as a line in the session stats widget (widget 2 of the shared console) the moment any Cluster-A session runs. The user does nothing to summon it.
- **Invoke (standalone peek):** `M-x org-gtd-completeness` — a one-shot `message`/minibuffer readout of the same number, for users who want the gauge without opening a review.
- **Discover:** it is simply *there*, on every review, next to reviewed/clarified/captured counts. The number is its own teacher — a `23 / 50` with a soft nudge line explains itself. No manual required.

## 3. Full-lifecycle walkthrough

This is a **derived stat**, so most CRUD verbs collapse. The heading stays; the one-liner says why.

- **Create / start** — never authored. Computed on session start and after every action: count entries where the next-action skip predicate says "include" (`ORG_GTD=Actions`, TODO not done) across `org-agenda-files`, via `org-map-entries`. That count is `N`.
- **See / preview** — the readout renders in the stats widget as a single line with a state glyph, and **redraws live** after every walk-step action (per the shared contract's "redraws live"). Three states off `org-gtd-completeness-threshold` (default 50):
  - `N < ~½ threshold` → **sparse**: gentle nudge to keep sweeping.
  - `~½ ≤ N < threshold` → **building**: encouraging.
  - `N ≥ threshold` → **healthy**: quiet check-mark, no nag.
- **Edit / reconfigure** — the *only* thing to edit is the bar itself: `M-x customize-variable RET org-gtd-completeness-threshold` (default `50`), or the in-session `,` (customize-profile) key already in the session vocabulary. Setting it to `nil` **hides the readout entirely** for users who reject the heuristic.
- **Save / name / recall** — N/A as a named object (nothing to persist). It *is* recorded passively: the session **completion log** (shared Cluster-A infra) stamps the day's `N`, so `over time` the user can see inventory grow. No recall UI of its own.
- **Delete / undo / back out** — N/A. Turn it off with `threshold = nil`; there is nothing to delete. It never mutates data, so nothing to undo.
- **Repeat / recur** — recurs for free: every review redraws it; the completion log gives the longitudinal view.

## 4. Interaction sketch

The readout lives inside the **already-mandated** three-widget console. Only widget 2's completeness line is new; shown in context:

```
┌─ *GTD Review* ───────────────────────────────────────────────┐
│ Phase 2 of 3  → Get Current        Step 4/9                   │  ← widget 1 (tracker)
├──────────────────────────────────────────────────────────────┤
│ reviewed 12   clarified 3   captured 5   inbox 2             │
│ next actions  23 / 50  ◐  inventory building — anything      │  ← widget 2 (stats)
│               still living only in your head?                 │     [the X-15 line]
├──────────────────────────────────────────────────────────────┤
│ n/SPC next · b back · s skip · c clarify · p pause · q quit   │  ← widget 3 (header-line)
└──────────────────────────────────────────────────────────────┘
```

Glyph vocabulary: `○` sparse · `◐` building · `✓` healthy. Line collapses to `next actions  61 / 50  ✓` (no nudge text) once healthy, so a good system is quiet.

Standalone peek:
```
M-x org-gtd-completeness  →  minibuffer:
  "Next actions in system: 23 / 50 (◐ building). A trusted system usually
   holds 30–100; a low count often means commitments are still in your head."
```

**Keymap** — the stat adds *no* keys of its own; it rides the shared session vocabulary:

| key | action | source |
|-----|--------|--------|
| `,` | customize profile → reach `org-gtd-completeness-threshold` | [session, shared] |
| `n`/`SPC` `b` `s` `p` `q` | advance/back/skip/pause/quit (each redraws the line) | [session, shared] |
| `M-x org-gtd-completeness` | one-shot standalone readout | new, this feature |

**Live preview:** the "before/after" *is* the redraw — the user clarifies an item into a next action (`c` → organize → `s`), the count ticks `23 → 24`, glyph may flip `◐→✓`, nudge text may disappear. That immediate feedback is the whole reward: sweeping visibly moves the gauge.

## 5. Fit with org-gtd

- **Extends** — the **Cluster-A guided-session-engine** stats widget (widget 2), set by `REC-REF-02`; and the **reflect** family's existing running-stats idiom from `org-gtd-reflect-someday-review`. Computation reuses the **skip-predicate** machinery (`org-gtd-skip.el` next-action closure) + `org-map-entries` over `org-agenda-files`. No transient, no WIP buffer, no new type.
- **Shared surface / cluster** — the stats block **MUST look and behave identically** across every Cluster-A member (REF-02, REF-06, CAP-09) — same widget, same position, same redraw cadence. X-15 owns exactly one line in it. The number's *voice* (teaching, non-nagging) must match Cluster B's teaching-error tone so a low count reads as guidance, never scolding.
- **Reuse vs. new** — reused: the console, the stats widget, the redraw hook, the completion log, the `,` customize path. Genuinely new: (a) the next-action *count* computation, (b) the threshold defcustom, (c) the tri-state phrasing, (d) the tiny `org-gtd-completeness` standalone command.
- **Release tag** — session engine + stats widget are `[U]` (REF-02, unreleased HEAD — fair game for v5). Skip-predicate closures and `org-agenda-files` iteration are `[R]` (4.6.1). No `[R]` rework needed — this only *adds* a line to an `[U]` surface.

### Type / extension-UX opportunities

- The count is defined *entirely* by "which types are next-actions." That predicate should come from the **type registry** (types carrying an `:counts-as-next-action` / `:actionable` flag) rather than a hard-coded `ORG_GTD=Actions` test — so a user-defined actionable type (via the resurrected `[X]` `define-type`) is counted automatically. This nudges the registry toward carrying **semantic role flags**, which also cleans up the DSL's `next-action` filter and the engage view. **Worth capturing:** one `:actionable` slot serves the stat, the filter, and any future type — a small, high-leverage type-UX refinement.

## 6. Edge cases & failure modes

- **Empty state (fresh onboarding):** `N = 0` → `next actions 0 / 50 ○ — nothing captured yet; a first sweep usually surfaces 30–100.` Encouraging, never an error.
- **Threshold `nil`:** line is omitted entirely; no glyph, no gap — the widget simply has one fewer row. Honors users who reject the heuristic.
- **Huge system / slow count:** counting is `O(headings)` over agenda files; if it exceeds a small budget it's computed **once on session start** and only re-diffed by the walk-step deltas (+1 on clarify-to-action) rather than re-scanning every keystroke — no perceptible lag, and the stat can never block a review.
- **When it goes wrong:** a failed count degrades to `next actions  … / 50` (dim ellipsis) with a `message` in the log — the session continues. Fail-soft, matching org-gtd's "errors teach, never crash" rule; a broken gauge must never abort a review.

## 7. Open questions & maintainer decisions

- **Default threshold:** `50` (Allen's canonical number) vs a softer band label ("aim 30–100"). Proposed: keep `50` as the numeric bar, phrase the nudge as a range.
- **What counts:** next-actions only (proposed), or next-actions + waiting-for + calendar? GTD's "50" is specifically *next actions*; recommend keeping it narrow.
- **Standalone command:** ship `M-x org-gtd-completeness`, or leave it session-only? Low cost; recommend shipping it.
- **Longitudinal view:** is a "your inventory over time" readout from the completion log in-scope for v5, or deferred? Recommend deferred — the passive stamp is enough for now.

## 8. Provenance & links

- `REC-X-15` · Not-implemented (`methodology/may`, audit add) · `gap-implementation-strategies.md` §1 (author-content / docs) + build route "§ `org-map-entries` / skip-predicate count" · related `WF` reflect.feature (Weekly Review invariants, l.94/121) · cluster **A** siblings `REC-REF-02` (session-engine contract, authoritative), `REC-REF-06`, `REC-CAP-09` · consumes the stats-widget contract set by REF-02.
