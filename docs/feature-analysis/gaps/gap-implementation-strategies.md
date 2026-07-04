# Gap Implementation Strategies

> Companion to deliverable #3 (`gaps/recommended-not-implemented.md`). Produced 2026-06-11; §4 revised 2026-07-03 (maintainer package review — no new dependencies).
>
> **Purpose.** Deliverable #3 says *which* recommended features are unbuilt and *how big* the gap is. This document asks the next question: **by what route would we build each one?** Every gap is sorted into the kind of work it actually needs — content to author, mechanics to reuse, core code to reshape, or an external package to lean on — so that planning a feature starts from "what does this really cost, and what does it touch" rather than from scratch.
>
> **Scope.** All 73 gaps (30 Partial + 21 Via-config + 22 Not-implemented) from deliverable #3, plus the closed/non-goal/deferred entries listed once in the parking lot (§11) for completeness. IDs are the `REC-*` ids from `recommended/INDEX.md`; reused mechanics cite `IMPL-*` ids from `implemented/INDEX.md`. Prior dispositions (`V-xx`, `2026-06-05 #n`) are carried through but **not re-litigated** — they describe *whether* to build; this doc describes *how*.
>
> **A feature appears in as many lists as apply.** Most gaps need several kinds of work (e.g. REC-REF-02 needs authored checklists *and* a reused session engine *and* a core refactor). The lists are lenses, not bins.

## How to read this

Each list answers one planning question:

1. **Supporting materials** (§1) — *What content/data must a human author before code can ship?* Trigger lists, checklists, prompt sets, docs prose. Often the true blocker: the code is trivial once the list exists.
2. **Reusable mechanics** (§2) — *What already-built machinery is the natural backbone?* Maps each gap to the `IMPL-*` it should extend rather than reinvent.
3. **Core modifications** (§3) — *Which gaps are best served by reshaping an existing subsystem* so the feature falls out naturally, instead of bolting it on?
4. **External Emacs packages** (§4) — *Which gaps want a published package — and should org-gtd depend on it?* Maintainer-reviewed 2026-07-03; **verdict: no new dependencies** — reuse / implement-ourselves / adapter-layer / recommend-in-docs instead.
5. **Other technical/usability categories** (§5–§10) — documentation-only, config/DSL-recipe, data-model/schema additions, new first-class types, hook/automation surface, UX-orchestration.
6. **Parking lot** (§11) — non-goals, dropped mandates, deferred/backlog.
7. **Uncategorized** (§12), then a primary-route cross-reference.

---

## §1 — Supporting materials needed (author-first)

These are *content artifacts*, not code. For several gaps (especially everything under the V-10 checklist umbrella) the authored list **is** the feature — once it exists, the code to surface it is small. Grouped by artifact.

### Trigger lists & checklists (the V-10 / V-24 corpus)

| Artifact | Feeds | Notes |
|---|---|---|
| **Incompletion Trigger List** (canonical mind-sweep prompt set: personal/professional, by area) | REC-CAP-06, REC-CAP-09, REC-X-15 | The single most-referenced missing artifact. Drives guided mind-sweep and "what's true now" sweeps. |
| **Six-Horizons review prompts** (Ground → 50k, one prompt block per altitude) | REC-CAP-06, REC-REF-05, REC-HOR-03/04/05 | Pairs with the horizons file (IMPL-103). |
| **Project Planning Trigger List** (brainstorming prompts) | REC-PRJ-07 | Allen's planning-step prompts. |
| **Project-identification verb checklist** (Finalize / Implement / Research / Maximize / Organize / Design / Roll-out / Update / …) | REC-PRJ-10 | Verb starters for naming projects. |
| **Weekly Review checklist** (Get Clear / Get Current / Get Creative step lists) | REC-REF-01, REC-REF-02, REC-REF-07 | The phase content the REF-02 engine walks through. |
| **System-maintenance review checklist** (purge, archive, file-integrity steps) | REC-REF-06 | Distinct from the weekly review; the maintenance cadence. |
| **Annual / year-end stock-taking checklist** (accomplishments review → goal-setting) | REC-REF-10 (V-25) | Feeds the optional annual flow. |
| **Recurring reflection-prompt list content** (sample prompt lists to resurface) | REC-CHK-02 | Becomes turnkey once V-10 + REC-TIC-02 land. |

### Docs prose & helper text

| Artifact | Feeds | Notes |
|---|---|---|
| **Natural Planning Model helper text** (purpose → vision → brainstorm → organize → next actions) | REC-PRJ-06 (V-04) | Helper-text now; structured schema is v5. Authoring the prose is the near-term deliverable. |
| **Trackable-channel guidance** (email > written note > voice…) | REC-DEL-05 | Docs-only candidate; one paragraph in the delegate flow/manual. |
| **Three-modes-of-work framing** (predefined / ad-hoc / defining work) | REC-ENG-06 (V-03) | Docs-only; the three commands already exist, the manual must name them as the three modes. |
| **Credo / personal-standards practice** (how to use horizons H5 as a decision filter) | REC-HOR-06 (#4) | Docs-only; document the credo practice. |
| **Top-down-prioritization explainer** (horizons replace ABC coding) | REC-HOR-08 | Docs territory; reasoning is human. |
| **R&D / "look into" project convention** | REC-PRJ-08 | Convention docs; nothing structural needed. |
| **Three-honest-exits framing** (renegotiate / complete / drop a broken agreement) | REC-CLA-11 | Decision-prompt copy for the stale-item surface. |
| **2-minute-threshold wording** (cheat-sheet text only — the tool enforces no cutoff) | REC-CLA-09 | IMPL-017 wording tweak. |
| **A–Z reference index scheme + yearly-purge policy** | REC-KNO-02, REC-KNO-05 | The filing/purge *policy* to document alongside any index code. |
| **≥50-actions completeness heuristic** (onboarding/review diagnostic copy) | REC-X-15 | Docs-only candidate, or a one-line stat in REF-02. |
| **Onboarding / migration meta-project content** (re-feed-old-lists script) | REC-X-10 (V-12) | The meta-project body text for the backlog scaffold. |

### DSL recipe examples (ship as docs + optionally as named views)

These are *achievable today* via the view-language DSL (IMPL-085/090/093); the missing deliverable is a worked example (and possibly a bundled named view).

| Recipe | Feeds |
|---|---|
| Dashboard spec (core lists + calendar-day block) | REC-UI-01 |
| "Weird-time" micro-window view `(effort . (< "0:10"))` | REC-ENG-08, REC-ENG-05 |
| Named project sub-lists, event-date grouped | REC-X-09 (V-13) |
| Informal "if I have time" list (tag + DSL view) | REC-NXT-04 |
| Per-person view via `(who . …)` + tags | REC-AGE-01, REC-AGE-02 |
| Per-area / per-horizon rollup composites | REC-UI-16 |
| Custom sort via `(native …)` escape hatch | REC-UI-08 |

---

## §2 — Implemented mechanics we can reuse

Each gap mapped to the existing `IMPL-*` machinery that is its natural backbone. "Reuse" ranges from *call it directly* to *extend its data model*.

### High-leverage mechanics (each backs several gaps)

- **Guided someday-review session engine (IMPL-084)** — the one-at-a-time, defer/clarify/quit, per-list, stats-tracking interactive loop. This is the **template for every guided walkthrough**: → REC-REF-02 (the three-phase review), REC-REF-04 (recovery flow), REC-CLA-11 (three-exits surface), REC-CAP-06 (driven mind-sweep). Generalizing it is the core move (see §3 #1).
- **View-language DSL (IMPL-085 + multi-block IMPL-093 + filters IMPL-090 + prefix chains IMPL-094)** — composes nearly every "view" gap: → REC-UI-01 (dashboard), REC-ENG-08/05 (effort views), REC-X-09 (sub-lists), REC-NXT-04 (if-I-have-time), REC-AGE-01/02 (per-person), REC-UI-16 (rollups), REC-REF-07 (coverage sweep). The `(native …)` escape hatch (IMPL-092) reaches sorting for REC-UI-08.
- **Type registry + organize pipeline (IMPL-034 + IMPL-035)** — adding a GTD type is declarative. Backs: REC-CHK-01 (checklist type), REC-AGE-03 (meeting type), REC-NXT-05 (read/review subcategory), REC-X-04 (energy attribute descriptor), REC-KNO-04 (reference category).
- **Named someday lists (ORG_GTD_SOMEDAY_LIST, IMPL-033)** — the named-list hook reused for: REC-NXT-05 (Read/Review subcategory, per V-06), REC-NXT-04 (if-I-have-time list), REC-CHK-02 (reflection-prompt lists).
- **Tickler future-date + resurface (IMPL-032)** and **Habit recurrence (IMPL-029)** — the scheduling primitives behind: REC-TIC-02 (recurring ticklers + lead-time), REC-CHK-02 (resurfacing prompt lists), REC-REF-01/05/06 (recurring review events), REC-PRJ-12 (project→habit).
- **Reflect views (IMPL-073–083) + stuck/missed detection (IMPL-053, 074–077, 081)** — the phase *content* for REC-REF-02, the coverage guarantee for REC-REF-07, the raw signal for REC-UI-02 (proactive flags) and REC-CLA-10 (degradation catch).
- **Refile-target auto-creation (IMPL-037, property-driven, multi-value)** — the filing engine to repoint at *topics* for REC-KNO-04/05 (reference index) instead of the chronological archive. *(Deferred: reference is now an adapter-layer question — §4; this reuse applies only if org-gtd ever builds an internal store.)*
- **Hook surfaces — six-stage observation hooks (IMPL-136), classic organize hooks (IMPL-021), state-change bookkeeping (IMPL-134), org-edna triggers (IMPL-045/057)** — the automation seams for: REC-CAP-08 (boundary prompts), REC-UI-10 (closure loops), REC-UI-05 (mark-done prompt), REC-PRJ-12 (project→habit), REC-UI-11 (rules engine, already exposed).

### Per-gap reuse map (remaining)

| Gap | Reuse |
|---|---|
| REC-DEL-03 (delegate whole project) | Project incubate w/ snapshot (IMPL-059, IMPL-064) + item delegation (IMPL-025) — wire the seam between them. |
| REC-DEL-04 (two-stage delegation) | WAIT→delegate conversion (IMPL-027) + delegate state (IMPL-025) — add a "plan-to-delegate" pre-state. |
| REC-UI-02 (integrity flags) | Mode-line inbox-count refresh machinery (IMPL-132) — generalize to stuck/missed counts. |
| REC-UI-05 (closure prompt) | State-change hooks (IMPL-134) + edna auto-advance (IMPL-045) — fire on chain-exhaustion. |
| REC-REF-09 (time-blocking) | Agenda transient date-setters (IMPL-068/069) + calendar create (IMPL-028) — add a review step. |
| REC-UI-04 (auto-migration w/ consent) | Missed views (IMPL-074/081) + agenda date-setters (IMPL-068/069) — add the prompt/action. |
| REC-PRJ-04 (subproject rollup / parallel wiring) | Multi-project membership (IMPL-044), DAG deps (IMPL-043), graph editors (IMPL-114–118) — add rollup affordance + parallel default. |
| REC-PRJ-06 (NPM) / REC-KNO-06 (forms) | Project templates (IMPL-018) + capture templates (IMPL-002) — template-driven scaffolds. |
| REC-HOR-03/04/05 (horizon linkage) | Areas-of-focus link + organize hook (IMPL-099–102) + horizons file (IMPL-103) — add linkage hooks. |
| REC-HOR-07 / REC-UI-16 (cross-horizon view) | Horizons outline (IMPL-103) + per-area multi-block review (IMPL-073) — compose Ground/Projects in. |
| REC-ENG-03/05 (criteria scaffolding) | Effort attribute (IMPL-068/090), context tags (IMPL-066), priority opt-in (IMPL-068) — add energy + ordering. |
| REC-KNO-02 (purge / fast filing) | Archive datetree (IMPL-063) + tickler-scheduled purge (IMPL-032). |
| REC-PRJ-09 (active/archive compartments) | Configurable archive datetree (IMPL-063) + org subtree structure. |
| REC-UI-13 (retrospective log) | Yearly archive (IMPL-063) + auto-CLOSED stamps (IMPL-133) + org-agenda log mode. |
| REC-UI-12 (print/export, briefs) | Graph exports (IMPL-125) + `org-agenda-write`; person briefs depend on REC-AGE-02. |
| REC-UI-17 (project ledger) | Graph task states (IMPL-106), captured-at (IMPL-003), area link (IMPL-101), cookies (IMPL-052), clock query (IMPL-054). |
| REC-UI-03 (coaching dialogs) | Teaching errors (IMPL-041, IMPL-139), guidance header (IMPL-008), cheat-sheet (IMPL-017). |
| REC-REF-02 hub | Command-center transient (IMPL-135) as the entry point. |
| REC-CLA-10 (anti-degradation) | Stuck/metadata views (IMPL-075–077) as the detection scaffold; add a concreteness prompt. |

---

## §3 — Best implemented via core modifications

These gaps are *cheaper and cleaner* if we reshape an existing subsystem so the feature is a natural part of the flow — rather than appending a parallel feature. Each names the **subsystem to reshape** and **why the refactor pays off**.

1. **Generalize the someday-review session engine → a reusable "guided session" framework.**
   *Reshape:* `org-gtd-someday-review.el`'s session loop (IMPL-084) into a phase-driven walkthrough that takes a sequence of (view, prompt, allowed-actions). *Unlocks naturally:* REC-REF-02 (Get Clear/Current/Creative as three phases composing IMPL-073–083), REC-REF-04 (recovery as a phase profile), REC-CAP-06 (mind-sweep as a phase), REC-CLA-11 (three-exits as session actions). *Why:* every "guided" gap is the same loop with different content; building REF-02 standalone would duplicate IMPL-084. Deliverable #3 already flags a **"reflect-stuck-* consolidation refactor"** under REC-REF-06.

2. **Promote checklists/trigger-lists to a first-class type in the registry + pipeline.**
   *Reshape:* add a checklist type to IMPL-034/IMPL-035 so trigger lists are organize destinations and reviewable lists, not a bolt-on. *Unlocks:* REC-CHK-01 and everything it gates — REC-CAP-06, REC-PRJ-07, REC-PRJ-10, REC-CAP-09, REC-CHK-02. *Why:* one type definition makes all bundled trigger lists (the §1 corpus) live in the system the same way every other list does. (The per-checkbox reset-on-DONE behavior is implemented **in-house** — org-checklist *pattern*, no org-contrib dependency; see §4.)

3. **Unify tickler & habit recurrence; add lead-time to ORG_GTD_TIMESTAMP.**
   *Reshape:* the tickler type (IMPL-032) currently can't recur because it has no TODO state, so recurrence lives only in habits (IMPL-029). Give ticklers a repeater path and a DEADLINE-style warning period, and teach the engage view (IMPL-065) / skip predicates (IMPL-088) to surface "N days ahead." *Unlocks:* REC-TIC-02 directly; strengthens REC-CHK-02 and recurring reviews (REF-01/05/06). *Why:* avoids a third scheduling mechanism; consolidates two near-duplicate ones.

4. **Reference disposition — deferred to the reference-adapters brainstorm.**
   *Status (2026-07-03):* org-gtd will **not** build its own topical reference store. The `'reference` disposition today just archives to a yearly datetree (IMPL-030 + IMPL-063). The intended direction is an **adapter layer** over the user's notes system (denote / org-roam / org-brain), so REC-KNO-02/04/05/06 are **deferred to a dedicated brainstorm** (see §4) — *not a near-term core refactor.* (Supersedes the earlier sketch of repointing the disposition at an internal topic tree via IMPL-037.)

5. **Fold context grouping into the default engage view.**
   *Reshape:* the default engage view (IMPL-065) leads with calendar then a flat next-action list; context filtering is a separate command (IMPL-066). Make context folding part of the default view, with post-completion "what remains today." *Unlocks:* REC-ENG-02 (must), partially REC-ENG-03. *Why:* it's a behavior of the existing view, not a new view. (Grouping is supplied by the view DSL itself; org-super-agenda is a user-side layering option, not a dependency — see §4.)

6. **Extend the timestamp model for one-date semantics + a distinct start-by slot.**
   *Reshape:* the single ORG_GTD_TIMESTAMP model (IMPL-069) + type validation (IMPL-075) to add a start-by slot and guard against org date-soup (SCHEDULED+DEADLINE+timestamp). *Unlocks:* REC-UI-06. *Why:* start-by is a property of the core date model; can't be added at the edges.

7. **Consolidate where areas & horizons are declared, then add linkage hooks.**
   *Reshape:* unify the areas-of-focus pattern (IMPL-099–102) and the horizons file (IMPL-103) into one declaration surface; hang optional project↔goal↔vision linkage off it. *Unlocks:* REC-HOR-03/04/05 (adjudication #5 explicitly calls for "unify where areas are declared"). *Why:* linkage needs a single source of truth for the horizon ladder.

8. **Generalize the inbox-count lighter into a proactive integrity monitor.**
   *Reshape:* the mode-line count + periodic refresh (IMPL-132) to also compute stuck-project / missed-calendar / overdue-delegated counts and surface them. *Unlocks:* REC-UI-02; operationalizes REC-PRJ-02 and REC-REF-08. *Why:* the refresh/display machinery already exists; this is a generalization, not a new subsystem.

9. **Add a delegate↔projects seam.**
   *Reshape:* a new organize path that collapses a decomposed project to one Waiting-For while snapshotting the breakdown — reusing project-incubate's lossless snapshot (IMPL-059, IMPL-064) and item delegation (IMPL-025). *Unlocks:* REC-DEL-03 (adjudicated IMPLEMENT). *Why:* it's a missing seam *between* two subsystems; best added as a first-class flow, not a manual procedure.

10. **Add a chain-exhaustion closure prompt to state-change bookkeeping.**
    *Reshape:* the TODO-change hooks (IMPL-134) + edna auto-advance (IMPL-045) to detect "project's last task just completed" and prompt "complete? what's next?" *Unlocks:* REC-UI-05. *Why:* the event is already observed for cookie/cancel bookkeeping; the prompt rides the same hook.

---

## §4 — External Emacs packages

> **Maintainer review, 2026-07-03.** Walked package-by-package with the maintainer. **Verdict: org-gtd adopts no new external package dependency.** Every gap here resolves to one of: the existing view DSL + skip predicates (§2/§3), a small bespoke implementation org-gtd owns, an org-native built-in, a *deferred adapter layer* (reference & contacts — each its own brainstorm), or a package we merely **recommend to users in the docs** — never hard-depend on.

**Why so few dependencies.** Three constraints decide almost every case:

1. **Display-layer control.** The org-gtd view-language DSL exists specifically to control `org-agenda-prefix-format` (see `org-gtd-view-language.el`). Any package that renders its *own* buffer outside org-agenda (org-ql, calfw, org-timeblock) can't honor that prefix formatting — so it's a downgrade, not a reuse.
2. **Don't force a UI/environment.** Grouping, search, notifications, calendar sync are user-environment choices. org-gtd emits standard org data; users layer their preferred package on top (org-super-agenda, consult, alert.el, org-gcal) with nothing in org-gtd's way. Bundling would impose a stack.
3. **Don't take a dependency for trivial code.** Where the behavior is ~a few lines (review-due properties, checkbox reset), owning it insulates org-gtd from upstream churn.

### Reuse — already a dependency

- **org-edna** (already declared, 1.1.2) — its `RESET` action + trigger/blocker DSL supplies **heading-level re-arm** for recurring checklists and reflection prompts. Covers **REC-CHK-02, REC-PRJ-07, REC-PRJ-10**. Zero new dependency. *(Per-checkbox reset is a separate, bespoke piece — below.)*

### Implement ourselves — pattern borrowed, no dependency

- **Review-due tracking** (pattern from `org-review`) — own the `LAST_REVIEW` / `REVIEW_DELAY` property convention + a "due for review" surface built on the view DSL + skip predicates. Covers **REC-REF-05, REC-REF-06**. Part of the **Reflect** flow (per *Making It All Work*, Allen renames GTD's "Review" phase to **Reflect**; the weekly *ritual* is still idiomatically the "Weekly Review"). *Rejected as a dependency:* too small to accept upstream-churn risk.
- **Per-checkbox reset on DONE** (pattern from `org-checklist`) — a state-change-hook function that walks the subtree and clears `[ ]` boxes. Covers **REC-CHK-01/02, REC-CAP-06**; composes with org-edna's heading-level re-arm. *Rejected as a dependency:* `org-checklist` ships only inside **org-contrib**, which is **not bundled with Emacs** (split out at Org 9.5) and is a loosely-maintained grab-bag — disproportionate for ~100 lines.

### Deferred to dedicated brainstorms (adapter layers, not package picks)

- **Reference / knowledge — whole REC-KNO-\* cluster.** org-gtd has no real reference system today (the `'reference` disposition just archives, same destination as trash). Intended direction is an **adapter layer** over the user's notes system — **denote / org-roam / org-brain** — not a bundled store or one hardcoded paradigm. *Its own brainstorm; out of scope here.* (Supersedes the earlier §3 #4 "internal topical filing" sketch.)
- **Contacts — contact resolution for REC-AGE-\*.** org-gtd owns the delegate act + `DELEGATED_TO`, and per-person lists (**REC-AGE-01/02**) build off that string via the DSL — **no adapter needed for the lists.** But *resolving/picking* a contact is the user's system — **bbdb / ebdb / org-contacts** — via a future **contacts-adapter** layer. *Its own design; deferred.*

### Recommend to users in docs — never a dependency

These work on org-gtd's standard org output today; a "pairs well with org-gtd" documentation section is the right home.

- **org-super-agenda** — grouped agenda sections. The user sets `org-super-agenda-groups` + mode; it wraps `org-agenda-finalize-entries`, the same machinery org-gtd already feeds (`org-agenda-custom-commands` / `org-agenda-finalize-hook`). Nothing blocks it. org-gtd's *own* grouping uses the DSL. (REC-UI-01/08, X-09, AGE-02 grouping is user-side.)
- **consult / consult-org** — live-preview navigation/search (**REC-UI-09**). Users layer their own completion framework (consult / helm / ivy); org-gtd depends on no completion stack. Native agenda search is the baseline.
- **calfw + calfw-blocks** — graphical month/week/day calendar (**REC-UI-18**). Bypasses our display layer; original repo unmaintained (use a fork). Inspiration + doc recommendation only.
- **org-timeblock** (`ichernyshovvv/org-timeblock`) — visual day/week time-blocking, drag-to-reschedule (**REC-REF-09**). **On MELPA** (<https://melpa.org/#/org-timeblock>). Still bypasses org-agenda/prefix-format → doc recommendation, not a dependency.
- **org-gcal / org-caldav** — external calendar sync. No org-gtd action resolves against the calendar backend (entries are already org entries the user's sync tool handles) — fully user-side, doc note only.
- **alert.el** — OS-level notifications, e.g. for a finished timer. Notification delivery is the user's environment; org-gtd's timer core stays org-native.

### Org-native — the real backing (no package at all)

- **REC-UI-15** timer (incl. 2-min): `org-timer-set-timer`; `org-timer-default-timer` → 2 for one keystroke.
- **REC-UI-17** project ledger: `org-columns` roll-ups + graph data (IMPL-106) for the query half.
- **REC-CAP-09** user-definable capture forms: `org-capture` templates.
- **REC-CAP-08** capture at activity boundaries: `org-clock-out-hook`.
- **REC-UI-04** auto-migrate overdue calendar → Next Actions (with consent): `org-agenda` bulk + `org-map-entries`; bespoke consent logic.
- **REC-X-04** energy attribute: an `ENERGY` property / tag, filterable via agenda + the DSL.
- **REC-REF-01** recurring Weekly Review: a repeating `SCHEDULED: +1w` heading.
- **REC-X-15** ≥50-action count: `org-map-entries` / skip-predicate count.

### Rejected outright (with rationale — do not resurface)

- **org-ql** — *nonstarter.* Standalone rendering ignores `org-agenda-prefix-format`; the author has a years-old open issue signalling he won't/can't add it — which is **why the org-gtd view DSL was built**. Its query half (`org-ql-select`) is redundant with `org-gtd-skip.el`'s predicate closures.
- **org-pomodoro / pomm.el** — Pomodoro is a work-rhythm *methodology* GTD doesn't prescribe; importing it adds an opinion the source material doesn't ask for. `org-timer` is the whole answer for REC-UI-15. Users can wire it in themselves.
- **org-superstar** — styles org bullet glyphs; unrelated to any gap (was wrongly suggested for REC-UI-04).
- **org-hyperscheduler** — browser-UI day planner; breaks the in-Emacs model.
- **khardel** — khard/vCard contact cards, not meetings; superseded by the deferred contacts-adapter question.

### §4 verdict table

| Package | Verdict |
|---|---|
| **org-edna** | **Reuse** (already a dep) — heading-level re-arm |
| org-review | Implement ourselves (pattern) — Reflect flow |
| org-checklist | Implement ourselves (pattern; org-contrib not bundled) |
| denote / org-roam / org-brain | **Deferred** → reference-adapters brainstorm |
| bbdb / ebdb / org-contacts | **Deferred** → contacts-adapter design |
| org-super-agenda | Doc recommendation (user-side layering) |
| consult / consult-org | Doc recommendation (user-side) |
| calfw + calfw-blocks | Doc recommendation / inspiration |
| org-timeblock | Doc recommendation / inspiration (on MELPA) |
| org-gcal / org-caldav | Doc note (user-side) |
| alert.el | Doc note (user-side) |
| org-timer / org-columns / org-capture / org-clock-out-hook / org-agenda | Org-native (no package) |
| **org-ql** | **Rejected** — prefix-format nonstarter |
| org-pomodoro / pomm.el | **Rejected** — not a GTD requirement |
| org-superstar | **Rejected** — irrelevant |
| org-hyperscheduler | **Rejected** — wrong paradigm |
| khardel | **Rejected** — wrong target |

---

## §5 — Documentation-only (no code)

Ship a doc, not a feature. (Several also need a §1 artifact authored.)

| Gap | Disposition | Artifact dependency |
|---|---|---|
| REC-ENG-06 | V-03 docs-only | §1 three-modes framing |
| REC-HOR-06 | #4 docs-only | §1 credo practice |
| REC-HOR-08 | Docs territory | §1 top-down explainer |
| REC-PRJ-08 | Convention docs | §1 R&D convention |
| REC-DEL-05 | Docs-only candidate | §1 channel guidance |
| REC-X-09 | V-13 docs example | §1 DSL recipe |
| REC-CLA-09 | Wording tweak | §1 2-min wording |
| REC-X-15 | Docs-only candidate | §1 ≥50 heuristic |
| REC-X-14 | Sync-mechanism docs (mobile front-end deferred) | — |

---

## §6 — Config / DSL-recipe only (achievable today, no org-gtd code)

The Via-config set: works now through the DSL, org-edna, or org-native structure. Deliverable = a documented recipe (§1) and *optionally* a bundled named view.

REC-NXT-04, REC-PRJ-09, REC-ENG-08, REC-UI-01, REC-UI-08, REC-UI-09, REC-UI-11, REC-UI-12, REC-UI-13, REC-UI-14, REC-UI-15, REC-UI-18, REC-REF-05 (cadence via user items), REC-CHK-02 (tickler/habit body), REC-HOR-03/04/05 (H-headings exist), REC-X-09.

> Tension to note: shipping these as *named views* (rather than leaving them as recipes) is itself a small build and moves them toward §2/§4. The decision per item is "recipe in docs" vs. "bundled view."

---

## §7 — Data-model / schema additions (new property/state, minimal UI)

Small core additions: a new property, state, or type descriptor — little or no new UI.

| Gap | Addition |
|---|---|
| REC-X-04 | Optional energy property/tag (V-07, off by default) — slot in type registry (IMPL-034); org-native storage. |
| REC-DEL-04 | "Plan-to-delegate" pre-state ahead of WAIT/DELEGATED_TO. |
| REC-UI-06 | Distinct start-by date slot on the timestamp model (also §3 #6). |
| REC-TIC-02 | Recurrence repeater + lead-time on ticklers (also §3 #3). |
| REC-NXT-05 | Read/Review as a someday subcategory (V-06; reuses IMPL-033). |
| REC-UI-17 | Stakeholder field + due-date-first project sort key. |

---

## §8 — New first-class objects / types (larger builds)

A genuinely new kind of thing in the system (type, object, or index). Higher cost; usually wants §2 reuse + a §1 artifact.

| Gap | New object | Backbone |
|---|---|---|
| REC-CHK-01 | Checklist / trigger-list type | Type registry (§3 #2) + in-house per-checkbox reset (§4) |
| REC-AGE-03 | Meeting lifecycle object (upcoming → occurred → outcomes to inbox) | Type registry + closure hooks; bespoke (§4) |
| REC-KNO-05 | Browsable categorized reference index | **Deferred** → reference-adapters brainstorm (§4); not built in-house |
| REC-NXT-05 | Read/Review queue (if its own type rather than a someday subcategory) | Type registry |
| REC-PRJ-06 | NPM project schema (structured planning) | v5 per V-04; templates as the near-term stand-in |

---

## §9 — Hook / automation surface (closure loops & boundary prompts)

Gaps whose essence is *firing the right thing at the right moment* — they live on org-gtd's hook surfaces (IMPL-021, IMPL-134, IMPL-136, IMPL-045/057).

| Gap | Hook |
|---|---|
| REC-CAP-08 | Capture prompt at workflow-exit boundaries (host on IMPL-136 stages; org-native `org-clock-out-hook` for the call/session case — §4). |
| REC-UI-10 | Closure loops (delegation→Waiting-For exists; meeting/email exits out of scope per V-16). |
| REC-UI-05 | Mark-done closure prompt on chain exhaustion (also §3 #10). |
| REC-UI-04 | Auto-migration-with-consent on missed calendar items (org-native engine — §4). |
| REC-PRJ-12 | Project-completion → spawn recurring Habit. |
| REC-UI-11 | Rules-based automation — *already exposed* via org-edna vocabulary (IMPL-057) + hooks; docs/example only. |

---

## §10 — UX-orchestration (sequencing existing pieces into a guided flow)

The ingredients all exist; the gap is the *orchestration*. These are the highest-UX-impact gaps and all route through §3 #1 (the guided-session framework).

REC-REF-02 (three-phase walkthrough — biggest UX gap vs. the books), REC-REF-01 (scheduled recurring review + completion tracking), REC-REF-04 (three triggers + recovery), REC-REF-06 (maintenance review), REC-REF-07 (coverage sweep), REC-REF-09 (time-block step), REC-REF-10 (annual flow), REC-CAP-06 (driven mind-sweep), REC-ENG-02/03 (guided choice scaffolding).

---

## §11 — Parking lot (non-goal / dropped / deferred — track, don't plan)

| Gap | Status |
|---|---|
| REC-KNO-03 | **Non-goal** (V-09) — no CRM/contacts. Closed. |
| REC-HOR-07 | **Dropped as mandate** (#3); reinstated MAY only. Lowest priority. |
| REC-X-14 | Mobile front-ends **deferred** (V-18/V-20); sync is user's own mechanism. |
| REC-X-10 | Onboarding scaffold **backlog** (V-12). |
| REC-CLA-06 | Core covered; LLM-assisted clarify → **v5 backlog** (#7). |

---

## §12 — Uncategorized

Gaps that resist the lenses above:

- **REC-PRJ-04** (subproject rollup vs own-entry; parallel/sequential wiring) — partly §2 (graph editors, IMPL-114–118), partly a UX-affordance question (when does a sub-tree "roll up"?) with no clean home. Genuinely a design question, not just a build route.
- **REC-CLA-05 / REC-X-13** etc. — already fully implemented (deliverable #3 appendix); no strategy needed.

---

## Cross-reference: every gap → its primary route

A gap's *primary* route (it may also appear elsewhere). Use this to pick a starting lens.

| Primary route | Gaps |
|---|---|
| §1 Author content | REC-CAP-06, REC-CAP-09, REC-PRJ-07, REC-PRJ-10, REC-REF-10, REC-CHK-02, REC-X-15 |
| §3 Core refactor | REC-REF-02, REC-CHK-01, REC-TIC-02, REC-KNO-04, REC-KNO-05, REC-ENG-02, REC-UI-06, REC-HOR-03/04/05, REC-UI-02, REC-DEL-03, REC-UI-05 |
| §4 → re-homed (no new dependency) | REC-UI-08/09, REC-X-09, REC-AGE-01/02 → view DSL (§2/§6); REC-REF-05/09 → Reflect flow (§10) + implement-ourselves; REC-UI-15/17 → org-native (§6); REC-UI-18 → docs recommendation (§5) |
| §5 Docs-only | REC-ENG-06, REC-HOR-06, REC-HOR-08, REC-PRJ-08, REC-DEL-05, REC-CLA-09 |
| §6 Config/DSL recipe | REC-NXT-04, REC-PRJ-09, REC-ENG-08, REC-UI-01/11/12/13/14, REC-X-14 |
| §7 Schema addition | REC-X-04, REC-DEL-04, REC-NXT-05, REC-UI-16 |
| §8 New object | REC-AGE-03, REC-PRJ-06 |
| §9 Hook surface | REC-CAP-08, REC-UI-10, REC-UI-04, REC-PRJ-12 |
| §10 Orchestration | REC-REF-01/04/06/07 |
| §11 Parking lot | REC-KNO-03, REC-HOR-07, REC-X-10, REC-CLA-06 |
| §12 Uncategorized | REC-PRJ-04 |
