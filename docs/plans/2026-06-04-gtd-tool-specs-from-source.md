# GTD Tool Specifications — Reconciled from Source (GTD 2001 + Making It All Work 2008)

> Single-user digital GTD tool. Reconciled from David Allen's two books; where they differ, *Making It All Work* (2008) language/framing supersedes *Getting Things Done* (2001). org-gtd-flavored. Generated 2026-06-04.

## 0. Summary & Reading Guide

This document merges GTD tool capabilities extracted by eight reader agents (four per book) into a single reconciled specification for **org-gtd.el**, a single-user Emacs/org-mode GTD implementation. Where the two books diverge, **Book 2 (*Making It All Work*) wins** — it is Allen's later, refined statement of the same methodology.

**Counts**
- **Canonical specs:** 71 (CAP 6, CLA 7, ORG 24 across categories, REF 8, ENG 6, HOR 8 + X 12 cross-cutting)
- **Book-1 specs superseded/merged into Book-2-framed canonicals:** ~95 of the ~120 raw reader specs collapsed into the 71 canonicals (heavy duplication across readers and books).
- **Book-1 → Book-2 deltas folded in:** 14 reconciliation deltas (Changes Log §4).
- **Vague / Needs-Review items:** 27 (aggregated from all 8 readers + new ambiguities found during merge) — **all 27 adjudicated by the user 2026-06-04; see the resolved decisions log in §5.**

**How to read a spec line**

> **ID** · canonical capability statement (org-gtd-flavored) · **provenance:** (book(s) + reader IDs) · **module:** org-gtd target · **confidence:** clear / medium

- *Provenance* cites the original reader IDs (e.g. `B1-A-02`, `B2-B-05`) so every canonical traces back to source.
- *Module* names the org-gtd module, or `new` / `not yet first-class` where org-gtd has no home for it.
- *Confidence* is `clear` when multiple readers/books converge, `medium` when borderline or single-sourced.

---

## 1. Core Model & Vocabulary (Book-2 canonical terms)

Book 2 reframes Book 1's mechanics into two master dynamics and a renamed five-stage workflow. **These are the canonical terms for the tool's UI and docs.**

### 1.1 The two master dynamics: Control × Perspective
*(B2: B2-A-D1, B2-A-D10, B2-B-D4, B2-D-D5)*

Book 2 replaces Book 1's "horizontal vs. vertical" framing with **Control** and **Perspective** — Allen states people relate better to "out of control" / "lost perspective" than to "horizontal/vertical."
- **Control** = the five-stage workflow (Capture → Clarify → Organize → Reflect → Engage). "Get control," of which *organizing* is only one (non-first) part.
- **Perspective** = the six Horizons of Focus (altitude).
- **Matrix of Self-Management** (new in Book 2): the Control×Perspective grid with four named profiles — **Captain & Commander** (ideal; renamed from "Master and Commander"), **Crazy Maker / Visionary**, **Micromanager / Implementer**, **Victim / Responder**. Diagnostic model only; no direct tool feature, but it motivates "both horizons AND control matter."

### 1.2 The five stages (Book-2 canonical → Book-1 term → org-gtd module)

| Book-2 (canonical) | Book-1 term | org-gtd module |
|---|---|---|
| **Capture** (also "collect," "clear," "corral") | Collect | `org-gtd-capture.el` |
| **Clarify** | Process | `org-gtd-clarify.el`, `org-gtd-wip.el`, `org-gtd-process.el` |
| **Organize** | Organize | `org-gtd-organize.el` + category modules |
| **Reflect** | Review | `org-gtd-review.el`, `org-gtd-oops.el` |
| **Engage** | Do | `org-gtd-agenda.el` |

Note: org-gtd's own module names use **Process** (the inbox loop, `org-gtd-process.el`) and **Clarify** (the per-item WIP-buffer thinking, `org-gtd-clarify.el`) as two pieces of Book-2's single "Clarify" stage. Book 2's appendices (Appendix iv) still print the *old* verbs (Collect/Process/Organize/Review/Do); the main text uses the new ones. Canonical here = main-text Book-2 vocabulary.

### 1.3 The six Horizons of Focus (Book-2 altitude map + review cadence)
*(B2: B2-B-D3, B2-C-D2, B2-D-D2)*

| Altitude | Horizon | Defining question | Review cadence |
|---|---|---|---|
| Runway | **Next Actions** (Ground) | What do I do? | daily / ongoing |
| 10k ft | **Projects** | What do I want to complete? | weekly |
| 20k ft | **Areas of Focus / Responsibility** (B1: "areas of responsibility") | What do I maintain? | monthly |
| 30k ft | **Goals / Objectives** (1–2 yr) | What do I want to achieve? | monthly–quarterly |
| 40k ft | **Vision** (3–5 yr) | What does success look like? | annually |
| 50k ft | **Purpose & Principles** | Why? | annually+ |

The Horizons double as (a) a **capture checklist** (mind-sweep against each level) and (b) a **review framework**. Allen's insight (B2-A-D8): the Horizons ARE the Natural Planning Model applied to a whole life. Priorities are determined **top-down** (purpose→…→actions), though each horizon is equally important to clarify (B2-D-D3).

### 1.4 Other canonical terms
- **3Ds** = Do / Delegate / Defer (Book-2-codified name; B2-C-D3). Applies to actionable items: Do (<2 min), Delegate (→ Waiting For), Defer (→ Next Actions).
- **Two-minute rule** = if the next action takes ≤2 min, do it now.
- **Hard landscape** = the calendar; only date/time-bound commitments.
- **Stuff** = anything landed that doesn't belong there permanently and hasn't been decided yet (Book-2 precise definition; B2-B-D11). Input to Clarify.
- **Tickler / bring-forward / suspense file** = date-triggered re-surfacing.
- **Organized** = "where things are matches what they mean to you" (Book-2 definition; B2-B-30).
- **Reflect** chosen over "Review" to stress absorbing meaning, not cursory scanning (B2-B-D2).

---

## 2. Specs by Workflow Stage

### 2.1 Capture
*(org-gtd-capture.el)*

- **CAP-01** · Capture any incomplete / open loop (anything with the user's attention, personal or professional, big or small) into a single trusted inbox, with zero up-front filtering or value judgment — so the mind can release it. · provenance: B1 (B1-A-01, B1-B-01, B1-D-01), B2 (B2-A-01, B2-B-11) · module: org-gtd-capture.el · confidence: clear
- **CAP-02** · Funnel all inputs through as few inboxes as needed and as many as necessary, kept "leakproof" (nothing strays outside them); org-gtd's `inbox.org` is the single canonical capture target. · provenance: B1 (B1-A-02), B2 (B2-A-02, B2-D-05) · module: org-gtd-capture.el · confidence: clear
- **CAP-03** · Capture must be available in every context / always at hand, so a thought can be recorded the instant it occurs. · provenance: B1 (B1-A-03, B1-C-V7), B2 (B2-A-03) · module: org-gtd-capture.el · confidence: clear
- **CAP-04** · Each captured item becomes its own discrete placeholder (one thought = one org heading), not lumped into one running list, so each can be processed individually. A captured item may be a textual placeholder for a physical/awkward thing ("Purge boat shed"). · provenance: B1 (B1-B-02, B1-B-03), B2 (B2-B-01) · module: org-gtd-capture.el · confidence: clear
- **CAP-05** · Captured items are date-stamped automatically at capture time (inactive timestamp / CREATED property) — load-bearing for later Waiting-For follow-up. · provenance: B1 (B1-B-04) · module: org-gtd-capture.el · confidence: clear
- **CAP-06** · Provide a guided "mind sweep / empty your head" bulk-capture action the user can invoke any time (especially when overwhelmed), driven by the **Incompletion Trigger** checklist (professional + personal categories) and/or the **six Horizons** as capture prompts, to externalize everything resident in head and environment. · provenance: B1 (B1-B-05, B1-B-06, B1-D-17), B2 (B2-A-25, B2-B-02, B2-B-03, B2-D-15) · module: org-gtd-capture.el + new (trigger-list checklist template) · confidence: clear

> Book-2 broadening (B2-B-D10): Capture is a *family* of techniques — journaling, brainstorming/mind-mapping, "bookmarking" mid-task notes (B2-B-13), and capturing-to-defuse-interruptions (B2-B-14) — all folded into one stage. The single-user digital realization is "drop any text into the inbox, structured later."

### 2.2 Clarify
*(org-gtd-clarify.el / org-gtd-wip.el / org-gtd-process.el)*

- **CLA-01** · Empty/process the inbox to zero on a regular cadence (target: every input reaches zero every 24–48 h). Emptying = deciding what each item *is* and where it goes, not completing it. Process each item one at a time, top item first, never skipping to easier ones. · provenance: B1 (B1-A-04, B1-B-07, B1-C-29, B1-D-28), B2 (B2-D-06, B2-D-34) · module: org-gtd-process.el · confidence: clear
- **CLA-02** · An item, once picked up, must never return to the inbox undecided — one-way path out of "in." Each item is dispatched to exactly one destination. ("Process" ≠ "spend time on" — beyond the 2-min rule, the loop is for *deciding*, not doing.) · provenance: B1 (B1-A-04, B1-B-08, B1-B-42), B2 (—) · module: org-gtd-process.el · confidence: clear
- **CLA-03** · For each item, first answer "What is it?" then the gating question **"Is it actionable?"** (yes / no; "maybe" resolves to "no — incubate"). This is the central branching decision. · provenance: B1 (B1-A-05, B1-A-06, B1-B-09), B2 (B2-A-04, B2-B-15) · module: org-gtd-process.el / org-gtd-organize.el · confidence: clear
- **CLA-04** · For an actionable item, Clarify must capture two things: the **desired outcome** ("what does done look like?") and the **next physical, visible action** ("what's the next physical step?"). Transforms vague stuff into a manageable project (e.g. "Mom" → "Give Mom a great 60th party" → "Draft invitee list"). · provenance: B1 (B1-A-10, B1-A-39, B1-B-09, B1-D-05, B1-D-22), B2 (B2-A-05, B2-B-16, B2-C-03) · module: org-gtd-clarify.el → Projects / Single Actions · confidence: clear
- **CLA-05** · The next-action clarity test (Book-2 sharpened, B2-C-D4): a next action is "thought through enough to act" when the user can answer *What has to happen first? / What does doing look like? / Where does it happen?* — i.e. concrete, physical, single ("Call Roberta re: Dad," not "deal with Dad"). · provenance: B1 (B1-D-22), B2 (B2-C-03, B2-C-D4) · module: org-gtd-clarify.el · confidence: clear
- **CLA-06** · Clarify can extract inherent projects/next-actions from "fuzzy" attention items and unrecognized concerns ("Exercise more" → project + next action; "son's math grades" → project). Outcomes include those whose "done" is merely accepting/closing a thing with no solution. · provenance: B1 (B1-C-24, B1-D-05), B2 (B2-B-04, B2-C-23) · module: org-gtd-clarify.el → Projects · confidence: clear
- **CLA-07** · Capture is non-committal: an item may be deferred/dismissed/dropped at clarify time without forcing resolution ("when in doubt, write it down; you can dismiss it later"). Sensitive/emotional items ("dad and hospice?") run through the same capture→outcome→next-action flow as any other. · provenance: B1 (—), B2 (B2-A-18, B2-B-10) · module: org-gtd-process.el / Incubate · confidence: clear

> The Clarify→Organize dispatch for actionable items is the **3Ds** (Do/Delegate/Defer) — see ORG-QA-01, ORG-DEL-01, ORG-SA-01. For non-actionable items the three buckets are Trash / Incubate / Reference — see ORG-TRASH-01, ORG-INC-01, ORG-KNOW-01.

### 2.3 Organize
*(org-gtd-organize.el + category modules)*

**ORG-00 (taxonomy)** · The Organize dispatcher routes each clarified item into exactly one meaning-category, kept pristinely separate ("hard edges" — never mix meanings in one location, to avoid "psychic numbness"). The canonical Book-2 category set (B2-B-31): **Outcomes** (Purpose, Principles, Vision, Goals, Areas of Focus, Projects, Waiting-on outcomes), **Actions** (Calendar, ASAP/context lists, Waiting For), **Incubating**, **Support Material**, **Reference**, **Trash**. · provenance: B1 (B1-A-28, B1-B-32, B1-B-36), B2 (B2-A-07, B2-B-22, B2-B-30, B2-B-31, B2-D-07, B2-D-08) · module: org-gtd-organize.el · confidence: clear

> "Organized = location matches meaning"; getting organized is **not a one-time event** but the ongoing output of clarification, kept current as meaning changes (B2-B-D12). Items may legitimately change category over time and be re-routed during Reflect (active project → someday; computer action → calendared have-to).

#### 2.3.1 Projects

- **ORG-PRJ-01** · Mark an item as a **Project** = any desired outcome requiring more than one action step, finishable within ~1 year. It lands on a single Projects list — an *index/overview* (one project per line, typically 30–100 open), not priorities and not the plans. The 1-year bound is the dividing line between Projects (weekly review) and Goals (30k, quarterly). · provenance: B1 (B1-A-14, B1-B-30, B1-D-06, B1-C-48), B2 (B2-A-06, B2-B-23, B2-C-15, B2-C-16) · module: org-gtd-projects.el · confidence: clear
- **ORG-PRJ-02** · Every active project must carry ≥1 defined next action; a project portion that can progress independently keeps its own next action. Review surfaces projects lacking one (stuck-project detection / oops). · provenance: B1 (B1-A-15, B1-A-45, B1-B-33, B1-C-06, B1-C-34), B2 (B2-C-16, B2-D-28) · module: org-gtd-projects.el + org-gtd-review.el / org-gtd-oops.el · confidence: clear
- **ORG-PRJ-03** · A blocked/dependency-gated project (or subproject) may legitimately have NO next action when it waits on another piece to finish; stuck-project detection must NOT flag these. Ordering handled via org-edna. · provenance: B1 (B1-C-05), B2 (—) · module: org-gtd-projects.el + org-edna · confidence: clear
- **ORG-PRJ-04** · Support subprojects/components either rolled up under one Projects-list entry (detail in support material) OR listed as their own entries — user's choice; the tool must not force one representation. For multi-component projects, define a next action for each independently-movable component (parallel) or identify the single linchpin action (sequential, via org-edna). · provenance: B1 (B1-A-41, B1-A-42, B1-C-04), B2 (—) · module: org-gtd-projects.el + org-edna · confidence: clear
- **ORG-PRJ-05** · Attach project **support material** (plans, notes, links, collateral, ad-hoc back-of-envelope ideas) to the project heading, kept separate from the action reminders and from the Projects index — support material must never double as the reminder. Active project files kept more accessible than pure reference. · provenance: B1 (B1-A-16, B1-B-31, B1-C-07, B1-C-09, B1-D-24), B2 (B2-B-24, B2-D-13) · module: org-gtd-projects.el (subtree body/drawers) · confidence: clear
- **ORG-PRJ-06** · Support **Natural Planning Model** for complex projects: (1) Purpose/Principles → (2) Vision/Successful Outcome → (3) Brainstorm → (4) Organize (subprojects/sequence/priority) → (5) Next Actions. Invoked when a project needs more clarity (raise focus) or more action (lower focus); ~80% of projects need only outcome+next-action. Heuristic: project off your mind = planning sufficient; still on your mind = more planning needed. · provenance: B1 (B1-A-38, B1-A-40, B1-C-49), B2 (B2-A-22, B2-D-25, B2-D-26) · module: org-gtd-projects.el · confidence: clear · note: **not yet first-class in org-gtd** — no Natural Planning UI today.
- **ORG-PRJ-07** · A project's next action can itself be a planning step ("process action": draft ideas, email X for input, set up planning meeting). A **Project Planning Trigger List** (Resources, Finance, Operations, Risks, Stakeholders, etc.) is available to drive brainstorming. · provenance: B1 (B1-A-44, B1-C-49), B2 (B2-D-27) · module: org-gtd-projects.el + new (trigger-list template) · confidence: clear
- **ORG-PRJ-08** · A "look into" / R&D outcome (unknown result) is a real project as soon as there is a commitment to decide (~10% of projects start this way: "research life coaches"). · provenance: B1 (—), B2 (B2-B-09) · module: org-gtd-projects.el · confidence: clear

#### 2.3.2 Single Actions (Next Actions)

- **ORG-SA-01** · **Defer**: the user's own >2-min, non-delegated next action is deferred onto a Next Actions list, context-tagged so it surfaces when relevant. The next action must be parked in a trusted place so the user can stop thinking about it. · provenance: B1 (B1-A-13, B1-A-21, B1-B-21), B2 (B2-A-08, B2-C-04, B2-C-05) · module: org-gtd-single-action.el / org-gtd-agenda.el · confidence: clear
- **ORG-SA-02** · A Next Action may carry data needed to do it without a lookup (e.g. a phone number on a Calls item) and may have an attached sublist/checklist (e.g. all hardware-store items under one Errands action). · provenance: B1 (B1-B-26, B1-B-29), B2 (—) · module: org-gtd-single-action.el (property/body) · confidence: medium

#### 2.3.3 Calendar

- **ORG-CAL-01** · The Calendar holds ONLY three things — (a) time-specific appointments, (b) day-specific (not time-specific) actions, (c) day-specific information — and nothing else. It is the "sacred" hard landscape: no undated to-dos, no "things I'd like to get done today." Supports others adding items to your calendar. · provenance: B1 (B1-A-17, B1-A-18, B1-A-19, B1-A-20, B1-B-22, B1-B-23, B1-D-15), B2 (B2-A-10, B2-B-29, B2-C-10) · module: org-gtd-calendar.el · confidence: clear
- **ORG-CAL-02** · The calendar can park day-specific **future triggers**: activate a project / surface a decision / revisit a deferred item on a future date; on arrival the user activates it (e.g. onto Projects). This is one realization of the tickler (see X-05). · provenance: B1 (B1-C-20, B1-C-21), B2 (B2-B-19, B2-D-12) · module: org-gtd-calendar.el · confidence: clear

#### 2.3.4 Delegate (Waiting For)

- **ORG-DEL-01** · **Delegate**: hand an action to the appropriate person/entity when the user isn't the right one (delegation can be down, sideways, or up). · provenance: B1 (B1-A-12, B1-B-18, B1-A-43), B2 (B2-C-07) · module: org-gtd-delegate.el · confidence: clear
- **ORG-DEL-02** · Every delegated/awaited item becomes a **Waiting For** entry recording who has it, the date requested, and any due date (date is the most crucial field). Maintain a reviewable Waiting-For list of all deliverables others owe the user (not the user's own steps). · provenance: B1 (B1-A-23, B1-B-19, B1-B-20, B1-D-12), B2 (B2-A-16, B2-B-27, B2-C-07) · module: org-gtd-delegate.el · confidence: clear
- **ORG-DEL-03** · Optionally maintain a project-altitude "outcomes I'm waiting on from others" list (delegated *projects*), distinct from action-level Waiting For. · provenance: B1 (—), B2 (B2-B-28) · module: org-gtd-delegate.el · confidence: medium
- **ORG-DEL-04** · **Per-person / per-meeting Agenda lists** ("talk-to" lists): collect topics to raise with a specific person or at a specific meeting, reviewable in that context; addable ad hoc (3–15 such lists; may be time-limited, e.g. a contractor for a project's duration). Distinct from Waiting For. · provenance: B1 (B1-B-27, B1-C-43, B1-D-13), B2 (B2-C-08) · module: **not yet first-class in org-gtd** (closest: a context tag) · confidence: clear

#### 2.3.5 Incubate (Someday/Maybe + Tickler)

- **ORG-INC-01** · Maintain a **Someday/Maybe** list for items with no current next action and no commitment to move now (defining trait: no next action). Typically longer than the active project list; ranges fantasy→realistic; populated from creative-imagination categories (trips, hobbies, skills, things to build). · provenance: B1 (B1-A-08, B1-A-24, B1-B-13, B1-C-15, B1-C-16, B1-D-19), B2 (B2-A-17, B2-B-18, B2-C-27) · module: org-gtd-incubate.el · confidence: clear
- **ORG-INC-02** · Incubate splits into two structurally distinct mechanisms (Book-2 refinement, B2-B-D7): (a) items for **regular review** (Someday/Maybe list scanned weekly), and (b) **calendared "later starts"** surfaced on a chosen future date via tickler/bring-forward. The user picks the exact reactivation date. · provenance: B1 (B1-A-26, B1-B-14, B1-D-20), B2 (B2-B-19, B2-B-20) · module: org-gtd-incubate.el + Calendar/tickler · confidence: clear
- **ORG-INC-03** · Allow moving a current active Project to Someday/Maybe when it won't get attention for months (and the reverse during review). · provenance: B1 (B1-C-17), B2 (B2-B-37) · module: org-gtd-organize.el / org-gtd-incubate.el · confidence: clear
- **ORG-INC-04** · Optionally subcategorize Someday/Maybe via tags/sub-headings (e.g. "do soon when resources allow" vs. "bucket-list"). Special-interest collection lists (books, movies, wines, gift ideas) blend reference + someday and are reviewed by urge/periodically, not on a fixed cadence. · provenance: B1 (B1-A-25, B1-C-18, B1-C-19), B2 (—) · module: org-gtd-incubate.el (tags) · confidence: medium

#### 2.3.6 Knowledge (Reference)

- **ORG-KNOW-01** · File non-actionable but valuable info into a **Reference** store, organized by topic, fast to file and easy to retrieve. Provide topic/area-specific stores alongside a general catch-all. · provenance: B1 (B1-A-09, B1-A-27, B1-B-12, B1-C-12, B1-C-13, B1-D-14), B2 (B2-B-21, B2-C-29, B2-D-11) · module: org-gtd-knowledge.el · confidence: clear
- **ORG-KNOW-02** · Reference filing must be near-instant — filing an item in under ~60 s, or users "stack" instead of file. Single A–Z index preferred (multi-level alpha allowed); purge at least yearly (schedulable via tickler). · provenance: B1 (B1-B-39, B1-B-40, B1-B-41), B2 (B2-B-33) · module: org-gtd-knowledge.el · confidence: clear (60-s rule) / medium (flat-index guidance)
- **ORG-KNOW-03** · Contact data (phone, email, birthdays) is stored as pure reference — no action triggers embedded in the contact record; actions about people belong on Agenda/Calls lists. · provenance: B1 (B1-C-14), B2 (—) · module: org-gtd-knowledge.el / new · confidence: medium · note: org-gtd has no contact manager.

#### 2.3.7 Habits

- **ORG-HAB-01** · "Process projects" — outcomes whose deliverable is a recurring procedure/habit/system ("set up bill-paying system," "set up exercise routine") — are clarified like normal projects and may spawn a recurring **Habit**. · provenance: B1 (—), B2 (B2-B-V3) · module: org-gtd-habit.el · confidence: medium · note: promoted from a reader's vague item; the recurring-output capability maps to org-gtd's Habits category.

#### 2.3.8 Quick Actions

- **ORG-QA-01** · **Two-minute rule**: if the clarified next action takes ≤2 min, **Do it immediately** during processing rather than tracking it. After completing it, if the project isn't done, clarify the new next action and re-route (3Ds). · provenance: B1 (B1-A-11, B1-B-16, B1-B-17, B1-D-16), B2 (B2-B-39, B2-C-05, B2-C-06) · module: org-gtd-quick-action.el · confidence: clear

#### 2.3.9 Read/Review (category mapping unsettled — see V-12)

- **ORG-RR-01** · Route longer-than-2-min reading into a **Read/Review** queue — a self-regulating inventory of material to consume in low-attention/spare windows (e.g. takeoff/landing), kept distinct from stored Reference. · provenance: B1 (B1-B-28, B1-C-44), B2 (B2-C-14, B2-D-10) · module: **not yet first-class in org-gtd** (single-action with a Read/Review context, or new) · confidence: clear (capability) / medium (mapping)

### 2.4 Reflect / Review
*(org-gtd-review.el / org-gtd-oops.el)*

- **REF-01** · Provide a **Weekly Review** (~1–2 h block, ideally a recurring calendar event) that brings projects/actions/calendar current. Reflect does two things: (a) update system contents to match current reality, and (b) deliver trusted higher-altitude perspective. · provenance: B1 (B1-A-29, B1-B-34, B1-D-06), B2 (B2-A-11, B2-B-34, B2-B-35, B2-C-01, B2-D-17) · module: org-gtd-review.el · confidence: clear
- **REF-02** · Weekly Review is structured in three guided phases (Book-2 formalized, B2-C-D6): **Get Clear** (collect loose ends into inbox, process to zero, empty head), **Get Current** (mark done items, mine past calendar, scan upcoming calendar, review Waiting For, review every project for ≥1 next action, review checklists), **Get Creative** (promote/prune Someday-Maybe, capture new/risk-taking ideas). · provenance: B1 (B1-A-30, B1-C-28 through B1-C-37), B2 (B2-C-19, B2-D-18, B2-D-19, B2-D-20) · module: org-gtd-review.el · confidence: clear
- **REF-03** · The review's core invariant: review the complete Projects list one-by-one and ensure every active project has ≥1 current next action; surface and correct stuck/malformed projects, and convert items that have morphed into projects. · provenance: B1 (B1-B-33, B1-C-34, B1-A-45), B2 (B2-C-16, B2-C-20, B2-D-19, B2-D-28) · module: org-gtd-review.el + org-gtd-oops.el · confidence: clear
- **REF-04** · Review must be runnable any time, not only on a fixed weekly cadence — Book-2 names three triggers (B2-C-18): (1) regular weekly, (2) when key projects feel like they're lagging, (3) when the user feels they've lost grip on short-term priorities. Includes a "get back on track" recovery flow: re-empty head, clean lists, reclaim items that leaked outside the system. · provenance: B1 (B1-D-11, B1-D-18), B2 (B2-C-18) · module: org-gtd-review.el / org-gtd-oops.el · confidence: clear
- **REF-05** · Review cadence scales with horizon (per-altitude review reminders): Runway daily, Projects weekly, Areas of Focus monthly, Goals monthly–quarterly, Vision annually, Purpose annually+. Each horizon is reviewable as its own dedicated process, one at a time, with associative ideas captured as they surface. · provenance: B1 (B1-A-31, B1-C-46, B1-C-47), B2 (B2-B-36, B2-C-28, B2-D-22) · module: org-gtd-review.el + areas-of-focus · confidence: clear
- **REF-06** · Provide a distinct **system-maintenance review**: purge filing, rethink list management, refresh tools; spawn a project+next-action when system outdatedness starts grabbing attention. "Elevated horizon events" (annual plan reviews, off-sites) need explicit calendar scheduling. · provenance: B1 (B1-C-11), B2 (B2-D-32, B2-D-33) · module: org-gtd-review.el + Calendar · confidence: clear

### 2.5 Engage
*(org-gtd-agenda.el)*

- **ENG-01** · From the organized inventory, surface trusted choices about what to do (and not do) at any given moment. The foundation is a **total-life action inventory** (typically 100–150+ items, personal + professional, all options accessible) — not a single daily to-do list reflecting only "latest and loudest." · provenance: B1 (B1-D-04), B2 (B2-A-13, B2-B-38, B2-C-12) · module: org-gtd-agenda.el · confidence: clear
- **ENG-02** · Review the **calendar first** (daily, the hard landscape), then context-filtered action lists; show only actions doable in the present context. After completing a calendar action, the user can see what else remains for the day. · provenance: B1 (B1-A-31, B1-A-32, B1-C-26, B1-C-27), B2 (B2-C-02, B2-D-21) · module: org-gtd-agenda.el · confidence: clear
- **ENG-03** · **Four-criteria action choice** (applied in order): **context → time available → energy available → priority**. Context is the first limiting factor. · provenance: B1 (B1-A-33, B1-C-39, B1-D-25), B2 (B2-A-14, B2-C-11, B2-D-23) · module: org-gtd-agenda.el · confidence: clear
- **ENG-04** · Context is a user-customizable tag dimension; the engage view filters/folds by it. Common contexts: Calls, At Computer, Office, Home, Anywhere, Errands, Agendas, Read/Review, Waiting For — plus user-invented ones ("At sailboat," "Brain-dead," "Before Trip," "Online"). · provenance: B1 (B1-A-22, B1-B-24, B1-B-25, B1-C-40, B1-C-45, B1-D-23), B2 (B2-A-15, B2-B-25, B2-B-26, B2-C-09, B2-D-09) · module: org-gtd-agenda.el / org tags · confidence: clear
- **ENG-05** · Filtering by **time available** and **energy available** implies stored attributes: a time/effort estimate (org `Effort`) and an energy level (tag/property, or "Brain Gone" context). Lets the user surface only ~10-min actions, or a low-horsepower inventory for low-energy states. · provenance: B1 (B1-A-34, B1-C-41, B1-C-42, B1-D-03), B2 (B2-A-14, B2-C-11) · module: org-gtd-agenda.el · confidence: medium · note: **energy is not yet first-class in org-gtd**; time = `Effort` property.
- **ENG-06** · Engage offers three work modes: do **predefined** work, do **ad-hoc** work as it appears, or take time to **define** work (processing inputs is itself a legitimate engage choice). The system must be organized enough to trust priority judgments about ad-hoc work. · provenance: B1 (B1-A-V3, B1-C-V5, B1-D-26), B2 (B2-D-24) · module: org-gtd-agenda.el / org-gtd-process.el · confidence: medium · note: in Book 1 this was a vague "mental model"; Book 2 (B2-D-24) states it as an explicit engage capability, so it is promoted to a spec here.

### 2.6 Horizons of Focus / Areas of Focus
*(org-gtd-areas-of-focus + higher-horizon support)*

- **HOR-01** · Maintain a **Areas of Focus / Responsibility** checklist (Horizon 2, 20k ft): ~7–15 ongoing responsibility/interest categories (health, finances, family, recreation, self-development, the "hats" of a job) — *not* finishable, reviewed ~monthly. One list for job + one for life suffices ("fewer than twenty items"). · provenance: B1 (B1-A-36, B1-A-37, B1-C-46, B1-D-07), B2 (B2-A-19, B2-B-05, B2-C-21) · module: org-gtd-areas-of-focus · confidence: clear
- **HOR-02** · Areas of Focus function as a **balance/coverage checklist** over the project inventory: ask of any project/action "what area of focus does this reflect?" and use the list to surface missing/unframed commitments and rebalance projects. A vague 20k theme ("staff development") can be converted into a concrete project with next actions. · provenance: B1 (B1-A-37, B1-D-07), B2 (B2-A-19, B2-C-22, B2-C-23) · module: org-gtd-areas-of-focus · confidence: clear
- **HOR-03** · Support **Goals / Objectives** (Horizon 3, 30k ft): 1–2 yr outcomes, reviewed monthly–quarterly. Any project taking >1 year is parked here rather than on the Projects list. · provenance: B1 (B1-A-36, B1-C-47, B1-D-08), B2 (B2-A-20, B2-B-06, B2-C-24, B2-D-30) · module: org-gtd-areas-of-focus / new · confidence: clear
- **HOR-04** · Support **Vision** (Horizon 4, 40k ft): 3–5 yr success picture, free-form text/scenario/"treasure map" (may link images/files), revisited yearly or on major transitions; supports reverse-engineering vision → goals → projects → actions. Vision artifacts can be re-surfaced via tickler every few months. · provenance: B1 (B1-A-36, B1-D-08), B2 (B2-A-21, B2-B-07, B2-C-25, B2-D-29) · module: org-gtd-areas-of-focus / new · confidence: clear
- **HOR-05** · Support **Purpose & Principles** (Horizon 5, 50k ft): statement(s) of purpose + a list of core values / personal credo / affirmations, used as the top priority criterion and reaffirmed periodically. · provenance: B1 (B1-A-36, B1-D-08), B2 (B2-A-21, B2-B-08, B2-C-26, B2-D-01) · module: org-gtd-areas-of-focus / new · confidence: clear
- **HOR-06** · Store a per-area **"rules of engagement" / standards** document used as decision criteria for tough choices ("Is this in keeping with my purpose?"). Decision-support reference, not actionable. · provenance: B1 (B1-A-V4), B2 (B2-D-02) · module: org-gtd-areas-of-focus · confidence: medium
- **HOR-07** · Allow a single linked **"Overview of my life"** outline spanning all six horizons (Purpose→Principles→Vision→Goals→Areas→Projects→Next Actions) so relationships between levels are visible together — maps naturally to one nested org outline. Horizon contents are mutable (recast over time). · provenance: B1 (—), B2 (B2-D-03, B2-D-04, B2-D-31) · module: org-gtd-areas-of-focus + Projects · confidence: clear · note: org-gtd does not currently provide a unified cross-horizon outline view.
- **HOR-08** · Priorities are determined **top-down** (purpose→values→vision→goals→areas→projects→actions), even though control is built bottom-up — the six Horizons replace Book 1's ABC/123 priority coding (see Changes Log §4). · provenance: B1 (B1-A-36), B2 (B2-A-D6, B2-C-D1, B2-D-D3) · module: org-gtd-review.el + areas-of-focus · confidence: clear

---

## 3. Cross-Cutting Specs

- **X-01** · **Every open loop / actionable item has a defined next action** (system invariant). Absence of one signals more thinking is needed → drives stuck-project detection in Reflect. (Exception: dependency-gated blocked items, ORG-PRJ-03.) · provenance: B1 (B1-A-45, B1-B-33), B2 (B2-C-16, B2-D-28) · module: org-gtd-review.el / org-gtd-oops.el · confidence: clear
- **X-02** · **Context tag** attribute on actions (the location/tool/situation required). User-definable; the primary engage filter. · provenance: B1 (B1-A-22, B1-B-24, B1-C-40), B2 (B2-B-25, B2-B-26, B2-C-09) · module: org tags · confidence: clear
- **X-03** · **Time-required / effort estimate** attribute on actions (enables "show only ~10-min actions"). · provenance: B1 (B1-C-41), B2 (B2-C-11) · module: org `Effort` property · confidence: medium
- **X-04** · **Energy-level** attribute on actions (enables a low-horsepower task inventory). · provenance: B1 (B1-A-34, B1-C-42, B1-D-03), B2 (B2-C-11) · module: new / tag · confidence: medium · note: not yet first-class in org-gtd.
- **X-05** · **Tickler / future-trigger**: any item can be future-dated to re-surface on its day (in inbox/agenda). Must be checked daily, with look-ahead before being away. Realizable as an org SCHEDULED/timestamp; serves Incubate later-starts, project activation, reference purge, vision re-surfacing. · provenance: B1 (B1-A-26, B1-B-14, B1-C-22, B1-D-20), B2 (B2-B-19, B2-B-20, B2-D-12, B2-D-29) · module: org-gtd-incubate.el + Calendar · confidence: clear
- **X-06** · **Auto date-stamp at capture** (CREATED / inactive timestamp) on every item — load-bearing for Waiting-For follow-up timing. · provenance: B1 (B1-B-04), B2 (—) · module: org-gtd-capture.el · confidence: clear
- **X-07** · **Reusable checklists / trigger lists** as first-class reviewable lists: Incompletion Trigger List, Project Planning Trigger List, Travel/Backup/Weekly-Review checklists, area "ingredient" lists. Easy to create and delete; surfaced on demand or during review. · provenance: B1 (B1-C-23, B1-D-21), B2 (B2-B-02, B2-D-14) · module: **not yet first-class in org-gtd** (new) · confidence: clear
- **X-08** · **User-customizable categories/contexts**: the user can create, rename, merge, and subdivide lists/contexts as their needs evolve; the tool must not hard-code a fixed taxonomy. Optional project/Someday subdivision (personal vs professional) allowed but with a warning against over-categorizing. · provenance: B1 (B1-B-V1, B1-C-01, B1-D-09), B2 (B2-B-26, B2-B-32) · module: org-gtd-core.el / org tags · confidence: clear
- **X-09** · **Hard separation of categories** ("hard edges"): each meaning lives in exactly one location; never blend actionable + reference + support, to avoid psychic numbness. · provenance: B1 (B1-B-32, B1-B-36), B2 (B2-B-22, B2-B-30) · module: org-gtd-organize.el · confidence: clear
- **X-10** · **Associate a project/action with an area of focus** (cross-horizon linkage), enabling area-grouped project views and coverage checks. · provenance: B1 (B1-C-03), B2 (B2-C-22) · module: areas-of-focus / org tags · confidence: clear
- **X-11** · **Group/filter Projects into named sub-lists** (e.g. "Presentations") and optionally sort a sub-list by an upcoming-event date — implies an optional date property on projects. · provenance: B1 (B1-C-01, B1-C-02), B2 (B2-B-32) · module: Projects / org tags + agenda filter · confidence: medium
- **X-12** · **Onboarding/migration**: re-feed pre-existing lists/organizers through the inbox as unprocessed "in"; bulk-capture-then-process one area at a time. Track a standing meta-project ("Finalize personal management system setup"). · provenance: B1 (B1-B-43, B1-D-27, B1-D-28), B2 (B2-D-16) · module: org-gtd-capture.el / org-gtd-process.el · confidence: medium

---

## 4. Book 1 → Book 2 Changes Log

Summarizes what evolved between *Getting Things Done* (2001) and *Making It All Work* (2008). Where they differ, the Book-2 form is canonical above.

| # | What changed | Book 1 | Book 2 (canonical) | Why it matters for the tool |
|---|---|---|---|---|
| **D-01** | Stage-1 name | Collect | **Capture** (also clear/corral) | UI/docs vocabulary; Capture is now a *family* of techniques (journaling, bookmarking, anti-interruption), not just inbox collection. |
| **D-02** | Stage-2 name | Process | **Clarify** | GTD recast as "mind management" not "time management." org-gtd splits this into Process (inbox loop) + Clarify (WIP thinking). |
| **D-03** | Stage-4 name | Review | **Reflect** | Stresses absorbing meaning + gaining perspective, not cursory scanning. |
| **D-04** | Stage-5 name | Do | **Engage** | Engage from a total-life inventory by context/time/energy/priority. |
| **D-05** | Top-level frame | horizontal vs vertical | **Control × Perspective** | More intuitive; org-gtd can frame docs around "get control" (the 5 stages) and "get perspective" (the 6 horizons). |
| **D-06** | Priority model | "Set priorities" / ABC-123 coding | **6 Horizons × 3 limiting factors (context/time/energy) × 3Ds**, top-down | Directly answers Book 1's most-criticized gap; drives the engage filter design (ENG-03) and per-horizon review (REF-05). |
| **D-07** | "Get organized" | a one-time event | **"Get control"; organize is one (non-first) part**; "organized = location matches meaning"; never a one-time event | Organize stays current as meaning changes; re-routing items is normal (ORG-00, X-09). |
| **D-08** | Horizons | brief 6-level mention | **Full per-horizon treatment** with defining question + format + review cadence | Justifies first-class higher-horizon support and per-altitude review reminders (HOR-01..08, REF-05). |
| **D-09** | Natural Planning ↔ Horizons | separate | **Horizons ARE Natural Planning at life scale** | Same why→outcome→next-action logic at every altitude; unifies ORG-PRJ-06 and §2.6. |
| **D-10** | 3Ds | Do/Delegate/Defer (unnamed) | **"3Ds"** codified | Naming for the Clarify→Organize dispatch (ORG-QA-01/DEL-01/SA-01). |
| **D-11** | Project definition | multi-step outcome within a year | **multi-step outcome, finishable within a year** — the year becomes a *review-cadence rule* dividing Projects (weekly) from Goals/30k (quarterly) | Sharpens ORG-PRJ-01 + the Projects/Goals boundary (HOR-03). |
| **D-12** | Someday/Maybe | single bucket | **split**: regular-review list vs calendared later-starts (tickler) | Requires two distinct mechanisms (ORG-INC-02, X-05). |
| **D-13** | Support Material | folded with Reference | **own organizing category**, distinct from Reference and from action reminders | Affects whether Knowledge vs Support are one store or two (see V-13). |
| **D-14** | Next-action test | "What's the next action?" | **3-question clarity test** (happen first / doing look like / where) | More operational clarify guidance (CLA-05). |

Terminology-only deltas (no tool impact beyond naming): "Master and Commander" → **"Captain and Commander"** (B2-D-D4); "areas of responsibility" → **"areas of focus"** (B2-A-D5); "information overload" → **"potential-meaning overload"** (B2-A-D11); "life/work balance" rejected as a fallacy (B2-A-D14). New conceptual models with no direct feature: **Matrix of Self-Management** (B2-A-D10), Power=Concentration=Elimination-of-Distraction chain (B2-A-D12). Appendix-vs-main-text vocabulary conflict noted: Book-2 appendices still print Collect/Process/Organize/Review/Do (B2-D-D1) — main-text Capture/Clarify/Organize/Reflect/Engage is canonical.

---

## 5. Resolved Decisions (was: Vague / Needs-Review)

All 27 items adjudicated by the user on 2026-06-04. Disposition legend:
- **IMPLEMENT** — accepted as a near-term spec.
- **ALREADY** — capability already exists in org-gtd; action (if any) is UX/docs only.
- **DOCS** — handle via documentation/examples, no code.
- **v5 / ROADMAP** — wanted, scheduled for a future major version.
- **BACKLOG** — wanted but parked, shape undefined.
- **MOBILE** — deferred until org-gtd has a real mobile-work story.
- **OUT** — explicitly out of scope (non-goal).

| ID | Item | Decision | Disposition |
|----|------|----------|-------------|
| V-01 | "Mind like water" / RAS philosophy | Keep as manual/motivation; not a spec. | DOCS |
| V-02 | Upper horizons (Goals/Vision/Purpose) as stored artifacts | Already implemented — `horizons.org` holds H2–H5. Open UX question: is it surfaced *usefully*? | ALREADY (UX review) |
| V-03 | Threefold daily-work model | Already maps: process-inbox = defining work, engage = doing predefined, ad-hoc capture = work as it appears. Document the mapping. | ALREADY / DOCS |
| V-04 | Purpose/Principles on projects | org-gtd defines project-ness after the fact, so near-term: mention in clarify helper text. Structured Natural Planning support → v5. | DOCS now / v5 |
| V-05 | Per-person "talk-to" agendas | Model as `#`-prefixed tags (e.g. `#sam`) + a command/view that finds `#` tags and lets you pick one. Dedicated lightweight module is a v5 candidate. | IMPLEMENT / v5 |
| V-06 | Read/Review queue | It's a Someday/Maybe subcategory. Use as the docs example for extending S/M; optional command to add the subcategory; consider a notification when settings won't surface refile choices. | IMPLEMENT (small) / DOCS |
| V-07 | Energy attribute | Add an optional energy tag/property; engage view can filter on it; off by default. | IMPLEMENT |
| V-08 | Reference vs Support Material | Unified Knowledge; support material = a knowledge entry *linked* to the project (org links / org-roam / denote). New idea: a PKM-integration API so org-gtd maps to PKM tools rather than owning storage. | IMPLEMENT (+ new PKM API) |
| V-09 | Contacts / CRM | Out of scope; contacts are plain reference. Explicit non-goal (defer to org-contacts/BBDB/PKM). | OUT |
| V-10 | Checklists / trigger lists | Build general user-defined checklist support; bundle the Incompletion + Project-Planning trigger lists as examples. | IMPLEMENT |
| V-11 | "Pending / Hold" holding area | Don't implement; the inbox already serves as the holding area. | OUT |
| V-12 | Granular Someday/Maybe subcategories | Already supported (see docs). Enhancement: offer common subcategories as opt-in presets via an `org-gtd-setup` command. | ALREADY / IMPLEMENT (small) |
| V-13 | Subdividing the Projects list | Already doable via the view DSL; add a documentation example/tutorial. | ALREADY / DOCS |
| V-14 | Extra example contexts (@Online, etc.) | Covered by user-definable contexts; document Allen's as examples. | ALREADY / DOCS |
| V-15 | Multitasking exception | Reversed to a spec: optionally allow viewing/processing 2–3 inbox items together. UX shape TBD. | BACKLOG (exploratory) |
| V-16 | Email action/waiting folders | Out of scope → the capability is "capture from email" into the inbox. | OUT |
| V-17 | Original item as its own reminder | Covered — captured item + links/attachments, filed by category. No new spec. | ALREADY |
| V-18 | Tiered reference by device/location | Out of scope until org-gtd defines mobile work beyond "use an org phone app." | MOBILE / OUT |
| V-19 | Brainstorming / mind-mapping | Rely on org outlines; mention `krvkir/org-mindmap` as an optional later integration. | DOCS / BACKLOG |
| V-20 | Voice capture + location surfacing | Defer to mobile app design (both the capture front-ends and context-triggered surfacing). | MOBILE |
| V-21 | Per-horizon review cadences | Implement per-horizon scheduled review reminders with user-configurable intervals. | IMPLEMENT |
| V-22 | Team/family/shared GTD | Fully out of scope — org-gtd is single-user. | OUT |
| V-23 | Journaling capture | Out of scope; served by existing org tools (org-journal/denote/datetree). | OUT |
| V-24 | Cleaning/purging physical space | Fold into the Incompletion Trigger List / mind-sweep content (V-10), not a separate feature. | DOCS (within V-10) |
| V-25 | Annual accomplishments / year-end review | Optional annual-review variant feeding the Goals horizon (with V-21's cadences). | IMPLEMENT (optional) |
| V-26 | GTD-Q self-assessment | Out of scope (external marketing quiz). | OUT |
| V-27 | Total-life unified view | Covered by the current engage view; consider **renaming it the "total-life" view**. | ALREADY (rename candidate) |

### Near-term implementable (rollup)
- **V-07** energy attribute (optional tag/property + engage filter)
- **V-10** general checklist support + bundled trigger lists (absorbs **V-24**)
- **V-21** per-horizon scheduled review reminders (configurable) + **V-25** optional annual review
- **V-05** `#`-tag agendas + discovery command
- **V-06** Read/Review as a Someday/Maybe subcategory (+ optional add-subcategory command, refile-surfacing notification)
- **V-12** opt-in subcategory presets via `org-gtd-setup`
- **V-08** PKM-integration API (links-based support material)

### Roadmap (v5+)
- **V-04** structured Natural Planning (purpose/principles) on projects
- **V-05** dedicated per-person agenda module (graduating from `#`-tags)

### Backlog (shape undefined)
- **V-15** optional multi-item inbox processing
- **V-19** optional org-mindmap integration

### Deferred to a future mobile story
- **V-18** tiered/device reference · **V-20** voice capture + location-triggered surfacing

### Docs / UX only
- **V-01** philosophy as motivation · **V-02** surface `horizons.org` better · **V-03** document the three-modes mapping · **V-13** view-DSL project subdivision example · **V-14** example contexts · **V-17** item-as-reminder · **V-27** rename engage → "total-life" view

### Out of scope (non-goals)
- **V-09** CRM/contacts · **V-11** pending/hold area · **V-16** email folder management · **V-22** team/shared GTD · **V-23** journaling · **V-26** GTD-Q quiz

## 6. Implementation-Gap Decisions (G-series)

These 7 are *not* vague-spec items — they are gaps the implementation-status audit (`2026-06-04-gtd-implementation-status.md`) surfaced where a spec exists but isn't built. Adjudicated with the user on **2026-06-05**, with book quotes pulled where the disposition hinged on the text. Same legend as §5.

| ID | Item | Decision | Disposition |
|----|------|----------|-------------|
| G-REF-02 | Guided three-phase reflect (Get Clear/Current/Creative) | **Guided sequential walkthrough**, reusing the existing `someday-review` guided-session pattern + `command-center` view aggregation. Configurable: per-phase content selection, pluggable incompletion-trigger-list/mind-sweep (V-10/V-24), and **named cadence profiles** (weekly/monthly/quarterly/biannual/yearly), each its own review (V-21). Wire "Get Clear" (process inbox/loose ends) into the hub. | IMPLEMENT |
| G-REF-06 | System maintenance + elevated-horizon reviews | `org-gtd-setup`-style **opt-in injecting recurring maintenance / higher-altitude review tasks**. Keep cadence orthogonal to altitude (a quarterly reflect ≠ a higher-horizon reflect). | IMPLEMENT |
| G-REF-06b | (refactor, separate issue) Consolidate `reflect-stuck-*` | Collapse the ~7–8 "find broken items" commands into one. | IMPLEMENT (refactor) |
| G-HOR-07 | Unified cross-horizon "overview of my life" view | **Over-literal** — books treat the six horizons as a thinking framework/checklist/per-altitude cadence, not a one-screen outline (Allen favors bottom-up). Intent met by `horizons.org` outline + REF-02 cadences. | DROP (spec-correction) |
| G-HOR-06 | Personal credo / principles (H5) | Document writing a credo in `horizons.org` H5 and reviewing it during higher-altitude reflects. Group "rules of engagement" framing is OUT (single-user, V-22). | DOCS |
| G-HOR-345 | Goals/Vision/Purpose linkage | **Optional hooks modeled on the area-of-focus pattern**, extended to higher horizons. Couples to a refactor: resolve the **area-of-focus duplication** (areas can be defined in both `horizons.org` and config — unify). | IMPLEMENT |
| G-DEL-03 | Delegate a whole (already-decomposed) project | A delegated outcome is just *one* Waiting For on our side (standard category already "includes all projects you've delegated"). Real capability: **delegate an entire decomposed project** → collapses to one Waiting For (who/when/status) while preserving the task breakdown for its return. Exec "high-level list" itself not needed. | IMPLEMENT |
| G-CLA-06 | Assisted extraction of hidden projects | Core already covered by clarify WIP buffer + organize-as-Project template (book describes no algorithmic extraction). Spun off as a **new, separate feature: LLM-assisted clarify** (vague → concrete project + next action). | ALREADY + new feature (v5/backlog) |

### G-series near-term implementable
- **G-REF-02** guided reflect walkthrough (+ cadence profiles, absorbs V-21/V-25 scheduling)
- **G-REF-06** opt-in recurring maintenance/higher-altitude review tasks
- **G-HOR-345** higher-horizon optional hooks + area-of-focus de-duplication refactor
- **G-DEL-03** delegate-a-whole-project capability
- **G-REF-06b** consolidate stuck-finders into one command (refactor)

### G-series roadmap (v5+) / backlog
- **G-CLA-06** LLM-assisted clarify (new feature)

### G-series docs / dropped
- **G-HOR-06** credo/principles practice (docs) · **G-HOR-07** dropped as over-literal

