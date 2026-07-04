# Canonical Recommended User Workflows — INDEX

Deliverable #5 of the feature analysis: the canonical merge of David Allen's
prescribed flows, progressions, questions, mindsets, and orders of operations,
shaped as acceptance tests for a single-user digital GTD application.

**Inputs merged:**
- `raw/book1-workflows.md` — 33 workflows (B1-W-01..33), *Getting Things Done* (revised ed.)
- `raw/book2-workflows.md` — 25 workflows (B2-W-01..25), *Making It All Work* (2008)
- `../sources/da-software-1994.md` — 9 workflows (DA94-W01..09), Allen's 1994 software sketches

**Precedence rule:** where the two books differ, book 2 wins (later refinement).
DA94 workflows are included where they add flow detail the books lack.

**Gherkin files:** one `.feature` per stage/area in this directory. Each
canonical workflow is one tagged `Rule:` block (`@WF-NN`, `@cadence-*`,
`@src-*`), with the trigger/situation as `Background` and Allen's verbatim
decision questions quoted in step text.

## Counts by stage

| Stage/area  | Count | IDs        | File                |
|-------------|-------|------------|---------------------|
| setup       | 1     | WF-01      | setup.feature       |
| capture     | 4     | WF-02..05  | capture.feature     |
| clarify     | 5     | WF-06..10  | clarify.feature     |
| organize    | 9     | WF-11..19  | organize.feature    |
| reflect     | 3     | WF-20..22  | reflect.feature     |
| engage      | 3     | WF-23..25  | engage.feature      |
| perspective | 8     | WF-26..33  | perspective.feature |
| planning    | 3     | WF-34..36  | planning.feature    |
| mindset     | 5     | WF-37..41  | mindset.feature     |
| **Total**   | **41**|            |                     |

## Registry

| ID | Name | Stage/area | Cadence | Provenance (merged) | Keywords |
|----|------|------------|---------|---------------------|----------|
| WF-01 | Initial full-scale implementation (time, space, tools) | setup | once | B1-W-01 | install, workspace, tools, filing, decks |
| WF-02 | Initial physical gathering ("corralling your stuff") | capture | once | B1-W-03, B2-W-04 (physical survey) | corral, in-tray, stays-put, sweep |
| WF-03 | The mind sweep (mental gathering with trigger lists) | capture | once / weekly | B1-W-04, B2-W-04, DA94-W01 | mind sweep, trigger list, head empty, placeholders |
| WF-04 | Higher-horizon capture sweep | capture | monthly+ | B2-W-05 | horizons, capture, attention, fuzzy |
| WF-05 | Ongoing capture habit and interrupt shielding | capture | per-item | B1-W-05, B2-W-06, B1-W-26 (interruptions), B2-W-04 (rules) | habit, buckets, interruption, bookmark |
| WF-06 | The per-item clarifying decision tree | clarify | per-item | B1-W-06, B2-W-07, B2-W-14 (specificity test), DA94-W02, DA94-W09 | actionable, decision tree, next action, outcome |
| WF-07 | Processing-session discipline ("in" to empty) | clarify | daily | B1-W-07, DA94-W09 | top item, one at a time, inbox zero |
| WF-08 | The two-minute rule | clarify | per-item | B1-W-08, B2-W-14 (Do), DA94-W02 (alarm) | two minutes, do now, efficiency cutoff |
| WF-09 | Delegation handoff and tracking | clarify | per-item | B1-W-09, B2-W-14 (Delegate), DA94-W05 | delegate, waiting for, handoff, closure |
| WF-10 | Outcome reframing and project identification | clarify | per-item | B1-W-10, B2-W-08 | outcome, reframe, projects list, resolve |
| WF-11 | The bucket system with hard edges | organize | per-item | B1-W-11, B2-W-09 | buckets, categories, no blending, meaning |
| WF-12 | Calendar discipline (hard landscape) | organize | per-item / daily | B1-W-12, B2-W-10, DA94-W08 | calendar, hard landscape, day-specific, migrate |
| WF-13 | Context-sorted next-action lists | organize | per-item | B1-W-13, B2-W-09 (actions) | contexts, calls, errands, agendas |
| WF-14 | Waiting For list management | organize | per-item / weekly | B1-W-14, B2-W-09 (waiting for), DA94-W05 (tracking) | waiting for, dates, follow-up, inventory |
| WF-15 | E-mail "in" to zero | organize | daily | B1-W-15 | email, @action, @waiting-for, zero |
| WF-16 | Incubation system: Someday/Maybe and date triggers | organize | per-item / daily / weekly | B1-W-16, B1-W-17, B1-W-18, B2-W-11 | someday maybe, tickler, incubate, resurface |
| WF-17 | Checklists as external mind | organize | monthly+ | B1-W-19 | checklists, recurring, fuzzy, review trigger |
| WF-18 | General-reference filing (sixty-second standard) | organize | per-item | B1-W-02, B2-W-09 (reference) | reference, filing, a-z, purge |
| WF-19 | Project support material handling | organize | per-item / weekly | B1-W-21, B2-W-09 (support) | support material, adjunct, harvest |
| WF-20 | Daily orientation review (calendar first) | reflect | daily | B1-W-22, B2-W-10 (morning), B2-W-12 (runway rung) | daily review, calendar first, context lists |
| WF-21 | The Weekly Review: Get Clear / Get Current / Get Creative | reflect | weekly | B1-W-23, B2-W-13, DA94-W07 | weekly review, clear, current, creative |
| WF-22 | The reflection cadence ladder (higher-horizon reviews) | reflect | monthly+ | B2-W-12, B1-W-24 | cadence, intervals, horizons, bottom-up |
| WF-23 | Choosing the action of the moment | engage | per-item | B1-W-25, B2-W-15 | context, time, energy, priority, intuition |
| WF-24 | The threefold nature of work | engage | daily | B1-W-26, B2-W-16 | predefined, ad hoc, defining, interruptions |
| WF-25 | Action-completion closure loop | engage | per-item | DA94-W04 | done, project complete, next action, unstuck |
| WF-26 | Perspective sequencing: one horizon at a time, bottom-up | perspective | monthly+ | B2-W-17, B1-W-27 | sequencing, bottom-up, altitude, alignment |
| WF-27 | Runway conversation: Next Actions | perspective | daily | B2-W-18 | runway, what to do, completeness |
| WF-28 | Projects conversation (10,000 ft) and hidden-project sweep | perspective | weekly | B2-W-19, B1-W-20 | projects, complete, trigger verbs, hidden |
| WF-29 | Areas of Focus conversation (20,000 ft) | perspective | monthly+ | B2-W-20 | areas, maintain, derivation, generation |
| WF-30 | Goals and Objectives conversation (30,000 ft) | perspective | monthly+ | B2-W-21 | goals, achieve, annual ritual, recalibrate |
| WF-31 | Vision conversation (40,000 ft) | perspective | monthly+ | B2-W-22 | vision, success, credibility, reverse-engineer |
| WF-32 | Purpose and Principles conversation (50,000 ft) | perspective | monthly+ | B2-W-23 | purpose, principles, why, values |
| WF-33 | Category corral: control then perspective | perspective | monthly+ | B2-W-24 | corral, category, control first |
| WF-34 | Natural planning model (five phases, 80/15/5 calibrated) | planning | per-item | B1-W-28, B2-W-25, B1-W-31, DA94-W03 | natural planning, purpose, vision, brainstorm |
| WF-35 | Unsticking a project (raise/lower the focus) | planning | per-item | B1-W-29, B2-W-25 (steering) | stuck, raise focus, lower focus |
| WF-36 | Activating a project's moving parts | planning | weekly | B1-W-30, B2-W-25 (next actions) | moving parts, components, dependencies |
| WF-37 | Appropriate Engagement: the Control + Perspective loop | mindset | per-item | B2-W-01 | control, perspective, weakest link, course-correct |
| WF-38 | Matrix of Self-Management self-diagnosis | mindset | monthly+ | B2-W-02 | matrix, victim, visionary, captain |
| WF-39 | "What's true right now?" | mindset | per-item | B2-W-03 | attention, start here, current reality |
| WF-40 | Managing self-agreements | mindset | per-item | B1-W-32 | agreements, renegotiate, self-trust |
| WF-41 | "What's the next action?" as a closure standard | mindset | per-item | B1-W-33, DA94-W06 | closure, meetings, owner, next action |

## Merge log

Every raw workflow is accounted for exactly once as a primary source (some
also contribute partial material elsewhere, noted in parentheses).

| Canonical | Merged from | Notes |
|-----------|-------------|-------|
| WF-01 | B1-W-01 | Setup is book-1-only; book 2 assumes an installed system. |
| WF-02 | B1-W-03 + B2-W-04 (steps 2–4) | B2's physical-environment survey and "Do you have any attention on this?" folded into B1's corralling flow. |
| WF-03 | B1-W-04 + B2-W-04 + DA94-W01 | **The two mind sweeps merge.** DA94 adds the quick-vs-full trigger lists and the explicit "process now vs. add to in-basket" exit choice. |
| WF-04 | B2-W-05 | New in book 2 (horizons as capture instrument); kept separate from WF-03 because its trigger is a *completed* obvious sweep. |
| WF-05 | B1-W-05 + B2-W-06 + B1-W-26 (interruption handling) + B2-W-04 (capturing success rules) | Ongoing habit and interrupt-shield merged: both are the same "write it down, into the funnel" move. |
| WF-06 | B1-W-06 + B2-W-07 + B2-W-14 (three-question specificity test) + DA94-W02 + DA94-W09 | **The clarify decision trees merge.** B2 wins on "Maybe = no, for now" and the verbatim outcome/next-action questions; B2-W-14's specificity test is folded in as a scenario; DA94 adds inline project creation and activate/incubate/eliminate vocabulary. |
| WF-07 | B1-W-07 + DA94-W09 | DA94 confirms one-at-a-time/top-first as a coached constraint. |
| WF-08 | B1-W-08 + B2-W-14 (Do branch) + DA94-W02 (2-minute alarm) | |
| WF-09 | B1-W-09 + B2-W-14 (Delegate branch) + DA94-W05 | DA94 adds the closure loops the books lack: waiting-for spawned on send; unfinished communication auto-becomes a next action. |
| WF-10 | B1-W-10 + B2-W-08 | B1's project-identification pass and B2's outcome-reframing patterns (resolve-/R&D-/process-projects) are the same move at two grains. |
| WF-11 | B1-W-11 + B2-W-09 | B2 wins on the formal definition: organized = meaning matches location; adds "as simple as possible, but no simpler". |
| WF-12 | B1-W-12 + B2-W-10 + DA94-W08 | DA94 adds auto-migration-with-consent ("Calendars are for items that expire only"); B2 adds the explicit morning sequencing (also reflected in WF-20). |
| WF-13 | B1-W-13 + B2-W-09 (actions branch) | |
| WF-14 | B1-W-14 + B2-W-09 (Waiting For) + DA94-W05 (tracking half) | |
| WF-15 | B1-W-15 | Book-1 only; book 2 has no separate e-mail flow. |
| WF-16 | B1-W-16 + B1-W-17 + B1-W-18 + B2-W-11 | B2-W-11's structural split (regular-review vs. date-trigger) is the canonical frame; B1's Someday/Maybe list, calendar future-options triggers, and 43-folder tickler ritual become its two arms. B2 adds the overwhelm→demote flow. |
| WF-17 | B1-W-19 | |
| WF-18 | B1-W-02 + B2-W-09 (reference best-practices restated verbatim) | |
| WF-19 | B1-W-21 + B2-W-09 (support material) | |
| WF-20 | B1-W-22 + B2-W-10 (morning step) + B2-W-12 (daily rung) | |
| WF-21 | B1-W-23 + B2-W-13 + DA94-W07 | **The two Weekly Reviews merge** into one Get Clear / Get Current / Get Creative flow (B2's naming canonical). DA94-W07 adds daily-page annotate/archive and the focus-area pass. |
| WF-22 | B2-W-12 + B1-W-24 | B2's codified per-horizon interval ladder supersedes B1's looser "appropriate intervals". |
| WF-23 | B1-W-25 + B2-W-15 | B2 wins: priority resolves via horizons + limiting factors; A-B-C/1-2-3/high-medium-low grading explicitly rejected. |
| WF-24 | B1-W-26 + B2-W-16 | B2 adds the 30–90 min/day budget for defining work. (B1-W-26's interruption-handling half lives in WF-05.) |
| WF-25 | DA94-W04 | DA94-only: mark-done → "project complete?" → "what's the next action?" — flow detail the books state only as a weekly-review invariant. |
| WF-26 | B2-W-17 + B1-W-27 | B2 codifies bottom-up-with-attention-override; B1's six-level model is the same ladder. |
| WF-27 | B2-W-18 | Horizon conversations are canonical per book 2. |
| WF-28 | B2-W-19 + B1-W-20 | B1's hidden-project sweep (three areas; "When is a problem a project? Always.") folded into the 10k conversation alongside B2's trigger verbs. |
| WF-29 | B2-W-20 | |
| WF-30 | B2-W-21 | |
| WF-31 | B2-W-22 | |
| WF-32 | B2-W-23 | |
| WF-33 | B2-W-24 | New in book 2. |
| WF-34 | B1-W-28 + B2-W-25 + B1-W-31 + DA94-W03 | The 80/15/5 calibration (B1-W-31) folded in as the model's scaling scenario; DA94-W03 adds brainstorm-note triage (to-do/resource/data/option → activate/incubate/eliminate). |
| WF-35 | B1-W-29 + B2-W-25 (steering rules) | B2's raise/lower formulation is canonical. |
| WF-36 | B1-W-30 + B2-W-25 (next-actions phase) | |
| WF-37 | B2-W-01 | New in book 2; subsumes B1's five-step master loop framing. |
| WF-38 | B2-W-02 | New in book 2. |
| WF-39 | B2-W-03 | Makes B1's bottom-up rationale an explicit master heuristic. |
| WF-40 | B1-W-32 | B2-W-12's "renegotiate commitments" reflects the same principle. |
| WF-41 | B1-W-33 + DA94-W06 | DA94's meeting lifecycle (per-topic decisions + next actions with who/when/due → in-basket) supplies the concrete flow behind B1's closure standard. |

### Book-1 elements superseded by book 2

- **Stage names:** capture / clarify / organize / reflect / engage are canonical
  (collect/process/review/do retired; book 2 body text is authoritative even
  over its own Appendix iv, which retains the old verbs).
- **Priorities:** resolved via the six horizons plus the three limiting factors
  (context, time, energy); simple A-B-C / 1-2-3 / high-medium-low grading is
  explicitly rejected (B2 6466–6468). B1's four-criteria model survives but its
  "priority" criterion is now defined by the horizons.
- **"Maybe" answers:** book 2 makes explicit that "Maybe" is actually "no, but
  the item might require action later".
- **Next-action sufficiency:** book 2's three-question test ("What has to
  happen first?" / "What does doing look like?" / "Where does it happen?") is
  the sharper formulation of B1's physical-visible requirement.
- **Organized, defined:** "where things are suits what they mean to you"
  (B2-W-09) is the canonical definition behind B1's bucket discipline.
- **Review intervals:** B2's cadence ladder (daily/weekly/monthly/quarterly/
  annually/annually+) supersedes B1's "appropriate intervals" language.
- **Weekly Review:** B2's named Get Clear / Get Current / Get Creative
  three-phase structure is canonical (B1 revised ed. already adopted it).
- **New canonical material with no B1 equivalent:** the Matrix of
  Self-Management (WF-38), the six horizon conversations as a co-equal half of
  the method (WF-27..32), the category corral (WF-33), the higher-horizon
  capture sweep (WF-04), and "What's true right now?" (WF-39).

### DA94 contributions (flow detail the books lack)

- Coached in-basket processing as guided navigation; one-item/top-first as a
  UI constraint (WF-06, WF-07).
- Mark-done → "project complete?" → "what's the next action?" closure loop
  (WF-25).
- Auto-migration of expired calendar items with user consent (WF-12).
- Closure loops on communication exits: waiting-for spawned on send,
  unfinished communication auto-becomes a next action (WF-09).
- Meeting lifecycle with per-topic decisions and owned next actions routed to
  the in-basket (WF-41).
- Quick/full trigger lists and the process-now vs. in-basket exit choice on
  capture (WF-03).
- Brainstorm-note triage and activate/incubate/eliminate vocabulary (WF-34,
  WF-06).

## Hard invariants encoded as `Then` steps

- Nothing ever returns to "in" (WF-06, WF-07).
- The calendar holds only time-specific actions, day-specific actions, and
  day-specific information — nothing else (WF-12).
- Every active project has at least one current next action (WF-21, WF-25,
  WF-28, WF-36).
- Actions of two minutes or less are executed, never tracked (WF-08).
- Every Waiting For entry carries the date recorded (WF-09, WF-14).
- The tickler is checked and emptied every day without exception (WF-16).
- Someday/Maybe is reviewed weekly and its items carry no next action (WF-16).
- Categories never blend; no priority scaffolding on lists (WF-11).
- Reference filing takes under sixty seconds; one A–Z system; yearly purge
  (WF-18).
- Support material is never the action reminder (WF-19).
- Everything is in the head or out of it — never in between (WF-05).
- Vision must be at least 51 percent credible (WF-31).
- Control before perspective when corralling a category (WF-33).

## Scope note

Worked examples from the books (Gracie's Gardens, client anecdotes, Allen's
travel itineraries) are excluded, per the raw extractions. Verbatim *questions*
and *mindsets* are in scope and quoted in the step text.
