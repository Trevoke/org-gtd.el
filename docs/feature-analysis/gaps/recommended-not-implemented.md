# Recommended Features Not Implemented in org-gtd.el

> Deliverable #3 of the feature analysis. Produced 2026-06-10.
>
> Method: for each of the 122 entries in
> `docs/feature-analysis/recommended/INDEX.md`, the baseline verdict was taken
> from `docs/plans/2026-06-04-gtd-implementation-status.md` (via the old→new ID
> map; audited accurate by `docs/feature-analysis/audit/implementation-status-audit.md`,
> with its one correction applied: **ENG-06 → Partial**), then cross-checked
> against the implemented registry (`docs/feature-analysis/implemented/INDEX.md`,
> IMPL-001..148). The 52 entries with no old-ID mapping (DA94 UI features,
> audit-miss additions, orphan reinstatements) were assessed fresh against the
> registry and verified in source (`*.el`) before any Not-implemented verdict.
>
> Status legend: **Implemented** (turnkey) · **Partial** (present but
> incomplete/manual/caveated) · **Via-config** (achievable today through the
> view-language DSL, org/org-edna settings, or org-native structure — no org-gtd
> code change) · **Not-implemented** (absent from code and config surface).
>
> Prior dispositions: `V-xx` = user-adjudicated decision from the 2026-06-04/05
> spec doc §5 and gap interview — **already decided, do not re-litigate**.

## Scoreboard

| Status | Count | of 122 |
|---|---|---|
| Implemented | 49 | 40% |
| Partial | 30 | 25% |
| Via-config | 21 | 17% |
| Not-implemented | 22 | 18% |
| **Total gaps (Partial + Via-config + Not-implemented)** | **73** | **60%** |

> **Update (2026-07-08):** REC-CHK-01 and REC-REF-02 have since been implemented in the checklists + guided-review work (PR #294, unmerged), so the live Not-implemented count is effectively **20**. Their rows below are re-marked **Implemented** with the deferred pieces tracked as *still open*. See `docs/plans/2026-07-06-checklists-and-guided-review-design.md` §8. (Scoreboard totals above are left at their 2026-06-10 baseline.)

Of the 22 Not-implemented: 1 is a deliberate non-goal (REC-KNO-03, V-09), 3 are
already adjudicated IMPLEMENT (REC-REF-02, REC-REF-06, REC-DEL-03), 4 have
planned dispositions (REC-PRJ-06→V-04, REC-NXT-05→V-06, REC-X-04→V-07,
REC-CHK-01→V-10), and 1 was adjudicated DROP-as-mandate but reinstated as MAY
(REC-HOR-07). 13 are new findings with no prior disposition — all of strength
*may* or *should*, none *must*.

Gap weight by strength × type (Partial/Via-config/Not-implemented only):

| | must | should | may |
|---|---|---|---|
| tool | 3 (REF-01, REF-02, ENG-02) | 24 | 33 |
| methodology | 1 (ENG-03) | 7 | 5 |

---

## Gaps by area

Each entry: **status** · what exists today · what's missing · `type/strength` · prior disposition (if any).

### Capture (CAP)

- **REC-CAP-06** — guided mind-sweep / bulk-capture driven by trigger lists · **Partial** · Exists: horizons file shown during clarify (IMPL-016, IMPL-103); plain capture loop (IMPL-001). Missing: Incompletion Trigger List, six-Horizons prompts, any *driven* sweep walkthrough. · `tool/should` · Ties **V-10** (checklists) and **V-24**; baseline CAP-06.
- **REC-CAP-08** — capture hooks at activity boundaries ("anything to process?" after a call/session) · **Not-implemented** · Nothing in source (no prompt hooks at workflow exits; org-gtd-hooks.el is observation-only stages of organize, IMPL-136 could host it). · `tool/may` · New (DA94-34); no prior disposition.
- **REC-CAP-09** — "what's true right now?" themed current-reality sweep · **Not-implemented** · No themed-sweep or trigger-list support anywhere (grep: zero checklist/mind-sweep hits). · `methodology/may` · New (orphan B2-C-V4); would fall out of V-10 checklist support.

### Clarify (CLA)

- **REC-CLA-06** — extract hidden projects/next-actions from fuzzy items · **Partial** · Exists: WIP free-edit + organize-as-project + project templates (IMPL-007, IMPL-018, IMPL-040). Missing: assisted extraction. · `methodology/should` · **Adjudicated 2026-06-05 #7: core already covered**; LLM-assisted clarify spun off as separate v5/backlog idea. Do not re-litigate.
- **REC-CLA-09** — configurable do-it-now threshold (2-min cutoff stretch/shrink) · **Via-config** · The tool never enforces any cutoff — quick-action (IMPL-023) is purely user-judged, so any threshold works today; only the hardcoded "<2 min" cheat-sheet text (IMPL-017) would need wording. · `tool/may` · New (audit add).
- **REC-CLA-10** — entries must not degrade back into "stuff" over their lifetime · **Not-implemented** · Stuck/metadata views (IMPL-075–077) catch missing *metadata*, not vague *phrasing*; no concreteness lint or review prompt. · `methodology/should` · New (audit add); natural rider on the REF-02 guided review.
- **REC-CLA-11** — three honest exits from a broken agreement (renegotiate / complete / drop), surfaced as a decision · **Partial** · Exists: each exit individually — re-clarify (IMPL-007), mark done, trash-with-audit-trail (IMPL-031), cancel cascade prompt (IMPL-051), someday-review defer/clarify/quit (IMPL-084). Missing: the unified surfaced choice for stale items. · `methodology/may` · New (orphan B1-D-V5).

### Organize: Projects (PRJ)

- **REC-PRJ-04** — subproject rollup vs own-entry choice; parallel/sequential component wiring · **Partial** · Exists: multi-project membership (IMPL-044), DAG deps (IMPL-043), graph editing (IMPL-114–118). Missing: subproject *rollup* affordance; parallel deps need manual wiring (default is sequential chain, IMPL-040). · `tool/should` · Baseline PRJ-04.
- **REC-PRJ-06** — Natural Planning Model support (purpose → vision → brainstorm → organize → next actions; NPM schema on project record) · **Not-implemented** · Project creation is mechanical (IMPL-040); project templates (IMPL-018) are the nearest hook. · `tool/should` · **V-04: helper-text now, structured planning → v5.**
- **REC-PRJ-07** — planning step as next action + Project Planning Trigger List · **Partial** · Exists: any task can be the NEXT action incl. a planning step. Missing: shipped trigger list / brainstorming view. · `tool/should` · Ties **V-10**; baseline PRJ-07.
- **REC-PRJ-08** — "look into" / commitment-to-decide R&D projects · **Partial** · Exists: as a normal project by convention. Missing: nothing structural distinguishes it (arguably fine). · `methodology/should` · Baseline PRJ-08; convention-only.
- **REC-PRJ-09** — per-project Active vs Archive support compartments · **Via-config** · Org-native subtree/drawer structure + configurable archive datetree (IMPL-063) cover it; no dedicated affordance. · `tool/may` · New (audit add).
- **REC-PRJ-10** — shipped project-identification verb checklist (Finalize/Implement/Research/…) · **Not-implemented** · No checklist infrastructure (grep: zero hits). · `tool/should` · New (audit miss #3); would fall out of V-10.
- **REC-PRJ-12** — process projects spawning a recurring Habit on completion · **Partial** · Exists: Habits first-class (IMPL-029). Missing: automated project→Habit conversion flow. · `tool/may` · Baseline ORG-HAB-01.

### Organize: Next Actions (NXT)

- **REC-NXT-04** — optional informal "if I have time" list · **Via-config** · A named someday list (IMPL-033) or a tag + DSL view (IMPL-085) realizes it today. · `tool/may` · New (orphan B1-A-35).
- **REC-NXT-05** — Read/Review queue distinct from Reference · **Not-implemented** · No read/review type or queue in code. · `tool/should` · **V-06: model as Someday/Maybe subcategory** (reuses IMPL-033 named-lists hook) — small build.

### Organize: Delegate / Waiting For (DEL)

- **REC-DEL-03** — delegate an entire decomposed project, collapsing to one Waiting For while preserving the breakdown · **Not-implemented** · Exists: item-level delegation (IMPL-025), project tickler/someday with state snapshot (IMPL-059, IMPL-064) as the pattern to follow. Missing: the projects↔delegate seam. · `tool/should` · **Adjudicated 2026-06-05 #6: IMPLEMENT (reframed)** — not an exec "high-level list".
- **REC-DEL-04** — two-stage delegation state (plan-to-delegate → delegated + date) · **Not-implemented** · Only the single WAIT/DELEGATED_TO state exists (IMPL-025, IMPL-027). · `tool/may` · New (DA94-39).
- **REC-DEL-05** — surface trackable-channel guidance (email > note > voice…) · **Not-implemented** · No guidance text in the delegate flow. · `methodology/may` · New (audit add); docs-only candidate.

### Organize: Tickler (TIC)

- **REC-TIC-02** — tickler recurrence ("every N…", annual re-add) and advance lead-time notification · **Partial** · Exists: one-shot future date per tickler (`org-gtd-tickler.el:41`, IMPL-032); recurrence only via the separate Habit type (IMPL-029, repeaters require a TODO state ticklers don't have). Missing: recurring/annual tickler entries and N-days-ahead surfacing (no DEADLINE-style warning on ORG_GTD_TIMESTAMP). · `tool/should` · New (audit add).

### Organize: Reference (KNO)

- **REC-KNO-02** — <60 s filing; flat A–Z index; yearly purge · **Partial** · Exists: one-key archive disposition (IMPL-030); purge schedulable via tickler. Missing: any index; purge not automated. · `tool/should` · Baseline ORG-KNOW-02.
- **REC-KNO-03** — contacts as pure reference, no embedded triggers · **Not-implemented** · No contact handling at all. · `methodology/may` · **V-09: deliberate non-goal.** Closed.
- **REC-KNO-04** — zero-friction new reference category in the filing flow · **Not-implemented** · The knowledge flow has no topical filing step at all — items are marked DONE and archived to a *chronological* datetree (IMPL-030, IMPL-063); refile auto-creation (IMPL-037) serves action categories, not reference topics. · `tool/should` · New (audit add).
- **REC-KNO-05** — browsable categorized reference index (search alone insufficient) · **Not-implemented** · Archive is a yearly datetree, browsable only chronologically; no topic map/overview. · `tool/should` · New (audit add). Biggest reference-area gap; pairs with REC-KNO-04.
- **REC-KNO-06** — user-definable forms/templates for reference lists + external-editor escape hatch · **Not-implemented** · Capture templates (IMPL-002) exist for the inbox only. · `tool/may` · New (DA94-40).

### Organize: Checklists (CHK)

- **REC-CHK-01** — reusable checklists / trigger lists as first-class reviewable lists · **Implemented (2026-07, PR #294 — unmerged)** · Shipped as `checklists.org` (each named top-level subtree = a template) + `org-gtd-checklist-insert` + bundled starters (Weekly Review triggers, Mind sweep prompts) + repeater-driven checkbox reset; consumers read items via `org-gtd-checklist--items`. **Rejected** the registry `checklist` type / `CHECKLIST_KIND` / `RESET_CHECK_BOXES` / org-edna RESET / Cluster-E CRUD manager (design doc §8). **Still open:** slim file manager transient, instance↔template linking, `kind` filtering. · `tool/should` · **V-10 delivered** — see `docs/plans/2026-07-06-checklists-and-guided-review-design.md` §8. Unblocks REC-CAP-06, REC-PRJ-07, REC-PRJ-10, REC-CAP-09.
- **REC-CHK-02** — recurring reflection-prompt lists resurfacing at chosen intervals · **Via-config** · A tickler or habit item whose body is the prompt list resurfaces on schedule today (IMPL-032, IMPL-029); turnkey form awaits V-10 + REC-TIC-02. · `tool/may` · New (orphan B1-D-V2/V3).

### Organize: Agendas (AGE)

- **REC-AGE-01** — per-person / per-meeting "talk-to" lists · **Partial** · Exists: `org-gtd-engage-tagged` (IMPL-066) + DSL tag filters approximate via person-tags. Missing: dedicated agenda lists, ad-hoc add, in-context review. · `tool/should` · **V-05: `#`-tags + discovery command now; module v5.**
- **REC-AGE-02** — aggregated per-person view (agendas + bidirectional waiting-fors + projects + dates), printable brief · **Partial** · Exists: DSL `(who . …)` filter over delegated items (IMPL-090) + tag filters give a manual per-person view. Missing: owed-by-me direction, resource-for-projects, special dates, one-command brief. · `tool/may` · New (DA94-29); builds on V-05.
- **REC-AGE-03** — meeting lifecycle object (upcoming → occurred, outcomes routed to inbox) · **Not-implemented** · Nothing meeting-shaped in source (verified by grep). · `tool/may` · New (DA94-32/W06).

### Reflect / Review (REF)

- **REC-REF-01** — Weekly Review as a scheduled recurring event bringing the system current · **Partial** · Exists: on-demand reflect views via command center (IMPL-073–084, IMPL-135). Missing: recurring calendar event, completion tracking; docs themselves say "weekly review is not yet implemented" (implemented-registry Discrepancies §2, DOC-56). · `tool/must` · Baseline REF-01; folded into the REF-02 adjudication.
- **REC-REF-02** — guided three-phase walkthrough (Get Clear / Get Current / Get Creative) · **Implemented (lean; 2026-07, PR #294 — unmerged)** · Shipped `M-x org-gtd-review` with `org-gtd-review-profiles` (Weekly Review default), step types prompt/command/view/checklist, keys `n s p q`, phase checkpoints, pause/resume (visible `review-state.eld`), and `org-gtd-review-schedule` (REF-01 reminder rider). **Rejected** org-edna involvement + hidden state file (design doc §8). **Still open (deferred):** `walk` step over org headings + no-next-action invariant guard, stats block / X-15 readout, back-step `b`, cadence-ladder profiles, `:allowed-actions` action bars, generalizing `org-gtd-someday-review`. · `tool/must` · **Adjudicated 2026-06-05 #1: IMPLEMENT** — delivered lean; see design doc §8. The single biggest UX gap vs. the books.
- **REC-REF-04** — review on three triggers incl. "get back on track" recovery flow · **Partial** · Exists: runnable anytime via command center (IMPL-135). Missing: explicit recovery flow. · `tool/should` · Baseline REF-04; largely subsumed by REF-02 build.
- **REC-REF-05** — review cadence scaling per horizon (daily→annual), each its own process · **Via-config** · Exists: horizons file (IMPL-103) + area review (IMPL-073); cadence via user's own recurring items. · `tool/should` · **V-21: implement configurable per-horizon reminders — near-term.**
- **REC-REF-06** — system-maintenance review + elevated-horizon calendar events · **Not-implemented** · Archive is single-shot (IMPL-061); no maintenance scaffold. · `tool/should` · **Adjudicated 2026-06-05 #2: IMPLEMENT** via opt-in injection of recurring maintenance/review tasks; cadence ≠ altitude rule recorded; reflect-stuck-* consolidation refactor noted.
- **REC-REF-07** — all reminder locations reviewed equally (no blind spots) · **Partial** · Exists: reflect views cover every category (IMPL-073–082) and custom missed views are appendable (IMPL-083). Missing: a sweep guaranteeing coverage — falls out of REF-02. · `methodology/should` · New (orphan B1-B-38).
- **REC-REF-08** — lists kept totally current; Weekly Review as the remedy · **Partial** · Exists: stuck/missed detection as remedy aids (IMPL-074–077, IMPL-081). Missing: the review itself (REF-01/02); proactive flags are REC-UI-02's territory. · `methodology/should` · New ID (orphan B1-C-38 corrected); remedy gap = REF-02.
- **REC-REF-09** — time-blocking out of the review (calendar blocks for big rocks) · **Partial** · Exists: agenda task transient sets dates (IMPL-068, IMPL-069); calendar items creatable (IMPL-028). Missing: a review-flow step that turns chosen actions into week blocks. · `tool/should` · New (audit miss #2); candidate REF-02 phase step.
- **REC-REF-10** — optional annual review / year-end stock-taking · **Partial** · Exists: recently-completed view with N-days prefix arg (IMPL-079) gives the accomplishments inventory. Missing: packaged annual flow feeding goal-setting. · `tool/may` · **V-25: IMPLEMENT (optional).**

### Engage (ENG)

- **REC-ENG-02** — calendar first, then context-filtered lists; post-completion "what remains today" · **Partial** · Exists: engage view leads with calendar block then all next actions (IMPL-065); context filter is a separate command (IMPL-066). Missing: automatic context folding in the default view. · `tool/must` · Baseline ENG-02.
- **REC-ENG-03** — four-criteria choice (context → time → energy → priority) scaffolded by filters · **Partial** · Exists: context tags (IMPL-066), effort filter (IMPL-090), priority opt-in (IMPL-068). Missing: energy (zero refs in code, → REC-X-04/V-07); no guided ordering. · `methodology/must` · Baseline ENG-03.
- **REC-ENG-05** — filter by time available via Effort estimate · **Partial** · Exists: DSL `(effort . …)` (IMPL-090) + set-effort in agenda transient (IMPL-068). Missing: turnkey "show ~10-min actions" entry point in engage. · `tool/should` · Baseline ENG-05 (energy half moved to REC-X-04/V-07).
- **REC-ENG-06** — three work modes (predefined / ad-hoc / defining) as legitimate choices · **Partial** · Exists: as separate commands (engage / capture+process / clarify). Missing: docs framing them as the three modes. · `methodology/should` · **V-03: docs-only, no build** (audit correction: Partial, not Not-implemented). Closed pending docs.
- **REC-ENG-08** — "weird time" micro-window view (very short / low-effort actions) · **Via-config** · DSL `(effort . (< "0:10"))` view spec (IMPL-085, IMPL-090) builds it today; not shipped as a named view. · `tool/may` · New (orphan B1-D-V1).

### Horizons (HOR)

- **REC-HOR-03** — Goals/Objectives (30k) reviewed monthly–quarterly · **Via-config** · H3 heading in horizons.org (IMPL-103). Missing: project linkage, cadence wiring. · `tool/should` · **Adjudicated 2026-06-05 #5: IMPLEMENT linkage** as optional hooks on the areas-of-focus pattern (+ unify where areas are declared); V-02/V-21 related.
- **REC-HOR-04** — Vision (40k) with reverse-engineering and tickler resurfacing · **Via-config** · H4 heading, free-form (IMPL-103); resurfacing via manual tickler. Missing: linkage (same adjudication #5). · `tool/should` · Adjudicated #5.
- **REC-HOR-05** — Purpose & Principles (50k) as top priority criterion · **Via-config** · H5 heading (IMPL-103). Missing: linkage/decision-filter integration (adjudication #5). · `tool/should` · Adjudicated #5.
- **REC-HOR-06** — personal standards/values as tough-choice decision reference · **Via-config** · horizons.org H5 already holds free-form credo text (IMPL-103). · `tool/may` · **Adjudicated 2026-06-05 #4: DOCS-only** — document the credo practice; group rules out of scope. Closed pending docs.
- **REC-HOR-07** — optional unified cross-horizon "overview of my life" view · **Not-implemented** · horizons.org is itself a foldable outline of H2–H5, but Ground/Projects are not composed in; no cross-horizon composite view. · `tool/may` · **Adjudicated 2026-06-05 #3: DROP as a mandate** (over-literal reading); registry reinstated only as optional MAY. Lowest priority; re-open only if demand appears.
- **REC-HOR-08** — top-down prioritization replacing ABC coding · **Partial** · Exists: horizons replace priority coding by design; per-area review (IMPL-073). Missing: nothing buildable — top-down reasoning is human. · `methodology/should` · Baseline HOR-08; effectively docs territory.

### Software / UI (UI — DA94)

- **REC-UI-01** — dashboard home view (core lists + calendar at a glance) · **Via-config** · Multi-block DSL views compose exactly this (IMPL-085, IMPL-093); engage view (IMPL-065) is a near-dashboard. Missing: a shipped "dashboard" spec. · `tool/may` · New (DA94-13).
- **REC-UI-02** — proactive integrity warning flags (N in inbox, N stuck projects, missed calendar) · **Partial** · Exists: live inbox count in mode line (IMPL-132); stuck/missed *views* on demand (IMPL-074–077, IMPL-081). Missing: proactive surfacing of stuck/missed counts. · `tool/should` · New (DA94-14); operationalizes REC-PRJ-02/REF-08.
- **REC-UI-03** — methodology-teaching coaching dialogs · **Partial** · Exists: teaching error on malformed projects (IMPL-041), corrective keyword errors (IMPL-139), clarify guidance header (IMPL-008), cheat-sheet window (IMPL-017). Missing: systematic coach. · `tool/may` · New (DA94-04).
- **REC-UI-04** — auto-migration with consent: uncompleted calendar item → Next Actions (user veto) · **Not-implemented** · Missed views surface past-due items (IMPL-074, IMPL-081) but nothing acts; no migration prompt. · `tool/should` · New (DA94-41/W08).
- **REC-UI-05** — mark-done closure prompt ("project complete? what's next?") · **Partial** · Exists: org-edna auto-advances dependents to NEXT (IMPL-045) — covers it when successors are wired. Missing: a prompt when the chain runs out; until the next stuck-view run the project sits silent. · `tool/should` · New (DA94-25/W04).
- **REC-UI-06** — exactly-one-date semantics per action (incl. start-by slot) · **Partial** · Exists: org-gtd's own model keys one ORG_GTD_TIMESTAMP per item (IMPL-069); types validate it (IMPL-075). Missing: nothing prevents org date soup (SCHEDULED+DEADLINE+timestamps); no distinct start-by. · `tool/may` · New (DA94-27).
- **REC-UI-07** — verb-first action entry with verb-driven routing · **Not-implemented** · No verb menu anywhere (verified by grep). · `tool/may` · New (DA94-26).
- **REC-UI-08** — customizable list sorting · **Via-config** · DSL has no sorting key (confirmed, DOC-44); the `(native …)` escape hatch (IMPL-092) reaches org-agenda-sorting-strategy. · `tool/may` · New (DA94-02).
- **REC-UI-09** — global multi-key search (person, keyword, date, project, area) · **Via-config** · DSL filters cover who/tags/dates/area/property (IMPL-090); org-search-view/agenda match are org-native. Missing: one unified search command (acceptable). · `tool/may` · New (DA94-06).
- **REC-UI-10** — closure loops at every exit (delegation spawns Waiting For, meeting outputs → inbox) · **Partial** · Exists: WAIT→delegated conversion (IMPL-027); delegate flow records who/when in one step (IMPL-025). Missing: email/meeting exit loops (out of scope per V-16 email-folder stance). · `tool/may` · New (DA94-33/W05/11).
- **REC-UI-11** — rules-based automation · **Via-config** · org-edna TRIGGER/BLOCKER user vocabulary (IMPL-057) + six-stage hooks (IMPL-136) + classic organize hooks (IMPL-021) are the rules engine. · `tool/may` · New (DA94-09).
- **REC-UI-12** — print/export first-class · **Via-config** · Graph exports SVG/DOT/ASCII (IMPL-125); agenda views print via org-native `org-agenda-write`. Missing: person briefs (→ REC-AGE-02). · `tool/may` · New (DA94-10).
- **REC-UI-13** — retrospective calendar archive/log · **Via-config** · Yearly archive datetree (IMPL-063) + automatic CLOSED stamps (IMPL-133) + org-agenda log mode give a queryable record. · `tool/may` · New (DA94-05).
- **REC-UI-14** — gateway to external apps while processing · **Via-config** · Emacs *is* the gateway (org links, mail clients, shell); nothing org-gtd-specific needed. · `tool/may` · New (DA94-07). Effectively satisfied by platform.
- **REC-UI-15** — alarms/timers incl. optional 2-minute timer · **Via-config** · org-timer / `M-x org-timer-set-timer` is org-native; not wired into quick-action. · `tool/may` · New (DA94-12).
- **REC-UI-16** — composite rollup views per area and per horizon · **Partial** · Exists: per-area multi-block review view (IMPL-073) — the DA94-36 half. Missing: per-horizon "sets" (DA94-37 half; ties REC-HOR-07's MAY status). · `tool/may` · New (DA94-36/37).
- **REC-UI-17** — project ledger & metadata (done-vs-next history, entry dates, due-sorted, stakeholders, upward link) · **Partial** · Exists: graph shows full task states (IMPL-106), captured-at stamps (IMPL-003), area link (IMPL-101), progress cookies (IMPL-052), last-clock-out query (IMPL-054). Missing: stakeholder fields, due-date-first project sorting, ledger presentation. · `tool/may` · New (DA94-24).
- **REC-UI-18** — layered calendar (month/week/day; day split time- vs day-specific) · **Via-config** · org-agenda native day/week/month spans + time grid already split timed vs untimed; DSL calendar-day block (IMPL-093). Missing: week location-strip (cosmetic). · `tool/may` · New (DA94-15).

### Cross-cutting (X)

- **REC-X-04** — optional energy-level attribute · **Not-implemented** · Zero "energy" references in source (re-verified). · `tool/may` · **V-07: add optional energy tag/property — near-term, off by default.**
- **REC-X-09** — named project sub-lists, optionally event-date sorted · **Partial** · Exists: DSL grouping/tag filters cover sub-lists. Missing: dedicated UI; date-sorted grouping. · `tool/may` · **V-13: docs example via DSL.** Closed pending docs.
- **REC-X-10** — onboarding/migration scaffolding (re-feed old lists, meta-project) · **Via-config** · Manual bulk-capture → process works today (IMPL-001, IMPL-005, IMPL-006). Missing: scaffold/meta-project. · `tool/should` · **V-12: backlog-ish.**
- **REC-X-14** — system available anywhere (home/office/transit) · **Via-config** · Plain org files sync by any user mechanism; additional inbox files ingest mobile/email captures in one processing session (IMPL-006). Missing: any front-end story — **V-18/V-20: mobile deferred.** · `tool/should` ·
- **REC-X-15** — ≥50-actions completeness heuristic as onboarding/review diagnostic · **Not-implemented** · No count diagnostic (inbox count IMPL-132 is the only live metric). · `methodology/may` · New (audit add); docs-only candidate or trivial REF-02 stat.

---

## Appendix: fully implemented (49)

One line each: REC id — IMPL evidence.

- REC-CAP-01 — IMPL-001, IMPL-004 (single trusted inbox, no pre-filtering)
- REC-CAP-02 — IMPL-001, IMPL-004, IMPL-006 (canonical inbox.org + declared extra inboxes)
- REC-CAP-03 — IMPL-001 (capture from anywhere; global keybind per README)
- REC-CAP-04 — IMPL-001, IMPL-003 (one heading per capture, multi-item aware)
- REC-CAP-05 — IMPL-003 (auto ORG_GTD_CAPTURED_AT inactive timestamp)
- REC-CAP-07 — IMPL-001, IMPL-005 (capture never forces processing; process is its own loop)
- REC-CLA-01 — IMPL-005 (top-to-bottom one-at-a-time loop until empty)
- REC-CLA-02 — IMPL-020, IMPL-035 (dispatch to exactly one disposition; source cut)
- REC-CLA-03 — IMPL-020 (transient with actionable/non-actionable branches)
- REC-CLA-04 — IMPL-040, IMPL-018, IMPL-034 (inline project creation in the flow)
- REC-CLA-05 — IMPL-025, IMPL-023 (type designs enforce concreteness; free WIP edit)
- REC-CLA-07 — IMPL-012, IMPL-009, IMPL-013 (cancel, in-place, duplicate — non-committal)
- REC-CLA-08 — IMPL-023 (quick action: done + archived immediately)
- REC-CLA-12 — IMPL-005, IMPL-020, IMPL-036 (single-key transient, DWIM, no forced forms)
- REC-PRJ-01 — IMPL-040 (single Projects index via ORG_GTD=Projects)
- REC-PRJ-02 — IMPL-053, IMPL-076, IMPL-087 (stuck detection config, views, DSL)
- REC-PRJ-03 — IMPL-043, IMPL-045, IMPL-046 (dependency-blocked ≠ stuck; WAIT counts)
- REC-PRJ-05 — IMPL-040 + org content model (support in subtree, reminders separate)
- REC-PRJ-11 — IMPL-040 (one Projects category; no personal/professional split; areas optional)
- REC-NXT-01 — IMPL-024, IMPL-067 (NEXT state, Actions list, engage surfacing)
- REC-NXT-02 — IMPL-024 + org body/checkbox content (inline data and sublists)
- REC-NXT-03 — design constraint met; priority exists only as opt-in org facility (IMPL-068, IMPL-090)
- REC-CAL-01 — IMPL-028, IMPL-075 (timestamp-required calendar type; invalid-metadata views)
- REC-CAL-02 — IMPL-032, IMPL-060 (future triggers; project incubate→reactivate by date)
- REC-DEL-01 — IMPL-025 (delegate with who + check-in)
- REC-DEL-02 — IMPL-025, IMPL-082 (DELEGATED_TO, timestamp, logbook; follow-up views)
- REC-SOM-01 — IMPL-033, IMPL-078 (someday with named lists; review view)
- REC-SOM-02 — IMPL-033, IMPL-032, IMPL-084 (list scan / later-start / review session)
- REC-SOM-03 — IMPL-059, IMPL-060, IMPL-064 (demote/promote with lossless state)
- REC-SOM-04 — IMPL-033 (ORG_GTD_SOMEDAY_LIST subcategories; per-list review IMPL-084)
- REC-TIC-01 — IMPL-032, IMPL-060, IMPL-064 (future-date, resurface, restore)
- REC-KNO-01 — IMPL-030 (reference disposition; topic stores via org file structure)
- REC-TRA-01 — IMPL-031 (trash as first-class organize destination; note: cancels+archives rather than deletes — audit trail by design)
- REC-REF-03 — IMPL-076, IMPL-050 (stuck-projects sweep + keyword repair)
- REC-ENG-01 — IMPL-065 (total-life engage view: calendar + due + all next actions)
- REC-ENG-04 — IMPL-066, IMPL-090 (context tags filter; single-list escape hatch is the default flat list IMPL-067)
- REC-ENG-07 — IMPL-065 (engage leads with the day's hard landscape)
- REC-HOR-01 — IMPL-099, IMPL-100 (areas-of-focus defcustom + assignment)
- REC-HOR-02 — IMPL-101, IMPL-102, IMPL-073 (project↔area link, organize hook, area review)
- REC-X-01 — IMPL-053, IMPL-087, IMPL-045 (next-action invariant + edna exception)
- REC-X-02 — IMPL-066, IMPL-090 (user-defined org tags as contexts; DSL filter)
- REC-X-03 — IMPL-068, IMPL-090 (org Effort attribute; set + filter)
- REC-X-05 — IMPL-020, IMPL-035 (hard-edged taxonomy; foreign-property cleanup)
- REC-X-06 — IMPL-034, IMPL-033 + org tags (customizable types, lists, contexts)
- REC-X-07 — IMPL-007, IMPL-036, IMPL-035 (re-clarify anything; DWIM; clean re-route)
- REC-X-08 — IMPL-100, IMPL-101, IMPL-102, IMPL-090 (area linkage with project propagation)
- REC-X-11 — IMPL-001, IMPL-005, IMPL-007, IMPL-020 (capture/clarify/organize as distinct modes)
- REC-X-12 — IMPL-002 minimal template; properties opt-in (only delegated/calendar require fields)
- REC-X-13 — IMPL-043, IMPL-055 (links/deps available, never gating)

(Areas with no fully-implemented entries: CHK, AGE, and UI.)

---

## Prior dispositions index (do not re-litigate)

| Decision | REC ids | Disposition |
|---|---|---|
| V-03 | REC-ENG-06 | Docs-only — three modes already map to commands |
| V-04 | REC-PRJ-06 (+07/08) | Helper-text now; structured NPM → v5 |
| V-05 | REC-AGE-01 | `#`-tags + discovery command now; module v5 |
| V-06 | REC-NXT-05 | Read/Review as Someday subcategory (small) |
| V-07 | REC-X-04, REC-ENG-03/05 energy | Optional energy attribute, off by default — near-term |
| V-09 | REC-KNO-03 | Non-goal (no CRM/contacts) |
| V-10 | REC-CHK-01 (+CAP-06, PRJ-07, PRJ-10, CAP-09, CHK-02) | General checklists + bundled trigger lists — near-term |
| V-12 | REC-X-10 | Onboarding scaffolding — backlog |
| V-13 | REC-X-09 | Docs example via DSL |
| V-18/V-20 | REC-X-14 | Mobile front-ends deferred |
| V-21 | REC-REF-05 | Configurable per-horizon review cadences — near-term |
| V-25 | REC-REF-10 | Annual review — IMPLEMENT (optional) |
| 2026-06-05 #1 | REC-REF-02 (+REF-01/04/07/08 riders) | IMPLEMENT guided three-phase review w/ cadence profiles |
| 2026-06-05 #2 | REC-REF-06 | IMPLEMENT via opt-in recurring maintenance tasks |
| 2026-06-05 #3 | REC-HOR-07 | DROP as mandate; reinstated MAY only |
| 2026-06-05 #4 | REC-HOR-06 | DOCS-only (credo practice in horizons.org) |
| 2026-06-05 #5 | REC-HOR-03/04/05 | IMPLEMENT linkage hooks on areas-of-focus pattern |
| 2026-06-05 #6 | REC-DEL-03 | IMPLEMENT delegate-whole-project (reframed) |
| 2026-06-05 #7 | REC-CLA-06 | Core covered; LLM-assisted clarify → v5 backlog |
