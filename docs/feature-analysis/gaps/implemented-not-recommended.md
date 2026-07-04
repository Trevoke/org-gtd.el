# Gap Analysis #4 — Implemented Features Not in the Recommended List

Deliverable #4 of the feature analysis. Every one of the 148 implemented
features (`implemented/INDEX.md`) classified against the 122 recommended
features (`recommended/INDEX.md`) and 41 canonical workflows
(`workflows/INDEX.md`).

**Classes:**
- **RECOMMENDED** — directly realizes one or more REC features.
- **SUPPORTING** — not itself in the canon, but necessary plumbing to realize
  recommended features/workflows in the Emacs/org-mode medium.
- **EXTENSION** — a genuine functional invention beyond the source material.

## Scoreboard

| Class | Count | Notes |
|---|---|---|
| RECOMMENDED | 49 | |
| SUPPORTING | 55 | includes the 3 docs-only entries (IMPL-026, -039, -091) |
| EXTENSION | 44 | of which 27 are the interactive graph-editor suite |
| **Total** | **148** | |

Of the 44 extensions: **1 conflicts** with a recommendation (IMPL-031,
audit-trail trash vs REC-TRA-01's "no residue"), **3 are in mild tension**
(IMPL-011 vs one-at-a-time clarify; IMPL-043/044 vs anti-over-structuring),
and the remaining 40 merely extend — almost all opt-in, which is itself what
REC-X-12 (anti-over-structuring: "structure is opt-in") asks of additions.

---

## 1. EXTENSION features, by theme

### Theme A — DAG dependency model (beyond sequential projects)

The books describe projects as a flat index whose plans live in support
material; sequencing is handled by exposing "the single linchpin action"
(REC-PRJ-04) and by the weekly review. DA94 sketched full relational linking
but the canon's verdict is REC-X-13: links are a MAY affordance, "never gate
the workflow on linkage." org-gtd builds a first-class dependency graph
instead.

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-043 | DAG dependency model: DEPENDS_ON / BLOCKS / FIRST_TASKS properties, multi-file via org-id | REC-PRJ-03/04 (blocked subprojects, sequential linchpin), REC-X-13 (DA94 relational linking, MAY) | **Extends** (tension w/ REC-X-12) | Serves the REC-X-01 next-action invariant mechanically; the sequential default is canon-faithful, but the general DAG is structure Allen handles with support material + review — REC-X-12 cautions against making life fit supplied forms. |
| IMPL-044 | Multi-project task membership (ORG_GTD_PROJECT_IDS) with AND-readiness | none; nearest REC-PRJ-04 (subproject rollup) | **Extends** (REC-X-12 risk) | No source ever puts one action in two projects; AND-readiness is relational-database thinking (DA94 flirted with it, books dropped it). Highest over-structuring risk in the package, though invisible until used. |
| IMPL-046 | Circular-dependency prevention with path display | none (only needed because of IMPL-043) | **Extends** | Pure consequence-management of the DAG invention; correct engineering, zero GTD provenance. |
| IMPL-047 | Add successor / blocker / root task to a project from any context | WF-36 (activating a project's moving parts), REC-PRJ-02 (kick-start action) | **Extends** | The add-a-task move is canonical; the successor/blocker vocabulary exists only because of the DAG. Serves GTD when used as "give the project a next action." |
| IMPL-048 | Remove task from project with predecessor→successor rewiring | REC-X-07 (low-friction re-routing) | **Extends** | Rewiring is DAG bookkeeping; the underlying "items move cheaply" goal is recommended. |
| IMPL-055 | Ad-hoc dependency editing between arbitrary tasks (add/remove blockers, dependents, clear, show) | REC-X-13 (optional cross-linking) | **Extends** | Stays on the right side of REC-X-13 because it is entirely opt-in and never gates the workflow. |
| IMPL-056 | Dependency integrity audit: broken ID refs, orphaned tasks | REC-REF-06 (system-maintenance review) — by analogy only | **Extends** | Maintenance tooling for a structure the canon doesn't have; necessary once IMPL-043 exists. |
| IMPL-062 | Multi-project-aware archiving: shared tasks refiled out instead of archived | none (consequence of IMPL-044) | **Extends** | Consequence-management; protects data integrity of an invented structure. |
| IMPL-019 | Live dependency-helper side window while editing a project in clarify | REC-PRJ-06 (NPM organize phase: components/sequences) | **Extends** | Visual aid for the DAG during clarify; mildly serves natural-planning's "organize" phase, but exists only because of the dependency model. |

### Theme B — Interactive project-graph editor suite (27 features)

A full SVG/ASCII graph application inside Emacs for viewing and editing
project DAGs. Closest canon overall: REC-X-13 (DA94's relational-link design,
adjudicated MAY), REC-PRJ-06 (NPM "organize" phase), WF-36 (activating moving
parts). **Verdict for the suite: extends, no direct conflict** — it is fully
opt-in (the textual workflow never requires it). The GTD-fit caution: Allen
says ~80% of projects need only an outcome and a next action and
"off-your-mind = planned enough" (REC-PRJ-06); a graph editor invites
plan-polishing beyond that threshold, the very over-structuring REC-X-12
warns about. Used during the weekly review's project sweep (REC-REF-03,
WF-35/36) it serves GTD; used daily it competes with engaging.

| IMPL | What it is | Nearest canon hook | Note |
|---|---|---|---|
| IMPL-104 | Open interactive dependency graph from anywhere | REC-X-13, WF-36 | suite entry point |
| IMPL-105 | Dedicated read-only graph major mode + keymap | — | medium plumbing for the suite |
| IMPL-106 | SVG DAG visualization (state colors, finish-line node, tooltips) | REC-PRJ-06 organize phase | the core invention |
| IMPL-107 | ASCII render mode for terminal Emacs | — | accessibility plumbing for the suite |
| IMPL-108 | Task details side panel (blockers/blocked lists) | REC-UI-17 (project metadata, partial echo) | |
| IMPL-109 | Mouse click node selection | — | suite plumbing |
| IMPL-110 | Keyboard navigation (n/p chain, TAB siblings, goto) | — | suite plumbing |
| IMPL-111 | Auto-select first actionable task on open | REC-PRJ-02 spirit (lead with the next action) | best GTD-aligned detail in the suite |
| IMPL-112 | Auto-refresh on file change (filenotify, debounce) | — | suite plumbing |
| IMPL-113 | Discoverable `?` transient menu | — | suite plumbing |
| IMPL-114 | Add root task (new or linked; FIRST_TASKS) | WF-36, REC-PRJ-02 kick-start | |
| IMPL-115 | Add successor task with multi-select predecessors | — | DAG editing |
| IMPL-116 | Add blocker task with multi-select blocked tasks | — | DAG editing |
| IMPL-117 | Bulk-edit a task's blockers | — | DAG editing |
| IMPL-118 | Bulk-edit a task's successors (shadowed key) | — | DAG editing |
| IMPL-119 | Change TODO state from the graph | WF-25 (completion closure) | recommended action, invented surface |
| IMPL-120 | Remove task with intelligent rewiring | REC-X-07 echo | |
| IMPL-121 | Trash task from graph (cleaned, canceled) | REC-TRA-01 echo (but see IMPL-031) | |
| IMPL-122 | Jump from graph node to org file | REC-UI-14 spirit (no forced context switch) | |
| IMPL-123 | Task relationships report | — | DAG introspection |
| IMPL-124 | Project-level incubate/someday/cancel from graph | REC-SOM-03 (demote) — invented surface | |
| IMPL-125 | Export graph to SVG / Graphviz DOT / ASCII | **REC-UI-12** (print/export first-class) | the one suite feature with direct DA94 backing |
| IMPL-126 | Quit / quit-and-kill with teardown | — | suite plumbing |
| IMPL-127 | Graph filtering engine (latent, no UI) | REC-ENG-04/05 filter spirit | latent — invention without even a user |
| IMPL-128 | Validation warnings on refresh (orphans, dangling edges, cycles) | REC-UI-02 spirit (integrity flags) | integrity-flag idea applied to invented structure |
| IMPL-129 | Cross-project dependency commands (unbound, M-x only) | REC-X-13 | latent-ish |
| IMPL-130 | Graph debug printers | — | developer tooling |

### Theme C — Duplicate-clarify queue and concurrent clarify

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-013 | Duplicate queue: clarify one inbox item into many (new-title / exact copies), live queue window | REC-CAP-04 (one thought = one heading), REC-CLA-06 (extract multiple inherent actions from fuzzy items) | **Extends** | A genuine invention in mechanism, but it exists to *enforce* the discrete-item invariant when a capture lumped several thoughts — serves GTD's goals directly. |
| IMPL-014 | Choose which side the pending-duplicates window appears on | — (part of IMPL-013) | **Extends** | Cosmetic plumbing for the queue invention. |
| IMPL-015 | Data-loss guard: quit/kill with queued duplicates prompts discard/save/cancel | trusted-system principle (REC-CAP-01 "trusted bucket") | **Extends** | Leak-proofing the invented queue; aligned with "leakproof" (REC-CAP-02) in spirit. |
| IMPL-011 | Switch between multiple concurrent clarification buffers | WF-05 (interrupt shielding / bookmark) | **Extends, mild conflict** | In tension with REC-CLA-01/WF-07 ("one at a time, top item first, never skipping to easier ones"); defensible only as interrupt handling, but it makes the anti-pattern cheap. |

### Theme D — Someday review-session machinery

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-084 | Guided one-at-a-time someday review sessions (defer/clarify/quit, per-list, stats, LOGBOOK review stamps) | REC-SOM-01 (weekly scan), REC-REF-02 (guided walkthrough), REC-REF-07 (every location swept) | **Extends** | The guided-session shape is recommended-adjacent (REC-REF-02 is itself a guided flow); the invention is per-item review timestamps + session statistics, metadata the books never ask for. Serves coverage (REC-REF-07) with low over-structuring cost since stamps are automatic, not user-filled forms. |

### Theme E — View-language DSL and clock-time filters

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-085 | `org-gtd-view-show`: a declarative view-definition language (alist specs → agenda views) | REC-UI-16 (composite rollups), REC-UI-08 (custom sorting/lists), REC-X-06 (user-defined lists) | **Extends** | The *views it builds* are recommended; the *language* is an invention. It honors REC-X-12 well: minimal opt-in structure replacing raw org-agenda elisp — an extension that reduces friction rather than adding forms. (Its 13 component filter features, IMPL-086..098, are classed SUPPORTING below because each realizes a recommended filter/view.) |
| IMPL-090 (sliver) + IMPL-054 (sliver) | Clock-time filters: `clocked`, `last-clocked-out` DSL keys; last-clock-out project query | none — GTD has no time-tracking concept | **Extends** | Pure org-mode-medium invention (leverages org clocking); harmless, but answers a question ("what was I working on?") Allen answers with the calendar log (REC-UI-13) and review, not timers. |

### Theme F — Trash with audit trail

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-031 | Trash = CNCL keyword + archive; audit trail preserved, never deleted (docs even say "delete") | REC-TRA-01 (trash destination: "deleted with no further tracking, **no residue** in the system") | **CONFLICTS** (the only direct conflict found) | The trash *destination* realizes REC-TRA-01's dispatch branch, but retention contradicts its letter: trashed items live forever in the archive. Defensible as digital-medium safety (deletion is irreversible; archives are out of sight), yet it quietly violates "no residue" and the docs/code disagreement shows the tension is unresolved. |

### Theme G — Identifier ergonomics

| IMPL | What it is | Closest canon | Verdict | Assessment |
|---|---|---|---|---|
| IMPL-141 | Human-readable org IDs (slug from heading text + timestamp) | none; serves the DAG/link plumbing (REC-X-13) | **Extends** | Invisible developer/file-format ergonomics; no GTD content at all, no conflict. |

---

## 2. SUPPORTING features (one line each)

| IMPL | Capability (short) | Supports |
|---|---|---|
| IMPL-004 | Inbox self-creation + `org-gtd-inbox-path` | REC-CAP-02 (one canonical target), WF-01 |
| IMPL-008 | Clarify major mode w/ guidance header + keymap | REC-CLA-01..04, REC-UI-03 (coaching), WF-06 |
| IMPL-009 | In-place update (skip refile) | REC-REF-03 (fix items where they sit), REC-X-07 |
| IMPL-010 | Crash-safe auto-saved WIP buffers | REC-CAP-01/CLA-02 (trusted, leakproof system) |
| IMPL-012 | Cancel clarification cleanly | REC-CLA-07 (non-committal clarify), WF-06 |
| IMPL-021 | Organize hooks on every organized item | REC-X-02/X-08 (attach context/area at organize), REC-X-05 |
| IMPL-022 | `org-gtd-organize-type-member-p` hook predicate | REC-X-05/X-06 (hook-author plumbing) |
| IMPL-026 | `org-gtd-delegate-read-func` — **docs-only/stale**, superseded by IMPL-034 | (was) REC-X-06; no code exists |
| IMPL-035 | Eight-stage organize pipeline + dispositions + property cleanup | REC-X-05 (hard-edged dispatch), WF-11 |
| IMPL-036 | DWIM dispatch across org buffer / agenda / clarify | REC-X-07 (cheap recategorization), WF-06 |
| IMPL-037 | Property-driven refile targets w/ silent auto-creation | REC-X-05 (location = meaning), REC-KNO-04 (zero-friction new category) |
| IMPL-038 | Per-type refile prompting + migration | REC-X-06, REC-X-05 |
| IMPL-039 | `org-reverse-note-order` — **docs-only/upstream** org variable | REC-X-06 (placement preference) |
| IMPL-049 | Change task TODO state from agenda/graph without visiting file | WF-25 (completion closure), REC-ENG-02 |
| IMPL-050 | Recalculate NEXT/TODO keywords (repair) | REC-X-01 invariant, REC-REF-03 (correct malformed projects) |
| IMPL-054 | Project iteration API (map projects/tasks) | REC-REF-03 (project sweep tooling); clock-out sliver → Theme E |
| IMPL-057 | Named org-edna finders/actions for user TRIGGER/BLOCKER | REC-UI-11 (rules automation), REC-X-13 |
| IMPL-058 | Multi-context resolution (buffer/agenda/graph) | REC-X-07, WF-23 (act from wherever you are) |
| IMPL-063 | Configurable archive location (yearly datetree) | REC-UI-13 (retrospective archive), REC-X-06 |
| IMPL-064 | Lossless PREVIOUS_* state snapshot on incubation | REC-SOM-03 (bidirectional promote/demote) |
| IMPL-068 | Agenda task-action transient (state/defer/dates/clock/effort…) | WF-23/25, REC-ENG-02, REC-X-03 (effort attribute entry) |
| IMPL-069 | Set/change GTD timestamp from heading or agenda | REC-CAL-01, REC-TIC-01, REC-DEL-02 (date upkeep) |
| IMPL-070 | Smart agenda prefixes (project → area → file) | REC-X-08 (area-grouped visibility) |
| IMPL-071 | Property display on agenda lines (vendored) | REC-DEL-02 (who/date visible on Waiting For views) |
| IMPL-072 | Set event date syncing inline body timestamp | REC-CAL-01 (calendar integrity) |
| IMPL-083 | User-defined custom missed views | REC-X-06, REC-UI-02 |
| IMPL-086 | DSL simple type filters | REC-X-05 categories as views; REC-SOM-01, REC-DEL-02, WF-14/16 |
| IMPL-087 | DSL computed filters (stuck-*, active/completed-project) | REC-UI-02, REC-PRJ-02, REC-X-01 |
| IMPL-088 | DSL semantic time filters + comparison windows | REC-REF-02 (scan upcoming), REC-UI-09 (date-range find) |
| IMPL-089 | DSL completion filters against CLOSED | REC-REF-02 Get Current, REC-UI-13 |
| IMPL-090 | DSL metadata filters (todo/area/who/tags/priority/effort/property) | REC-ENG-04/05, REC-X-02/X-03, REC-UI-09; clocked keys → Theme E |
| IMPL-091 | Filter keys `level`/`tags-match`/`invalid-timestamp` — **docs-only/stale** | (would support REC-ENG-04); validator rejects them |
| IMPL-092 | DSL native escape hatch (raw agenda blocks, sorting) | REC-UI-08 (sorting), REC-X-12 (keeps DSL minimal) |
| IMPL-093 | Multi-block views, defaults, special block types | REC-UI-01 (dashboard), REC-UI-16 (rollups), REC-ENG-02 (calendar-first layout) |
| IMPL-094 | DSL prefix fallback-chain | REC-X-08 (area-grouped display) |
| IMPL-095 | Reusable skip functions for hand-rolled agendas | REC-X-06 (user-built views) |
| IMPL-096 | DSL filter-key validation with named errors | REC-UI-03 (teach-while-correcting, applied to config) |
| IMPL-097 | Multiple simultaneous sticky views | REC-UI-01 (core lists visible together at a glance) |
| IMPL-098 | Done/cancelled-project tasks auto-excluded from active views | REC-ENG-01 (trusted choices: only live options shown) |
| IMPL-131 | `org-gtd-mode` global wiring (edna, hooks, agenda props, cleanup) | REC-X-01, WF-25, REC-DEL-02 — the wiring behind the automations |
| IMPL-133 | Automatic CLOSED timestamps on done | REC-REF-02 Get Current, REC-UI-13 (queryable record) |
| IMPL-134 | Automatic project bookkeeping on TODO changes | REC-X-01, WF-25, REC-UI-05 |
| IMPL-135 | GTD command-center transient menu | REC-UI-01 (single home surface), discoverability of all WFs |
| IMPL-136 | Six-stage observation-only hook system | REC-UI-11 (user automation hooks) |
| IMPL-137 | GTD directory + auto-created tasks/horizons files | WF-01 (setup), REC-HOR-03..05 |
| IMPL-138 | Opt-in auto-save after organize | trusted-system integrity (REC-CAP-01 spirit) |
| IMPL-139 | Custom TODO keyword mapping + validation | REC-X-06 (rename/customize vocabulary) |
| IMPL-140 | Keyword setup wizard | WF-01 (setup), REC-UI-03 (guided config) |
| IMPL-142 | Programmatic `org-gtd-create-item` API | REC-CAP-03 (capture hooks from anywhere), REC-UI-11 |
| IMPL-143 | Domain accessor/predicate API + value objects | internal plumbing for all REC-backed behaviors |
| IMPL-144 | Older-Emacs compat shims | medium plumbing (keeps the tool available, REC-X-14 spirit) |
| IMPL-145 | v2→v3 data migration | REC-X-10 (migration), system continuity/trust |
| IMPL-146 | v3→v4 data migration (idempotent, backup-confirmed) | REC-X-10, trusted-system continuity |
| IMPL-147 | Major-version upgrade warning + ack | trusted-system continuity (protects REC-CAP-01 trust) |
| IMPL-148 | Backward-compat alias layer | system continuity; protects users' existing WF habits |

---

## 3. Appendix — RECOMMENDED mappings (one line each)

| IMPL | → REC |
|---|---|
| IMPL-001 | REC-CAP-01, REC-CAP-02, REC-CAP-03 |
| IMPL-002 | REC-CAP-03 (back-link template: "any file… tagged into the inbox"), REC-X-06 |
| IMPL-003 | REC-CAP-05 |
| IMPL-005 | REC-CLA-01, REC-CLA-02 (WF-07) |
| IMPL-006 | REC-CAP-02 ("as few as needed, as many as necessary"; email subsystem) |
| IMPL-007 | REC-CLA-03, REC-CLA-04 (WF-06); REC-REF-04 (clarify any time) |
| IMPL-016 | REC-HOR-07, REC-HOR-08 (horizons visible while deciding) |
| IMPL-017 | REC-UI-03 (context-sensitive coaching panel) |
| IMPL-018 | REC-PRJ-06 (NPM schema), REC-CHK-01, REC-KNO-06 (templates/forms) |
| IMPL-020 | REC-X-05 (hard-edged dispatch), REC-CLA-03 (WF-11) |
| IMPL-023 | REC-CLA-08 (WF-08) |
| IMPL-024 | REC-NXT-01 |
| IMPL-025 | REC-DEL-01, REC-DEL-02 |
| IMPL-027 | REC-DEL-02 + REC-UI-03/REC-UI-10 (correcting state into a proper Waiting For) |
| IMPL-028 | REC-CAL-01 |
| IMPL-029 | REC-PRJ-12, REC-TIC-02 (recurrence) |
| IMPL-030 | REC-KNO-01 (partial — archive-only realization of Reference) |
| IMPL-032 | REC-TIC-01, REC-SOM-02(b) |
| IMPL-033 | REC-SOM-01, REC-SOM-04 (named sublists) |
| IMPL-034 | REC-X-06 (create/rename/customize categories) |
| IMPL-040 | REC-PRJ-01, REC-PRJ-02, REC-CLA-04 (inline project), REC-PRJ-04 (sequential default) |
| IMPL-041 | REC-UI-03 (teaching error that corrects while explaining the rule) |
| IMPL-042 | REC-X-07, WF-36; REC-REF-03 |
| IMPL-045 | REC-UI-05 / WF-25 (completion closure), REC-PRJ-02, REC-X-01 — automated rather than prompted |
| IMPL-051 | REC-CLA-11 (consciously drop), REC-REF-02 Get Creative (prune) |
| IMPL-052 | REC-UI-17 (partial: per-project done-vs-remaining count) |
| IMPL-053 | REC-PRJ-02, REC-UI-02 (stuck-project flags are DA94-recommended), REC-X-01 |
| IMPL-059 | REC-SOM-03 (demote an active project) |
| IMPL-060 | REC-SOM-03 (promote back), REC-SOM-02 |
| IMPL-061 | REC-REF-02 Get Current, REC-PRJ-09, REC-UI-13 |
| IMPL-065 | REC-ENG-02, REC-ENG-07 (calendar-first + discretionary time), WF-20 |
| IMPL-066 | REC-ENG-04, REC-X-02 (context-filtered lists) |
| IMPL-067 | REC-ENG-01 (total inventory), REC-ENG-04 escape hatch (single list) |
| IMPL-073 | REC-UI-16 (per-area rollup), REC-HOR-02, REC-REF-05 |
| IMPL-074 | REC-UI-02 ("previous calendar item not completed"), REC-REF-02 |
| IMPL-075 | REC-UI-02 (integrity warning flags generalized to metadata), REC-DEL-02 (every entry carries its date) |
| IMPL-076 | REC-PRJ-02, REC-UI-02, REC-X-01 (WF-21/28) |
| IMPL-077 | REC-UI-02, REC-X-01 |
| IMPL-078 | REC-SOM-01 (weekly scan), WF-16 |
| IMPL-079 | REC-REF-02 Get Current, REC-REF-10 (accomplishment inventory), REC-UI-13 |
| IMPL-080 | REC-REF-02 Get Current (archive candidates) |
| IMPL-081 | REC-UI-02, REC-REF-02; view-only cousin of REC-UI-04 |
| IMPL-082 | REC-DEL-02 (the date is the crucial field), WF-14 |
| IMPL-099 | REC-HOR-01 |
| IMPL-100 | REC-X-08 |
| IMPL-101 | REC-X-08 (actions inherit project's area) |
| IMPL-102 | REC-X-08, REC-HOR-02 ("what area does this reflect?" at organize time) |
| IMPL-103 | REC-HOR-03, REC-HOR-04, REC-HOR-05 |
| IMPL-132 | REC-UI-02 ("N items to process in your in-basket" — the DA94 dashboard count, verbatim) |

---

## 4. Method notes

- **Not rubber-stamped as extensions:** the mode-line inbox counter
  (IMPL-132), stuck-project views (IMPL-053/076/077), and stuck-metadata
  linting (IMPL-075) were flagged as inventions by the inventory agents but
  are in fact recommended — DA94's REC-UI-02 explicitly designs proactive
  integrity flags ("N items in your in-basket", "N projects with no next
  action defined", "previous calendar item not completed").
- **Not rubber-stamped as recommended:** the trash destination exists in the
  canon (REC-TRA-01) but the implementation's never-delete audit trail
  contradicts its "no residue" clause, so IMPL-031 sits in EXTENSION/conflict
  rather than the appendix.
- **DSL split:** the view *language* (IMPL-085) is an invention; its filter
  vocabulary (IMPL-086..098) is classified SUPPORTING because each key
  realizes a recommended filter or view. The two clock-time keys are the only
  filters with no canonical referent.
- **Docs-only entries** (IMPL-026, -039, -091) carry no code; they are parked
  in SUPPORTING with stale-doc notes so the 148-count reconciles.
- 27/44 extensions are the graph suite — a single product decision, not 27
  independent departures from canon.
