# Semantic Cross-Index — feature-analysis corpus

Lookup table: GTD concept → every related artifact. Built 2026-06-10.

**Pointer conventions** (stated once, not repeated per entry):
- `REC-*` entries are defined in `recommended/INDEX.md`; each has one tagged Gherkin scenario (`@REC-<id>`) in the named file under `recommended/features/`.
- `IMPL-NNN` entries are defined in `implemented/INDEX.md` (evidence column there gives `file.el:line`).
- `WF-NN` entries are defined in `workflows/INDEX.md`; each is a tagged `Rule:` (`@WF-NN`) in the named file under `workflows/`.
- `DA94-NN` / `DA94-WNN` are defined in `sources/da-software-1994.md` (master feature list / workflows tables).
- Paths are relative to `docs/feature-analysis/` unless rooted with `docs/`.

---

## 1. Concept index

### agendas (per-person / per-meeting lists; "talk-to" lists; person view; meeting lifecycle)
- REC: AGE-01..03 → `organize-agendas.feature`
- IMPL: none (gap — no agenda-list feature; nearest: IMPL-090 `who` filter, IMPL-025 delegate "who")
- WF: 13 (context lists incl. agendas) → `organize.feature`; 41 (meeting closure) → `mindset.feature`
- DA94: 29, 32, 35, W06
- See also: delegation, contexts, closure loops

### annual review
- REC: REF-10 → `reflect.feature`
- WF: 30 (goals annual ritual) → `perspective.feature`
- See also: weekly review, review cadence, goals

### anti-over-structuring (simplicity; minimal forms; no priority scaffolding)
- REC: X-12 → `cross-cutting.feature`; NXT-03 → `organize-next-actions.feature`
- WF: 11 ("as simple as possible, but no simpler") → `organize.feature`
- See also: priority, organize taxonomy

### archive
- REC: PRJ-09 (active/archive split) → `organize-projects.feature`; UI-13 (calendar log) → `ui-software.feature`
- IMPL: 061–063 (archive commands/location), 062 (multi-project-aware), 080 (completed-projects view), 023/030/031 (auto-archive on quick-action/knowledge/trash)
- DA94: 05
- See also: trash, calendar, knowledge

### areas of focus (Horizon 2; 20,000 ft; responsibilities; "hats"; CATEGORY property)
- REC: HOR-01, HOR-02, X-08 (area linkage/inheritance) → `horizons.feature`, `cross-cutting.feature`
- IMPL: 099–102 (config/assign/propagate/hook), 073 (review by area), 070 (agenda prefix), 090 (area-of-focus filter)
- WF: 29 → `perspective.feature`; 04 (horizon capture sweep) → `capture.feature`
- DA94: 28 (inheritance), 36 (per-area rollup)
- See also: horizons, goals, rollup views

### broken agreements (renegotiate / complete / forgive; self-trust)
- REC: CLA-11 → `clarify.feature`
- WF: 40 → `mindset.feature`
- See also: clarify, someday/maybe

### calendar (hard landscape; day-specific vs time-specific; sacred territory)
- REC: CAL-01, CAL-02 → `organize-calendar.feature`; UI-04 (auto-migration with consent), UI-06 (exactly-one-date semantics), UI-18 (layered month/week/day), UI-13 (retrospective log) → `ui-software.feature`
- IMPL: 028 (calendar item), 069 (set/change GTD timestamp), 072 (event date sync), 065 (daily engage), 074/081 (missed/past-due views), 075 (stuck-metadata)
- WF: 12 (calendar discipline) → `organize.feature`; 20 (calendar-first daily review) → `reflect.feature`
- DA94: 15, 27, 41, W08
- See also: tickler, engage, dashboard/flags

### capture (collect — book-1 name; inbox; in-basket; open loops; trusted bucket)
- REC: CAP-01..05 (single inbox, leakproof funnel, ubiquitous, discrete items, auto date-stamp), CAP-07 (decoupled capture), CAP-08 (boundary hooks) → `capture.feature`
- IMPL: 001–004 (capture command/templates/timestamp/inbox file), 006 (additional inboxes), 132 (mode-line inbox count)
- WF: 02 (corralling), 05 (ongoing habit + interrupt shielding) → `capture.feature`
- DA94: 08, 11, 17, 18, 34
- See also: mind sweep, inbox zero, clarify

### checklists / trigger lists (incompletion trigger list; reusable lists; reflection prompts)
- REC: CHK-01, CHK-02 → `organize-checklists.feature`; CAP-06 (sweep driven by trigger lists) → `capture.feature`; PRJ-07/PRJ-10 (planning trigger list, verb checklist) → `organize-projects.feature`
- IMPL: none (gap — adjudicated V-10; grep confirms no checklist machinery, `audit/implementation-status-audit.md`)
- WF: 17 (checklists as external mind) → `organize.feature`; 03 (trigger-list sweep) → `capture.feature`
- DA94: 17
- See also: mind sweep, weekly review

### clarify (= "process", book-2 rename of book-1 "process/processing"; decide what it means)
- REC: CLA-01..12 → `clarify.feature`
- IMPL: 005 (inbox loop), 007–019 (WIP buffers, in-place, cancel, duplicate queue, side windows, templates, dependency helper), 036 (DWIM dispatch)
- WF: 06 (decision tree), 07 (session discipline), 08 (two-minute rule), 09 (delegation), 10 (outcome reframing) → `clarify.feature`
- DA94: 19, 20, 21, W02, W09
- See also: two-minute rule, organize taxonomy, inbox zero, WIP buffers, coaching

### closure loops (completion prompt; "project complete? what's next?"; no exit leaks)
- REC: UI-05 (mark-done prompt), UI-10 (closure at every exit) → `ui-software.feature`
- IMPL: 045 (auto-advance dependents via org-edna — the implemented analogue), 134 (state-change bookkeeping)
- WF: 25 (action-completion closure) → `engage.feature`; 41 ("what's the next action?" standard) → `mindset.feature`
- DA94: 25, 33, W04, W05, W06
- See also: dependencies, stuck project, delegation

### coaching (expert-system assistance; methodology-enforcing dialogs; teaching errors)
- REC: UI-03 → `ui-software.feature`
- IMPL: 008 (clarify guidance header), 041 (malformed-project teaching error), 017 (cheat-sheet window), 139/140 (corrective keyword errors, wizard)
- DA94: 04, 19
- See also: clarify, dashboard/flags

### contexts (context tags; @calls/@errands; first limiting factor)
- REC: X-02, ENG-04 (customizable, escape hatch below ~25 actions) → `cross-cutting.feature`, `engage.feature`
- IMPL: 066 (engage filtered by tag), 021 (tags hook), 090 (tags filter), 093 (tags-grouped block)
- WF: 13 (context-sorted lists) → `organize.feature`; 23 (four criteria) → `engage.feature`
- DA94: 16
- See also: four-criteria model, engage, customization

### control & perspective (Appropriate Engagement; Matrix of Self-Management; category corral)
- REC: (mindset-level; no direct REC entry — feeds REC-REF/HOR rationale)
- WF: 37 (control+perspective loop), 38 (matrix self-diagnosis), 39 ("what's true right now?") → `mindset.feature`; 33 (category corral: control first) → `perspective.feature`
- Provenance: B2-W-01..03, B2-W-24 (`workflows/raw/book2-workflows.md`)
- See also: horizons, current reality, weekly review

### current reality ("what's true right now?"; distractions inventory)
- REC: CAP-09 → `capture.feature`
- WF: 39 → `mindset.feature`
- See also: mind sweep, control & perspective

### customization (user-defined categories/types/lists; rename/merge; type registry)
- REC: X-06 → `cross-cutting.feature`; KNO-06 (list forms) → `organize-reference.feature`
- IMPL: 034 (type registry / `org-gtd-customize-type` / `org-gtd-user-types`), 002 (capture templates), 033 (named someday lists), 083 (custom missed views), 099 (areas), 139/140 (keyword mapping/wizard)
- WF: 11 (buckets shaped to meaning) → `organize.feature`
- DA94: 09 (rules), 40 (forms)
- See also: contexts, view DSL, organize taxonomy

### dashboard / flags (home view; system-integrity warnings; nags)
- REC: UI-01 (dashboard), UI-02 (warning flags: N in inbox, N stuck projects, calendar item not completed) → `ui-software.feature`
- IMPL: 132 (mode-line inbox count `GTD[5]`), 135 (command center), 075–077 (stuck views — pull, not push), 128 (graph validation warnings)
- DA94: 13, 14
- See also: stuck project, weekly review, engage

### delegation / waiting-for (handoff; who + date requested; check-ins)
- REC: DEL-01..05 → `organize-delegate.feature`
- IMPL: 025 (delegate command: who + check-in + WAIT + DELEGATED_TO), 026 (docs-only stale `read-func`), 027 (WAIT→delegate conversion), 082 (upcoming check-ins view), 074/081 (missed delegations), 090 (`who` filter)
- WF: 09 (handoff + tracking) → `clarify.feature`; 14 (waiting-for list management) → `organize.feature`
- DA94: 33, 38, 39, W05
- See also: agendas, closure loops, calendar

### dependencies (DAG; blockers; org-edna; linchpin actions)
- REC: PRJ-03 (blocked subprojects), PRJ-04 (parallel/sequential), X-13 (optional cross-linking) → `organize-projects.feature`, `cross-cutting.feature`
- IMPL: 043 (DAG property model), 044 (multi-project tasks), 045 (auto-advance), 046 (cycle prevention), 047/048 (add/remove tasks), 055 (ad-hoc dependency editing), 056 (integrity audit), 057 (edna vocabulary), 019 (dependency helper window), 129 (cross-project commands, latent)
- WF: 36 (activating moving parts) → `planning.feature`
- DA94: 03
- See also: projects, graph view, closure loops, stuck project

### duplicate queue (clarify one item into many; org-gtd-specific)
- REC/WF/DA94: none (pure tool affordance)
- IMPL: 013 (C-c d / C-c D), 014 (window side), 015 (data-loss guard)
- See also: clarify, WIP buffers

### effort / time-available (time estimate; `Effort` property; "show 10-minute actions")
- REC: ENG-05, X-03 → `engage.feature`, `cross-cutting.feature`
- IMPL: 090 (effort filter), 068 (set effort from agenda transient)
- WF: 23 → `engage.feature`
- See also: four-criteria model, energy, weird-time view

### energy (energy-level attribute — demoted to optional, off by default)
- REC: X-04 → `cross-cutting.feature`
- IMPL: none (grep `energy` = 0 hits in `*.el`, per `audit/implementation-status-audit.md`)
- WF: 23 → `engage.feature`
- See also: four-criteria model, effort

### engage (do; daily view; choosing the action of the moment)
- REC: ENG-01..08 → `engage.feature`
- IMPL: 065 (daily engage view), 066 (by context tag), 067 (flat NEXT list), 068 (agenda transient), 070/071 (prefixes, property display)
- WF: 20 (daily orientation), 23 (choosing), 24 (threefold work), 25 (closure) → `reflect.feature`, `engage.feature`
- See also: calendar, contexts, four-criteria model, threefold work, view DSL

### four-criteria model (context → time → energy → priority)
- REC: ENG-03 → `engage.feature`
- WF: 23 → `engage.feature`
- See also: contexts, effort, energy, priority, horizons

### goals (Horizon 3; 30,000 ft; 1–2 yr outcomes)
- REC: HOR-03 → `horizons.feature`
- IMPL: 103 (horizons file template includes Goals)
- WF: 30 → `perspective.feature`
- See also: horizons, vision, areas of focus, annual review

### graph view (project dependency visualization; SVG/ASCII DAG; org-gtd extension, no book source)
- REC/WF/DA94: none
- IMPL: 104–130 (entry point, mode, SVG/ASCII render, panel, mouse/keyboard nav, add/remove/edit tasks, project actions, export, filter engine [latent], validation, debug)
- Inventory: `implemented/inventory/graph-suite.md` (IMPL-GR / GR-01..27)
- See also: dependencies, projects, stuck project

### habits (recurring actions; "process projects"; org-habit)
- REC: PRJ-12 → `organize-projects.feature`
- IMPL: 029 (habit type: repeating SCHEDULED + STYLE=habit), 075 (stuck-metadata incl. habit), 090 (`not-habit` filter)
- See also: tickler, projects, recurrence (under tickler)

### horizons / altitude (six levels; runway→50k ft; perspective ladder)
- REC: HOR-01..08 → `horizons.feature`; UI-16 (per-horizon "sets") → `ui-software.feature`
- IMPL: 103 (horizons file: purpose/vision/goals), 016 (horizons side window in clarify)
- WF: 22 (cadence ladder) → `reflect.feature`; 26–32 (horizon conversations) → `perspective.feature`; 04 (horizon capture sweep) → `capture.feature`
- DA94: 36, 37
- See also: areas of focus, goals, vision, purpose, review cadence, life overview

### inbox zero (emptying "in"; one at a time, top first; nothing returns to in)
- REC: CLA-01, CLA-02 → `clarify.feature`
- IMPL: 005 (continuation loop until empty)
- WF: 07 (session discipline) → `clarify.feature`; 15 (e-mail to zero) → `organize.feature`
- DA94: 19, W09
- See also: capture, clarify

### knowledge / reference (filing; A–Z; sixty-second standard; browsable index)
- REC: KNO-01..06 → `organize-reference.feature`
- IMPL: 030 (knowledge type: DONE + archive), 086 (`reference` type filter)
- WF: 18 (sixty-second filing) → `organize.feature`
- DA94: 40
- See also: archive, support material, someday/maybe (collection lists: REC-SOM-04)

### life overview (unified cross-horizon view; "overview of my life")
- REC: HOR-07 → `horizons.feature`; UI-16 (rollup views) → `ui-software.feature`
- DA94: 36, 37
- See also: horizons, view DSL

### mind sweep (brain dump; bulk capture; corralling)
- REC: CAP-06, CAP-09 → `capture.feature`
- IMPL: none dedicated (capture command IMPL-001 is the substrate)
- WF: 02 (physical corralling), 03 (mind sweep w/ trigger lists), 04 (higher-horizon sweep) → `capture.feature`
- DA94: 17, W01
- See also: capture, checklists, onboarding

### natural planning model (purpose→vision→brainstorm→organize→next actions; 80/15/5)
- REC: PRJ-06, PRJ-07 → `organize-projects.feature`
- IMPL: none (gap — adjudicated V-04; grep confirms no NPM/brainstorm machinery)
- WF: 34 (five phases), 35 (unsticking: raise/lower focus), 36 (moving parts) → `planning.feature`
- DA94: 22, 23, W03
- See also: projects, stuck project, purpose

### next action (next physical visible action; defer; NEXT state)
- REC: NXT-01..05 → `organize-next-actions.feature`; CLA-04/05 (clarity test) → `clarify.feature`; X-01 (system invariant) → `cross-cutting.feature`
- IMPL: 024 (next-action type), 067 (flat NEXT view), 066 (by context), 142 (programmatic create)
- WF: 06 (decision tree), 13 (context lists), 27 (runway conversation), 41 (closure standard)
- DA94: 16, 26 (verb-first entry → REC-UI-07)
- See also: contexts, stuck project, two-minute rule, projects

### onboarding / setup (initial implementation; migration of old lists; meta-project)
- REC: X-10 → `cross-cutting.feature`; X-15 (≥50-actions completeness heuristic)
- IMPL: 137 (directory/file auto-creation), 140 (keyword wizard), 135 (command center)
- WF: 01 (full-scale implementation: time/space/tools) → `setup.feature`; 02–03 (initial gathering)
- See also: upgrade, mind sweep, capture

### organize taxonomy (hard edges; buckets; one meaning-category per item; dispatch)
- REC: X-05 (hard-edged taxonomy), X-06 (customizable), X-07 (low-friction re-routing) → `cross-cutting.feature`
- IMPL: 020 (transient organize menu), 035 (eight-stage pipeline), 036 (DWIM dispatch), 021/022 (hooks), 037–039 (refile targets/prompting/order), 138 (auto-save after organize)
- WF: 11 (bucket system) → `organize.feature`
- Gherkin: all `recommended/features/organize-*.feature` files
- See also: clarify, customization, refile (IMPL-037..039)

### print / export (hard copy; person briefs)
- REC: UI-12 → `ui-software.feature`
- IMPL: 125 (graph export SVG/DOT/ASCII — only export feature)
- DA94: 10
- See also: agendas (person brief), graph view

### priority (no ABC/123 coding; intuition against whole inventory; top-down via horizons)
- REC: NXT-03 (no priority scaffolding) → `organize-next-actions.feature`; HOR-08 (top-down) → `horizons.feature`
- IMPL: 090 (priority filter in DSL), 068 (set priority) — affordances, not scaffolding
- WF: 23 (ABC explicitly rejected), 26 (bottom-up control, top-down priority)
- See also: four-criteria model, horizons, anti-over-structuring

### projects (multi-step outcome <1 yr; projects list as index; 30–100 open)
- REC: PRJ-01..12 → `organize-projects.feature`; X-09 (named sub-lists) → `cross-cutting.feature`; UI-17 (project ledger/metadata) → `ui-software.feature`
- IMPL: 040–058 (create, validate, extend, DAG, multi-project, auto-advance, cancel, cookies, stuck config, iteration API), 018 (project templates), 059/060 (sleep/reactivate whole project)
- WF: 10 (project identification), 28 (projects conversation + hidden-project sweep), 34–36 (planning) → `clarify.feature`, `perspective.feature`, `planning.feature`
- DA94: 21, 22, 23, 24, 25
- See also: dependencies, stuck project, natural planning, subprojects, support material, graph view

### purpose & principles (Horizon 5; 50,000 ft; credo; values as decision criteria)
- REC: HOR-05, HOR-06 → `horizons.feature`
- IMPL: 103 (horizons file template includes Purpose)
- WF: 32 → `perspective.feature`
- See also: horizons, vision, natural planning (purpose phase)

### quick action — see two-minute rule

### reactivate (promote from someday/tickler; lossless state restore)
- REC: SOM-03 (bidirectional move) → `organize-someday.feature`
- IMPL: 060 (reactivate w/ PREVIOUS_* restore), 064 (state snapshot on incubation), 087 (computed incubated-project filters)
- WF: 16 (resurfacing arm) → `organize.feature`
- See also: someday/maybe, tickler

### read/review queue (longer-than-2-min reading; spare-time inventory)
- REC: NXT-05 → `organize-next-actions.feature`
- IMPL: none (gap — adjudicated V-06: map to someday subcategory or context list)
- See also: someday/maybe, contexts

### review cadence (ladder: daily/weekly/monthly/quarterly/annual; per-horizon reviews)
- REC: REF-05, REF-06 (maintenance review), REF-10 (annual) → `reflect.feature`
- IMPL: 073 (area-of-focus review), 084 (guided someday review) — partial substrate
- WF: 22 (cadence ladder) → `reflect.feature`; 26 (one horizon at a time) → `perspective.feature`
- See also: weekly review, horizons, annual review

### search (global find; multi-key)
- REC: UI-09 → `ui-software.feature`; KNO-05 (search alone insufficient — browsable index required)
- IMPL: none dedicated (view DSL filters are the analogue — IMPL-085..090)
- DA94: 06
- See also: view DSL, knowledge/reference

### someday/maybe (incubate; no current next action; collection lists)
- REC: SOM-01..04 → `organize-someday.feature`
- IMPL: 033 (someday w/ named lists), 059 (someday a whole project), 060/064 (snapshot/reactivate), 078 (list view), 084 (guided review sessions), 124 (from graph)
- WF: 16 (incubation system) → `organize.feature`
- DA94: 30
- See also: tickler, reactivate, read/review queue, trash (eliminate)

### stuck project (no next action; detection; kick-start)
- REC: PRJ-02 (invariant + detection), X-01, UI-02 (proactive flags), UI-05 (completion prompt) → `organize-projects.feature`, `cross-cutting.feature`, `ui-software.feature`
- IMPL: 053 (stuck-projects config/predicates), 076 (stuck projects view), 077 (stuck single actions), 075 (stuck metadata), 050 (recalculate keywords repair), 087 (computed stuck-* filters)
- WF: 35 (unsticking: raise/lower focus) → `planning.feature`; invariant in WF-21/25/28
- DA94: 14, 25, W04
- See also: projects, dashboard/flags, dependencies, closure loops

### subprojects (rollup vs own entry; parallel/sequential components)
- REC: PRJ-03, PRJ-04 → `organize-projects.feature`
- IMPL: 044 (multi-project membership), 043 (DAG covers sequencing)
- WF: 36 (moving parts) → `planning.feature`
- See also: projects, dependencies

### support material (project support; never the reminder; harvest at review)
- REC: PRJ-05, PRJ-09 → `organize-projects.feature`; REF-03 (support-material scan in review) → `reflect.feature`
- IMPL: none dedicated (org body/drawers/links are the substrate)
- WF: 19 (support material handling) → `organize.feature`
- See also: projects, knowledge/reference, weekly review

### threefold work (predefined / ad hoc / defining; 30–90 min/day defining budget)
- REC: ENG-06 → `engage.feature`; CLA-12 (speed budget) → `clarify.feature`
- WF: 24 → `engage.feature`
- See also: engage, clarify

### tickler (future trigger; 43 folders; resurface on date; recurrence + lead time)
- REC: TIC-01, TIC-02 → `organize-tickler.feature`; CAL-02 (day-specific future triggers) → `organize-calendar.feature`
- IMPL: 032 (tickler type), 059 (tickler a project), 064 (snapshot), 074/075 (missed/stuck-metadata views), 086 (type filter)
- WF: 16 (date-trigger arm of incubation) → `organize.feature`
- DA94: 31
- See also: someday/maybe, calendar, reactivate, habits

### time blocking (calendar blocks for big rocks, out of review)
- REC: REF-09 → `reflect.feature`
- WF: 21 (weekly review output) → `reflect.feature`
- See also: weekly review, calendar

### trash (eliminate; discard; no residue — note: code cancels+archives instead of deleting)
- REC: TRA-01 → `organize-trash.feature`
- IMPL: 031 (trash type: cancel + archive, audit trail — diverges from book "delete"), 048 (trash task from project), 121 (trash from graph)
- WF: 06 (eliminate branch) → `clarify.feature`
- DA94: 20 (eliminate), W02 ("dump it")
- See also: archive, clarify, someday/maybe

### two-minute rule (do it now; quick action; configurable threshold)
- REC: CLA-08, CLA-09 → `clarify.feature`; UI-15 (2-minute timer) → `ui-software.feature`
- IMPL: 023 (quick action: DONE + archive immediately)
- WF: 08 → `clarify.feature`
- DA94: 12, W02
- See also: clarify, next action

### upgrade (version migration; v2→v3→v4; ack; backward compat aliases)
- REC/WF/DA94: none (tool lifecycle, not GTD)
- IMPL: 145 (v2→v3), 146 (v3→v4), 147 (upgrade warning + ack), 148 (alias layer: review→reflect, oops→missed, incubate→tickler), 144 (emacs-28 shims)
- See also: onboarding
- Docs note: `implemented/INDEX.md` Discrepancies §1 — two documentation generations coexist

### view DSL (org-gtd-view-show; declarative agenda specs; filters; blocks)
- REC: UI-08 (sorting — a known DSL gap, DOC-44), UI-16 (rollup views) → `ui-software.feature`
- IMPL: 085–098 (DSL core, type/computed/time/done/metadata filters, native escape hatch, multi-block, prefix chain, skip functions, validation, sticky views, inactive-project exclusion), 083 (custom missed views)
- See also: engage, search, dashboard/flags, customization

### vision (Horizon 4; 40,000 ft; 3–5 yr; treasure map; 51% credible)
- REC: HOR-04 → `horizons.feature`
- IMPL: 103 (horizons file template includes Vision)
- WF: 31 → `perspective.feature`
- DA94: 30 ("include in ideal vision?" flag)
- See also: horizons, goals, purpose, natural planning

### weekly review (Get Clear / Get Current / Get Creative; de-briefing; staleness remedy)
- REC: REF-01..04, REF-07, REF-08 → `reflect.feature`
- IMPL: **gap** — no packaged weekly-review command (DOC-56 confirmed in `implemented/INDEX.md` Discrepancies §2); pieces exist: 073–084 (reflect views), 050 (repair), 061 (archive sweep)
- WF: 21 (three-phase walkthrough) → `reflect.feature`
- DA94: 01, W07
- See also: review cadence, stuck project, support material, time blocking, dashboard/flags

### weird-time view (micro-windows; very-short/low-effort actions)
- REC: ENG-08 → `engage.feature`
- IMPL: via effort filter (IMPL-090), not dedicated
- See also: effort, engage

### WIP buffers (clarify workspace; crash-safe temp files; org-gtd-specific)
- REC/WF/DA94: none (tool plumbing)
- IMPL: 007–012 (WIP buffer, major mode, in-place, auto-save/recovery, switch, cancel), 016/017/019 (side windows)
- See also: clarify, duplicate queue

---

## 2. ID-prefix directory

| Prefix | Pattern | Defined in | Referenced by |
|---|---|---|---|
| REC-CAP/CLA/PRJ/NXT/CAL/DEL/SOM/TIC/KNO/TRA/CHK/AGE/REF/ENG/HOR/UI/X | `REC-<AREA>-NN` (122) | `recommended/INDEX.md` | `recommended/features/*.feature` (as `@REC-*` tags), this index |
| IMPL | `IMPL-NNN` (148) | `implemented/INDEX.md` | this index |
| IMPL-WF | `WF-NN` in `implemented/inventory/workflow-modules.md` | that file | `implemented/INDEX.md` Sources column (cited **bare** as `WF-xx` — do not confuse with workflows registry WF-NN) |
| IMPL-VW | `VW-NN` | `implemented/inventory/views-and-system-modules.md` | `implemented/INDEX.md` Sources column |
| IMPL-GR | `GR-NN` | `implemented/inventory/graph-suite.md` | `implemented/INDEX.md` Sources column |
| IMPL-DOC | `DOC-NN` | `implemented/inventory/documented-features.md` | `implemented/INDEX.md` Sources + Discrepancies |
| WF (workflows) | `WF-NN` (41) | `workflows/INDEX.md` | `workflows/*.feature` (as `@WF-NN` tags) |
| B1-W | `B1-W-NN` (33) | `workflows/raw/book1-workflows.md` | `workflows/INDEX.md` merge log |
| B2-W | `B2-W-NN` (25) | `workflows/raw/book2-workflows.md` | `workflows/INDEX.md` merge log; `@src-B2-W-*` tags |
| DA94 | `DA94-NN` (41) | `sources/da-software-1994.md` (master feature list) | `recommended/INDEX.md` provenance; `workflows/INDEX.md` |
| DA94-W | `DA94-WNN` (9) | `sources/da-software-1994.md` (workflows table) | `workflows/INDEX.md`; `@src-DA94-W*` tags |
| B1-A/B/C/D | `B1-<reader>-NN`, `-VN` (vague) (166 audited) | `docs/source-material/extraction/book1-reader-{A,B,C,D}.md` | `audit/book1-readers-audit.md`, `audit/reconciliation-audit.md`, `recommended/INDEX.md` provenance, `docs/plans/2026-06-04-gtd-tool-specs-from-source.md` |
| B2-A/B/C/D | `B2-<reader>-NN`, `-DN` (delta), `-VN` (vague) (193 audited) | `docs/source-material/extraction/book2-reader-{A,B,C,D}.md` | `audit/book2-readers-audit.md`, `audit/reconciliation-audit.md`, `recommended/INDEX.md` provenance, plans doc |
| old canonicals | `CAP/CLA-NN`, `ORG-00`, `ORG-{PRJ,SA,CAL,DEL,INC,KNOW,HAB,QA,RR,TRASH}-NN`, `REF/ENG/HOR/X-NN` (72) | `docs/plans/2026-06-04-gtd-tool-specs-from-source.md` | `docs/plans/2026-06-04-gtd-implementation-status.md`, `audit/implementation-status-audit.md`, `audit/reconciliation-audit.md`, `recommended/INDEX.md` (old→new ID map in changelog) |
| V | `V-NN` (27 adjudications) | plans spec doc §5 | implementation-status doc, `recommended/INDEX.md`, `audit/reconciliation-audit.md` |
| G | `G-*` (8 gap adjudications, e.g. G-DEL-03, G-HOR-07) | `docs/plans/2026-06-04-gtd-implementation-status.md` | `recommended/INDEX.md`, `audit/reconciliation-audit.md` |
| tags | `@type-*`, `@strength-*`, `@src-*`, `@cadence-*` | feature-file conventions per `recommended/INDEX.md` and `workflows/INDEX.md` headers | all `.feature` files |

**Collision warning:** `WF-NN` is ambiguous. In `workflows/` it is a canonical workflow; in `implemented/INDEX.md`'s Sources column it is shorthand for `IMPL-WF-NN` from `implemented/inventory/workflow-modules.md`. Context (file) disambiguates.

## 3. File map

| File | Answers the question |
|---|---|
| `recommended/INDEX.md` | What *should* a single-user digital GTD tool do? 122 REC entries, provenance, strength, changelog vs 2026-06-04, old→new ID map. |
| `recommended/features/capture.feature` | Gherkin specs for REC-CAP-01..09. |
| `recommended/features/clarify.feature` | REC-CLA-01..12. |
| `recommended/features/organize-projects.feature` | REC-PRJ-01..12. |
| `recommended/features/organize-next-actions.feature` | REC-NXT-01..05. |
| `recommended/features/organize-calendar.feature` | REC-CAL-01..02. |
| `recommended/features/organize-delegate.feature` | REC-DEL-01..05. |
| `recommended/features/organize-someday.feature` | REC-SOM-01..04. |
| `recommended/features/organize-tickler.feature` | REC-TIC-01..02. |
| `recommended/features/organize-reference.feature` | REC-KNO-01..06. |
| `recommended/features/organize-trash.feature` | REC-TRA-01. |
| `recommended/features/organize-checklists.feature` | REC-CHK-01..02. |
| `recommended/features/organize-agendas.feature` | REC-AGE-01..03. |
| `recommended/features/reflect.feature` | REC-REF-01..10. |
| `recommended/features/engage.feature` | REC-ENG-01..08. |
| `recommended/features/horizons.feature` | REC-HOR-01..08. |
| `recommended/features/ui-software.feature` | REC-UI-01..18 (DA94 software ideas). |
| `recommended/features/cross-cutting.feature` | REC-X-01..15. |
| `implemented/INDEX.md` | What does org-gtd.el *actually* do? 148 IMPL entries with code evidence, status (code+docs/code-only/docs-only/latent), discrepancies, known code defects. |
| `implemented/inventory/workflow-modules.md` | Bottom-up inventory of core workflow modules (capture→archive); defines IMPL-WF ids. |
| `implemented/inventory/views-and-system-modules.md` | Bottom-up inventory of views/DSL/horizons/mode/upgrades; defines IMPL-VW ids. |
| `implemented/inventory/graph-suite.md` | Bottom-up inventory of the 10 graph-*.el files; defines IMPL-GR ids. |
| `implemented/inventory/documented-features.md` | What the docs *promise*; defines IMPL-DOC ids. |
| `workflows/INDEX.md` | What flows/cadences does Allen prescribe? 41 WF entries, merge log, B1-vs-B2 supersessions, hard invariants. |
| `workflows/setup.feature` | WF-01 (initial implementation). |
| `workflows/capture.feature` | WF-02..05. |
| `workflows/clarify.feature` | WF-06..10. |
| `workflows/organize.feature` | WF-11..19. |
| `workflows/reflect.feature` | WF-20..22. |
| `workflows/engage.feature` | WF-23..25. |
| `workflows/perspective.feature` | WF-26..33 (horizon conversations). |
| `workflows/planning.feature` | WF-34..36 (natural planning). |
| `workflows/mindset.feature` | WF-37..41 (control/perspective mindsets). |
| `workflows/raw/book1-workflows.md` | Raw B1-W-01..33 extraction from GTD (revised ed.). |
| `workflows/raw/book2-workflows.md` | Raw B2-W-01..25 extraction from Making It All Work. |
| `sources/da-software-1994.md` | Page-by-page transcription + classification of Allen's 1994 software designs; defines DA94-01..41 and DA94-W01..09; examples-vs-features table. |
| `audit/book1-readers-audit.md` | Are the Book-1 reader extractions faithful to the source text? (166 specs: 158 supported, 7 overreaches, ~17 missed items.) |
| `audit/book2-readers-audit.md` | Are the Book-2 reader extractions faithful? (193 items: 188 supported, 4 overreach, 1 example-as-feature.) |
| `audit/reconciliation-audit.md` | Does the 2026-06-04 synthesis trace to its 8 reader inputs? (0 phantom citations; orphans; count errors; dangling refs.) |
| `audit/implementation-status-audit.md` | Is the 2026-06-04 implementation-status doc still accurate vs current code at fa3d30e? (72 rows; 0 flipped verdicts.) |
| `index/semantic-index.md` | This file: concept → artifacts lookup. |
| `docs/plans/2026-06-04-gtd-tool-specs-from-source.md` | The original 72-spec reconciled synthesis (old CAP/CLA/ORG/REF/ENG/HOR/X ids; V-NN adjudications §5) — superseded by `recommended/INDEX.md` but still the V-NN registry. |
| `docs/plans/2026-06-04-gtd-implementation-status.md` | Spec-by-spec verdicts (Implemented/Partial/Via/Not) for the old 72 specs; G-* gap adjudications. |
| `docs/source-material/extraction/book{1,2}-reader-{A,B,C,D}.md` | Raw per-slice spec extractions from the two books (B1-*/B2-* ids), ground truth for provenance. |
