# GTD Spec → org-gtd.el Implementation Status

> For every spec in `2026-06-04-gtd-tool-specs-from-source.md`, does org-gtd.el implement it? Audited 2026-06-04 by 5 read-only exploration agents reading the actual source **and** the org-mode / org-edna / transient / org-agenda / view-language-DSL mechanisms behind it (not just literal functions). Evidence cites `file.el:line` or the specific construct.

## Verdict legend
- **Implemented** — first-class in org-gtd (turnkey).
- **Partial** — present but incomplete, manual, or caveated.
- **Via-config** — achievable through the view-language DSL / org settings / user config, not turnkey.
- **Via-org/dep** — delegated to a mechanism org-gtd wires up (org SCHEDULED, org-edna, CATEGORY, etc.).
- **Not-implemented** — absent. Where a resolved decision (§5 of the spec doc) already covers it, the disposition is noted.

## Scoreboard (72 specs)
| Verdict | Count |
|---|---|
| Implemented | 41 |
| Partial | 15 |
| Via-config / Via-org/dep | 5 |
| Not-implemented | 11 |

**Reading the gaps:** of the 11 not-implemented, **1 is a deliberate non-goal** (ORG-KNOW-03 contacts, decision V-09), **1 is "already maps, docs-only"** (ENG-06, decision V-03), and **5 already have a planned disposition** (PRJ-06→V-04, RR-01→V-06, X-04→V-07, X-07→V-10, REF-05 cadence→V-21). The 7 previously-unplanned gaps (REF-02, REF-06, HOR-06, HOR-07, HOR-03/04/05 linkage, ORG-DEL-03, CLA-06) were **adjudicated with the user on 2026-06-05** — see "Unplanned gaps — adjudicated" below. Net result: 4 IMPLEMENT (REF-02, REF-06, HOR-linkage, ORG-DEL-03 reframed), 1 DOCS-only (HOR-06), 1 DROP/spec-correction (HOR-07), 1 already-covered + new spin-off feature (CLA-06 → LLM-assisted clarify).

---

## Capture (org-gtd-capture.el)
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| CAP-01 | Implemented | `org-gtd-capture.el:82` `org-gtd-capture` wraps `org-capture`; raw template `* %?` to inbox, no pre-filtering. | — |
| CAP-02 | Implemented | `org-gtd-capture.el:62,111` canonical `inbox.org`; `org-gtd-process.el:38` + optional `org-gtd-additional-inbox-files`. | — |
| CAP-03 | Implemented | `org-gtd-capture.el:82` interactive, meant for global keybind (README:83). | "Always at hand" depends on user keybinding. |
| CAP-04 | Implemented | Template `* %?` = one heading per capture; process loop handles one heading at a time. | — |
| CAP-05 | Implemented | `org-gtd-capture.el:96` `--add-captured-at-timestamp` `:before-finalize` → `ORG_GTD_CAPTURED_AT` inactive timestamp. | — |
| CAP-06 | **Partial** | `org-gtd-horizons.el:40` + `org-gtd-clarify.el:53` show horizons during clarify; horizons file is hardcoded Purpose/Vision/Goals/Areas. | No **Incompletion Trigger** checklist and no *driven* mind-sweep walk-through. Ties to **X-07 / decision V-10** (checklists). |

## Clarify (org-gtd-clarify.el / org-gtd-wip.el / org-gtd-process.el)
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| CLA-01 | Implemented | `org-gtd-process.el:62` `org-gtd-process-inbox` iterates top-to-bottom, self-continues until empty. | — |
| CLA-02 | Implemented | `org-gtd-organize-core.el:111` dispatch routes to exactly one disposition; WIP cleaned, source cut. | — |
| CLA-03 | Implemented | `org-gtd-organize.el:71` transient with explicit Actionable / Non-actionable sections. | Decision is the menu choice; no literal "Is it actionable?" prompt (by design). |
| CLA-04 | Implemented | Type system `org-gtd-types.el:31+`; project route via `org-gtd-project-new`; delegated requires `:who`/`:when`. | — |
| CLA-05 | Implemented | Type designs enforce concreteness (delegated `:who`+`:when`, quick-action <2min); free edit in WIP. | No explicit concreteness *validation* (user discipline). |
| CLA-06 | **Partial** | `org-gtd-organize.el:82` project option + WIP free-edit + `clarify-project-insert-template`. | No assisted *extraction* of a hidden project from a fuzzy item — manual only. |
| CLA-07 | Implemented | `org-gtd-clarify-stop` cancels w/o organizing; duplicate-queue; `--skip-refile` edit-in-place. | — |

## Organize — taxonomy / Projects / Single Actions / Calendar
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| ORG-00 | Implemented | `org-gtd-organize.el:71` transient, 8 canonical categories; `organize-core.el:162` clears foreign props (hard edges). | — |
| ORG-PRJ-01 | Implemented | `org-gtd-projects.el:532` `org-gtd-project-new`; single Projects index via `ORG_GTD=Projects`. | — |
| ORG-PRJ-02 | Implemented | `org-gtd-projects.el:376` `org-gtd-stuck-projects`; `:867` `--is-stuck-p`; reflect view `:160`. | — |
| ORG-PRJ-03 | Implemented | `org-gtd-dependencies.el:46` BFS respects DEPENDS_ON/BLOCKS; WAIT counts as actionable so blocked≠stuck. | — |
| ORG-PRJ-04 | **Partial** | Multi-project task membership via `ORG_GTD_PROJECT_IDS`; default sequential edna deps (`projects.el:740`). | No subproject *roll-up*; parallel deps need manual setup. |
| ORG-PRJ-05 | Implemented | Support material lives in the project subtree (org content model); reminders are separate items. | No enforced "support drawer" structure (org-native nesting). |
| ORG-PRJ-06 | **Not-implemented** | No Natural Planning stages (Purpose/Vision/Brainstorm/Organize); project creation is mechanical. | **Disposition V-04:** helper-text now, structured planning → v5. |
| ORG-PRJ-07 | **Partial** | Any task (incl. a planning step) can be the NEXT action; readiness via `--find-ready-tasks`. | No Project-Planning **trigger list** / brainstorming view. Ties to X-07/V-10. |
| ORG-PRJ-08 | **Partial** | An R&D outcome can be made a normal project; no distinct "look-into / commitment-to-decide" type. | Convention only. |
| ORG-SA-01 | Implemented | `org-gtd-next-action.el:49` → dispatch `next-action`; `types.el:32` `:refile-target Actions`,`:state :next`; context via organize hooks. | (Note: single actions live in `next-action.el`, not a `single-action.el`.) |
| ORG-SA-02 | Implemented | Next-action headings carry arbitrary org body / `- [ ]` checklists / data. | No dedicated "lookup data" property (org content). |
| ORG-CAL-01 | Implemented | `types.el:54` calendar requires `ORG_GTD_TIMESTAMP`; `skip.el:89` flags missing/invalid; reflect surfaces them. | Strict hard-landscape enforcement. |
| ORG-CAL-02 | Implemented | `org-gtd-tickler.el:41` future-dated triggers; `projects.el:1172` incubate→reactivate by date; DSL `(when . future)`. | — |

## Organize — Delegate / Incubate / Knowledge / Habits / Quick / Read-Review
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| ORG-DEL-01 | Implemented | `org-gtd-delegate.el:44` → dispatch `delegated`; `types.el:40` `:state :wait`; `DELEGATED_TO`. | — |
| ORG-DEL-02 | Implemented | `DELEGATED_TO` (who) + `ORG_GTD_TIMESTAMP` (when) + LOGBOOK note; reviewable via engage/reflect + DSL `stuck-delegated`. | — |
| ORG-DEL-03 | **Not-implemented** | No project-altitude "outcomes I'm waiting on from others" list distinct from action-level. | Spec was "optional/medium." Unplanned — minor. |
| ORG-DEL-04 | **Partial** | `org-gtd-engage-tagged` filters by any tag; `org-gtd-context.el` resolves contexts. No per-person talk-to lists. | **Disposition V-05:** `#`-tags + discovery command now; module v5. |
| ORG-INC-01 | Implemented | `org-gtd-someday.el:51`; `types.el:76` `Someday`, no state; `reflect.el:148` review. | (Incubate = someday/tickler/reactivate, not an `incubate.el`.) |
| ORG-INC-02 | Implemented | (a) `org-gtd-someday-lists` review; (b) `tickler.el:41` later-starts + `reactivate.el:103` on chosen date. | — |
| ORG-INC-03 | Implemented | `projects.el:1228` `org-gtd-project-someday` (+ reactivate); state in `PREVIOUS_ORG_GTD`. | — |
| ORG-INC-04 | Implemented | `org-gtd-someday-lists` defcustom → `ORG_GTD_SOMEDAY_LIST`; `someday-review.el:70` filters by list. | This is the extension hook referenced by decision V-06/V-12. |
| ORG-KNOW-01 | Implemented | `org-gtd-knowledge.el:37` → `reference`; `types.el:103` `:disposition done-and-archive`. | Topic stores via file/refile structure, not explicit code. |
| ORG-KNOW-02 | **Partial** | Fast archive disposition; yearly purge schedulable via tickler. | No automated purge; <60s is design intent not enforced. |
| ORG-KNOW-03 | **Not-implemented** | No contact manager; contacts would be pure reference. | **By design — non-goal (decision V-09).** |
| ORG-HAB-01 | **Partial** | Habits first-class (`habit.el:38`, repeater `.+3d`). | No automated "process-project → Habit" conversion flow. |
| ORG-QA-01 | Implemented | `types.el:117` quick-action `done-and-archive`; clarify help "Do it now (<2 min)"; organize key `q`. | — |
| ORG-RR-01 | **Not-implemented** | No Read/Review type/queue anywhere. | **Disposition V-06:** model as a Someday/Maybe subcategory (uses ORG-INC-04 hook). |

## Reflect / Review (org-gtd-reflect.el / org-gtd-someday-review.el / command-center)
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| REF-01 | **Partial** | On-demand reflect views (`reflect.el:70+`) via command-center; surface missed/stuck items. | No recurring weekly-review *calendar event* or completion tracking. |
| REF-02 | **Not-implemented** | No guided Get-Clear / Get-Current / Get-Creative three-phase workflow; review is view-centric, not process-centric. | **Unplanned gap** — arguably the biggest UX gap vs. the book. |
| REF-03 | Implemented | `reflect.el:160` `reflect-stuck-projects`; DSL `stuck-project` → skip fn (`view-language.el:553`). | — |
| REF-04 | **Partial** | Runnable anytime via `command-center.el:42` (Stuck / Missed branches). | No explicit "get back on track" recovery flow (implicit via fixing surfaced items). |
| REF-05 | **Via-org/dep** | `horizons.el:51` horizons file + `reflect.el:70` area review. | No per-horizon *cadence/scheduling* wiring. **Disposition V-21:** implement configurable per-horizon reminders. |
| REF-06 | **Not-implemented** | `archive.el` is single-shot; no scheduled system-maintenance review or elevated-horizon calendar events. | **Unplanned gap.** |

## Engage (org-gtd-engage.el / org-gtd-agenda.el)
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| ENG-01 | Implemented | `engage.el:47` `engage-view-spec`: calendar-today + due tickler/delegated + **all** next actions (total-life inventory). | (Decision V-27: consider renaming to "total-life" view.) |
| ENG-02 | **Partial** | Calendar block first, then due items, then all next actions; context filter needs `engage-tagged`. | No automatic context-folding in default view. |
| ENG-03 | **Partial** | Prefix shows project + area-of-focus; effort filter in DSL. | No guided four-criteria order; energy absent; priority not foregrounded. |
| ENG-04 | Implemented | `engage-tagged` filters by tag; DSL `(tags . …)`; contexts = user-defined org tags. | Default view doesn't auto-fold by context. |
| ENG-05 | **Partial** | Effort filtering via DSL (`skip.el` effort pred / org `Effort`). Energy: **zero** references in codebase. | **Disposition V-07:** add energy attribute (→ X-04). |
| ENG-06 | **Not-implemented** | Three modes exist as *separate* commands (engage / clarify / capture), not a unified mode choice. | **Disposition V-03:** already maps to existing commands — **docs-only**, no build. |

## Horizons of Focus (org-gtd-horizons.el / org-gtd-areas-of-focus.el)
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| HOR-01 | Implemented | `areas-of-focus.el:39` defcustom list; `:47` set via `CATEGORY` property. | — |
| HOR-02 | Implemented | `areas-of-focus.el:81` project↔area link; `skip.el:181` area filter; DSL `(area-of-focus . …)`. | — |
| HOR-03 | **Via-org/dep** | Goals stored as H3 heading in `horizons.org` (`horizons.el:51`). | Not linked to projects / not in engage views. (Decision V-02: surface usefully.) |
| HOR-04 | **Via-org/dep** | Vision = H4 heading, free-form. | Static doc; no vision↔project linkage / reverse-engineering. |
| HOR-05 | **Via-org/dep** | Purpose & Principles = H5 heading. | Not integrated as a decision-criterion filter. |
| HOR-06 | **Not-implemented** | No per-area "rules of engagement" standards tooling (manual in horizons.org only). | **Unplanned gap** (spec confidence was medium). |
| HOR-07 | **Not-implemented** | No unified outline view spanning all six horizons (spec doc already notes this). | **Unplanned gap** — would need a custom view/DSL construct. |
| HOR-08 | **Partial** | Horizons replace ABC/123; per-horizon review exists. | Top-down priority is *manual reasoning*; engage filtering is bottom-up. |

## Cross-Cutting
| ID | Verdict | Evidence | Gaps / disposition |
|---|---|---|---|
| X-01 | Implemented | Next-action invariant via stuck detection (`view-language.el:623`, `skip.el:400`); edna exception for blocked. | — |
| X-02 | Implemented | Org tags; DSL `tags-matches` + static/dynamic context grouping. | — |
| X-03 | Implemented | Org `Effort`; `agenda-transient.el:167` set-effort; DSL `(effort . …)`. | — |
| X-04 | **Not-implemented** | No ENERGY property/attribute; grep "energy" = 0 hits. | **Disposition V-07:** add optional energy tag/property (near-term). |
| X-05 | Implemented | Tickler + org SCHEDULED; DSL `(when . past/today/future)` incl. comparisons. | — |
| X-06 | Implemented | `capture.el:96` auto `ORG_GTD_CAPTURED_AT`. | — |
| X-07 | **Not-implemented** | No checklist/trigger-list templates or surfacing UI. | **Disposition V-10:** general checklist support + bundled trigger lists. |
| X-08 | Implemented | Areas defcustom; user-defined org tags; DSL dynamic grouping; no hard-coded taxonomy. | — |
| X-09 | Implemented | `organize.el`/`organize-core.el` route to exactly one location (hard edges). | — |
| X-10 | Implemented | `areas-of-focus.el:93` project↔area via CATEGORY; tasks inherit parent area; DSL filter. | — |
| X-11 | **Partial** | Achievable via DSL (`group-by context`, tag filters) + optional date property. | No dedicated "project sub-list" UI. (Decision V-13: docs example via DSL.) |
| X-12 | **Via-config** | Onboarding = manual bulk-capture → process; standard capture templates. | No migration scaffolding / "finalize setup" meta-project. (Decision V-12 BACKLOG-ish.) |

---

## Cross-reference: gaps vs. the resolved decisions (§5)
Several already-decided items map directly onto real implementation gaps — these are the natural near-term build list:

| Decision | Spec gap it closes | Status |
|---|---|---|
| **V-07** energy attribute | X-04, ENG-03/05 | Not-implemented → near-term |
| **V-10** general checklists + trigger lists | X-07, CAP-06, ORG-PRJ-07 | Not-implemented → near-term |
| **V-21** per-horizon review cadences | REF-05 | Via-org → near-term |
| **V-05** `#`-tag agendas + discovery cmd | ORG-DEL-04 | Partial → near-term |
| **V-06** Read/Review as Someday subcategory | ORG-RR-01 | Not-implemented → small (reuses ORG-INC-04) |
| **V-04** structured Natural Planning | ORG-PRJ-06 (+07/08) | Not-implemented → v5 |

**By design / no build:** ORG-KNOW-03 (V-09 non-goal) · ENG-06 (V-03 docs-only).

## Unplanned gaps — adjudicated (interview 2026-06-05)
All 7 were interviewed with the user (quotes pulled from *Making It All Work* / *Getting Things Done* where disposition hinged on the text). Dispositions:

1. **REF-02 — guided three-phase reflect → IMPLEMENT.** A **guided sequential walkthrough** (Get Clear → Get Current → Get Creative), reusing the existing `org-gtd-someday-review` guided-session pattern and the `org-gtd-command-center` view aggregation. Must be **configurable**: per-phase content selection (e.g. which someday/maybe categories to review), a pluggable **incompletion-trigger-list / mind-sweep** (ties [[V-24]], [[V-10]]), and **named cadence profiles** — weekly is one of several (monthly / quarterly / biannual / yearly), each its own configurable review (ties [[V-21]]). *Audit correction:* the codebase already uses "reflect" (Book 2 term) and already has the guided someday-review session + command-center hub — the gap is the orchestration across phases + wiring "Get Clear" into the hub, not "no review process."
2. **REF-06 — system maintenance + elevated-horizon reviews → IMPLEMENT** via an `org-gtd-setup`-style **opt-in that injects recurring maintenance / higher-altitude review tasks** into the system (not a bespoke engine). *Conceptual rule:* cadence ≠ horizon altitude — a "quarterly reflect" may just be items reviewed less often, NOT a higher-horizon review; REF-02 cadence profiles stay orthogonal to altitude. *Refactor candidate (separate issue):* collapse the ~7–8 `reflect-stuck-*` "find broken items" commands into **one** command.
3. **HOR-07 — unified cross-horizon outline → DROP (spec-correction).** Over-literal reading; the books frame the six horizons as a *thinking framework / mind-sweep checklist / per-altitude cadence review*, not a one-screen "overview of my life" (Allen explicitly favors **bottom-up**, warns the top-down unified picture overwhelms). Intent already met by `horizons.org` (a foldable org outline) + per-altitude reflect cadences (REF-02).
4. **HOR-06 — personal credo / principles → DOCS-only.** `horizons.org` H5 (Purpose & Principles) already holds free-form text; document the practice of writing a personal credo and reviewing it during higher-altitude reflects. Group "rules of engagement" framing is **out** (single-user, per [[V-22]]); the book's individual reading is just "a document for review and inspiration."
5. **HOR-03/04/05 linkage → IMPLEMENT** as **optional hooks modeled on the existing area-of-focus pattern**, extended to the higher horizons (Goals / Vision / Purpose). *Refactor coupled to this:* resolve the **area-of-focus duplication** — areas can currently be defined in *both* `horizons.org` and org-gtd config; unify where areas/horizons are declared.
6. **ORG-DEL-03 → IMPLEMENT (reframed).** Not the exec "high-level list" (text confirms a delegated outcome is just *one* Waiting For item on our side — standard category already "includes all the projects you've delegated to others"). The real capability: **delegate an entire already-decomposed project** — collapses to a single Waiting For (track who/when/status) while the task breakdown is **preserved** for when it returns. Lives at the projects↔delegate seam.
7. **CLA-06 → core already covered** by the clarify WIP buffer + organize-as-Project template (the book describes no algorithmic extraction, just the human clarify questions). Spun off as a **new, separate feature idea: LLM-assisted clarify** (turn a vague item into a concrete project + next action) — v5 / backlog.

## Surprises from the audit
- **The view-language DSL (`org-gtd-view-language.el`, ~1130 lines)** is the engine behind most "Via-config" verdicts — declarative filters (type, when, deadline, area-of-focus, tags, priority, effort, clocked) compiled to org-agenda skip functions, with a `(native …)` escape hatch. Much "not built-in" is actually expressible here.
- **Stuck detection is multi-dimensional** — not just projects: `stuck-next-action`, `stuck-delegated`, `stuck-calendar`, `stuck-tickler`, `stuck-habit` (missing required metadata), plus computed `active/completed/incubated-project` types.
- **Someday/Maybe exceeds the spec** — a dedicated interactive `org-gtd-someday-review-mode` with deferred/clarify actions and per-list filtering.
- **Tickler/reactivate is a true bring-forward system** — projects park with full state in `PREVIOUS_ORG_GTD` and auto-restore on date.
- **Naming drift from the old module map:** no `single-action.el` (→ `next-action.el`), no `incubate.el` (→ `someday.el`/`tickler.el`/`reactivate.el`), no `review.el`/`oops.el` (→ `reflect.el` + `view-language` stuck types).
