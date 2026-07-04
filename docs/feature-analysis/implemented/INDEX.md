# org-gtd.el — Canonical Registry of Implemented Features

Deliverable #2 of the feature analysis. Deduplicated consolidation of the four
inventories in `inventory/` (workflow-modules, views-and-system-modules,
graph-suite, documented-features). One entry per distinct feature.

Source-ID prefixes: WF = IMPL-WF (workflow-modules.md), VW = IMPL-VW
(views-and-system-modules.md), GR = IMPL-GR (graph-suite.md), DOC = IMPL-DOC
(documented-features.md). Evidence paths are relative to repo root.

**Status legend:**
- `code+docs` — implemented and documented
- `code-only` — implemented, not documented in the user manual
- `docs-only` — documented, no implementation found (verified by grep against source; see notes)
- `latent` — code exists and works, but no UI wiring (no keybinding/command path)

## Header / Counts

| Metric | Count |
|---|---|
| **Total deduplicated features** | **148** |
| Capture | 4 |
| Process / Clarify | 15 |
| Organize & categories | 20 |
| Projects & dependencies | 18 |
| Lifecycle (tickler/someday/archive/trash) | 6 |
| Engage & views | 8 |
| Review / Reflect | 12 |
| View-language DSL | 14 |
| Horizons & areas of focus | 5 |
| Graph suite | 27 |
| Mode & infrastructure | 15 |
| Upgrades / compat | 4 |
| `code-only` (implemented but undocumented) | 12 |
| `docs-only` (documented, no code — 2 verified stale, 1 upstream-org) | 3 |
| `latent` (code without UI wiring) | 2 |

---

## Capture

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-001 | Capture a thought into the GTD inbox from anywhere | command | org-gtd-capture.el:82 | WF-01, DOC-01 | code+docs | capture, inbox, org-capture |
| IMPL-002 | Define custom inbox capture templates (built-in: plain, with back-link) | customization | org-gtd-capture.el:40 | WF-02, DOC-02 | code+docs | capture, templates, customization |
| IMPL-003 | Every captured item auto-stamped with ORG_GTD_CAPTURED_AT (multi-item aware) | automatic-behavior | org-gtd-capture.el:96 | WF-03, DOC-03 | code+docs | capture, timestamp, property |
| IMPL-004 | Inbox file self-creates with explanatory banner; `org-gtd-inbox-path` for automation | automatic-behavior | org-gtd-capture.el:111 | WF-04, DOC-05 | code+docs | inbox, file-creation, path |

## Process / Clarify

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-005 | Process the whole inbox one item at a time until empty (continuation loop) | command | org-gtd-process.el:62 | WF-05, DOC-06 | code+docs | process, inbox, loop |
| IMPL-006 | Declare extra inbox files (mobile/email) processed in one continuous session | customization | org-gtd-process.el:38 | WF-06, DOC-04 | code+docs | inbox, additional-files, sync |
| IMPL-007 | Clarify any org heading or agenda item in a dedicated WIP buffer | command | org-gtd-clarify.el:245, :230 | WF-07, WF-09, DOC-07 | code+docs | clarify, wip-buffer, agenda |
| IMPL-008 | Clarify buffers run a dedicated major mode with guidance header and keymap | command | org-gtd-clarify.el:208 | WF-10, DOC-08 | code+docs | clarify, major-mode, keymap |
| IMPL-009 | In-place update (skip refile): C-u prefix or -n transient toggle keeps item location/level | command, automatic-behavior | org-gtd-clarify.el:255; org-gtd-organize-core.el:90 | WF-08, DOC-16 | code+docs | in-place, skip-refile, prefix-arg |
| IMPL-010 | Crash-safe WIP buffers backed by auto-saved temp files; re-clarifying reuses the buffer | automatic-behavior | org-gtd-wip.el:70; org-gtd-clarify.el:221 | WF-11, DOC-14 | code+docs | wip, auto-save, crash-recovery |
| IMPL-011 | Switch between multiple concurrent clarification buffers via completion | command | org-gtd-clarify.el:270 | WF-12, DOC-17 | code+docs | clarify, switch-buffer, concurrent |
| IMPL-012 | Cancel clarification (C-c C-k): cleanup temp file, restore windows, end session | command | org-gtd-clarify.el:288 | WF-13, DOC-15 | code+docs | clarify, cancel, abort |
| IMPL-013 | Duplicate queue: clarify one item into many (C-c d new title / C-c D exact), live queue window | command, automatic-behavior | org-gtd-clarify.el:317, :340, :630 | WF-14, DOC-13 | code+docs | duplicate, queue, clarify |
| IMPL-014 | Choose which side the pending-duplicates window appears on | customization | org-gtd-clarify.el:79 | WF-15, DOC-13 | code+docs | duplicate, window-position, customization |
| IMPL-015 | Data-loss guard: quitting Emacs / killing buffer with queued duplicates prompts discard/save/cancel | automatic-behavior | org-gtd-clarify.el:740, :762 | WF-16, DOC-14 | code+docs | data-safety, kill-emacs, queue |
| IMPL-016 | Horizons file shown in a read-only side window during clarification (auto or toggle) | customization, command | org-gtd-clarify.el:53, :279 | WF-17, DOC-09 | code+docs | horizons, side-window, clarify |
| IMPL-017 | Organize-types cheat-sheet side window (auto or toggle) | customization, command | org-gtd-clarify.el:71, :479 | WF-18, DOC-10 | code+docs | help-window, organize-types, clarify |
| IMPL-018 | Named project templates inserted under the item being clarified | customization, command | org-gtd-clarify.el:44, :371 | WF-19, DOC-12 | code+docs | project-templates, skeleton, clarify |
| IMPL-019 | Live dependency-helper side window rendering task relationships while editing a project | customization, command | org-gtd-clarify.el:63, :396 | WF-20, DOC-11 | code+docs | dependencies, helper-window, project |

## Organize & categories

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-020 | Transient organize menu: pick the item's GTD destiny with single keys (q/s/d/c/h/p/a/i/y/k/t) | command | org-gtd-organize.el:71 | WF-21, DOC-19, DOC-20, DOC-28 | code+docs | organize, transient, menu |
| IMPL-021 | Classic organize hooks run on every organized item (default: set tags) | customization, hook | org-gtd-organize-core.el:53 | WF-22, DOC-25 | code+docs | hooks, organize, metadata |
| IMPL-022 | `org-gtd-organize-type-member-p` lets hook authors branch on item type | hook | org-gtd-organize.el:94 | WF-23, DOC-25 | code+docs | hooks, type-predicate, api |
| IMPL-023 | Quick action: done in under two minutes → DONE + archive immediately | command | org-gtd-quick-action.el:36 | WF-24, DOC-20 | code+docs | quick-action, two-minute, archive |
| IMPL-024 | Next action (single action): NEXT state, refiled to Actions, shown in engage | command | org-gtd-next-action.el:49 | WF-25, DOC-20 | code+docs | next-action, single-action, organize |
| IMPL-025 | Delegate: prompts who + check-in date; WAIT state, DELEGATED_TO, logbook note | command | org-gtd-delegate.el:44 | WF-26, DOC-21, DOC-57 | code+docs | delegate, waiting-for, check-in |
| IMPL-026 | Custom "who" prompt for delegation via `org-gtd-delegate-read-func` | customization | NOT FOUND in source (grep of *.el); superseded by type-registry `:input-fn` (org-gtd-types.el) | DOC-21 | docs-only | delegate, read-func, stale |
| IMPL-027 | Manually flipping an action to WAIT offers conversion into a proper delegated item | automatic-behavior | org-gtd-next-action.el:71; org-gtd-mode.el:148 | WF-27, DOC-22 | code+docs | wait, delegate, conversion |
| IMPL-028 | Calendar item: date-specific, ORG_GTD_TIMESTAMP, appears in agenda that day | command | org-gtd-calendar.el:38 | WF-28, DOC-20 | code+docs | calendar, date, timestamp |
| IMPL-029 | Habit: repeating SCHEDULED + STYLE=habit, refiled to Habits | command | org-gtd-habit.el:38 | WF-29, DOC-20 | code+docs | habit, recurring, org-habit |
| IMPL-030 | Knowledge/reference: marked DONE and archived | command | org-gtd-knowledge.el:37 | WF-30, DOC-20 | code+docs | knowledge, reference, archive |
| IMPL-031 | Trash: canceled keyword + archived — audit trail preserved, never deleted (docs say "delete"; code cancels+archives) | command, automatic-behavior | org-gtd-trash.el:36; org-gtd-organize-core.el:238 | WF-31, WF-60, DOC-20 | code+docs | trash, cancel, audit-trail |
| IMPL-032 | Tickler: defer item to a future date; resurfaces in agenda then | command | org-gtd-tickler.el:41 | WF-32, DOC-71 | code+docs | tickler, incubate, defer |
| IMPL-033 | Someday/maybe with optional named lists (ORG_GTD_SOMEDAY_LIST) | command, customization | org-gtd-someday.el:51, :41 | WF-33, DOC-29, DOC-72 | code+docs | someday, maybe, lists |
| IMPL-034 | Declarative type registry; customize built-in types via `org-gtd-customize-type` / `org-gtd-user-types` | customization, data-model | org-gtd-types.el:31, :151, :259 | WF-34, DOC-89, DOC-90 | code+docs | type-registry, customization, extension |
| IMPL-035 | Eight-stage organize pipeline with dispositions (list / done-and-archive / cancel-and-archive) and foreign-property cleanup | hook, automatic-behavior | org-gtd-organize-core.el:246, :201, :166 | WF-35, DOC-91 | code+docs | pipeline, dispositions, hooks |
| IMPL-036 | DWIM dispatch: category commands work from org buffer, agenda, or clarify flow, project-aware | automatic-behavior | org-gtd-organize-core.el:288 | WF-36, DOC-68 | code+docs | dwim, dispatch, context |
| IMPL-037 | Property-driven refile targets (ORG_GTD_REFILE, multi-value) with silent auto-creation | automatic-behavior, data-model | org-gtd-refile.el:170, :220 | WF-62, DOC-24, DOC-76 | code+docs | refile, targets, auto-create |
| IMPL-038 | Per-type refile prompting (`:prompt-to-refile`, `org-gtd-refile-prompt-default`) with deprecated-var migration | customization | org-gtd-refile.el:89, :85, :147 | WF-63, DOC-23 | code+docs | refile, prompting, deprecation |
| IMPL-039 | `org-reverse-note-order` controls top/bottom placement of refiled tasks | customization | no org-gtd code (grep of *.el); upstream org-mode variable honored by org-refile | DOC-30 | docs-only | refile, note-order, upstream |

## Projects & dependencies

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-040 | Create a project from a multi-heading item: tasks wired sequentially, first task NEXT, cookie, refile | command | org-gtd-projects.el:532, :660 | WF-37, DOC-60 | code+docs | project, create, sequential |
| IMPL-041 | Malformed-project validation with teaching error (required shape, suggests single action) | automatic-behavior | org-gtd-projects.el:606 | WF-38, DOC-18 | code+docs | project, validation, error |
| IMPL-042 | Add a clarified item to an existing project (chained after current leaf) | command | org-gtd-projects.el:417, :1341 | WF-39, DOC-20 | code+docs | project, extend, attach |
| IMPL-043 | DAG dependency model: ORG_GTD_DEPENDS_ON / ORG_GTD_BLOCKS / ORG_GTD_FIRST_TASKS, multi-file via org-id | data-model | org-gtd-dependencies.el:182, :46 | WF-40, DOC-61, DOC-96 | code+docs | dag, dependencies, properties |
| IMPL-044 | Tasks can belong to multiple projects (ORG_GTD_PROJECT_IDS, AND-readiness) | data-model | org-gtd-projects.el:504, :169 | WF-41, DOC-73 | code+docs | multi-project, shared-task, readiness |
| IMPL-045 | Completing a task auto-advances dependents to NEXT (org-edna trigger) | automatic-behavior | org-gtd-projects.el:732, :1417 | WF-42, DOC-60, DOC-80 | code+docs | auto-advance, org-edna, trigger |
| IMPL-046 | Circular-dependency prevention with path display | automatic-behavior | org-gtd-dependencies.el:95 | WF-43, DOC-61 | code+docs | cycle, validation, dag |
| IMPL-047 | Add successor / blocker / root task to a project from any context | command | org-gtd-projects.el:238, :256, :274 | WF-44, DOC-68 | code+docs | project, add-task, successor |
| IMPL-048 | Remove task from project (rewires predecessors→successors) or trash it; incl. `org-gtd-remove-task-from-project` variant | command | org-gtd-projects.el:292, :310; org-gtd-task-management.el:788 | WF-45, DOC-68, DOC-69 | code+docs | remove-task, trash-task, rewire |
| IMPL-049 | Change project task TODO state from agenda or graph without visiting the file | command | org-gtd-projects.el:328 | WF-46, DOC-68 | code+docs | todo-state, agenda, dwim |
| IMPL-050 | Recalculate NEXT/TODO keywords for one project or all projects (repair) | command | org-gtd-projects.el:448, :457 | WF-47, DOC-97 | code+docs | fix-keywords, repair, review |
| IMPL-051 | Cancel a whole project (commands + cascade prompt on manual CNCL) | command, automatic-behavior | org-gtd-projects.el:388, :148 | WF-48, DOC-70 | code+docs | cancel, project, cascade |
| IMPL-052 | Project progress cookies [N/M][P%] with position customization and auto/bulk update | customization, automatic-behavior, command | org-gtd-core.el:218; org-gtd-projects.el:1006, :1017 | WF-49, VW-45, DOC-75 | code+docs | progress, cookies, project |
| IMPL-053 | Stuck-project detection config (`org-gtd-stuck-projects`) and predicates | command, data-model | org-gtd-projects.el:376, :867 | WF-50 | code+docs | stuck, projects, config |
| IMPL-054 | Project iteration API: map over projects/tasks, last clock-out query | data-model | org-gtd-projects.el:1114, :1093, :1135 | WF-51, DOC-92 | code+docs | api, traversal, clock |
| IMPL-055 | Ad-hoc dependency editing between arbitrary tasks (add/remove blockers, dependents, clear, show) | command | org-gtd-task-management.el:45–:601 | WF-52, DOC-69 | code+docs | dependencies, blockers, editing |
| IMPL-056 | Dependency integrity audit: broken ID refs and orphaned tasks | command | org-gtd-task-management.el:649 | WF-53, DOC-69 | code+docs | audit, validation, orphans |
| IMPL-057 | Named org-edna finders/actions usable in user TRIGGER/BLOCKER properties | hook, data-model | org-gtd-projects.el:1397–:1426 | WF-67 | code-only | org-edna, vocabulary, extension |
| IMPL-058 | Multi-context resolution: project commands work from org buffer, agenda line, or graph view | automatic-behavior | org-gtd-context.el:63 | VW-49, DOC-68 | code+docs | context, resolution, dwim |

## Lifecycle (tickler / someday / archive / trash)

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-059 | Tickler/someday an entire project with safety checks (external-dependency warning, shared-task skip) | command, automatic-behavior | org-gtd-projects.el:1173, :1228 | WF-54, DOC-71, DOC-72 | code+docs | project, incubate, someday |
| IMPL-060 | Reactivate a slept item or project: PREVIOUS_* restore, property confirmation, graph view for projects | command, data-model | org-gtd-reactivate.el:103; org-gtd-projects.el:1279 | WF-55, DOC-74 | code+docs | reactivate, restore, previous-state |
| IMPL-061 | Archive item at point / one-sweep archive of everything completed | command | org-gtd-archive.el:115, :83 | WF-57, DOC-58 | code+docs | archive, completed, cleanup |
| IMPL-062 | Multi-project-aware archiving: shared tasks refiled out instead of archived with the project | automatic-behavior | org-gtd-archive.el:210, :155 | WF-58 | code-only | archive, shared-task, multi-project |
| IMPL-063 | Configurable archive location (yearly datetree default; nil → plain org-archive-location) | customization | org-gtd-archive.el:45, :126 | WF-59, DOC-58 | code+docs | archive, location, datetree |
| IMPL-064 | Lossless state snapshot (PREVIOUS_ORG_GTD/TODO/props) on any move into someday/tickler | data-model, automatic-behavior | org-gtd-reactivate.el:37; org-gtd-projects.el:1049 | WF-61, DOC-27 | code+docs | state-save, snapshot, incubation |

## Engage & views

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-065 | Daily engage view: today's schedule + tickler/delegated due + all next actions | command, view | org-gtd-engage.el:84 | VW-01, DOC-31 | code+docs | engage, daily, agenda |
| IMPL-066 | Next actions filtered by tag (context tags), with completion | command, view | org-gtd-engage.el:90 | VW-02, DOC-94 | code+docs | engage, tags, context |
| IMPL-067 | All NEXT actions in one flat list | command, view | org-gtd-engage.el:99 | VW-03, DOC-32 | code+docs | next-actions, list, view |
| IMPL-068 | Agenda task action transient (state, defer, dates, clock, effort/priority/tags/note, clarify) | command | org-gtd-agenda-transient.el:203 | VW-04, DOC-33 | code+docs | agenda, transient, actions |
| IMPL-069 | Set/change GTD timestamp (Calendar/Delegated/Tickler) from heading or agenda | command | org-gtd-core.el:83 | VW-05, DOC-34 | code+docs | timestamp, reschedule, dwim |
| IMPL-070 | Smart agenda prefixes: project → area-of-focus → file fallback chain, width/ellipsis customization | automatic-behavior, customization | org-gtd-agenda.el:110; org-gtd-core.el:230, :239 | VW-06, DOC-36 | code+docs | prefix, agenda, fallback |
| IMPL-071 | Org property display on agenda lines (vendored org-agenda-property) | automatic-behavior, customization | org-gtd-agenda-property.el:117 | VW-07, DOC-35 | code+docs | agenda, properties, display |
| IMPL-072 | Set event date on heading at point, syncing the inline body timestamp | command | org-gtd-core.el:446 | VW-48 | code-only | event-date, timestamp, sync |

## Review / Reflect

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-073 | Review by area of focus: multi-block overview filtered to one life area | command, view | org-gtd-reflect.el:70 | VW-08, DOC-93 | code+docs | review, area-of-focus, overview |
| IMPL-074 | Missed items view: past-due calendar/tickler/delegated | command, view | org-gtd-reflect.el:105 | VW-09, DOC-52 | code+docs | missed, past-due, review |
| IMPL-075 | Stuck-metadata views: calendar/delegated/habit/tickler items with missing/invalid metadata | command, view | org-gtd-reflect.el:113–:139 | VW-10, DOC-50 | code+docs | stuck, metadata, audit |
| IMPL-076 | Stuck projects view: undone tasks but no NEXT/WAIT | command, view | org-gtd-reflect.el:160 | VW-11, DOC-49 | code+docs | stuck, projects, review |
| IMPL-077 | Stuck single-actions view: undone Actions not in NEXT state | command, view | org-gtd-reflect.el:170 | VW-12, DOC-50 | code+docs | stuck, single-action, anomaly |
| IMPL-078 | Someday/maybe agenda list view | command, view | org-gtd-reflect.el:148 | VW-13, DOC-54 | code+docs | someday, list, view |
| IMPL-079 | Recently completed items view (last N days, prefix arg) | command, view | org-gtd-reflect.el:184 | VW-14, DOC-55 | code+docs | completed, weekly-review, closed |
| IMPL-080 | Completed projects view (archive candidates) | command, view | org-gtd-reflect.el:200 | VW-15, DOC-55 | code+docs | completed, projects, archive |
| IMPL-081 | Missed engagements views (delegated / calendar / projects past deadline or start), combined or per category | command, view | org-gtd-reflect.el:254–:273 | VW-16, DOC-51 | code+docs | missed, oops, engagements |
| IMPL-082 | Upcoming delegated check-ins view | command, view | org-gtd-reflect.el:281 | VW-17, DOC-53 | code+docs | delegated, upcoming, follow-up |
| IMPL-083 | User-defined custom missed views appended via customization | customization, command | org-gtd-reflect.el:295, :309 | VW-18, DOC-51 | code+docs | custom-views, missed, dsl |
| IMPL-084 | Guided one-at-a-time someday review sessions (defer/clarify/quit, stats, per-list) | command | org-gtd-someday-review.el:185 | WF-56, VW-19, DOC-54 | code+docs | someday, review-session, interactive |

## View-language DSL

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-085 | `org-gtd-view-show`: declarative agenda views from alist specs | DSL | org-gtd-view-language.el:1081 | VW-20, DOC-40 | code+docs | dsl, views, declarative |
| IMPL-086 | Simple type filters (next-action, delegated, calendar, tickler, someday, project, habit, reference, trash, quick-action) | DSL | org-gtd-view-language.el:215, :479 | VW-21, DOC-41 | code+docs | dsl, type-filter, org-gtd |
| IMPL-087 | Computed type filters (stuck-*, active/completed/tickler/incubated-project) | DSL | org-gtd-view-language.el:220, :517–:623 | VW-22, DOC-41 | code+docs | dsl, computed, stuck |
| IMPL-088 | Semantic time filters (when/deadline/scheduled: past/today/future) and comparison windows `(< "7d")` | DSL | org-gtd-view-language.el:824; org-gtd-skip.el:509 | VW-23, DOC-42 | code+docs | dsl, time, duration |
| IMPL-089 | Completion filters: `(done . t/recent/N/(< "7d"))` against CLOSED | DSL | org-gtd-view-language.el:728, :672 | VW-24, DOC-43 | code+docs | dsl, done, closed |
| IMPL-090 | Metadata filters: todo, area-of-focus, who, tags, priority, effort, clocked, last-clocked-out, property, not-habit (with project-fallback semantics) | DSL | org-gtd-view-language.el:812; org-gtd-skip.el:75–:442 | VW-25, DOC-43 | code+docs | dsl, filters, metadata |
| IMPL-091 | Documented filter keys `level`, `tags-match`, `invalid-timestamp` | DSL | NOT in known-filter-keys (org-gtd-view-language.el:178); validation rejects them (:776). Doc'd at doc/reference.org:3683, :3872, :3902 | DOC-43 | docs-only | dsl, stale, validation |
| IMPL-092 | Native escape hatch: embed raw org-agenda blocks in DSL specs (sorting etc.) | DSL | org-gtd-view-language.el:251 | VW-26, DOC-44 | code+docs | dsl, native, escape-hatch |
| IMPL-093 | Multi-block views: explicit `blocks`, implicit expansion of repeated `type`, four-tier defaults, special block types (calendar-day, todo, agenda, tags-grouped) | DSL | org-gtd-view-language.el:1037, :1010, :264–:449 | VW-27, DOC-44, DOC-46, DOC-47 | code+docs | dsl, blocks, defaults |
| IMPL-094 | Prefix fallback-chain DSL `(prefix . (project area-of-focus "—"))` with width and block inheritance | DSL | org-gtd-view-language.el:947, :209 | VW-28, DOC-45 | code+docs | dsl, prefix, chain |
| IMPL-095 | Reusable skip functions for hand-rolled org-agenda-custom-commands | hook | org-gtd-skip.el:57, :61 | VW-29, DOC-48 | code+docs | skip-functions, agenda, api |
| IMPL-096 | Filter-key validation: typo'd keys rejected with named user-error | automatic-behavior | org-gtd-view-language.el:178, :776 | VW-30 | code-only | dsl, validation, errors |
| IMPL-097 | Multiple simultaneous sticky GTD views via distinct dispatch KEYS | view | org-gtd-view-language.el:1081, :1133 | VW-31, DOC-40 | code+docs | sticky, concurrent, views |
| IMPL-098 | Tasks of fully done/cancelled projects auto-excluded from active views (fail-open) | automatic-behavior | org-gtd-view-language.el:916; org-gtd-skip.el:408 | DOC-98 (notes in VW-22, VW-25) | code+docs (CHANGELOG-only doc) | inactive-project, filtering, automatic |

## Horizons & areas of focus

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-099 | Configurable areas of focus driving completion/validation/review | customization | org-gtd-areas-of-focus.el:39 | VW-32, DOC-93 | code+docs | areas-of-focus, horizons, customization |
| IMPL-100 | Assign area of focus to heading at point (CATEGORY property) | command | org-gtd-areas-of-focus.el:47 | VW-33, DOC-93 | code+docs | area-of-focus, category, assign |
| IMPL-101 | Assign area of focus from agenda, propagating across the whole project | command | org-gtd-areas-of-focus.el:57, :93 | VW-34, DOC-93 | code+docs | area-of-focus, agenda, project |
| IMPL-102 | Area-of-focus prompt as an organize hook (skips types where it makes no sense) | hook | org-gtd-areas-of-focus.el:89, :108 | VW-35, DOC-93 | code+docs | area-of-focus, hook, organize |
| IMPL-103 | Horizons file (purpose/vision/goals), auto-created from template | customization, automatic-behavior | org-gtd-horizons.el:40, :51 | VW-36, DOC-95 | code+docs | horizons, file, template |

## Graph suite

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-104 | Open interactive project dependency graph from anywhere (context-inferred) | command | org-gtd-graph-mode.el:154 | GR-01, DOC-62 | code+docs | graph, project, entry-point |
| IMPL-105 | Dedicated read-only graph major mode with single-key bindings (note: `S` double-bound, see Discrepancies) | command | org-gtd-graph-mode.el:127, :54 | GR-02, DOC-62, DOC-63 | code+docs | graph, major-mode, keymap |
| IMPL-106 | SVG DAG visualization: Okabe-Ito state colors, finish-line project node, tooltips, selection highlight | view | org-gtd-graph-view.el:255; org-gtd-svg-render.el:37 | GR-03, DOC-62 | code+docs | svg, dag, visualization |
| IMPL-107 | ASCII render mode toggle (`v`) for terminal Emacs, with state glyphs | command, customization | org-gtd-graph-view.el:340, :48 | GR-04, DOC-65 | code+docs | ascii, terminal, render-mode |
| IMPL-108 | Task details side panel (magit-style split, blockers/blocked lists, configurable ratio) | view, customization | org-gtd-graph-ui.el:61, :126, :42 | GR-05, DOC-65 | code+docs | details, side-panel, split |
| IMPL-109 | Mouse click selection of nodes (SVG hit-testing and ASCII coordinates) | command | org-gtd-graph-view.el:141 | GR-06, DOC-64 | code+docs | mouse, selection, click |
| IMPL-110 | Keyboard navigation: n/p chain, TAB siblings (configurable semantics), G goto-by-name | command, customization | org-gtd-graph-navigation.el:228, :196, :259, :38 | GR-07, DOC-64 | code+docs | navigation, keyboard, siblings |
| IMPL-111 | Auto-select first actionable task on open | automatic-behavior | org-gtd-graph-view.el:204; org-gtd-graph-data.el:488 | GR-08 | code-only | auto-select, actionable, bfs |
| IMPL-112 | Auto-refresh on file change (filenotify, 300ms debounce) + manual `g` | automatic-behavior, command | org-gtd-graph-view.el:216, :255 | GR-09, DOC-66, DOC-64 | code+docs | auto-refresh, filenotify, debounce |
| IMPL-113 | Discoverable `?` transient menu with sticky mode | command | org-gtd-graph-transient.el:78, :46 | GR-10, DOC-64 | code+docs | transient, menu, discoverability |
| IMPL-114 | Add root task (`r`): new heading or link existing task; added to FIRST_TASKS | command | org-gtd-graph-transient.el:231 | GR-11, DOC-63 | code+docs | add-task, root, graph |
| IMPL-115 | Add successor task (`s`) with multi-select predecessor checkboxes | command | org-gtd-graph-transient.el:916, :1006 | GR-12, DOC-63 | code+docs | successor, checkbox, multi-select |
| IMPL-116 | Add blocker task (`b`) with multi-select blocked-task checkboxes | command | org-gtd-graph-transient.el:1104 | GR-13, DOC-63 | code+docs | blocker, checkbox, graph |
| IMPL-117 | Bulk-edit a task's blockers (`B`): sync dependency set to checked list | command | org-gtd-graph-transient.el:573, :532 | GR-14, DOC-63 | code+docs | modify-blockers, bulk, sync |
| IMPL-118 | Bulk-edit a task's successors (nominal `S`; key shadowed — reachable via `?` menu only) | command | org-gtd-graph-transient.el:700, :669 | GR-15, DOC-63 | code+docs | modify-successors, shadowed-key, bulk |
| IMPL-119 | Change TODO state from the graph (`t t`) | command | org-gtd-graph-transient.el:862 | GR-16, DOC-63 | code+docs | todo-state, graph, edit |
| IMPL-120 | Remove task from project with intelligent rewiring (`t r`) | command | org-gtd-graph-transient.el:796, :332 | GR-17, DOC-63 | code+docs | remove-task, rewire, graph |
| IMPL-121 | Trash task (`t d`): removed from all projects, dependencies cleaned, canceled | command | org-gtd-graph-transient.el:836, :449 | GR-18, DOC-63 | code+docs | trash-task, cancel, cleanup |
| IMPL-122 | Jump to task in org file (`t e`, RET in details panel) | command | org-gtd-graph-ui.el:265 | GR-19, DOC-63 | code+docs | jump, org-file, edit |
| IMPL-123 | Show task relationships report (`t i`) — duplicate definition, see Discrepancies | command | org-gtd-graph-transient.el:288 (wins); org-gtd-graph-view.el:434 (shadowed) | GR-20, DOC-63 | code+docs | relationships, report, duplicate-def |
| IMPL-124 | Project-level actions from the graph: incubate `I`, someday `S`, cancel `C` | command | org-gtd-graph-transient.el:885, :894, :903 | GR-21, DOC-63 | code+docs | project-actions, incubate, cancel |
| IMPL-125 | Export graph to SVG / Graphviz DOT / ASCII (`x s` / `x d` / `x a`) | command | org-gtd-graph-view.el:521, :534, :547 | GR-22, DOC-67 | code+docs | export, svg, graphviz |
| IMPL-126 | Quit graph (`q`) / quit-and-kill (`Q`) with window/watch teardown | command | org-gtd-graph-mode.el:44; org-gtd-graph-transient.el:323 | GR-23, DOC-63 | code+docs | quit, teardown, windows |
| IMPL-127 | Graph filtering engine (TODO state, priority, tags, schedule windows) — complete back end, no command/key sets the filter | data-model | org-gtd-graph-filter.el:43, :60, :182; org-gtd-graph-view.el:64 | GR-24 | latent | filter, latent, no-ui |
| IMPL-128 | Graph validation warnings on refresh: orphans, dangling edges, cycles | automatic-behavior | org-gtd-graph-data.el:377; org-gtd-graph-view.el:261 | GR-25 | code-only | validation, warnings, cycles |
| IMPL-129 | Cross-project dependency commands (add-dependency / add-blocker / clear-relationships) — M-x only, unbound | command | org-gtd-graph-view.el:352, :390, :475 | GR-26 | latent | cross-project, unbound, m-x |
| IMPL-130 | Graph debug printers (edge dumps to *Messages*) | data-model | org-gtd-graph-debug.el:37, :51 | GR-27 | code-only | debug, developer, edges |

## Mode & infrastructure

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-131 | `org-gtd-mode` global minor mode: wires edna, state-change hooks, agenda properties, WIP cleanup (v4: users configure org-agenda-files directly; agenda advice is a pass-through) | command | org-gtd-mode.el:83 | VW-37, DOC-38, DOC-80, DOC-81 | code+docs | mode, global, wiring |
| IMPL-132 | Live inbox count in mode line (`GTD[5]`), periodic refresh, display policy; `org-gtd-inbox-count` API | automatic-behavior, customization | org-gtd-mode.el:207, :182, :57, :66 | VW-38, DOC-38, DOC-39 | code+docs | mode-line, inbox-count, lighter |
| IMPL-133 | Automatic CLOSED timestamps on GTD items when marked done (without org-log-done) | automatic-behavior | org-gtd-mode.el:171 | VW-39 | code-only | closed, timestamp, automatic |
| IMPL-134 | Automatic project bookkeeping on TODO changes (cookies, WAIT→delegate, project-cancel detection) — the hook wiring | automatic-behavior | org-gtd-mode.el:146–:150 | VW-40, DOC-38 | code+docs | hooks, state-change, wiring |
| IMPL-135 | GTD command center transient menu covering the whole workflow | command | org-gtd-command-center.el:42 | VW-41, DOC-37 | code+docs | command-center, transient, discoverability |
| IMPL-136 | Six-stage observation-only hook system (global defvars + per-type :hooks), error-isolated | hook | org-gtd-hooks.el:40–:97 | VW-42, DOC-91 | code+docs | hooks, pipeline, extension |
| IMPL-137 | GTD directory with automatic creation of tasks/horizons files | customization, automatic-behavior | org-gtd-core.el:187; org-gtd-files.el:41 | VW-43, DOC-76 | code+docs | directory, files, auto-create |
| IMPL-138 | Opt-in auto-save of modified GTD buffers after each organize | customization | org-gtd-core.el:212, :551 | VW-44, DOC-26 | code+docs | auto-save, organize, buffers |
| IMPL-139 | Custom TODO keyword mapping with validation and corrective errors | customization, automatic-behavior | org-gtd-core.el:377, :261 | VW-46, DOC-77, DOC-78 | code+docs | keywords, mapping, validation |
| IMPL-140 | Interactive keyword setup wizard (defect: omits `done`, see Discrepancies) | command | org-gtd-core.el:348 | VW-47, DOC-79 | code+docs | wizard, keywords, setup |
| IMPL-141 | Human-readable org IDs (slugged from heading text + timestamp) | automatic-behavior | org-gtd-id.el:38, :63 | WF-64 | code-only | org-id, slug, readable |
| IMPL-142 | Programmatic item creation: `org-gtd-create-item` + obsolete per-type `*-create` wrappers (incl. `org-gtd-single-action-create` alias, org-gtd-next-action.el:107) | data-model | org-gtd-create.el:33 | WF-65, DOC-87, DOC-88 | code+docs | programmatic, api, create |
| IMPL-143 | Domain accessor/predicate API over task properties + task-deps value object | data-model | org-gtd-accessors.el:49–:171; org-gtd-value-objects.el:54–:145 | WF-66 | code-only | accessors, predicates, api |
| IMPL-144 | Older-Emacs compatibility shims via `compat` (28.1+) | automatic-behavior | org-gtd-backward-compatibility.el:39 | VW-54 | code-only | compat, emacs-28, shims |

## Upgrades / compat

| ID | Capability | Kind | Evidence | Sources | Status | Keywords |
|---|---|---|---|---|---|---|
| IMPL-145 | v2→v3 data migration command | command | org-gtd-upgrades.el:38 | VW-50 | code-only (only historical doc/old-upgrade-documentation.org mentions it) | migration, v2, v3 |
| IMPL-146 | v3→v4 data migration command (5 idempotent steps, backup confirmation) | command | org-gtd-upgrades.el:158 | VW-51, DOC-83 | code+docs | migration, v3, v4 |
| IMPL-147 | Major-version upgrade warning with `org-gtd-update-ack` acknowledgment | automatic-behavior | org-gtd.el:94, :104 | VW-52, DOC-82, DOC-86 | code+docs | upgrade, warning, ack |
| IMPL-148 | Backward-compatibility alias layer (review→reflect, oops→missed, incubate→tickler, with-org-gtd-context no-op, DSL key normalization) | automatic-behavior | org-gtd-reflect.el:316; org-gtd-core.el:465, :110 | VW-53, DOC-59 | code+docs | aliases, compatibility, deprecation |

---

## Discrepancies

### 1. Two documentation generations coexist

The shipped 4.x manual (doc/org-gtd.org and its includes: setting-up, using,
reference, troubleshooting) and the unreleased Diataxis set (doc/diataxis/,
doc/extending-org-gtd.org) describe different states of the code. Where they
conflict, **the code sides with the Diataxis set** in every case checked:

- **Refile prompting** (DOC-23): code has the new per-type `:prompt-to-refile` +
  `org-gtd-refile-prompt-default`; the old `org-gtd-refile-to-any-target` and
  `org-gtd-refile-prompt-for-types` still exist but are `make-obsolete-variable`'d
  with one-shot migration (org-gtd-refile.el:39–:103). Main manual presents the
  old vars as current → stale.
- **Organize transient keys** (DOC-28): code uses lowercase keys q/s/d/c/h/p/a/i/y/k/t
  (org-gtd-organize.el:71–:81) and the name "next action". The main manual's
  capitalized keys and "Single action" naming are stale; Diataxis is correct.
- **`org-gtd-delegate-agenda-item`** (DOC-57): exists only as an obsolete alias
  (org-gtd-delegate.el:55). Main manual documents it as current → stale but functional.
- **single-action → next-action rename**: code keeps `org-gtd-single-action` and
  `org-gtd-single-action-create` as obsolete aliases (org-gtd-next-action.el:104–:112).

### 2. POSSIBLY-STALE verdicts (13 flagged DOC items, each checked against code)

| DOC item | Claim under suspicion | Verdict |
|---|---|---|
| DOC-23 | Refile-prompt vars current vs obsolete | **Confirmed stale in main manual.** Both old vars exist but obsolete with migration (org-gtd-refile.el:39–:103). |
| DOC-24 | Refile targets "merged with org-refile-targets" | **Partially confirmed.** Prompted refile merges user + GTD targets (WF-63); auto-refile takes the first GTD-target match (org-gtd-refile.el:170). Manual prose conflates the two paths. |
| DOC-28 | Organize keys capitalized vs lowercase | **Main manual stale.** Code = lowercase (org-gtd-organize.el:71–:81). |
| DOC-41 | `(type . habit)` matches `ORG_GTD="Habits"` | **Filter doc wrong.** Item property is `"Habit"` singular (org-gtd-types.el:92, used by the DSL match builder via `org-gtd-type-org-gtd-value`); `"Habits"` is only the refile-target heading value (org-gtd-types.el:93). Confusingly, `defconst org-gtd-habit` is `"Habits"` (org-gtd-core.el:106). |
| DOC-44 | "The DSL doesn't support sorting yet" | **Confirmed accurate.** No sorting key in the DSL; native escape hatch is the documented workaround (org-gtd-view-language.el:129 commentary). |
| DOC-56 | "Weekly review is not yet implemented" | **Confirmed.** No packaged weekly-review command exists in code; README's "review" listing refers to the individual reflect views. |
| DOC-57 | delegate-agenda-item current | **Stale.** Obsolete alias only (org-gtd-delegate.el:55). |
| DOC-67 | Graph export keys `E s` vs `x s` | **Tutorial stale.** Code binds `x s`/`x d`/`x a` (GR-22, org-gtd-graph-view.el:521–:547); `E`-prefix, `j`/`k` navigation, and `ESC` deselect do not exist. |
| DOC-78 | Keyword mapping `done` entry inconsistent | **Real code+doc gap.** Validator requires `done` (org-gtd-core.el error example includes it), but the wizard omits it (see defect 3c). Docs disagree among themselves. |
| DOC-86 | README pins 4.0.0 | **Confirmed stale** (docs issue only; package is 4.6.x). |
| DOC-90 | `org-gtd-user-types` vs `org-gtd-customize-type`; `:project-fn` field name | **Both mechanisms exist** (org-gtd-types.el:151, :259) — docs underspecify the relationship. Field name in code is `:organize-project-fn` (org-gtd-organize-core.el:277–:328); CHANGELOG's `:project-fn` is wrong. |
| DOC-94 | Engage-by-context documented only as menu entry | **Command exists** (`org-gtd-engage-tagged`, org-gtd-engage.el:90). Note: the command center binds the *obsolete alias* `org-gtd-engage-grouped-by-context` (org-gtd-command-center.el:46) — minor smell. Manual omission confirmed. |
| DOC-96 | Properties contract; Habit/Habits; `d`/`D` duplicate keys | **Properties all exist in code** (WF-40, WF-61). Habit/Habits split is real (see DOC-41 row). Duplicate-command keys are `C-c d` / `C-c D` (org-gtd-clarify.el:179); reference.org's bare `d`/`D` is a docs error. |

### 3. Defects noted in passing (code bugs, not doc bugs)

a. **Double-bound `S` key in graph mode** — org-gtd-graph-mode.el:78 binds `S` to
   `org-gtd-graph-modify-successors`, then line 98 rebinds it to
   `org-gtd-graph-someday-project`; the second wins, so the on-screen legend
   ("B/S modify") is wrong and modify-successors is reachable only via the `?` menu.
   (IMPL-105, IMPL-118, IMPL-124)

b. **Duplicate definition of `org-gtd-graph-view-show-relationships`** —
   org-gtd-graph-view.el:434 (reads cross-project properties) is shadowed by
   org-gtd-graph-transient.el:288 (reads project-local in-memory graph), which
   loads last and wins. (IMPL-123)

c. **Keyword wizard omits `done`** — `org-gtd-setup-keywords-wizard`
   (org-gtd-core.el:348) saves only todo/next/wait/canceled, but the
   `org-gtd-keyword-mapping` validator requires a `done` entry — a wizard-produced
   config can fail validation. (IMPL-140)

### 4. Verified docs-only / stale documentation claims

- **`org-gtd-delegate-read-func`** (IMPL-026) — documented in setting-up-org-gtd.org:720,
  reference.org:2831, using-org-gtd.org:622; **does not exist anywhere in the source**
  (no defcustom/defvar in org-gtd-delegate.el at all). Superseded by the type
  registry's `:input-fn` property descriptor (WF-34).
- **DSL filter keys `level`, `tags-match`, `invalid-timestamp`** (IMPL-091) —
  documented in doc/reference.org:3683/:3872/:3902 but absent from
  `org-gtd-view-lang--known-filter-keys` (org-gtd-view-language.el:178); the
  validator would reject specs using them with a user-error. The
  invalid-timestamp *predicate* exists internally (org-gtd-skip.el:89) but is
  not exposed as a filter key.
- **`org-reverse-note-order`** (IMPL-039) — no org-gtd code references it; it is
  an upstream org-mode variable honored by org-refile. Plausibly accurate for
  prompted refile, but unverified for v4's project task chaining — flag for
  manual verification.

### 5. Items deliberately excluded from the registry (not software features)

- DOC-84 (copy-paste configuration examples for vanilla/Doom/Spacemacs) — documentation
  content, not a code feature.
- DOC-85 (native compilation performance claim) — environmental claim, no dedicated code.
- DOC-97 (troubleshooting guidance) — maps onto existing commands (IMPL-041, IMPL-050,
  IMPL-075, IMPL-076); no distinct feature.
- DOC-56 (weekly review disclaimer) — a documented *gap*, recorded in Discrepancies §2.
