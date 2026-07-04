# Audit: 2026-06-04 GTD Implementation Status vs. Current Code

> Audited 2026-06-10 against working tree at `fa3d30e`. Every cited construct was located by grep/read in the current source (not trusted from the doc). Post-doc code changes are limited to three files: `org-gtd-agenda-transient.el`, `org-gtd-organize-core.el`, `org-gtd-refile.el` (commits `01a1229`, `11e5261`, `d046107`, all 2026-06-05 bug fixes).

## Summary

| Metric | Count |
|---|---|
| Rows checked | 72 |
| Confirmed — verdict and evidence valid, cite exact (or within ±1 line) | 62 |
| Evidence drifted but valid (construct moved; verdict unchanged) | 6 |
| Evidence imprecise but verdict holds (wrong detail/attribution) | 3 |
| Verdict questionable (flagged, not strictly wrong) | 1 |
| Verdict stale (flipped by post-doc commits) | 0 |

- **Scoreboard arithmetic checks out**: row-by-row tally reproduces Implemented 41 / Partial 15 / Via 5 / Not-implemented 11 = 72.
- **No verdict was flipped by the June-5 commits.** All three fixes land inside features the doc already credits: `01a1229` (refile-target point preservation, `org-gtd-refile.el`), `11e5261` (outline level on clarify-in-place — strengthens CLA-07's edit-in-place claim), `d046107` (agenda refresh on state change, `org-gtd-agenda-transient.el`).
- **One verdict flagged as too harsh**: ENG-06 (see Engage section).
- All negative claims re-verified by grep: `energy` = 0 hits in `*.el` (X-04, ENG-05); no checklist/trigger-list (X-07); no Natural Planning/Brainstorm (ORG-PRJ-06); no Read/Review (ORG-RR-01); no Get Clear/Current/Creative (REF-02); no habit-conversion flow (ORG-HAB-01); no project-level delegation (ORG-DEL-03); only "contact" hit is an EBDB docstring example at `org-gtd-types.el:173` (ORG-KNOW-03).
- Minor doc nit (Surprises section): view-language DSL is 1157 lines, not "~1130".

Status legend below: **OK** = confirmed; **OK/drift** = construct moved, verdict valid; **OK/imprecise** = verdict valid, evidence detail wrong; **FLAG** = verdict questionable.

## Capture

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| CAP-01 | Implemented | OK | `org-gtd-capture` at `org-gtd-capture.el:82` exact; wraps `org-capture` (l.89–90); template is `"* %?\n\n\n  %i"` (l.43) — doc's "`* %?`" is a fair summary. |
| CAP-02 | Implemented | OK | `:62` = `(defconst org-gtd-inbox "inbox")` exact; `:111` = `org-gtd-inbox-path` exact; `org-gtd-process.el:38` = `org-gtd-additional-inbox-files` defcustom exact. |
| CAP-03 | Implemented | OK/drift | `capture.el:82` interactive exact. README keybind cite drifted 83→86 (README edited by `fa3d30e` on 2026-06-05); `("C-c d c" . org-gtd-capture)` present. |
| CAP-04 | Implemented | OK | Template = one level-1 heading per capture; process loop handles one heading at a time (`process.el:62–87`). |
| CAP-05 | Implemented | OK | `:96` `org-gtd-capture--add-captured-at-timestamp` exact; wired as `:before-finalize` in both templates (l.45, 50); writes `ORG_GTD_CAPTURED_AT` (l.103–104). |
| CAP-06 | Partial | OK | `horizons.el:40` (`org-gtd-horizons-file` defcustom) exact; `clarify.el:53` (`org-gtd-clarify-show-horizons`) exact; template hardcodes Purpose/Vision/Goals/Areas (`horizons.el:51–55`). No checklist/trigger machinery (grep = 0) — Partial is right. |

## Clarify

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| CLA-01 | Implemented | OK | `process.el:62` `org-gtd-process-inbox` exact; self-continuation at l.87. |
| CLA-02 | Implemented | OK/drift | `organize-core.el:111` → now **:115** (`org-gtd-organize--call`; file +4 net lines from the clarify-in-place fix `11e5261`). Old line 111 confirmed via `git show 0d4b67c`. Verdict unchanged. |
| CLA-03 | Implemented | OK | `organize.el:71` transient exact; literal `"Actionable"` (l.76) and `"Non-actionable"` (l.84) sections present. |
| CLA-04 | Implemented | OK | `types.el:31` `org-gtd-types` exact; delegated requires `:who`/`:when` (l.49–52); project route via `org-gtd-project-new` (organize.el:82). |
| CLA-05 | Implemented | OK | Judgment row; cited mechanisms (delegated required props, quick-action semantics, WIP free edit) all exist. Caveat about no validation is accurate. |
| CLA-06 | Partial | OK | `organize.el:82` exact; `org-gtd-clarify-project-insert-template` exists at `clarify.el:371`. No assisted extraction (confirmed). Adjudication item 7 plans LLM-assist; code unchanged. |
| CLA-07 | Implemented | OK | `org-gtd-clarify-stop` at `clarify.el:288`; duplicate-queue (`clarify.el:165`, `organize-core.el:133–151`); `--skip-refile` (`clarify.el:156`, infix `organize.el:64–75`, honored `organize-core.el:228`). Post-doc fix `11e5261` improved the in-place path (outline level) — strengthens, doesn't flip. |

## Organize — taxonomy / Projects / Single Actions / Calendar

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| ORG-00 | Implemented | OK/imprecise + drift | Transient at `organize.el:71` exact, but it offers **10 routes** (quick/next/delegate/calendar/habit/project/extend/tickler/someday/knowledge/trash — types table defines 10 types), not "8 canonical categories". `organize-core.el:162` → now **:166** (`org-gtd--clear-foreign-properties`). Verdict fine. |
| ORG-PRJ-01 | Implemented | OK | `projects.el:532` `org-gtd-project-new` exact; `ORG_GTD=Projects` via `types.el:86–87`. |
| ORG-PRJ-02 | Implemented | OK | `:376` `org-gtd-stuck-projects` exact; `:867` `org-gtd-projects--is-stuck-p` exact; `reflect.el:160` `reflect-stuck-projects` exact. |
| ORG-PRJ-03 | Implemented | OK | `dependencies.el:46` `org-gtd-dependencies-find-ready-tasks` (BFS) exact; uses `ORG_GTD_DEPENDS_ON`/`ORG_GTD_BLOCKS`; NEXT/WAIT counted actionable per `skip.el:400` docstring. |
| ORG-PRJ-04 | Partial | OK | `projects.el:740` `--add-default-sequential-dependencies` exact; `ORG_GTD_PROJECT_IDS` multivalued throughout. No roll-up found — Partial is right. |
| ORG-PRJ-05 | Implemented | OK | Org content-model claim; no contrary evidence. |
| ORG-PRJ-06 | Not-implemented | OK | grep Natural-Planning/Brainstorm = 0 hits. |
| ORG-PRJ-07 | Partial | OK | `org-gtd-project--find-ready-tasks` at `projects.el:566`; no trigger-list UI (grep = 0). |
| ORG-PRJ-08 | Partial | OK | No look-into/commitment-to-decide type in `types.el` — convention-only claim holds. |
| ORG-SA-01 | Implemented | OK/imprecise | `next-action.el:49` exact; `types.el:32` exact for next-action — **but** next-action carries `:org-gtd "Actions"` (l.33), not `:refile-target "Actions"`; that literal belongs to *delegated* (l.42). Functionally equivalent (`:refile-target` falls back to `:org-gtd`, types.el:300–304), so verdict holds. Naming note (next-action.el, not single-action.el) accurate. |
| ORG-SA-02 | Implemented | OK | Org content model; accurate. |
| ORG-CAL-01 | Implemented | OK | `types.el:54` calendar exact, requires `ORG_GTD_TIMESTAMP` (l.61); `skip.el:89` `org-gtd-pred--property-invalid-timestamp` exact. |
| ORG-CAL-02 | Implemented | OK | `tickler.el:41` `org-gtd-tickler` exact; `projects.el:1172` → defun `org-gtd-project-incubate` at **:1173** (autoload cookie at 1172 — effectively exact); DSL `(when . future)` documented `view-language.el:59`. |

## Organize — Delegate / Incubate / Knowledge / Habits / Quick / Read-Review

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| ORG-DEL-01 | Implemented | OK | `delegate.el:44` exact; `types.el:40` delegated exact (`:state :wait` l.43, `DELEGATED_TO` l.49). |
| ORG-DEL-02 | Implemented | OK | Required who/when props (types.el:49–52); LOGBOOK note via `delegate.el:89` `--add-delegation-note`; `stuck-delegated` DSL type (`view-language.el:197`). |
| ORG-DEL-03 | Not-implemented | OK | No delegation constructs in `projects.el` (grep = 0). Adjudication item 6 reframed it as planned work; code unchanged, verdict still correct. |
| ORG-DEL-04 | Partial | OK | `org-gtd-engage-tagged` (`engage.el:90`); `context.el:63` `org-gtd-context-at-point`. No per-person lists — Partial right. |
| ORG-INC-01 | Implemented | OK | `someday.el:51` exact; `types.el:76` someday exact (`:state nil`); `reflect.el:148` `reflect-someday-maybe` exact. |
| ORG-INC-02 | Implemented | OK | `org-gtd-someday-lists` (`someday.el:41`); `tickler.el:41` exact; `reactivate.el:103` `org-gtd-reactivate` exact. |
| ORG-INC-03 | Implemented | OK | `projects.el:1228` `org-gtd-project-someday` exact; `PREVIOUS_ORG_GTD` save/restore (`projects.el:1052–1079`). |
| ORG-INC-04 | Implemented | OK | Defcustom → `ORG_GTD_SOMEDAY_LIST`; `someday-review.el:70` `--start-session` exact (filter logic itself at l.93–119 — slightly below the cite, same construct). |
| ORG-KNOW-01 | Implemented | OK | `knowledge.el:37` exact; `types.el:103` reference, `:disposition done-and-archive` (l.105) exact. |
| ORG-KNOW-02 | Partial | OK | Archive disposition exists; no automated purge found — Partial right. |
| ORG-KNOW-03 | Not-implemented | OK | Sole "contact" hit is a docstring example (EBDB) at `types.el:173`. Non-goal per V-09. |
| ORG-HAB-01 | Partial | OK | `habit.el:38` exact (repeater `.+3d` in docstring l.40); no convert-to-habit flow (grep = 0). |
| ORG-QA-01 | Implemented | OK | `types.el:117` quick-action exact (`done-and-archive` l.119); key `q` at `organize.el:77`. |
| ORG-RR-01 | Not-implemented | OK | grep read/review = 0 hits. |

## Reflect / Review

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| REF-01 | Partial | OK | `reflect.el:70` `reflect-area-of-focus` + ~15 sibling on-demand commands; no recurring event/completion tracking found. |
| REF-02 | Not-implemented | OK | No Get Clear / Get Current / Get Creative anywhere (grep = 0). Adjudicated IMPLEMENT, not yet built. |
| REF-03 | Implemented | OK | `reflect.el:160` exact; `view-language.el:553` `--build-skip-function-for-project-type` (stuck-project) exact. |
| REF-04 | Partial | OK | `command-center.el:42` transient exact; Stuck branch (l.61, 69), Missed branch (l.62, 80). |
| REF-05 | Via-org/dep | OK | `horizons.el:51` template defconst exact; `reflect.el:70` exact; no cadence wiring found. |
| REF-06 | Not-implemented | OK | `archive.el` exposes only interactive single-shot commands (`archive-completed-items` :83, `archive-item-at-point` :115); no scheduling. |

## Engage

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| ENG-01 | Implemented | OK | `engage.el:47` `engage-view-spec` exact; blocks match claim verbatim: calendar-day, tickler today, delegated today, **all** next-actions. |
| ENG-02 | Partial | OK | Block order verified in spec; no automatic context folding in the default view (folding requires `engage-tagged` / grouped views). |
| ENG-03 | Partial | OK | `(prefix . (project area-of-focus "—"))` at `engage.el:55`; effort in DSL; energy absent (grep = 0). |
| ENG-04 | Implemented | OK | `engage-tagged` (`engage.el:90`); note `org-gtd-engage-grouped-by-context` is now an obsolete alias for it (`engage.el:108`). |
| ENG-05 | Partial | OK | Re-ran grep: "energy" = 0 hits across all `*.el`. Effort DSL attr (`view-language.el:181`). |
| ENG-06 | Not-implemented | **FLAG — arguably too harsh** | `org-gtd-command-center` (`command-center.el:42`) is a single unified transient exposing Engage / Capture & Process / Reflect from one entry point — a reasonable reading of "unified mode choice" would call this **Partial**. The doc's own V-03 disposition ("already maps to existing commands, docs-only") implicitly concedes the capability exists. No code change needed either way; the row's verdict understates the code. |

## Horizons of Focus

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| HOR-01 | Implemented | OK | `areas-of-focus.el:39` defcustom exact; `:47` `org-gtd-area-of-focus-set-on-item-at-point` (sets CATEGORY) exact. |
| HOR-02 | Implemented | OK | `:81` project-task CATEGORY branch; `skip.el:181` `org-gtd-pred--area-of-focus-matches` exact; DSL `area-of-focus` attr. |
| HOR-03 | Via-org/dep | OK/imprecise | "H3 heading" reads as GTD Horizon-3, **not** an org level-3 heading — in the `horizons.org` template Goals is a level-1 `*` heading (`horizons.el:54`). Same caveat for HOR-04/05. Verdict fine. |
| HOR-04 | Via-org/dep | OK/imprecise | Vision = level-1 heading in template (`horizons.el:53`); "H4" = horizon number. |
| HOR-05 | Via-org/dep | OK/imprecise | Purpose & principles = level-1 heading (`horizons.el:52`); "H5" = horizon number. |
| HOR-06 | Not-implemented | OK | No standards/rules-of-engagement tooling found. Adjudicated DOCS-only. |
| HOR-07 | Not-implemented | OK | No cross-horizon outline view. Adjudicated DROP (spec-correction). |
| HOR-08 | Partial | OK | Per-horizon review via `reflect-area-of-focus`; no priority machinery (no ABC/energy) — Partial right. |

## Cross-Cutting

| ID | Doc verdict | Audit | Findings |
|---|---|---|---|
| X-01 | Implemented | OK | `view-language.el:623` `--build-skip-function-for-stuck-next-action` exact; `skip.el:400` `org-gtd-pred--project-is-stuck` exact. |
| X-02 | Implemented | OK | `org-gtd-pred--tags-matches` at `skip.el:168`; DSL `group-contexts`/`group-by` (`view-language.el:183, 393`). |
| X-03 | Implemented | OK/drift | `agenda-transient.el:167` → now **:175–178** (`org-gtd-agenda-transient--effort` calling `org-agenda-set-effort`; file changed by `d046107`). DSL `effort` attr (`view-language.el:181`). |
| X-04 | Not-implemented | OK | "energy" grep = 0 hits, re-verified 2026-06-10. |
| X-05 | Implemented | OK | DSL `(when . past/future/today)` + deadline/scheduled comparisons (`view-language.el:58–64`); tickler real. |
| X-06 | Implemented | OK | `capture.el:96` exact. |
| X-07 | Not-implemented | OK | checklist/trigger-list grep = 0 hits. |
| X-08 | Implemented | OK | Areas defcustom, user tags, dynamic grouping all confirmed; no hard-coded taxonomy beyond the type table. |
| X-09 | Implemented | OK | Single-route dispatch (`organize-core.el:288` `org-gtd--dispatch`; `--run-disposition` :201). |
| X-10 | Implemented | OK/drift | `areas-of-focus.el:93` → **:92** `--set-on-project-tasks` (trivial ±1); CATEGORY inheritance confirmed. |
| X-11 | Partial | OK | `tags-grouped` view type + `--create-grouped-views` (`view-language.el:271, 388`); no dedicated sub-list UI. |
| X-12 | Via-config | OK | No onboarding scaffolding; `org-gtd-configure.el` is heading-type configuration, not setup/migration tooling. |

## Drift ledger (all instances)

| Cite in doc | Current location | Cause |
|---|---|---|
| `organize-core.el:111` (CLA-02) | `:115` `org-gtd-organize--call` | `11e5261` clarify-in-place fix (+4 net lines) |
| `organize-core.el:162` (ORG-00) | `:166` `org-gtd--clear-foreign-properties` | same |
| `agenda-transient.el:167` (X-03) | `:175–178` `--effort` | `d046107` agenda-refresh fix |
| `README:83` (CAP-03) | `:86` keybind block | `fa3d30e` README edit |
| `projects.el:1172` (ORG-CAL-02) | `:1173` defun (cookie at 1172) | pre-existing ±1 |
| `areas-of-focus.el:93` (X-10) | `:92` | pre-existing ±1 |

All other ~55 line cites resolve **exactly** in the current tree (those files are untouched since 2026-06-04).
