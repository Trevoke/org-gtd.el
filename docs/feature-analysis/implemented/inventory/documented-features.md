# Documented Features Inventory (org-gtd.el)

Inventory of user-facing features **as promised in the documentation**. Sources: README.org,
doc/org-gtd.org (manual index), doc/setting-up-org-gtd.org, doc/using-org-gtd.org,
doc/reference.org, doc/getting-started-with-org-gtd-manual-section.org (and the four
audience-specific getting-started variants), doc/extending-org-gtd.org, doc/whats-new-3.0.org,
doc/whats-new-4.0.org, doc/troubleshooting.org, doc/sams-gtd-journey.org, doc/diataxis/
(newer Diataxis doc set), and recent CHANGELOG.org entries.

Status legend: **core** = described as core workflow; **optional** = optional/configurable;
**advanced** = advanced/extension-point/programmatic.

`POSSIBLY-STALE` marks features where docs conflict with each other, with the CHANGELOG
deprecation list, or describe mechanisms that appear superseded.

## Summary

| Area | Count |
|------|-------|
| Capture | 5 |
| Clarify / Process | 13 |
| Organize | 12 |
| Engage | 9 |
| Views (View DSL) | 9 |
| Review / Reflect | 10 |
| Projects | 16 |
| Configuration | 11 |
| Extension points | 8 |
| Other | 5 |
| **Total** | **98** |

**Possibly-stale items: 13** (IMPL-DOC-23, -24, -28, -41, -44, -56, -57, -67, -78, -86, -90, -94, -96)

---

## Capture

- **IMPL-DOC-01** — `org-gtd-capture` opens an org-capture session targeting the GTD inbox file (`inbox.org` in `org-gtd-directory`). — README.org "Your First GTD Cycle"; using-org-gtd.org "Adding things to the inbox"; reference.org "Commands Reference / org-gtd-capture"; diataxis/reference/commands.org "Capture". — **core**
- **IMPL-DOC-02** — `org-gtd-capture-templates` lets users customize capture templates (two defaults: plain item, item with back-link), subject to two constraints ("* " prefix, `entry (file ,#'org-gtd-inbox-path)`). — using-org-gtd.org "Adding things to the inbox"; reference.org "Capture Configuration". — **optional**
- **IMPL-DOC-03** — Captured items automatically receive an `ORG_GTD_CAPTURED_AT` inactive-timestamp property (each level-1 heading in a multi-item capture session gets one). — whats-new-4.0.org "Capture timestamps"; CHANGELOG 4.5.0 "Multi-item capture timestamps". — **core** (automatic)
- **IMPL-DOC-04** — `org-gtd-additional-inbox-files` configures extra inbox files (mobile, email, per-context) processed in order after the main inbox empties. — using-org-gtd.org "Processing the inbox"; reference.org "org-gtd-additional-inbox-files". — **optional**
- **IMPL-DOC-05** — `org-gtd-inbox-path` returns (and creates if missing) the inbox file path for use in custom templates/automation. — reference.org "Utility Commands"; diataxis/reference/commands.org. — **advanced**

## Clarify / Process

- **IMPL-DOC-06** — `org-gtd-process-inbox` starts a looping clarify session over every inbox heading (continuing through additional inbox files) until all inboxes are empty. — README.org; using-org-gtd.org "Processing the inbox"; reference.org; diataxis commands.org "Process". — **core**
- **IMPL-DOC-07** — `org-gtd-clarify-item` clarifies any org heading one-off; `org-gtd-clarify-agenda-item` does the same from an agenda line. — using-org-gtd.org "How to start clarifying"; reference.org "Clarification Commands"; whats-new-3.0.org. — **core**
- **IMPL-DOC-08** — Clarify buffers use `org-gtd-clarify-mode`, a major mode derived from org-mode, with a dedicated `org-gtd-clarify-mode-map` for user keybindings (old name `org-gtd-clarify-map` is an obsolete alias). — using-org-gtd.org "Options and commands related to clarification"; CHANGELOG 4.0.0. — **core**
- **IMPL-DOC-09** — Horizons helper window: `org-gtd-clarify-show-horizons` (side or nil) shows the horizons file during clarification; `org-gtd-clarify-toggle-horizons-window` toggles it. — using-org-gtd.org "Working with the GTD Horizons"; reference.org. — **optional**
- **IMPL-DOC-10** — Organize-help window: `org-gtd-clarify-show-organize-help` shows all GTD item types with key shortcuts/descriptions; `org-gtd-clarify-toggle-organize-help` toggles it. — using-org-gtd.org; reference.org. — **optional**
- **IMPL-DOC-11** — Project-dependencies helper window: `org-gtd-clarify-display-helper-buffer` shows a live view of task relationships when clarifying multi-task projects (diataxis also documents the command `org-gtd-clarify-display-dependency-helper`). — using-org-gtd.org; reference.org; diataxis commands.org. — **optional**
- **IMPL-DOC-12** — Project templates: `org-gtd-clarify-project-templates` (alist name→tasks) plus `org-gtd-clarify-project-insert-template` to insert one during clarification. — using-org-gtd.org; reference.org; whats-new-3.0.org. — **optional**
- **IMPL-DOC-13** — Duplicate-during-clarify: `C-c d` (`org-gtd-clarify-duplicate`, prompts for title) and `C-c D` (`org-gtd-clarify-duplicate-exact`) queue duplicates that are clarified/organized immediately after the current item; queue side window shows pending items; nested duplicates supported; queue position via `org-gtd-clarify-duplicate-queue-position`. — using-org-gtd.org "Duplicating items during clarification"; reference.org; CHANGELOG 4.4.0, 4.6.1. — **optional**
- **IMPL-DOC-14** — Duplicate-queue data safety: canceling with pending duplicates offers discard / save-to-inbox / abort; exiting Emacs prompts if clarify buffers have pending duplicates; WIP buffers have auto-save. — using-org-gtd.org "What happens if you cancel?" / "Data safety"; CHANGELOG 4.4.0. — **optional**
- **IMPL-DOC-15** — `org-gtd-clarify-stop` (`C-c C-k`) aborts the current clarify buffer (and queue), restoring the previous window configuration. — reference.org "Clarification Commands"; diataxis commands.org. — **core**
- **IMPL-DOC-16** — Update-in-place (skip-refile): `C-u` prefix on the clarify commands, or the `-n` toggle in the organize transient, reconfigures an item (type, properties, tags, hooks) without moving it; archive types always archive. Replaces removed `org-gtd-delegate-item-at-point`. — reference.org "Updating Items Without Refiling" and "Obsolete Commands". — **optional**
- **IMPL-DOC-17** — `org-gtd-clarify-switch-to-buffer` picks an existing WIP buffer with completion (multiple clarify buffers can coexist). — diataxis/reference/commands.org "Clarify"; CHANGELOG 4.4.0. — **advanced**
- **IMPL-DOC-18** — Data-shape requirement: simple items are a single heading; projects are a level-1 heading with level-2 task subheadings. — using-org-gtd.org "data shape requirements"; troubleshooting.org "I can't create a project...". — **core**

## Organize

- **IMPL-DOC-19** — `org-gtd-organize` opens a transient menu (in clarify buffers) to categorize the item; organizing = choose type → run hooks → refile. — README.org; using-org-gtd.org "Organizing an item into the system"; reference.org. — **core**
- **IMPL-DOC-20** — Eleven organize destinations are documented: Project, Add-to-existing-project, Calendar, Delegate, Single action, Habit, Tickler, Someday/Maybe, Knowledge, Quick action (mark DONE + archive), Trash (delete). Each with type-specific prompts and behavior. — using-org-gtd.org "Understanding GTD Item Types"; reference.org "org-gtd-organize". — **core**
- **IMPL-DOC-21** — Delegation prompts for person + follow-up date, sets `DELEGATED_TO` and WAIT state; `org-gtd-delegate-read-func` customizes the person prompt (e.g. completion). — using-org-gtd.org "Delegate [D]"; setting-up-org-gtd.org "Optional configuration". — **core** (customization optional)
- **IMPL-DOC-22** — Changing any action's state to WAIT (e.g. `C-c C-t`) prompts "Convert to delegated item?" and, on confirm, collects who/when so the waiting-for list keeps full context. — using-org-gtd.org "Converting actions to delegated". — **core** (automatic)
- **IMPL-DOC-23** — `POSSIBLY-STALE` Refile-prompt control via `org-gtd-refile-to-any-target` (set nil to opt in) + `org-gtd-refile-prompt-for-types` (list of types that prompt). Documented as current in README config examples, setting-up-org-gtd.org, using-org-gtd.org "Controlling Refile Prompts by Item Type", reference.org. **But** extending-org-gtd.org and diataxis/reference/customization.org mark both obsolete, replaced by per-type `:prompt-to-refile` (via `org-gtd-customize-type`) and `org-gtd-refile-prompt-default`; CHANGELOG "Unreleased" confirms the deprecation with load-time migration. The shipped manual contradicts itself. — **optional**
- **IMPL-DOC-24** — `POSSIBLY-STALE` Refile target system: headings anywhere/any level marked with `ORG_GTD_REFILE` (values Projects/Actions/Calendar/Tickler/Someday/Habits; multiple space-separated values supported) are merged with the user's `org-refile-targets` (user targets listed first); a missing target is auto-created in the default GTD file; inbox is always excluded. — using-org-gtd.org "Refiling to the appropriate area"; setting-up-org-gtd.org "Working with Your Existing Org Setup"; CHANGELOG 4.3.0, 4.2.2. *Stale note:* CHANGELOG 4.3.0 says **auto**-refile now only uses targets in `org-gtd-tasks.org`, ignoring `org-refile-targets` — the "merge" prose in using-org-gtd.org does not clearly distinguish prompted vs auto-refile and may overstate the merge. — **core** (target creation) / **optional** (custom targets)
- **IMPL-DOC-25** — `org-gtd-organize-hooks` runs user functions on each organized item (default `org-set-tags-command`); helper `org-gtd-organize-type-member-p` lets hooks act per item type. — using-org-gtd.org "Adding your own hooks when organizing"; reference.org "Hooks Framework Reference" (extensive examples: effort, priority, org-roam, keyword-based properties). — **optional**
- **IMPL-DOC-26** — `org-gtd-save-after-organize` saves all modified buffers after each organize step. — reference.org; setting-up-org-gtd.org "all the levers". — **optional**
- **IMPL-DOC-27** — Someday state preservation: moving an item/project to someday or tickler saves `ORG_GTD`, TODO keyword, and type-specific properties into `PREVIOUS_*` for later reactivation. — using-org-gtd.org "Someday/Maybe [y]" and "Tickler [I]"; CHANGELOG 4.5.1. — **core** (automatic)
- **IMPL-DOC-28** — `POSSIBLY-STALE` Organize transient keys: main manual documents capitalized keys ([P] [A] [C] [D] [S] [H] [I] [y] [K] [Q] [T]) and the name "Single action"; diataxis/reference/commands.org documents lowercase keys (q s d c h p a i y k t) and per-type commands `org-gtd-next-action`, `org-gtd-project-new`, `org-gtd-project-extend` (single-action renamed to next-action per CHANGELOG Unreleased). The two doc sets disagree on keys and naming. — **core**
- **IMPL-DOC-29** — Someday lists: `org-gtd-someday-lists` optionally groups someday items into named lists; organizing as someday then prompts for a list. — reference.org "Organizing Your Someday Items"; diataxis customization.org. — **optional**
- **IMPL-DOC-30** — `org-reverse-note-order` (org var) controls whether tasks added to a project go to top or bottom. — setting-up-org-gtd.org "Optional configuration"; using-org-gtd.org "Add to existing project". — **optional**

## Engage

- **IMPL-DOC-31** — `org-gtd-engage` shows the daily view: today's agenda (appointments/deadlines/scheduled) plus all NEXT actions grouped by project. — README.org; using-org-gtd.org "Engaging with your GTD items"; reference.org. — **core**
- **IMPL-DOC-32** — `org-gtd-show-all-next` lists every NEXT action only (no calendar section). — README.org; reference.org. — **core**
- **IMPL-DOC-33** — `org-gtd-agenda-transient` (suggested `C-c .` in agenda) offers quick task actions from agenda views: state changes (done/wait/next/cancel/cycle), defer-1-day and set-date for time-based types (calendar/delegated/tickler; habits excluded), clock in/out, effort/priority/tags/note/area-of-focus, clarify (refile or in-place), quit. State changes refresh the agenda line immediately. — using-org-gtd.org "Quick Task Actions from the Agenda"; README config; CHANGELOG Unreleased fix #289. — **optional** (recommended)
- **IMPL-DOC-34** — `org-gtd-set-timestamp` (DWIM) reschedules Calendar/Delegated/Tickler items from org headings or agenda items by setting `ORG_GTD_TIMESTAMP`. — reference.org "Timestamp Commands (DWIM)"; CHANGELOG 4.5.0. — **optional**
- **IMPL-DOC-35** — Agenda property display (vendored org-agenda-property): `org-gtd-agenda-property-list` / `-position` / `-column` / `-separator` show chosen properties (e.g. LOCATION, DELEGATED_TO) on agenda lines; org links in values display their description. — reference.org "Agenda Display Configuration"; setting-up-org-gtd.org "org-agenda-property has been vendored"; CHANGELOG 4.0.4. — **optional**
- **IMPL-DOC-36** — Prefix column configuration: `org-gtd-prefix-width` (default 12; old name `org-gtd-engage-prefix-width` deprecated) and `org-gtd-agenda-truncate-ellipsis` control width and truncation of the left-hand context column. — reference.org "View Prefix Configuration"; CHANGELOG 4.5.0. — **optional**
- **IMPL-DOC-37** — `org-gtd-command-center` is a single-binding transient menu covering engage, capture/process, reflect, stuck/missed sub-menus, and archive. — setting-up-org-gtd.org "Command Center"; reference.org "org-gtd-command-center". — **optional**
- **IMPL-DOC-38** — `org-gtd-mode` (global minor mode) shows an inbox-count mode-line lighter (e.g. `GTD[5]`) across all inbox files, refreshes periodically (`org-gtd-mode-update-interval`), display controlled by `org-gtd-mode-lighter-display` (always/never/when-non-zero), enables org-edna, and detects manual CNCL on a project heading to offer cascading cancellation. — setting-up-org-gtd.org "org-gtd-mode has a new purpose"; reference.org "Mode-Line Integration"; CHANGELOG 4.5.0, 4.6.0. — **optional**
- **IMPL-DOC-39** — `org-gtd-inbox-count` returns the inbox item count programmatically. — reference.org "Mode-Line Integration". — **advanced**

## Views (View DSL)

- **IMPL-DOC-40** — `org-gtd-view-show` renders declarative view specs (alist of filters, AND logic) as org-agenda views; the built-in views (engage, reflect-*) use it internally; optional KEYS arg + `org-agenda-sticky` allow multiple concurrent views. — reference.org "Custom View Commands / org-gtd-view-show" and "View DSL Filter Reference"; getting-started manual section "Tutorial: Creating Your First Custom View"; whats-new-4.0.org. — **advanced**
- **IMPL-DOC-41** — `POSSIBLY-STALE` Type filters: `type` accepts delegated, calendar, project, active-project, completed-project, stuck-project, tickler, someday, habit, next-action, tickler-project, incubated-project, stuck-delegated, stuck-calendar, stuck-tickler, stuck-habit. *Stale note:* filter docs describe e.g. `(type . habit)` matching `ORG_GTD="Habits"` while properties reference elsewhere documents `ORG_GTD: Habit` (singular) for migration — minor internal inconsistency worth verifying against code. — **advanced**
- **IMPL-DOC-42** — Time filters: semantic `(when . today|past|future)` (auto-selects the right timestamp property per type), `(deadline . ...)`, `(scheduled . ...)`. — reference.org "Time-based Filters". — **advanced**
- **IMPL-DOC-43** — State/metadata filters: `(todo . (...))`, `(done . t|N|today|recent|past-day|past-week|past-month|past-year)`, `(area-of-focus . "X")`, `(tags . ...)`, `(tags-match . ...)`, `(priority . ...)`, `(effort . ...)`, `(clocked . ...)`, `(last-clocked-out . ...)`, `(property . ...)`, `(level . N)`, `(not-habit . t)`, `(invalid-timestamp . t)`. — reference.org "Filter Reference" subsections; getting-started manual section "Quick Wins and Focus Work Views" (effort/priority/clocked tutorials). — **advanced**
- **IMPL-DOC-44** — `POSSIBLY-STALE` Multi-block views via `(blocks . ...)`, special block types `calendar-day` and `todo`, and `native` blocks as an escape hatch to raw `org-agenda-custom-commands` syntax (agenda/todo/tags/tags-todo/search) for sorting, custom skip functions, etc. *Stale note:* the native-block section states "The DSL doesn't support sorting yet" — verify against current code. — **advanced**
- **IMPL-DOC-45** — Prefix DSL: `(prefix . (project area-of-focus "literal"))` fallback chain controls the prefix column; `(prefix-width . N)` per view; inheritance to blocks with per-block override; low-level `(prefix-format . ...)`. Area-of-focus prefix resolves a project task's CATEGORY through its project. — reference.org "Prefix DSL"; CHANGELOG 4.0.7. — **advanced**
- **IMPL-DOC-46** — Simplified multi-block views: multiple top-level `(type . X)` keys expand into implicit blocks with smart per-type defaults (calendar/delegated/tickler default to `when . today`), top-level filter inheritance, and a four-tier precedence model. — reference.org "Simplified Multi-Block Views". — **advanced**
- **IMPL-DOC-47** — `(view-type . agenda)` + `(agenda-span . N)` create day/week agenda-style views. — reference.org "View Configuration Options". — **advanced**
- **IMPL-DOC-48** — Users can still define plain `org-agenda-custom-commands` views; GTD items appear as long as `org-gtd-directory` is in `org-agenda-files`. — using-org-gtd.org "Defining your own agenda views". — **advanced**

## Review / Reflect

- **IMPL-DOC-49** — `org-gtd-reflect-stuck-projects` finds projects with active tasks but no NEXT action. — README.org; reference.org; troubleshooting.org. — **core** (weekly review)
- **IMPL-DOC-50** — Per-type stuck-item reviews: `org-gtd-reflect-stuck-calendar-items`, `-stuck-tickler-items`, `-stuck-habit-items`, `-stuck-delegated-items`, `-stuck-next-action-items` find items with missing/invalid metadata. — troubleshooting.org "Finding lost tasks"; reference.org "Stuck Item Review Commands". — **optional**
- **IMPL-DOC-51** — Missed-engagements reviews: `org-gtd-reflect-missed-engagements` (all), `-missed-calendar`, `-missed-delegated`, `-missed-projects`, and `-missed-with-custom` (adds user views from `org-gtd-reflect-missed-custom-views`); old `org-gtd-oops*` names are obsolete aliases. — reference.org "Reflect Commands (Missed Engagements)" and "Missed Engagements Review Configuration"; using-org-gtd.org. — **core** (safety net) / custom views **optional**
- **IMPL-DOC-52** — `org-gtd-reflect-missed-items` finds items that should have been processed but weren't (past-due tickler/delegated/calendar). — reference.org; whats-new-4.0.org; diataxis commands.org. — **optional**
- **IMPL-DOC-53** — `org-gtd-reflect-upcoming-delegated` lists active delegated items with future check-in dates for proactive planning. — reference.org "Reflect Commands (Upcoming Delegated)". — **optional**
- **IMPL-DOC-54** — `org-gtd-reflect-someday-review` walks someday items one at a time with single-keystroke actions: `d` defer (logs "Reviewed" timestamp to LOGBOOK), `c` clarify (restores previous state and enters organize flow), `q` quit; shows statistics; optional list filtering via `org-gtd-someday-lists` (also callable with a list argument). Diataxis additionally documents `org-gtd-reflect-someday-maybe` (plain agenda of all someday items). — reference.org "Reviewing Someday/Maybe Items"; whats-new-4.0.org; diataxis commands.org. — **core** (review) / lists **optional**
- **IMPL-DOC-55** — `org-gtd-reflect-completed-items` (last N days, default 7, prefix arg) and `org-gtd-reflect-completed-projects` (all tasks DONE) support weekly-review reflection. — reference.org "Reflect Commands". — **optional**
- **IMPL-DOC-56** — `POSSIBLY-STALE` "The weekly review is not yet implemented." — using-org-gtd.org "Engaging with your GTD items" explicitly disclaims a packaged weekly-review workflow, while README lists "review" among built-in views; treat the README phrasing as aspirational. — n/a (documented gap)
- **IMPL-DOC-57** — `POSSIBLY-STALE` `org-gtd-delegate-agenda-item` delegates the agenda item at point (prompts who/when, refiles). Documented as current in using-org-gtd.org "Commands you can call on org-agenda" and reference.org "Agenda-Specific Commands", **but** CHANGELOG Unreleased deprecates it in favor of DWIM `org-gtd-delegate`. — **optional**
- **IMPL-DOC-58** — `org-gtd-archive-completed-items` archives all DONE/CNCL GTD items; archive location controlled by `org-gtd-archive-location` (a zero-arity function, default year-suffixed `gtd_archive_YYYY.org::datetree/`; set to `nil` to honor the user's `org-archive-location`). Diataxis also documents `org-gtd-archive-item-at-point`. — using-org-gtd.org "Cleaning up / archiving completed work"; reference.org "Archive Commands"; setting-up-org-gtd.org "Working with Your Existing Org Setup"; diataxis commands.org. — **core** (cleanup) / location **optional**
- **IMPL-DOC-59** — Terminology aliases: `org-gtd-review-*` → `org-gtd-reflect-*`, `org-gtd-oops-*` → `org-gtd-reflect-missed-*`, `org-gtd-incubate` → `org-gtd-tickler` all kept as obsolete aliases. — whats-new-4.0.org "Updated terminology"; reference.org "Backward Compatibility Note". — **optional** (compat)

## Projects

- **IMPL-DOC-60** — Organizing a multi-task heading as a Project creates dependency relationships, marks first task(s) NEXT and the rest TODO, and auto-advances tasks as predecessors complete (requires org-edna). — README.org; using-org-gtd.org "Project [P]"; getting-started manual section "Tutorial: Your First Project". — **core**
- **IMPL-DOC-61** — Projects are DAGs: any dependency relationship (sequential, parallel, fan-out, fan-in, diamond), multiple NEXT tasks simultaneously, automatic cycle prevention; tasks can live anywhere and link to projects by ID (`ORG_GTD_PROJECT_IDS`). — whats-new-4.0.org "Flexible project dependencies"; using-org-gtd.org "Working with Projects (Advanced)". — **core** (v4 headline feature)
- **IMPL-DOC-62** — `org-gtd-show-project-graph` opens an interactive visual graph (Sugiyama layout, blue=NEXT, orange=TODO, color-blind-safe palette); works from project headings, task headings, agenda items, with project selection prompts. — using-org-gtd.org "Your First Project Graph View"; CHANGELOG 4.2.0, 4.3.1. — **core** (for complex projects)
- **IMPL-DOC-63** — Graph view editing: add root task (`r`), add successor (`s`), add blocker (`b`) with multi-select checkbox menus for multiple edges; bulk-modify blockers (`B`) and successors (`S`); task ops under `t` prefix (change state, remove, trash, edit in org file, show relationships); project ops Cancel (`C`), Incubate/tickler (`I`), Someday (`S`). — using-org-gtd.org "Graph View Command Reference"; CHANGELOG 4.5.1. — **core**
- **IMPL-DOC-64** — Graph view navigation: dependency up/down (`n`/`p`), sibling TAB/S-TAB with configurable `org-gtd-graph-sibling-mode` (any-same-blocker / all-same-blockers / dag-level), goto-by-name (`G`), click-to-select nodes, `?` transient menu, refresh (`g`). — using-org-gtd.org; reference.org "Graph View Configuration"; CHANGELOG 4.1.0. — **core**
- **IMPL-DOC-65** — Graph rendering modes: SVG (default, GUI) and ASCII (terminal/screen-reader), toggle with `v`, default via `org-gtd-graph-render-mode`; panel split via `org-gtd-graph-ui-split-ratio`. — using-org-gtd.org "Graph Rendering Modes"; reference.org. — **optional**
- **IMPL-DOC-66** — Graph view auto-refreshes via file watching (300ms debounce) when the org file changes; multiple graph views stay in sync; no in-graph undo (use org-file undo + auto-refresh). — using-org-gtd.org "Auto-Refresh on File Changes" / "Undo/Redo Support". — **core** (automatic)
- **IMPL-DOC-67** — `POSSIBLY-STALE` Graph export to SVG / DOT / ASCII files. The reference tables document `x s` / `x d` / `x a`, but the "Exporting Project Graphs" tutorial in the same file says `E s` / `E d` / `E a`, mentions `j`/`k` navigation and `ESC` deselect that appear nowhere else — internally contradictory key documentation. — using-org-gtd.org "Exporting Project Graphs" vs "Graph View Command Reference". — **optional**
- **IMPL-DOC-68** — DWIM project task commands working from org buffer, agenda, or graph: `org-gtd-project-add-successor`, `-add-blocker`, `-add-root-task`, `-remove-task` (rewires predecessors→successors), `-trash-task` (cancels + removes from all projects), `-change-state`. — using-org-gtd.org "Text-Based Alternatives"; reference.org "Project Task Commands (DWIM)". — **core**
- **IMPL-DOC-69** — Text-based dependency commands: `org-gtd-task-add-blockers`, `-remove-blockers`, `-add-dependent`, `-add-dependents`, `-clear-relationships`, `-show-relationships`, `org-gtd-validate-project-dependencies` (broken refs, orphans, cycles), `org-gtd-remove-task-from-project`. — reference.org "Task Dependency Commands". — **advanced**
- **IMPL-DOC-70** — Project cancellation: `org-gtd-project-cancel` (on heading), `org-gtd-project-cancel-from-agenda`, graph-view `C` — all confirm then mark incomplete tasks CNCL preserving DONE; manual CNCL on a project heading triggers the same cascade when `org-gtd-mode` is on. — using-org-gtd.org "Canceling Projects"; CHANGELOG 4.6.0; diataxis commands.org (adds `-from-context` variants). — **core**
- **IMPL-DOC-71** — Tickler projects: `org-gtd-tickler` smart dispatch (project heading → whole project; project task → its project; single item → item) pauses with a review date in `ORG_GTD_TIMESTAMP`, clears TODO keywords, preserves dependencies, and warns when external tasks depend on the project. — using-org-gtd.org "Tickler Projects"; reference.org "org-gtd-tickler". — **core**
- **IMPL-DOC-72** — Someday projects: `org-gtd-someday` smart dispatch puts whole projects (or single items) on someday/maybe with no review date, with the same state preservation. — using-org-gtd.org "Someday/Maybe [y]"; reference.org "org-gtd-someday"; CHANGELOG 4.5.1. — **core**
- **IMPL-DOC-73** — Multi-project tasks use "last active project" logic: a shared task is only incubated/hidden when the project being paused is its last active project; `fix-all-todo-keywords` applies AND semantics for NEXT readiness. — using-org-gtd.org "Multi-Project Tasks"; troubleshooting.org; reference.org. — **core** (automatic)
- **IMPL-DOC-74** — `org-gtd-reactivate` restores tickler/someday items: restores `PREVIOUS_*` properties, prompts to confirm/update each type-specific property for non-projects, recalculates NEXT/TODO for projects and opens the graph view; items with no saved state enter the organize flow. — using-org-gtd.org "Reactivation Details"; reference.org "org-gtd-reactivate". — **core**
- **IMPL-DOC-75** — Progress cookies `[3/7][42%]` on project headings, counting all `ORG_GTD_PROJECT_IDS`-linked tasks, auto-updating on state changes, with `org-gtd-project-progress-cookie-position` (end/start/nil) and `org-gtd-project-update-all-cookies` for manual refresh. — using-org-gtd.org "Project Progress Cookies"; reference.org. — **core** (default-on) / position **optional**

## Configuration

- **IMPL-DOC-76** — `org-gtd-directory` (default `~/gtd/`) holds all GTD files; `inbox.org` and `org-gtd-tasks.org` are created automatically, with category headings auto-created on first refile. — README.org; reference.org "Directory and File Configuration". — **core**
- **IMPL-DOC-77** — Required keyword setup: `org-todo-keywords` (all GTD keywords in one sequence; org keyword options like `(t)` / `(d!)` supported) plus `org-gtd-keyword-mapping` mapping todo/next/wait/done/canceled to user keywords, with validation (setopt on 29+); power users should add a separate GTD sequence. — README.org; setting-up-org-gtd.org "Required keyword configuration"; reference.org. — **core**
- **IMPL-DOC-78** — `POSSIBLY-STALE` `org-gtd-keyword-mapping` default: reference.org shows the default without a `done` entry while listing `done` as a required key, and README's recommended mapping omits `done` while setting-up-org-gtd.org includes it — inconsistent guidance on whether `done` must be mapped. — README.org vs setting-up-org-gtd.org vs reference.org. — **core**
- **IMPL-DOC-79** — `org-gtd-setup-keywords-wizard` interactively walks through keyword mapping setup with validation. — reference.org "Utility Commands". — **optional**
- **IMPL-DOC-80** — `org-edna-mode` must be enabled for project dependencies/task advancement (documented as REQUIRED). — README.org; setting-up-org-gtd.org "Configuring org-edna". — **core**
- **IMPL-DOC-81** — Users configure `org-agenda-files` directly (v4: org-gtd no longer wraps agenda commands); directory entries are valid; existing users should merge rather than overwrite. — README.org "Existing org-agenda-files users"; CHANGELOG 4.0.0 BREAKING; setting-up-org-gtd.org. — **core**
- **IMPL-DOC-82** — `org-gtd-update-ack` suppresses the load-time major-version upgrade warning; must be set before the package loads. — README.org; setting-up-org-gtd.org "Acknowledge the upgrade". — **core**
- **IMPL-DOC-83** — `org-gtd-upgrade-v3-to-v4` migrates v3 data (incubated→tickler/someday split, ORG_GTD properties, delegated type, habit property, TRIGGER→dependency conversion, ORG_GTD_PROJECT name cache); safe to run multiple times; backup recommended. — setting-up-org-gtd.org "Required: Data migration"; reference.org; CHANGELOG 4.0.1. — **core** (for upgraders)
- **IMPL-DOC-84** — Complete copy-paste configurations for vanilla Emacs, Doom, and Spacemacs, plus configuration tests and common-problem troubleshooting. — setting-up-org-gtd.org "Complete Configuration Examples"; README.org. — **core** (docs promise)
- **IMPL-DOC-85** — Native compilation support with claimed 10-20% improvement on 500+ task datasets; Emacs 28.1+ primitives. — README.org "Performance"; CHANGELOG 4.0.0. — **optional**
- **IMPL-DOC-86** — `POSSIBLY-STALE` README "Version Notice" and config examples pin `org-gtd-update-ack "4.0.0"` and describe the package as "org-gtd 4.0.0"; the package is at 4.6.x with an unreleased type-system overhaul — version references in README are out of date. — README.org "Version Notice". — n/a

## Extension points

- **IMPL-DOC-87** — Programmatic creation helpers: `org-gtd-habit-create`, `org-gtd-calendar-create`, `org-gtd-delegate-create`, `org-gtd-tickler-create`, `org-gtd-someday-create`, `org-gtd-single-action-create` (each takes at least a heading string), e.g. for email-send hooks. — using-org-gtd.org "Automating through emacs"; reference.org "Programmatic Creation Commands"; whats-new-3.0.org. — **advanced**
- **IMPL-DOC-88** — `org-gtd-create-item TYPE TITLE [CONFIG]` is the unified programmatic entry point for any registered type (per-type `*-create` helpers retained for compatibility). — extending-org-gtd.org "Programmatic item creation"; diataxis how-to/create-items-programmatically.org; CHANGELOG Unreleased. — **advanced**
- **IMPL-DOC-89** — Type registry: built-in types live in `org-gtd-types` with wiring fields (`:organize-fn`, `:disposition`, `:prompt-to-refile`, `:transient-key`, `:refile-target`, `:hooks`, ...); `org-gtd-customize-type` overrides fields on built-in types (new top-level types cannot be registered). — extending-org-gtd.org "Customizing types"; diataxis reference/type-registry.org; diataxis explanation/about-the-type-system.org. — **advanced**
- **IMPL-DOC-90** — `POSSIBLY-STALE` `org-gtd-user-types` alist customizes property prompts/input functions per built-in type (BBDB/EBDB delegation completion examples). Documented as current in reference.org "Advanced Item Configuration", and diataxis hooks.org still says setting `:hooks` via it follows merge rules — but extending-org-gtd.org presents `org-gtd-customize-type` as the way to do the same overrides; the relationship/preference between the two mechanisms is not consistently documented. Also extending-org-gtd.org names a field `:organize-project-fn` while CHANGELOG Unreleased names it `:project-fn`. — **advanced**
- **IMPL-DOC-91** — Six-stage observation-only hook pipeline: `:before-clarify`, `:after-clarify`, `:before-organize`, `:after-organize`, `:before-file`, `:after-file`; global defvars (`org-gtd-before-clarify-hook` etc.) plus per-type `:hooks` local listeners; hooks receive POM, errors are caught and logged; dispatcher `org-gtd-hooks-run`. — extending-org-gtd.org "Hook stages"; diataxis reference/hooks.org; diataxis how-to/write-a-hook.org; CHANGELOG Unreleased. — **advanced**
- **IMPL-DOC-92** — Project traversal API: `org-gtd-project-map-tasks` (apply fn to each task in one project's dependency graph) and `org-gtd-projects-map` (apply fn to each project, returns marker alist). — extending-org-gtd.org "Project Traversal Functions". — **advanced**
- **IMPL-DOC-93** — Areas of focus (Horizon 2): `org-gtd-areas-of-focus` list + `org-gtd-set-area-of-focus` organize hook; `org-gtd-reflect-area-of-focus` review view; `org-gtd-area-of-focus-set-on-item-at-point` / `-on-agenda-item` (the agenda variant detects project tasks and sets the project's CATEGORY instead). — using-org-gtd.org "Areas of focus"; reference.org "Area of Focus Commands". — **optional**
- **IMPL-DOC-94** — `POSSIBLY-STALE` Engage by context: the command-center menu lists "`@` Engage by context"; diataxis commands.org documents `org-gtd-engage-tagged` (prompts for a tag) with `org-gtd-engage-grouped-by-context` as an obsolete alias "since 4.0". The main manual (setting-up/using/reference) never documents either command by name — only the menu entry. — setting-up-org-gtd.org "Command Center"; diataxis reference/commands.org "Engage". — **optional**

## Other

- **IMPL-DOC-95** — Horizons file: long-term horizons stored in `org-gtd-horizons-file` (default `horizons.org`) in the GTD directory, displayable during clarification; visual-line-mode tip for the narrow window. — using-org-gtd.org "Longer-term horizons"; reference.org. — **optional**
- **IMPL-DOC-96** — `POSSIBLY-STALE` Properties contract documented for users: `ORG_GTD`, `ORG_GTD_REFILE`, `ORG_GTD_FIRST_TASKS`, `ORG_GTD_DEPENDS_ON`, `ORG_GTD_BLOCKS`, `ORG_GTD_PROJECT_IDS`, `ORG_GTD_TIMESTAMP`, `DELEGATED_TO`, `STYLE=habit`, `CATEGORY`, `ID`, plus `PREVIOUS_*` snapshots — labeled "Internal Implementation" but used in troubleshooting and manual-repair guidance. *Stale watch:* doc set inconsistently uses `ORG_GTD: Habit` vs `Habits` and reference duplicate-command bindings (`d`/`D` in reference.org vs `C-c d`/`C-c D` everywhere else). — reference.org "Properties Reference"; using-org-gtd.org. — **advanced**
- **IMPL-DOC-97** — Troubleshooting guidance promises specific diagnosable failure modes: projects without NEXT (fix commands), project-shape errors when organizing, keyword misconfiguration, items missing from engage view, and tickler/someday tasks still visible (multi-project logic). — troubleshooting.org (whole file); reference.org "Troubleshooting Projects". — **core** (support docs)
- **IMPL-DOC-98** — Inactive-project task filtering: tasks belonging exclusively to done/cancelled projects are automatically excluded from active views (stuck/engage), fail-open for unresolvable project IDs; done/cancelled project headings excluded from stuck/active project views. — CHANGELOG 4.6.0 (not yet described in the manual proper — candidate doc gap). — **core** (automatic)

---

## Notes for downstream agents

1. **Two doc generations coexist.** The shipped manual (doc/org-gtd.org includes setting-up,
   getting-started, using, reference, extending, troubleshooting) mostly describes the 4.x
   feature set, while doc/diataxis/ and doc/extending-org-gtd.org describe the
   unreleased unified type/hook model (per CHANGELOG "Unreleased"). Where they conflict
   (refile prompting, organize transient keys, single-action vs next-action,
   delegate-agenda-item) the items above are flagged POSSIBLY-STALE.
2. **doc/old-upgrade-documentation.org** still describes `with-org-gtd-context` and
   v2→v3-era mechanics; it is not included in the compiled manual and appears intentionally
   historical, but any code-inventory match against it should be treated as stale.
3. **README version notice** (4.0.0) and the `org-gtd-update-ack "4.0.0"` advice lag the
   actual release line (4.6.x).
4. Features documented only in CHANGELOG (IMPL-DOC-98 inactive-project filtering; parts of
   IMPL-DOC-13 duplicate-queue fixes) are promises users can rely on but have no manual
   section yet — candidates for the "documented-but-thin" gap list.
