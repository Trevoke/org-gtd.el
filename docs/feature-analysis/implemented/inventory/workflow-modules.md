# Implemented Feature Inventory — Core GTD Workflow Modules

Derived bottom-up from source code only (no spec/status documents consulted).
Scope: org-gtd-{capture,process,clarify,wip,organize,organize-core,types,next-action,
quick-action,delegate,calendar,habit,knowledge,someday,someday-review,tickler,trash,
projects,project-operations,dependencies,refile,create,id,accessors,value-objects,
task-management,reactivate,archive}.el

All paths relative to repo root `/home/stag/src/projects/org-gtd.el/`.

## Summary

| Area | Features |
|---|---|
| 1. Capture | 4 |
| 2. Process / Clarify | 16 |
| 3. Organize (dispatcher + categories) | 16 |
| 4. Projects & Dependencies | 17 |
| 5. Lifecycle: tickler / someday / reactivate / archive / trash | 8 |
| 6. Infrastructure (refile, IDs, programmatic API) | 6 |
| **Total** | **67** |

Note: some automatic behaviors implemented in these modules are *activated* by
`org-gtd-mode.el` (out of scope), which adds them to `org-after-todo-state-change-hook`
(org-gtd-mode.el:144-150). This is flagged per feature below.

---

## 1. Capture

### IMPL-WF-01 — Capture to GTD inbox
- **Capability**: User can capture a thought into the GTD inbox from anywhere via a wrapper around `org-capture` that guarantees the inbox file exists first.
- **Entry point**: `org-gtd-capture` (command, autoloaded) — org-gtd-capture.el:82
- **Kind**: command
- **Notes**: Accepts optional GOTO/KEYS pass-through to `org-capture`. Uses `with-org-gtd-capture` macro (org-gtd-capture.el:73) to bind `org-capture-templates` to the GTD set.

### IMPL-WF-02 — Customizable capture templates (plain + with-link)
- **Capability**: User can define their own inbox capture templates; two are built in — `i` (plain entry) and `l` (entry with an org link back to the capture context).
- **Entry point**: `org-gtd-capture-templates` (defcustom) — org-gtd-capture.el:40
- **Kind**: customization
- **Notes**: Templates must start with a single asterisk (top-level heading) or behavior is undefined.

### IMPL-WF-03 — Automatic capture timestamp (ORG_GTD_CAPTURED_AT)
- **Capability**: Every captured item is automatically stamped with an inactive `ORG_GTD_CAPTURED_AT` property recording when it entered the system.
- **Entry point**: `org-gtd-capture--add-captured-at-timestamp` via `:before-finalize` in default templates — org-gtd-capture.el:96, :45, :50
- **Kind**: automatic-behavior
- **Notes**: Applies to all level-1 headings in a multi-item capture (same timestamp for all); never overwrites an existing value.

### IMPL-WF-04 — Self-creating inbox file with explanatory banner
- **Capability**: The inbox file (`inbox.org` in `org-gtd-directory`) is created on demand with an explanatory comment block; user never has to set it up.
- **Entry point**: `org-gtd-inbox-path` (autoloaded) — org-gtd-capture.el:111; template at :64
- **Kind**: automatic-behavior

---

## 2. Process / Clarify

### IMPL-WF-05 — Sequential inbox processing loop
- **Capability**: User can process the entire inbox one item at a time; each item opens the clarification interface, and finishing/filing one automatically advances to the next until the inbox is empty.
- **Entry point**: `org-gtd-process-inbox` (command, autoloaded) — org-gtd-process.el:62
- **Kind**: command
- **Notes**: Continuation-passing: `org-gtd-clarify-inbox-item` stores `org-gtd-process-inbox` as the continuation (org-gtd-process.el:85-87). On completion, runs `whitespace-cleanup` and saves GTD buffers (org-gtd-process.el:107).

### IMPL-WF-06 — Multiple additional inbox files
- **Capability**: User can declare extra inbox files (e.g. from mobile sync) that are processed in order after the main inbox empties, in one continuous session.
- **Entry point**: `org-gtd-additional-inbox-files` (defcustom) — org-gtd-process.el:38; consumed at :91
- **Kind**: customization
- **Notes**: Missing files are silently skipped. Session state tracked in `org-gtd-process--session-active` / `--pending-inboxes`.

### IMPL-WF-07 — Clarify any heading in a dedicated WIP buffer
- **Capability**: User can pull any org heading (not just inbox items) into an isolated editing buffer to refine it before organizing.
- **Entry point**: `org-gtd-clarify-item` (command, autoloaded) — org-gtd-clarify.el:245
- **Kind**: command
- **Notes**: Copies the subtree into the WIP buffer; original is cut only after successful organization (org-gtd-organize-core.el:137-146). Stores window config and restores it afterward.

### IMPL-WF-08 — In-place update (skip refile) via prefix arg / transient toggle
- **Capability**: With `C-u` before clarifying (or the `-n` toggle in the organize menu), the reorganized item replaces the original heading in place — preserving its file location and outline level — instead of being refiled.
- **Entry points**: prefix handling in `org-gtd-clarify-item` — org-gtd-clarify.el:255; transient infix `org-gtd-organize--skip-refile-infix` — org-gtd-organize.el:64; replacement logic `org-gtd-organize--update-in-place` — org-gtd-organize-core.el:90
- **Kind**: command (modifier) + automatic-behavior
- **Notes**: Re-levels the level-1 WIP subtree to the source heading's original outline level (issue #291 fix). The toggle is hidden during inbox processing (org-gtd-organize.el:73).

### IMPL-WF-09 — Clarify directly from agenda views
- **Capability**: User can clarify the item under point in any agenda view; supports the same `C-u` in-place mode.
- **Entry point**: `org-gtd-clarify-agenda-item` (command) — org-gtd-clarify.el:230
- **Kind**: command

### IMPL-WF-10 — Dedicated clarification major mode with guidance header
- **Capability**: WIP buffers run `org-gtd-clarify-mode` (derived from org-mode) showing a header line with the key actions (file / duplicate / cancel) and keybindings `C-c C-k`, `C-c d`, `C-c D`.
- **Entry point**: `org-gtd-clarify-mode` (autoloaded major mode) — org-gtd-clarify.el:208; keymap at :179
- **Kind**: command (mode)
- **Notes**: Obsolete alias `org-gtd-clarify-map` kept for back-compat (:188-190).

### IMPL-WF-11 — Crash-safe, resumable WIP buffers backed by temp files
- **Capability**: Each clarification is backed by a real temp file with auto-save enabled, so an Emacs crash does not lose in-flight clarification work, and re-clarifying the same item reuses its existing buffer/file.
- **Entry points**: `org-gtd-wip--get-buffer` — org-gtd-wip.el:70; auto-save in mode init — org-gtd-clarify.el:221
- **Kind**: automatic-behavior
- **Notes**: Files live in `$TMPDIR/org-gtd/`; filenames truncated to stay under NAME_MAX (org-gtd-wip.el:40,59); symlink-safe cleanup (issue #271, org-gtd-wip.el:109).

### IMPL-WF-12 — Switch between multiple concurrent clarifications
- **Capability**: User can have several items mid-clarification at once and jump between WIP buffers via completing-read.
- **Entry point**: `org-gtd-clarify-switch-to-buffer` (command) — org-gtd-clarify.el:270
- **Kind**: command

### IMPL-WF-13 — Cancel clarification and restore state
- **Capability**: User can abort clarifying (`C-c C-k`); the temp file is cleaned up, the prior window configuration restored, and an active inbox session is terminated.
- **Entry point**: `org-gtd-clarify-stop` (command) — org-gtd-clarify.el:288
- **Kind**: command
- **Notes**: If a duplicate queue is pending, only the current item is discarded and the next queued duplicate is loaded.

### IMPL-WF-14 — Duplicate queue: clarify one item into many
- **Capability**: While clarifying, user can duplicate the current item (with a new title via `C-c d`, or exactly via `C-c D`); duplicates queue up and are clarified one after another, each independently organizable.
- **Entry points**: `org-gtd-clarify-duplicate` — org-gtd-clarify.el:317; `org-gtd-clarify-duplicate-exact` — :340; queue processing `org-gtd-clarify--process-next-queued-item` — :630
- **Kind**: command + automatic-behavior
- **Notes**: Each duplicate gets a fresh org ID (:649-651). Queue shown in a live side window with pending count (:601).

### IMPL-WF-15 — Duplicate queue window position
- **Capability**: User can choose which side (top/right/left/bottom) the pending-duplicates window appears on.
- **Entry point**: `org-gtd-clarify-duplicate-queue-position` (defcustom, default `bottom`) — org-gtd-clarify.el:79
- **Kind**: customization

### IMPL-WF-16 — Data-loss guard for pending duplicates
- **Capability**: Quitting Emacs or killing a clarify buffer with queued duplicates prompts to discard / save-to-inbox / cancel, so queued items are never silently lost.
- **Entry points**: `org-gtd-clarify--kill-emacs-query` on `kill-emacs-query-functions` — org-gtd-clarify.el:740,758; buffer-local `org-gtd-clarify--kill-buffer-query` — :762
- **Kind**: automatic-behavior
- **Notes**: "Save" appends raw queue content to the inbox file (:694).

### IMPL-WF-17 — Horizons side window during clarification
- **Capability**: User can have a read-only view of their higher-horizons file displayed beside the clarify buffer (configurable side), or toggle it manually, to keep life goals visible while deciding.
- **Entry points**: `org-gtd-clarify-show-horizons` (defcustom) — org-gtd-clarify.el:53; `org-gtd-clarify-toggle-horizons-window` (command) — :279
- **Kind**: customization + command
- **Notes**: Uses a read-only indirect buffer (:430); cleaned up when processing stops.

### IMPL-WF-18 — Organize-types help side window
- **Capability**: User can display (always or on toggle) a cheat-sheet buffer describing every organize destination and its key (Quick Action [q], Project [p], ...).
- **Entry points**: `org-gtd-clarify-show-organize-help` (defcustom) — org-gtd-clarify.el:71; `org-gtd-clarify-toggle-organize-help` (command) — :479; content at :92
- **Kind**: customization + command

### IMPL-WF-19 — User-defined project templates inserted at clarify time
- **Capability**: User can register named project skeletons and insert one under the item being clarified (e.g. a standard "publish blog post" task list).
- **Entry points**: `org-gtd-clarify-project-templates` (defcustom alist) — org-gtd-clarify.el:44; `org-gtd-clarify-project-insert-template` (command) — :371
- **Kind**: customization + command

### IMPL-WF-20 — Live project-dependency helper window in WIP buffer
- **Capability**: When enabled, a side window renders each task's relationships as `(depends-on) -> task -> (blocks)` plus an "orphaned tasks" section while the user edits a multi-task item.
- **Entry points**: `org-gtd-clarify-display-helper-buffer` (defcustom) — org-gtd-clarify.el:63; `org-gtd-clarify-display-dependency-helper` (command) — :396
- **Kind**: customization + command
- **Notes**: Resolves IDs in ORG_GTD_DEPENDS_ON/ORG_GTD_BLOCKS to task names.

*(also in this area but recorded as behavior of IMPL-WF-07: clarify initialization strips stale org-gtd state properties — ORG_GTD_TIMESTAMP, DELEGATED_TO, STYLE, ORG_GTD_PROJECT — so the item is organized fresh; org-gtd-clarify.el:414-428)*

---

## 3. Organize (dispatcher + categories)

### IMPL-WF-21 — Transient organize menu
- **Capability**: User picks the GTD destiny of the clarified item from a transient menu grouped Actionable / Project / Non-actionable, with single-key choices (q/s/d/c/h/p/a/i/y/k/t).
- **Entry point**: `org-gtd-organize` (transient prefix) — org-gtd-organize.el:71
- **Kind**: command

### IMPL-WF-22 — Organize hooks (per-item decoration)
- **Capability**: User can configure functions to run on every item as it's organized (default: `org-set-tags-command`; suggested options include effort and priority), enabling consistent metadata.
- **Entry point**: `org-gtd-organize-hooks` (defcustom) — org-gtd-organize-core.el:53; applied by `org-gtd-organize-apply-hooks` — :79
- **Kind**: customization / hook
- **Notes**: For projects, hooks are applied to the heading (as `project-heading`) and to every task (as `project-task`) (org-gtd-projects.el:615-631, 1386).

### IMPL-WF-23 — Type predicate for conditional hooks
- **Capability**: Hook authors can branch on the item's chosen type via `org-gtd-organize-type-member-p` (with `everything` wildcard and `single-action`→`next-action` normalization); invalid types signal a dedicated error.
- **Entry point**: `org-gtd-organize-type-member-p` — org-gtd-organize.el:94; valid types at :49
- **Kind**: hook (support API)

### IMPL-WF-24 — Quick action (do it now, < 2 min)
- **Capability**: User marks an item as already-done-in-under-two-minutes; it's set to DONE and archived immediately.
- **Entry point**: `org-gtd-quick-action` (command) — org-gtd-quick-action.el:36; type def org-gtd-types.el:117 (`done-and-archive`)
- **Kind**: command

### IMPL-WF-25 — Next action (single action)
- **Capability**: User files an item as a standalone next action: NEXT state, refiled to the Actions list, appears in engage view.
- **Entry point**: `org-gtd-next-action` (command; obsolete alias `org-gtd-single-action`) — org-gtd-next-action.el:49, aliases :104-112
- **Kind**: command

### IMPL-WF-26 — Delegate (with person + check-in date + logbook note)
- **Capability**: User delegates an item: prompted for who and a check-in date; item gets WAIT state, `DELEGATED_TO`, `ORG_GTD_TIMESTAMP`, and a logbook note "programmatically delegated to NAME".
- **Entry point**: `org-gtd-delegate` (command, optional non-interactive args) — org-gtd-delegate.el:44; note at :89; type def org-gtd-types.el:40
- **Kind**: command
- **Notes**: Obsolete alias `org-gtd-delegate-agenda-item` (:55).

### IMPL-WF-27 — Automatic WAIT→delegate conversion prompt
- **Capability**: When the user manually flips a next action to WAIT, org-gtd offers to convert it into a proper delegated item (prompting for person and check-in date).
- **Entry point**: `org-gtd-next-action--maybe-convert-to-delegated` — org-gtd-next-action.el:71
- **Kind**: automatic-behavior (activated by org-gtd-mode on `org-after-todo-state-change-hook`, org-gtd-mode.el:148)

### IMPL-WF-28 — Calendar item (date/time-specific)
- **Capability**: User files an item that must happen on a specific date; prompted for the date (stored in `ORG_GTD_TIMESTAMP`), no TODO keyword, shows in agenda on that day.
- **Entry point**: `org-gtd-calendar` (command, optional date arg) — org-gtd-calendar.el:38; type def org-gtd-types.el:54
- **Kind**: command

### IMPL-WF-29 — Habit (org-habit recurring action)
- **Capability**: User files an item as a recurring habit: prompted for a repeating SCHEDULED timestamp, `STYLE: habit` set automatically, refiled to a "Habits" heading.
- **Entry point**: `org-gtd-habit` (command, optional repeater arg) — org-gtd-habit.el:38; type def org-gtd-types.el:91
- **Kind**: command

### IMPL-WF-30 — Knowledge / reference
- **Capability**: User marks an item as reference material; it is set DONE and archived (expected workflow: user files the content in their own knowledge system first).
- **Entry point**: `org-gtd-knowledge` (command) — org-gtd-knowledge.el:37; type `reference`, `done-and-archive` — org-gtd-types.el:103
- **Kind**: command

### IMPL-WF-31 — Trash
- **Capability**: User discards an item; it's set to the canceled keyword and archived (audit trail preserved rather than deleted).
- **Entry point**: `org-gtd-trash` (command) — org-gtd-trash.el:36; type def org-gtd-types.el:110 (`cancel-and-archive`)
- **Kind**: command

### IMPL-WF-32 — Tickler (date-based incubation)
- **Capability**: User defers an item to a specific future date; TODO keyword cleared, review date stored, item resurfaces in agenda then. Works on whole projects too (see IMPL-WF-54).
- **Entry point**: `org-gtd-tickler` (command, optional date arg; obsolete alias `org-gtd-incubate`) — org-gtd-tickler.el:41, :99
- **Kind**: command

### IMPL-WF-33 — Someday/Maybe (timeless incubation) with named lists
- **Capability**: User parks an item with no date commitment; previous GTD state is saved for later reactivation; if `org-gtd-someday-lists` is configured, the user assigns the item to a named list (e.g. "Books", "Trips") stored in `ORG_GTD_SOMEDAY_LIST`.
- **Entry points**: `org-gtd-someday` (command) — org-gtd-someday.el:51; `org-gtd-someday-lists` (defcustom) — :41; organize fn — :73
- **Kind**: command + customization
- **Notes**: Clears TODO keyword, SCHEDULED, DEADLINE, and ORG_GTD_TIMESTAMP. Distinct from tickler (no date).

### IMPL-WF-34 — Type registry with user-level type customization
- **Capability**: All GTD behavior is driven by a declarative type registry (states, prompted properties, disposition, refile target, transient key, per-stage hooks); user can customize built-in types — e.g. change the "who" prompt for delegation to use a contacts completion function — via `org-gtd-customize-type` or `org-gtd-user-types`.
- **Entry points**: `org-gtd-types` (defconst) — org-gtd-types.el:31; `org-gtd-user-types` (defcustom) — :151; `org-gtd-customize-type` (autoloaded fn) — :259
- **Kind**: customization / data-model
- **Notes**: Merge rules: scalars replace, `:properties` merge by semantic name, `:hooks` append per stage; `:org-gtd` value can never be overridden. New types cannot be added — only built-ins customized. Property descriptors support `:default` (skips prompting) and `:input-fn` (custom reader).

### IMPL-WF-35 — Eight-stage organize pipeline with per-type hooks
- **Capability**: Every organize action runs a documented pipeline (clear foreign props → :before-organize → organize-fn → classic organize-hooks → :after-organize → :before-file → disposition → :after-file), giving users/extensions stable interception points.
- **Entry point**: `org-gtd-process-heading` — org-gtd-organize-core.el:246; dispositions `org-gtd--run-disposition` — :201
- **Kind**: hook / automatic-behavior
- **Notes**: Dispositions: `list` (refile), `done-and-archive`, `cancel-and-archive`, `externalize` (reserved, errors). Automatic foreign-property cleanup when re-typing an item removes properties belonging to the old type but not the new (org-gtd-organize-core.el:166).

### IMPL-WF-36 — DWIM dispatch: organize commands work from org buffer, agenda, or clarify flow, with project awareness
- **Capability**: The same category command (e.g. `org-gtd-tickler`) does the right thing wherever invoked: inside a WIP buffer it runs the clarify pipeline; on an agenda line it resolves the marker; on a project heading or project task it routes to project-level handling (prompting which project for multi-project tasks).
- **Entry point**: `org-gtd--dispatch` — org-gtd-organize-core.el:288; project routing `org-gtd-process-project` — :276
- **Kind**: automatic-behavior

---

## 4. Projects & Dependencies

### IMPL-WF-37 — Create a project from a multi-heading item
- **Capability**: User organizes a clarified item with subheadings as a project: heading becomes the project, subheadings become tasks, sequential dependencies are auto-wired, first task(s) marked NEXT, progress cookie added, refiled under Projects.
- **Entry points**: `org-gtd-project-new` (command) — org-gtd-projects.el:532; workflow `org-gtd-project-new--apply` — :660
- **Kind**: command

### IMPL-WF-38 — Malformed-project validation with teaching error
- **Capability**: Attempting to organize a single heading as a project aborts with an explanatory message showing the required structure and suggesting "single action" instead; user is returned to clarifying.
- **Entry point**: `org-gtd-project--validate-format` — org-gtd-projects.el:606; message text :100
- **Kind**: automatic-behavior

### IMPL-WF-39 — Add a clarified item to an existing project
- **Capability**: User attaches the item being clarified to an existing project (selected via completion); it is chained after the project's current leaf task (or becomes a first task in an empty project), refiled into the project, and states recalculated.
- **Entry points**: `org-gtd-project-extend` (command) — org-gtd-projects.el:417; `org-gtd-project-extend--apply` — :1341
- **Kind**: command
- **Notes**: Respects skip-refile mode (task stays in place, only properties set).

### IMPL-WF-40 — DAG dependency data model (beyond sequential projects)
- **Capability**: Tasks form a directed acyclic graph via multivalued `ORG_GTD_DEPENDS_ON` / `ORG_GTD_BLOCKS` properties plus `ORG_GTD_FIRST_TASKS` on the project heading; the graph may span multiple files (resolved via org-id).
- **Entry points**: traversal `org-gtd-dependencies-collect-project-tasks` — org-gtd-dependencies.el:182; readiness BFS `org-gtd-dependencies-find-ready-tasks` — :46
- **Kind**: data-model
- **Notes**: A "ready" task is one whose dependencies are all DONE/CNCL; multiple parallel NEXT branches are supported.

### IMPL-WF-41 — Tasks can belong to multiple projects (AND-readiness semantics)
- **Capability**: A single task can be shared by several projects via multivalued `ORG_GTD_PROJECT_IDS`; it becomes NEXT only when ready in *all* of its projects, and project-scoped operations prompt the user to choose which project when ambiguous.
- **Entry points**: AND semantics in `org-gtd-projects-fix-all-todo-keywords` — org-gtd-projects.el:504-527; multi-project chooser `org-gtd-project--get-marker-at-point` — :169
- **Kind**: data-model
- **Notes**: Archive and tickler/someday flows are multi-project aware (skip/preserve shared tasks; see IMPL-WF-54, IMPL-WF-58).

### IMPL-WF-42 — Automatic task advancement on completion (org-edna)
- **Capability**: Marking a project task DONE automatically recalculates the project: dependent tasks whose blockers are all complete become NEXT, with no user action.
- **Entry points**: `TRIGGER: self org-gtd-update-project-after-task-done!` set on tasks — org-gtd-projects.el:732, org-gtd-dependencies.el:127; edna action `org-edna-action/org-gtd-update-project-after-task-done!` — org-gtd-projects.el:1417-1426
- **Kind**: automatic-behavior
- **Notes**: Recalculates every project the task belongs to. Legacy edna finder/action `org-gtd-next-project-action` / `org-gtd-update-project-task!` also defined (:1397-1410).

### IMPL-WF-43 — Circular-dependency prevention with path display
- **Capability**: Creating a dependency that would form a cycle is rejected with a `user-error` showing the existing path (`A -> B -> C -> A`).
- **Entry points**: `org-gtd-dependencies-validate-acyclic` — org-gtd-dependencies.el:95; used by `org-gtd-dependencies-create` — :110 and task-management commands
- **Kind**: automatic-behavior

### IMPL-WF-44 — Add successor / blocker / root task to a project (context-aware)
- **Capability**: From an org buffer, agenda line, or project graph view, user can grow a project: add a task after the one at point (successor), before it (blocker), or as a new independent root; each prompts with completion over existing tasks or creates a new one, then recalculates states.
- **Entry points**: `org-gtd-project-add-successor` — org-gtd-projects.el:238; `org-gtd-project-add-blocker` — :256; `org-gtd-project-add-root-task` — :274; simple implementations org-gtd-project-operations.el:54, :105, :163 (all autoloaded commands)
- **Kind**: command
- **Notes**: Selecting an existing task from another project links it (adds project ID) rather than duplicating — another route into multi-project membership.

### IMPL-WF-45 — Remove task from project (with dependency rewiring) / trash task
- **Capability**: User can remove the task at point from its project — its predecessors are reconnected to its successors — or trash it entirely (removed from all projects, dependencies cleaned, marked canceled), with confirmation prompts.
- **Entry points**: `org-gtd-project-remove-task` — org-gtd-projects.el:292; `org-gtd-project-trash-task` — :310; simple impls org-gtd-project-operations.el:213, :233
- **Kind**: command

### IMPL-WF-46 — Change task TODO state from any context
- **Capability**: User can change a project task's TODO state from agenda or graph view without visiting the file.
- **Entry points**: `org-gtd-project-change-state` — org-gtd-projects.el:328; org-gtd-project-operations.el:252
- **Kind**: command

### IMPL-WF-47 — Recalculate project TODO keywords (single project and bulk)
- **Capability**: User can repair/normalize NEXT/TODO states for the project at point or for *all* projects in agenda files (recommended for weekly review or after bulk edits); explicit user states (WAIT/DONE/CNCL) are always preserved.
- **Entry points**: `org-gtd-projects-fix-todo-keywords-for-project-at-point` (command) — org-gtd-projects.el:448; `org-gtd-projects-fix-all-todo-keywords` (command) — :457; engine `org-gtd-projects-fix-todo-keywords` — :576
- **Kind**: command
- **Notes**: Reset rules encoded in `org-gtd-todo-state-should-reset-p` (org-gtd-value-objects.el:81).

### IMPL-WF-48 — Cancel a whole project (command + automatic prompt on CNCL)
- **Capability**: User can cancel a project from the heading, agenda, or graph view; all incomplete tasks are marked canceled (with logging note). Additionally, manually setting a project heading to the canceled keyword triggers a confirmation prompt and cascades the cancellation — or reverts the state if declined.
- **Entry points**: `org-gtd-project-cancel` — org-gtd-projects.el:388; `org-gtd-project-cancel-from-agenda` — :405; `org-gtd-project-cancel-from-context` — :366; hook `org-gtd-project--maybe-cancel-from-hook` — :148 (activated via org-gtd-mode, org-gtd-mode.el:150)
- **Kind**: command + automatic-behavior
- **Notes**: Temporarily disables org-edna during cascade to avoid trigger storms (:137).

### IMPL-WF-49 — Project progress cookies [N/M][P%] with auto-update
- **Capability**: Project headings show progress as `[completed/total][percent%]`, positioned at start or end of the heading (or disabled); cookies update automatically whenever a project task changes state, and can be bulk-refreshed.
- **Entry points**: `org-gtd-project-progress-cookie-position` (defcustom, default `end`) — org-gtd-core.el:218; auto-update `org-gtd-project--maybe-update-cookies` — org-gtd-projects.el:1006 (activated via org-gtd-mode); `org-gtd-project-update-all-cookies` (command) — :1017
- **Kind**: customization + automatic-behavior + command
- **Notes**: Counts via graph traversal (multi-file aware). Fires `org-after-todo-statistics-hook` for integrations. Falls back to org's built-in `[/]` statistics cookies when the custom position is nil (obsolete path :911).

### IMPL-WF-50 — Stuck-project detection
- **Capability**: User gets an `org-stuck-projects`-compatible configuration for agenda stuck-project views, plus predicates classifying projects as stuck (active tasks but nothing NEXT/WAIT) or active.
- **Entry points**: `org-gtd-stuck-projects` (autoloaded) — org-gtd-projects.el:376; `org-gtd-projects--is-stuck-p` — :867; `org-gtd-projects--has-active-tasks-p` — :846
- **Kind**: command (config helper) / data-model

### IMPL-WF-51 — Project iteration API (map over projects/tasks, last clock-out)
- **Capability**: Users/extensions can map a function over all projects, over all tasks of one project (graph order), and query a project's most recent clock-out time.
- **Entry points**: `org-gtd-projects-map` — org-gtd-projects.el:1114; `org-gtd-project-map-tasks` — :1093; `org-gtd-project-last-clock-out-time` — :1135 (all autoloaded)
- **Kind**: data-model (public API)

### IMPL-WF-52 — Ad-hoc dependency editing between arbitrary tasks
- **Capability**: On any heading, user can interactively add multiple blockers, add one or many dependents, remove selected blockers, or clear all relationships — with multi-select completion (task names annotated with project and file), automatic same-project linking, cycle checks, and state recalculation; plus a relationships summary display ("Blocked by: ... / Blocks: ...").
- **Entry points**: `org-gtd-task-add-blockers` — org-gtd-task-management.el:45; `org-gtd-task-remove-blockers` — :85; `org-gtd-task-add-dependent` — :121; `org-gtd-task-add-dependents` — :153; `org-gtd-task-clear-relationships` — :193; `org-gtd-task-show-relationships` — :601 (all autoloaded commands)
- **Kind**: command
- **Notes**: Lazy ID creation for candidate tasks; multi-select loop terminates on empty selection.

### IMPL-WF-53 — Dependency integrity audit
- **Capability**: User can scan all agenda files for broken dependency references (IDs that no longer exist) and orphaned tasks (top-level tasks carrying dependency properties), receiving structured results and remediation guidance.
- **Entry point**: `org-gtd-validate-project-dependencies` (autoloaded command) — org-gtd-task-management.el:649
- **Kind**: command

*(also in this area: `org-gtd-remove-task-from-project` — org-gtd-task-management.el:788 — an alternative removal command that optionally reconnects children to parents and converts the task to a standalone next action when its last project is removed; counted within IMPL-WF-45's capability family but a distinct entry point worth noting.)*

---

## 5. Lifecycle: tickler / someday / reactivate / archive / trash

### IMPL-WF-54 — Tickler / someday an entire project (with safety checks)
- **Capability**: User can put a whole project to sleep — tickler (with review date) or someday — and every task's prior ORG_GTD/TODO state is saved; org-gtd warns if external tasks depend on the project's tasks, and skips shared tasks that still belong to other active projects.
- **Entry points**: `org-gtd-project-incubate` (autoloaded command) — org-gtd-projects.el:1173; `org-gtd-project-someday` (autoloaded command) — :1228; context wrappers `org-gtd-project-incubate-from-context` / `org-gtd-project-someday-from-context` — :343, :355; type-level routing org-gtd-tickler.el:80, org-gtd-someday.el:88
- **Kind**: command + automatic-behavior

### IMPL-WF-55 — Reactivate a someday/tickler item or project
- **Capability**: User restores a slept item to its previous GTD state (saved `PREVIOUS_*` properties), confirming/updating each type-specific property (dates, delegate, ...); items with no saved state are routed back into clarification; projects restore all tasks and recalculate NEXT/TODO, opening the project graph view when interactive.
- **Entry points**: `org-gtd-reactivate` (autoloaded command, works from agenda too) — org-gtd-reactivate.el:103; engine `org-gtd-save-state`/`org-gtd-restore-state` — :37, :59; `org-gtd-project-reactivate` (autoloaded) — org-gtd-projects.el:1279
- **Kind**: command + data-model (PREVIOUS_ORG_GTD / PREVIOUS_TODO / PREVIOUS_<prop> snapshot scheme)

### IMPL-WF-56 — Guided someday/maybe review sessions
- **Capability**: User reviews someday items one at a time in a read-only buffer with single-key actions — `d` defer (stamps a "Reviewed [timestamp]" logbook entry), `c` clarify/reactivate, `q` quit — with a progress indicator (n/N) and end-of-session statistics (reviewed/clarified counts); can review a specific named list, "Unassigned" items, or everything.
- **Entry points**: `org-gtd-reflect-someday-review` (autoloaded command) — org-gtd-someday-review.el:185; mode — :158; defer/clarify/quit — :208, :227, :244
- **Kind**: command
- **Notes**: Evil-mode integration forces emacs state in the review buffer (:178). Items located by scanning agenda files for `ORG_GTD=Someday`.

### IMPL-WF-57 — Archive item at point / archive everything completed
- **Capability**: User can archive the subtree at point to the GTD archive, or run one sweep that archives all done standalone actions, delegated, quick, calendar, and tickler items plus every project whose tasks are all done.
- **Entry points**: `org-gtd-archive-item-at-point` (command) — org-gtd-archive.el:115; `org-gtd-archive-completed-items` (autoloaded command) — :83
- **Kind**: command
- **Notes**: Refreshes org-id locations first so cross-file graphs resolve; skips category headings containing other projects (:283).

### IMPL-WF-58 — Multi-project-aware project archiving
- **Capability**: When a finished project is archived, each shared task only loses that project's ID; tasks still owned by other projects are moved out of the subtree (refiled to Actions) instead of being archived with it — nothing shared is lost.
- **Entry point**: `org-gtd--archive-project-with-tasks` — org-gtd-archive.el:210; per-task logic `org-gtd--archive-task-if-no-projects` — :155
- **Kind**: automatic-behavior

### IMPL-WF-59 — Configurable archive location (yearly datetree default)
- **Capability**: By default items archive to `gtd_archive_<year>` files with a datetree in `org-gtd-directory`; user can supply their own location function or set nil to respect plain `org-archive-location`.
- **Entry points**: `org-gtd-archive-location` (defcustom) — org-gtd-archive.el:45; default fn `org-gtd-archive-location-func` — :126
- **Kind**: customization

### IMPL-WF-60 — Trash disposition preserves an audit trail
- **Capability**: "Trashing" never deletes: items are marked with the canceled keyword and archived, so they remain searchable in the archive.
- **Entry points**: `cancel-and-archive` disposition — org-gtd-organize-core.el:238; trash type — org-gtd-types.el:110
- **Kind**: automatic-behavior

### IMPL-WF-61 — Save/restore state machinery exposed for items moving in/out of incubation
- **Capability**: Any organize into someday/tickler first snapshots the item's full prior identity (ORG_GTD, TODO, type-specific properties) so reactivation is lossless; no-ops when already someday/tickler.
- **Entry points**: `org-gtd-save-state` — org-gtd-reactivate.el:37 (called from org-gtd-someday.el:79, org-gtd-tickler.el:76); project variant org-gtd-projects.el:1049
- **Kind**: data-model / automatic-behavior

---

## 6. Infrastructure (refile, IDs, programmatic API)

### IMPL-WF-62 — Property-driven refile targeting with auto-created targets
- **Capability**: Organized items refile to headings tagged with an `ORG_GTD_REFILE` property matching their type (multi-value allowed, so one heading can accept several types); if no target exists, org-gtd silently creates the canonical heading in the default GTD file. Users can add their own refile target headings anywhere.
- **Entry points**: `org-gtd-refile--do` — org-gtd-refile.el:170; verify fn — :119; auto-create — :220; template builder — org-gtd-organize-core.el:192
- **Kind**: automatic-behavior / data-model
- **Notes**: WIP temp files and the inbox are never valid targets; files outside `org-gtd-directory` are accepted as custom targets. Point preservation on target creation (issue #288).

### IMPL-WF-63 — Per-type refile prompting control
- **Capability**: User controls, per GTD type, whether refiling auto-files to the first matching target or prompts with completion over merged user + GTD targets; with a global fallback default and a deprecated global override (`org-gtd-refile-to-any-target`, with one-time deprecation warning) plus one-shot migration of the legacy `org-gtd-refile-prompt-for-types` list.
- **Entry points**: `:prompt-to-refile` in type registry (set via `org-gtd-customize-type`); `org-gtd-refile-prompt-default` (defcustom) — org-gtd-refile.el:89; precedence logic — :147; legacy migration — :85
- **Kind**: customization

### IMPL-WF-64 — Human-readable org IDs
- **Capability**: Headings get IDs slugged from their text (sanitized, de-cookied, link/priority/timestamp-stripped, umlaut-transliterated, truncated to 50 chars at word boundaries) plus an ISO timestamp — IDs that users can read in property drawers and links.
- **Entry points**: `org-gtd-id-get-create` — org-gtd-id.el:38; generator — :63
- **Kind**: automatic-behavior

### IMPL-WF-65 — Programmatic item creation API
- **Capability**: External code (scripts, email integrations, etc.) can create a fully-organized GTD item of any type in one call, passing config like `(:when . "<2026-05-01>")` or `(:who . "Alice")` to skip interactive prompts; runs the full pipeline including refile.
- **Entry point**: `org-gtd-create-item` (autoloaded) — org-gtd-create.el:33
- **Kind**: data-model (API)
- **Notes**: Per-type wrappers (`org-gtd-delegate-create`, `org-gtd-calendar-create`, `org-gtd-habit-create`, `org-gtd-tickler-create`, `org-gtd-someday-create`, `org-gtd-next-action-create`) are kept as obsolete aliases (4.1.0). Category commands also accept optional args for non-interactive use (e.g. `org-gtd-delegate "Alice" "2026-05-01"`).

### IMPL-WF-66 — Domain accessor / predicate API over task properties
- **Capability**: A stable functional surface for reading/writing GTD task data by ID — state, dependencies, blockers, project membership, category — plus business-rule predicates (is-active, is-done, blocks-others, should-reset, deps-ready) and a `org-gtd-task-deps` value object; usable by user extensions and views.
- **Entry points**: org-gtd-accessors.el:49-171 (readers/writers/lookup); org-gtd-value-objects.el:54-145 (predicates, struct)
- **Kind**: data-model (API)
- **Notes**: Task lookup checks the current buffer before `org-id-find` for speed/testability (org-gtd-accessors.el:140).

### IMPL-WF-67 — Edna extension vocabulary
- **Capability**: org-gtd registers named org-edna finders/actions (`org-gtd-next-project-action`, `org-gtd-update-project-task!`, `org-gtd-update-project-after-task-done!`) that users can reference in their own TRIGGER/BLOCKER properties.
- **Entry points**: org-gtd-projects.el:1397-1426
- **Kind**: hook / data-model
