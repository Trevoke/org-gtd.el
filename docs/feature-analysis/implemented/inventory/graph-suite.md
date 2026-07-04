# Implemented Feature Inventory: Project-Graph Visualization Suite

**Scope:** org-gtd-graph-data.el, org-gtd-graph-debug.el, org-gtd-graph-filter.el,
org-gtd-graph-mode.el, org-gtd-graph-navigation.el, org-gtd-graph-transient.el,
org-gtd-graph-ui.el, org-gtd-graph-view.el, org-gtd-dag-draw.el, org-gtd-svg-render.el

**Method:** Bottom-up reading of the source files only (no spec/status documents consulted).

**Feature count: 27** (23 fully wired user-facing features, 2 latent/unbound feature sets,
2 developer-facing utilities)

## Overview

The project-graph visualization suite renders an org-gtd project's tasks and their
blocking relationships as an interactive DAG (directed acyclic graph), shown as an
SVG image (or optional ASCII art) in a read-only buffer with a magit-style details
side panel. The user invokes it with the autoloaded command `M-x org-gtd-show-project-graph`,
which detects context (project heading, task heading, agenda line, or a completing-read
prompt over all projects); it is also opened automatically when a project is reactivated
interactively (`org-gtd-project-reactivate`, org-gtd-projects.el:1312). It solves the
problem that a project's task dependency structure (stored as `ORG_GTD_DEPENDS_ON` /
`ORG_GTD_BLOCKS` / `ORG_GTD_FIRST_TASKS` properties scattered across headings) is
invisible and hard to edit in plain org text: the graph makes the structure visible
(tasks colored by TODO state, project node as the "finish line"), navigable by keyboard
and mouse, and directly editable — adding/removing tasks, rewiring blockers and
successors, changing TODO states, and project-level actions (incubate/someday/cancel) —
with all changes written back to the org files immediately and the view auto-refreshing
when the underlying file changes.

## Features

### Entry, Mode, and Display

**IMPL-GR-01 — Open project graph from anywhere**
- Capability: The user runs one command to open an interactive dependency graph for a project, with the project inferred from point.
- Entry point: `org-gtd-show-project-graph` (autoloaded), org-gtd-graph-mode.el:154
- Kind: Interactive command (suite entry point)
- Notable behaviors: On a project heading (ORG_GTD=Projects) opens that project; on a task heading reads `ORG_GTD_PROJECT_IDS` and prompts if the task is in multiple projects (org-gtd-graph-mode.el:238); in org-agenda uses the marker at point (org-gtd-graph-mode.el:172); otherwise prompts with completion over all projects in `org-agenda-files` (org-gtd-graph-mode.el:262). Also invoked programmatically by `org-gtd-project-reactivate` (org-gtd-projects.el:1312).

**IMPL-GR-02 — Dedicated graph major mode**
- Capability: The graph appears in a read-only special-mode buffer (`*Org GTD Graph: NAME*`) with single-key bindings for all operations.
- Entry point: `org-gtd-graph-view-mode`, org-gtd-graph-mode.el:127; keymap org-gtd-graph-mode.el:54; buffer creation `org-gtd-graph-view-create`, org-gtd-graph-view.el:175
- Kind: Major mode + keymap
- Notable behaviors: Derived from special-mode, undo disabled. Evil-mode integration forces emacs state (org-gtd-graph-mode.el:120) to avoid C-z/transient conflicts. KEYMAP BUG: `S` is bound twice — first to `org-gtd-graph-modify-successors` (line 78) then to `org-gtd-graph-someday-project` (line 98); the second binding wins, so `S` actually triggers someday/maybe and modify-successors is reachable only via the `?` menu, contradicting the on-screen legend ("B/S modify").

**IMPL-GR-03 — SVG DAG visualization with GTD-aware styling**
- Capability: The user sees tasks as nodes and dependencies as arrows, with node color encoding TODO state and the project heading drawn as the terminal "finish line" node.
- Entry points: rendering pipeline `org-gtd-graph-view-refresh`, org-gtd-graph-view.el:255; translation `org-gtd-dag-draw-translate`, org-gtd-dag-draw.el:68; `org-gtd-dag-draw-render(-with-bounds)`, org-gtd-dag-draw.el:107/118; styling helpers org-gtd-svg-render.el:37-125
- Kind: Visualization (rendering pipeline over external `dag-draw` library)
- Notable behaviors: State colors use the color-blind-accessible Okabe-Ito palette (org-gtd-svg-render.el:37): TODO orange, NEXT blue, WAIT yellow, DONE/CNCL grey; done/canceled nodes rendered at 0.5 opacity. Selected node highlighted with red (#e74c3c) thick (4px) border (org-gtd-dag-draw.el:52-64). Nodes carry SVG tooltips with title/state/priority/tags/scheduled/deadline (org-gtd-svg-render.el:97). Leaf tasks get synthetic edges into the project node (org-gtd-graph-data.el:165-188). A keybinding legend is inserted below the image (org-gtd-graph-view.el:307).

**IMPL-GR-04 — ASCII render mode and per-buffer toggle**
- Capability: The user can toggle the same graph between SVG and text-based ASCII art (e.g., for terminal Emacs), with `v`.
- Entry points: `org-gtd-graph-toggle-render-mode`, org-gtd-graph-view.el:340; defcustom `org-gtd-graph-render-mode` (default `svg`), org-gtd-graph-view.el:48; ASCII markers org-gtd-dag-draw.el:40
- Kind: Interactive command + customization variable
- Notable behaviors: ASCII mode prefixes node labels with state glyphs (✓ done, ✗ canceled, → next, ⏸ wait, ○ todo). Render mode is buffer-local; the defcustom only sets the initial mode.

**IMPL-GR-05 — Task details side panel**
- Capability: A dedicated side window shows full details of the selected task (state, dates, body text, and its blockers/blocked lists), updating on every selection.
- Entry points: `org-gtd-graph-ui-setup-windows`, org-gtd-graph-ui.el:61; `org-gtd-graph-ui-update-details`, org-gtd-graph-ui.el:126; defcustom `org-gtd-graph-ui-split-ratio` (default 0.7), org-gtd-graph-ui.el:42
- Kind: UI panel + customization variable
- Notable behaviors: Magit-style 70/30 split with a dedicated, atomic side window. Details show heading, TODO state, file, SCHEDULED/DEADLINE, FIRST_TASKS, and dependency lists partitioned into same-project vs "other projects" sections (org-gtd-graph-ui.el:151). Body text shown with property drawer and child headings stripped. RET in the panel jumps to the task in its org file.

### Selection and Navigation

**IMPL-GR-06 — Mouse click selection**
- Capability: The user clicks a node in the rendered image (SVG or ASCII) to select it and populate the details panel.
- Entry point: `org-gtd-graph-view-click-select` (bound to mouse-1), org-gtd-graph-view.el:141
- Kind: Interactive command (mouse)
- Notable behaviors: Per-node bounding boxes are extracted after layout (org-gtd-graph-view.el:151); SVG clicks are hit-tested through display-size→native-size scaling and viewbox offset correction (org-gtd-graph-view.el:110); ASCII clicks use character coordinates.

**IMPL-GR-07 — Keyboard navigation through the dependency structure**
- Capability: The user walks the graph from the keyboard: `n`/`p` down/up the dependency chain, `TAB`/`S-TAB` between siblings, `G` to jump to any node by name with completion.
- Entry points: `org-gtd-graph-nav-down-dependency` / `-up-dependency`, org-gtd-graph-navigation.el:228/240; `org-gtd-graph-nav-next-sibling` / `-previous-sibling`, org-gtd-graph-navigation.el:196/211; `org-gtd-graph-nav-goto`, org-gtd-graph-navigation.el:259; defcustom `org-gtd-graph-sibling-mode`, org-gtd-graph-navigation.el:38
- Kind: Interactive commands + customization variable
- Notable behaviors: Upward navigation prefers non-project parents over the project node. Sibling semantics are configurable: `any-same-blocker` (default), `all-same-blockers`, or `dag-level` (same DAG depth). Sibling order follows a cached breadth-first traversal from root tasks. `org-gtd-graph-nav-next`/`-previous` (org-gtd-graph-navigation.el:77/91) are additional interactive commands not bound to any key.

**IMPL-GR-08 — Auto-select first actionable task on open**
- Capability: When the graph opens, the first actionable task (NEXT/WAIT etc., not TODO/DONE/CNCL) is pre-selected so its details show immediately.
- Entry points: behavior in `org-gtd-graph-view-create`, org-gtd-graph-view.el:204; BFS search `org-gtd-graph-data-find-first-actionable`, org-gtd-graph-data.el:488
- Kind: Automatic behavior
- Notable behaviors: BFS from `ORG_GTD_FIRST_TASKS` roots; falls back to the project node, then to any node.

**IMPL-GR-09 — Auto-refresh on file change + manual refresh**
- Capability: The graph redraws itself when the underlying org file changes on disk; `g` forces a refresh.
- Entry points: file watch setup org-gtd-graph-view.el:216; debounced callback org-gtd-graph-view.el:238; `org-gtd-graph-view-refresh` (interactive), org-gtd-graph-view.el:255
- Kind: Automatic behavior + interactive command
- Notable behaviors: Uses filenotify with a 300ms debounce timer; watch is removed on buffer kill. Refresh re-extracts the graph from the org file each time, runs validation, and applies any active filter.

### Command Menu

**IMPL-GR-10 — Discoverable transient command menu (`?`)**
- Capability: Pressing `?` opens a magit-style menu listing every graph command, grouped (Add Tasks / Modify Relationships / Task Operations / Navigation / View / Export / Session), with the selected task shown in the header.
- Entry points: `org-gtd-graph-transient-main` (autoloaded), org-gtd-graph-transient.el:78; context header org-gtd-graph-transient.el:119; sticky toggle org-gtd-graph-transient.el:46
- Kind: Transient prefix (interactive)
- Notable behaviors: A "sticky mode" toggle (`=`) keeps the menu open after each command (org-gtd-graph-transient.el:43-73), implemented via a custom transient pre-command and a resume hook called by sub-menus.

### Editing the Graph (write-back to org files)

**IMPL-GR-11 — Add root task**
- Capability: The user adds a task with no dependencies as a new starting point of the project (`r`), either creating a new heading or linking an existing GTD task from any agenda file.
- Entry point: `org-gtd-graph-transient-add-root`, org-gtd-graph-transient.el:231
- Kind: Interactive command
- Notable behaviors: Candidate list excludes tasks already in the project (org-gtd-graph-transient.el:204). New tasks are created as `**` children of the project with TODO state, `ORG_GTD: Actions`, and `ORG_GTD_PROJECT_IDS`; the task ID is added to the project's `ORG_GTD_FIRST_TASKS`; TODO keywords are recomputed and the view refreshes. Test-oriented internal `org-gtd-graph--add-root-internal` at org-gtd-graph-transient.el:1305.

**IMPL-GR-12 — Add successor task (multi-select predecessors)**
- Capability: The user adds a task that comes after one or more existing tasks (`s`): step 1 select-or-create the task, step 2 toggle checkboxes in a transient for which tasks must complete first.
- Entry points: `org-gtd-graph-add-successor`, org-gtd-graph-transient.el:916; checkbox menu `org-gtd-graph-add-successor-menu`, org-gtd-graph-transient.el:1006; apply org-gtd-graph-transient.el:1059
- Kind: Interactive command + transient sub-menu
- Notable behaviors: Candidate completion prioritizes current-project tasks (org-gtd-graph-transient.el:176); the currently selected node is pre-checked as a predecessor. Checkbox state lives on a custom transient-prefix EIEIO class (`org-gtd-graph-transient-prefix`, org-gtd-graph-transient.el:568) rather than globals. New successors are deliberately NOT added to FIRST_TASKS (they have blockers). Requires graph view (`user-error` otherwise).

**IMPL-GR-13 — Add blocker task (multi-select blocked tasks)**
- Capability: The user adds a task that must come before one or more existing tasks (`b`), choosing the blocked tasks via checkboxes.
- Entry points: `org-gtd-graph-add-blocker`, org-gtd-graph-transient.el:1104; menu org-gtd-graph-transient.el:1133; apply org-gtd-graph-transient.el:1186
- Kind: Interactive command + transient sub-menu
- Notable behaviors: New blocker is added to the project's `ORG_GTD_FIRST_TASKS` (it has no blockers itself); dependencies created bidirectionally via `org-gtd-dependencies-create`; existing external tasks get the project linked into their `ORG_GTD_PROJECT_IDS`.

**IMPL-GR-14 — Bulk-edit a task's blockers**
- Capability: The user opens a checkbox menu (`B`) pre-populated with the selected task's current blockers, toggles entries, and applies — the dependency set is synced to exactly the chosen list.
- Entry points: `org-gtd-graph-modify-blockers`, org-gtd-graph-transient.el:573; menu org-gtd-graph-transient.el:593; core sync `org-gtd-graph--modify-blockers-internal`, org-gtd-graph-transient.el:532
- Kind: Interactive command + transient sub-menu
- Notable behaviors: Computes set differences to add/remove only what changed; if the resulting blocker list is empty the task is added to `ORG_GTD_FIRST_TASKS`, otherwise removed from it; TODO keywords recomputed.

**IMPL-GR-15 — Bulk-edit a task's successors**
- Capability: The user toggles which tasks the selected task blocks, in a pre-populated checkbox menu.
- Entry points: `org-gtd-graph-modify-successors`, org-gtd-graph-transient.el:700; menu org-gtd-graph-transient.el:720; core sync org-gtd-graph-transient.el:669
- Kind: Interactive command + transient sub-menu
- Notable behaviors: Removed successors that end up with no blockers are promoted into `ORG_GTD_FIRST_TASKS`. Nominally on `S`, but that key is shadowed by `org-gtd-graph-someday-project` (see IMPL-GR-02); in practice reachable only via the `?` menu.

**IMPL-GR-16 — Change TODO state from the graph (`t t`)**
- Capability: The user changes the selected task's TODO state without leaving the graph; the node recolors on refresh.
- Entry point: `org-gtd-graph-change-state`, org-gtd-graph-transient.el:862
- Kind: Interactive command
- Notable behaviors: Runs `org-todo` interactively at the task's location and saves the buffer; works via the shared `org-gtd-context-at-point` abstraction.

**IMPL-GR-17 — Remove task from project with intelligent rewiring (`t r`)**
- Capability: The user removes a task from the current project; its predecessors are automatically connected to its successors so the chain stays intact.
- Entry points: `org-gtd-graph-remove-task`, org-gtd-graph-transient.el:796; core `org-gtd-graph--remove-from-project`, org-gtd-graph-transient.el:332
- Kind: Interactive command
- Notable behaviors: yes-or-no-p confirmation; successors left with no predecessors are promoted to root tasks; cross-project dependencies are deleted only when the two tasks no longer share any project; selection moves to a nearby node after removal. A standalone variant `org-gtd-graph--keep-as-independent` (org-gtd-graph-transient.el:398) strips a task from all projects (no interactive binding).

**IMPL-GR-18 — Trash task (`t d`)**
- Capability: The user trashes a task entirely: it is removed from all projects, all its dependency links are cleaned up everywhere, and it is marked canceled.
- Entry points: `org-gtd-graph-trash-task`, org-gtd-graph-transient.el:836; core `org-gtd-graph--trash-task`, org-gtd-graph-transient.el:449
- Kind: Interactive command
- Notable behaviors: Rewires per project before global dependency cleanup; successors that become unblocked are added to FIRST_TASKS in all their projects; sets the canceled TODO keyword (`org-gtd-keywords--canceled`); confirmation prompt explains the consequences.

**IMPL-GR-19 — Jump to task in org file (`t e`, RET in details panel)**
- Capability: The user jumps from the graph to the selected task's heading in its source org file for free-form editing.
- Entry point: `org-gtd-graph-ui-jump-to-task`, org-gtd-graph-ui.el:265
- Kind: Interactive command
- Notable behaviors: Uses org-id lookup, reveals folded context (`org-fold-show-context`).

**IMPL-GR-20 — Show task relationships report (`t i`)**
- Capability: The user sees a plain-text report of what blocks the selected task and what it blocks.
- Entry points: `org-gtd-graph-view-show-relationships` — defined TWICE: org-gtd-graph-view.el:434 (reads ORG_GTD_DEPENDS_ON/ORG_GTD_BLOCKS properties, buffer `*Org GTD Relationships*`) and org-gtd-graph-transient.el:288 (reads the in-memory graph, buffer `*Task Relationships*`); the transient.el definition loads last and wins
- Kind: Interactive command
- Notable behaviors: Duplicate-definition smell; the effective version shows project-local graph edges only, while the shadowed one would have shown cross-project property data.

**IMPL-GR-21 — Project-level actions from the graph (`I`, `S`, `C`)**
- Capability: The user incubates, somedays, or cancels the whole project being viewed, then the graph closes.
- Entry points: `org-gtd-graph-incubate-project`, org-gtd-graph-transient.el:885; `org-gtd-graph-someday-project`, org-gtd-graph-transient.el:894; `org-gtd-graph-cancel-project`, org-gtd-graph-transient.el:903
- Kind: Interactive commands (delegating to org-gtd-projects context commands)
- Notable behaviors: Each cleans up the details window before quitting. `S` for someday is the binding that shadows modify-successors (IMPL-GR-02).

**IMPL-GR-22 — Export graph to SVG / Graphviz DOT / ASCII (`x s` / `x d` / `x a`)**
- Capability: The user saves the current graph (including selection highlight) to a file in SVG, DOT, or ASCII format.
- Entry points: `org-gtd-graph-export-svg`, org-gtd-graph-view.el:521; `org-gtd-graph-export-dot`, org-gtd-graph-view.el:534; `org-gtd-graph-export-ascii`, org-gtd-graph-view.el:547
- Kind: Interactive commands
- Notable behaviors: DOT export enables downstream Graphviz processing; all three error helpfully if no graph is loaded.

**IMPL-GR-23 — Quit commands (`q`, `Q`)**
- Capability: The user quits the graph (restoring window layout) or quits and kills the buffer.
- Entry points: `org-gtd-graph-quit`, org-gtd-graph-mode.el:44; `org-gtd-graph-quit-and-kill`, org-gtd-graph-transient.el:323
- Kind: Interactive commands
- Notable behaviors: Both tear down the details side window; kill-buffer hooks also remove the file watch (org-gtd-graph-view.el:191).

### Latent / Partially Wired Features

**IMPL-GR-24 — Graph filtering by TODO state, priority, tags, and schedule (NO interactive entry point)**
- Capability (latent): The graph can be narrowed to nodes matching TODO states, priorities, tags, and a scheduling window (overdue / today / within-a-week / unscheduled), AND-combined.
- Entry points: filter struct `org-gtd-graph-filter` org-gtd-graph-filter.el:43; `org-gtd-graph-filter-apply` org-gtd-graph-filter.el:60; `org-gtd-graph-filter-create-filtered-graph` org-gtd-graph-filter.el:182; honored on refresh via buffer-local `org-gtd-graph-view--filter` org-gtd-graph-view.el:64,263
- Kind: Infrastructure (complete back end, no UI)
- Notable behaviors: The refresh pipeline fully supports it, but no command, keybinding, or transient entry sets `org-gtd-graph-view--filter`; a user can only activate filtering by setting the variable from Lisp. Filtered graphs keep only edges between visible nodes and recompute roots.

**IMPL-GR-25 — Graph validation warnings on refresh**
- Capability: On every refresh the user is warned (via `message`) about structural problems: orphaned tasks, edges to non-existent tasks, and dependency cycles.
- Entry points: `org-gtd-graph-data-validate`, org-gtd-graph-data.el:377; invoked in refresh org-gtd-graph-view.el:261,269
- Kind: Automatic diagnostic
- Notable behaviors: Cycle detection via DFS with recursion stack, reporting the cycle path (org-gtd-graph-data.el:439).

**IMPL-GR-26 — Cross-project dependency commands (interactive but unbound)**
- Capability (latent): The user can (via M-x only) add a dependency or root blocker whose blocker may be ANY task in the agenda files — including tasks outside the project — or clear all relationships of the selected node.
- Entry points: `org-gtd-graph-view-add-dependency`, org-gtd-graph-view.el:352; `org-gtd-graph-view-add-blocker`, org-gtd-graph-view.el:390; `org-gtd-graph-view-clear-relationships`, org-gtd-graph-view.el:475
- Kind: Interactive commands, not in any keymap or transient
- Notable behaviors: External blockers get the project added to their `ORG_GTD_PROJECT_IDS` (TRIGGER set by `org-gtd-dependencies-create`). Note `org-gtd-graph-view-add-blocker` (view.el) is a distinct command from the bound `org-gtd-graph-add-blocker` (transient.el). Clear-relationships removes both directions with a count-aware confirmation.

### Developer Utilities

**IMPL-GR-27 — Graph debug printers**
- Capability: A developer can dump all edges of a graph, or the incoming edges of one node, to *Messages* for troubleshooting.
- Entry points: `org-gtd-graph-debug-print-edges`, org-gtd-graph-debug.el:37; `org-gtd-graph-debug-print-incoming-edges`, org-gtd-graph-debug.el:51
- Kind: Non-interactive functions (development/troubleshooting only, per file commentary)
- Notable behaviors: Resolves node titles for readability; "UNKNOWN" for dangling IDs.

## Cross-Cutting Notes

- All graph edits write through immediately (`save-buffer` after each mutation) and call `org-gtd-projects-fix-todo-keywords` so NEXT/TODO states stay consistent with the dependency structure.
- Graph construction (org-gtd-graph-data.el:95) is read-from-org each time: project metadata + tasks collected via `org-gtd-dependencies-collect-project-tasks`, edges built from both `ORG_GTD_BLOCKS` and `ORG_GTD_DEPENDS_ON` then deduplicated, edges to out-of-project tasks filtered out, roots taken from `ORG_GTD_FIRST_TASKS` with a no-incoming-edges fallback.
- Rendering depends on the external `dag-draw` package (layout + SVG/ASCII/DOT backends); org-gtd-dag-draw.el is the translation layer and org-gtd-svg-render.el supplies GTD-specific colors/opacity/tooltips.
- Customization variables in the suite: `org-gtd-graph-render-mode` (org-gtd-graph-view.el:48), `org-gtd-graph-ui-split-ratio` (org-gtd-graph-ui.el:42), `org-gtd-graph-sibling-mode` (org-gtd-graph-navigation.el:38).
- Known defects observed while reading: the `S` keybinding collision (org-gtd-graph-mode.el:78 vs 98) and the duplicate `org-gtd-graph-view-show-relationships` definition (org-gtd-graph-view.el:434 vs org-gtd-graph-transient.el:288).
