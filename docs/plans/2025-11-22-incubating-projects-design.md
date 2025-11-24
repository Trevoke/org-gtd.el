# Incubating Projects Design

**Date:** 2025-11-22
**Status:** Design Complete - Ready for Implementation

## Overview

Add the ability to pause/hibernate entire projects in org-gtd. An incubated project maintains its full structure (tasks, dependencies) but becomes dormant - invisible in engage views and action lists. Users can reactivate projects on a scheduled review date, restoring the original state with automatic dependency recalculation.

## Problem

Currently, org-gtd supports incubating single items (someday/maybe), but there's no way to pause an entire project without deleting it or manually managing task states. Users need to temporarily set aside projects they're not ready to pursue while preserving the planning work already done.

## Goals

- Pause entire projects (heading + all tasks) with a single command
- Preserve project structure completely (tasks, dependencies, states)
- Make incubated projects invisible in engage/action views
- Allow scheduled reactivation with automatic state restoration
- Handle multi-project tasks correctly (don't break shared tasks)
- Maintain consistency with existing GTD patterns (delegated items, calendar items)

## Design

### Core Mechanism

**State Representation**

When a project is incubated:
- Project heading: `ORG_GTD: Incubated` (previous value `Projects` saved to `PREVIOUS_ORG_GTD`)
- All project tasks: `ORG_GTD: Incubated` (previous values like `Actions`, `Calendar`, `Delegated` saved to `PREVIOUS_ORG_GTD`)
- All task TODO keywords: cleared and saved to `PREVIOUS_TODO` (e.g., `NEXT` → `PREVIOUS_TODO: NEXT`)
- Review date: stored in `ORG_GTD_TIMESTAMP: <YYYY-MM-DD>` property on project heading
- All dependency relationships (`ORG_GTD_BLOCKS`, `ORG_GTD_DEPENDS_ON`): remain intact but dormant

**Visibility Logic**

Incubated projects and tasks are invisible in:
- `org-gtd-engage` - tasks have no TODO keyword, so `(todo . "NEXT")` filter excludes them naturally
- Project lists - views filter for `ORG_GTD: Projects`, not `Incubated`
- Stuck project detection - no TODO keywords to detect

They remain visible in:
- Agenda view on review date (via `ORG_GTD_TIMESTAMP` property, like delegated items)
- Area-of-focus reviews (separate section showing incubated projects with review dates)
- Direct navigation (still physically present in files)

**Multi-Project Task Handling**

Tasks belonging to multiple projects (identified by multiple IDs in `ORG_GTD_PROJECT_IDS`):
- Are NOT incubated - they remain active with their TODO keywords
- Skip them during project incubation traversal
- User is notified: "Task 'X' belongs to multiple projects - skipping"

### User Operations

**Incubating a Project**

Command: `org-gtd-incubate` (smart command, context-aware)

From project heading:
1. Detects it's on a project heading (`ORG_GTD: Projects`)
2. Prompts for review date (YYYY-MM-DD format)
3. Validates: checks for edge cases (see below)
4. Saves all states for project + all tasks:
   - `ORG_GTD` → `PREVIOUS_ORG_GTD`
   - TODO keyword → `PREVIOUS_TODO` property
   - Clear TODO keyword
5. Sets `ORG_GTD_TIMESTAMP` on project heading
6. Marks project heading and all tasks as `ORG_GTD: Incubated`

From project task (in agenda or directly):
1. Detects task has `ORG_GTD_PROJECT_IDS` property
2. If task belongs to 1 project: incubate that project
3. If task belongs to multiple projects: show transient menu with checkboxes (like modify-blockers UI)
   - User selects which project(s) to incubate
   - Can select multiple, each gets its own review date prompt
4. For each selected project: same incubation flow as above

From project graph view:
- Transient menu option: `i` - "Incubate this project"
- Calls `org-gtd-incubate` (same smart command)

From single item (existing behavior):
- If on single action/calendar/delegated item: incubate just that item
- Uses existing `org-gtd-incubate.el` logic

**Reactivating a Project**

Command: `org-gtd-reactivate` (smart command, context-aware)

From project heading or agenda:
1. Detects item has `ORG_GTD: Incubated`
2. If it's a project heading:
   - Restores `PREVIOUS_ORG_GTD` → `ORG_GTD` for project + all tasks
   - Restores `PREVIOUS_TODO` → TODO keyword for all tasks
   - Recalculates NEXT/TODO states based on current dependencies (using `org-gtd-projects-fix-todo-keywords`)
   - Removes `ORG_GTD_TIMESTAMP` property
   - Opens project graph view for user review/adjustments
3. If it's a single item: reactivate just that item

### View Language Integration

**New Category Filters**

Add support for these category values in `org-gtd-view-language.el`:
- `(category . incubated-projects)` → shows projects with `ORG_GTD: Incubated`
- `(category . incubated)` → shows any item with `ORG_GTD: Incubated`

**No Changes to Existing Filters**

Existing category filters naturally exclude incubated items:
- `(category . projects)` checks for `ORG_GTD: Projects` (not `Incubated`)
- `(category . actions)` checks for active actions (incubated tasks have no TODO keyword)
- `(category . stuck-projects)` finds projects with TODO keywords (incubated projects have none)

**View Spec Updates**

Update these view specs in `org-gtd-agenda.el` and `org-gtd-review.el`:

`org-gtd-review-area-of-focus-view-specs`:
- Add separate section for incubated projects
- Format: "Incubated: Project Name (review: YYYY-MM-DD)"
- Only show project headings, not task details

No changes needed for:
- `org-gtd-engage-view-spec` - already filters by `(todo . "NEXT")`, excludes cleared keywords
- `org-gtd-show-all-next-view-spec` - same reason
- `org-gtd-review-stuck-projects` - looks for TODO keywords, won't find incubated tasks

### Edge Cases & Validation

**Multi-Project Tasks**

During project incubation:
- Check each task's `ORG_GTD_PROJECT_IDS`
- If task belongs to multiple projects: skip it, notify user
- Task remains active with TODO keyword intact

**External Dependencies**

Before incubating, check for external tasks that depend on this project's tasks:
1. For each task in project: find all tasks with `ORG_GTD_DEPENDS_ON` pointing to it
2. Filter to external tasks (not in current project's task list)
3. If external dependencies exist:
   - Show warning: "External tasks depend on this project:"
   - List each affected task
   - Prompt: "Continue incubating? (y/n)"
4. If user cancels: abort incubation
5. If user continues: incubate anyway (external tasks will have deps pointing to cleared keywords)

**Perma-Blocked Children**

Check for tasks that would become permanently blocked:
1. For each task being incubated: get its children (tasks it blocks via `ORG_GTD_BLOCKS`)
2. For each child: check if ALL its dependencies (`ORG_GTD_DEPENDS_ON`) are being incubated
3. If a child would have zero active dependencies:
   - Show warning: "Task 'X' would become permanently blocked"
   - List such tasks
   - Suggest: "Consider removing the blocking dependency first"
   - Prompt: "Continue incubating? (y/n)"

**Manual Changes While Incubated**

No special handling:
- Users can manually edit incubated projects (add tasks, change dependencies, etc.)
- On reactivation: recalculation handles any changes
- User's responsibility to keep things sane

## Implementation Architecture

### Components to Modify

**`org-gtd-projects.el`**

New functions:
- `org-gtd-project-incubate` - main incubation logic for projects
- `org-gtd-project-reactivate` - main reactivation logic for projects
- `org-gtd-project--save-state` - saves ORG_GTD and TODO to PREVIOUS_* properties
- `org-gtd-project--restore-state` - restores from PREVIOUS_* properties
- `org-gtd-project--check-external-dependencies` - finds external tasks linking to this project
- `org-gtd-project--check-perma-blocked-children` - finds children that would be permanently blocked
- `org-gtd-project--get-all-tasks` - returns all tasks in a project (via graph traversal)

Modified functions:
- `org-gtd-projects--collect-tasks-by-graph` - already handles tasks with no TODO keywords correctly (will naturally skip incubated tasks)

**`org-gtd-incubate.el`**

Modified functions:
- `org-gtd-incubate` - becomes smart dispatcher:
  - Detect context (project heading, project task, single item)
  - Route to appropriate logic (`org-gtd-project-incubate` vs existing single-item logic)

New functions:
- `org-gtd-reactivate` - smart dispatcher for reactivation:
  - Detect context (project vs single item)
  - Route to appropriate logic

**`org-gtd-graph-transient.el`**

Add transient menu option:
- Key: `i`
- Label: "Incubate this project"
- Action: calls `org-gtd-incubate`

For multi-project task incubation:
- Create transient menu with project checkboxes (similar to `org-gtd-graph-modify-blockers-menu`)
- User selects which projects to incubate
- Each selected project gets incubated with its own review date

**`org-gtd-view-language.el`**

Add category filter handlers:
- `incubated-projects` - filters for `ORG_GTD: Incubated` at heading level
- `incubated` - filters for any item with `ORG_GTD: Incubated`

No changes to existing filter handlers (they naturally exclude incubated via ORG_GTD or TODO checks).

**`org-gtd-review.el`**

Update view spec:
- `org-gtd-review--area-of-focus-view-specs` - add section for incubated projects

**`org-gtd-agenda.el`**

No changes needed:
- `org-gtd-engage-view-spec` naturally excludes cleared TODO keywords
- `org-gtd-show-all-next-view-spec` same

### Data Flow

**Incubation Flow**

```
User: M-x org-gtd-incubate (on project heading)
  ↓
org-gtd-incubate (dispatcher)
  ↓ detects: ORG_GTD: Projects
  ↓
org-gtd-project-incubate
  ↓ prompt for review date
  ↓ validate: check external deps, perma-blocked children
  ↓ user confirms
  ↓
org-gtd-project--get-all-tasks (graph traversal)
  ↓ for each task (skip multi-project tasks):
  ↓
org-gtd-project--save-state
  ↓ ORG_GTD → PREVIOUS_ORG_GTD
  ↓ TODO → PREVIOUS_TODO
  ↓ clear TODO keyword
  ↓ set ORG_GTD: Incubated
  ↓
Set ORG_GTD_TIMESTAMP on project heading
Set ORG_GTD: Incubated on project heading
```

**Reactivation Flow**

```
User: M-x org-gtd-reactivate (on incubated project)
  ↓
org-gtd-reactivate (dispatcher)
  ↓ detects: ORG_GTD: Incubated, is project heading
  ↓
org-gtd-project-reactivate
  ↓
org-gtd-project--get-all-tasks (graph traversal)
  ↓ for each task:
  ↓
org-gtd-project--restore-state
  ↓ PREVIOUS_ORG_GTD → ORG_GTD
  ↓ PREVIOUS_TODO → TODO keyword
  ↓
Remove ORG_GTD_TIMESTAMP from project heading
  ↓
org-gtd-projects-fix-todo-keywords (recalculate NEXT/TODO based on dependencies)
  ↓
org-gtd-show-project-graph (open graph view for user review)
```

## Testing Strategy

**Unit Tests**

Test in `test/incubate-test.el`:
- Incubating single items (existing behavior preserved)
- Incubating projects (new behavior)
- State preservation (PREVIOUS_ORG_GTD, PREVIOUS_TODO)
- TODO keyword clearing/restoration
- Review date handling (ORG_GTD_TIMESTAMP)

**Integration Tests**

Test in `test/end-to-end-test.el`:
- Full incubation → reactivation cycle
- Multi-project task handling (task stays active when only some projects incubated)
- External dependency warnings
- Perma-blocked children warnings
- Smart command dispatch (same command works from different contexts)

**View Tests**

Test in `test/reviews-test.el` and agenda tests:
- Incubated projects appear in area-of-focus reviews
- Incubated tasks don't appear in engage view
- Agenda shows incubated projects on review date

## Open Questions

None - design is complete.

## Future Enhancements (Not in Scope)

- Bulk incubation (incubate multiple projects at once from a list)
- Incubation templates (common review intervals like "review in 1 week", "review in 1 month")
- Statistics tracking (how long projects stay incubated, reactivation rate)
- Automatic incubation suggestions (projects with no activity for X days)
