# Project Someday/Maybe Support

## Problem

`org-gtd-someday` doesn't handle projects. Unlike `org-gtd-tickler` which has a
smart dispatcher that detects project headings/tasks and cascades state saves to
all child tasks, `org-gtd-someday` treats everything as a single item. This means:

- Calling it on a project heading only modifies the heading, not child tasks
- Calling it on a project task treats it as a standalone item
- The graph view has no someday option

## Design Decisions

- **Separate functions** (Option B): `org-gtd-project-someday` is a standalone
  function parallel to `org-gtd-project-incubate`, not a generalization
- **Generic save-state**: `org-gtd-project--save-state` becomes generic (saves
  PREVIOUS_* and clears TODO, does NOT set ORG_GTD type). Each flow sets the
  type itself after calling save-state
- **Graph view**: `S` keybinding for "Someday/Maybe" alongside existing `I`
  (Incubate/tickler) and `C` (Cancel)
- **Someday lists**: The `org-gtd-someday-lists` prompt applies to projects too
  (set on project heading only, not child tasks)
- **Reactivation**: No changes needed — `org-gtd-project-reactivate` and
  `org-gtd-reactivate` already handle both types via PREVIOUS_* properties

## Changes

### `org-gtd-projects.el`

1. Refactor `org-gtd-project--save-state`: save PREVIOUS_ORG_GTD, PREVIOUS_TODO,
   clear TODO keyword, skip multi-project tasks. Do NOT set ORG_GTD to any value.

2. Update `org-gtd-project-incubate`: after calling generic save-state on heading
   and each task, set ORG_GTD to "Tickler" on each. Set ORG_GTD_TIMESTAMP on
   heading.

3. Add `org-gtd-project-someday(project-marker)`: check external deps, call
   generic save-state on heading + tasks, set ORG_GTD to "Someday" on each.
   Prompt for someday-list on heading if configured. No timestamp.

4. Add `org-gtd-project-someday-from-context()`: get marker from context, call
   `org-gtd-someday` on it (mirrors `org-gtd-project-incubate-from-context`).

### `org-gtd-someday.el`

5. Smart dispatcher in `org-gtd-someday`: detect project heading (ORG_GTD =
   "Projects") → `org-gtd-project-someday`. Detect project task (has
   ORG_GTD_PROJECT_IDS) → find project → `org-gtd-project-someday`. Otherwise →
   existing single-item logic.

### `org-gtd-graph-transient.el`

6. Add `S` "Someday/Maybe" keybinding calling `org-gtd-graph-someday-project`.

7. Add `org-gtd-graph-someday-project`: call
   `org-gtd-project-someday-from-context`, clean up windows, quit.

### `org-gtd-graph-mode.el`

8. Add `S` keybinding in mode map.
