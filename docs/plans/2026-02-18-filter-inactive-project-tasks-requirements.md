# Filter Inactive Project Tasks — Requirements

## Problem

GitHub issue #283: Tasks from cancelled projects appear in `org-gtd-reflect-stuck-single-action-items` and other views. When a project is cancelled, its child tasks may still have `ORG_GTD=Actions` and a non-done TODO state (e.g., TODO). The view DSL skip functions only check the task's own state, not whether its parent project is still active.

## Principle

A task whose projects are ALL inactive (done or cancelled) is itself effectively inactive. It should not appear in any view that shows active tasks. This is a DSL-level concern — fix it in the skip function builder and all views benefit automatically.

## Key Constraint

Multi-project tasks (multiple entries in `ORG_GTD_PROJECT_IDS`) are only hidden when ALL their projects are inactive. If even one project is still active, the task remains visible.

## Existing Infrastructure

- `org-gtd-project--active-p`: Returns t when project has `ORG_GTD=Projects` and is not done/cancelled. Reuse this.
- `org-gtd-view-language.el`: Contains the skip function builders for all view types.
- The new cancel-hook (implemented in this branch) prevents future occurrences by cascading CNCL to children. This fix handles pre-existing data and any edge cases the hook doesn't cover.

---

## Stories

### Story 1: DSL skips tasks from fully inactive projects

**As a** GTD practitioner reviewing my system,
**I want** tasks belonging to cancelled or completed projects to be automatically excluded from active views,
**So that** I only see tasks that are genuinely actionable.

#### Acceptance Criteria

```gherkin
Given a project with TODO state CNCL
And the project has child tasks with ORG_GTD=Actions and TODO state TODO
When I open any active-task view (stuck single actions, engage, etc.)
Then those tasks do not appear
```

```gherkin
Given a project with TODO state DONE
And the project has child tasks with ORG_GTD=Actions and TODO state NEXT
When I open any active-task view
Then those tasks do not appear
```

```gherkin
Given a task belonging to two projects via ORG_GTD_PROJECT_IDS
And one project is CNCL and the other is active
When I open any active-task view
Then the task still appears (one project is still active)
```

```gherkin
Given a task belonging to two projects via ORG_GTD_PROJECT_IDS
And both projects are CNCL
When I open any active-task view
Then the task does not appear
```

#### Edge Cases

```gherkin
Given a task with ORG_GTD=Actions but no ORG_GTD_PROJECT_IDS
When I open any active-task view
Then the task appears normally (it's a standalone single action, not a project task)
```

```gherkin
Given a task with ORG_GTD_PROJECT_IDS pointing to a project ID that no longer exists
When I open any active-task view
Then the task still appears (fail open — don't hide tasks we can't verify)
```

### Story 2: Stuck project review excludes cancelled/done projects

**As a** GTD practitioner doing a weekly review,
**I want** cancelled and completed projects excluded from the stuck projects view,
**So that** I only focus on projects that actually need attention.

#### Acceptance Criteria

```gherkin
Given a project with TODO state CNCL
When I run org-gtd-reflect-stuck-projects
Then the cancelled project does not appear
```

```gherkin
Given a project with TODO state DONE
When I run org-gtd-reflect-stuck-projects
Then the completed project does not appear
```

```gherkin
Given a project with TODO state CNCL and child tasks still in TODO state
When I run org-gtd-reflect-stuck-projects
Then the project does not appear (it's cancelled, not stuck)
```

#### Edge Cases

```gherkin
Given a project with no TODO state at all (just a heading)
When I run org-gtd-reflect-stuck-projects
Then the project does not appear (it's not an active project)
```
