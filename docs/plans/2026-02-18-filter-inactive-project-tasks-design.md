# Filter Inactive Project Tasks — Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task.

**Goal:** Implicitly exclude tasks from fully inactive (done/cancelled) projects in all active-task views, at the DSL skip-function level.

**Architecture:** Add a new predicate `org-gtd-pred--task-has-active-project` to `org-gtd-skip.el` and wire it into the skip function builders in `org-gtd-view-language.el`. All four builder paths get the check: stuck-single-action, stuck-type (calendar/delegated/tickler/habit), project-type (stuck/active project headings), and the generic predicate-composition path. No DSL keys or view spec changes needed.

**Approach chosen:** Implicit skip (Approach A from design interview). Every view automatically excludes orphaned project tasks. No user configuration.

---

## Components

### `org-gtd-skip.el` — New Predicate

Add `org-gtd-pred--task-has-active-project`:
- Returns a closure (follows existing predicate pattern)
- Closure checks `ORG_GTD_PROJECT_IDS` on current heading
- Returns `t` (include) if:
  - No `ORG_GTD_PROJECT_IDS` (standalone action)
  - At least one project ID resolves to an active project (`org-gtd-project--active-p`)
  - Any project ID can't be found via `org-id-find` (fail open)
- Returns `nil` (skip) only when ALL project IDs resolve to inactive projects

### `org-gtd-view-language.el` — Integration Points

Four skip function builders get the check:

1. **`org-gtd-view-lang--build-skip-function-for-stuck-single-action`** — Check before stuck logic. Tasks from inactive projects are skipped entirely.

2. **`org-gtd-view-lang--build-skip-function-for-stuck-type`** — Same treatment for stuck-calendar, stuck-delegated, stuck-tickler, stuck-habit.

3. **`org-gtd-view-lang--build-skip-function` generic path** (predicate composition branch) — Add to the composed predicate list. Covers engage views and custom view specs.

4. **`org-gtd-view-lang--build-skip-function-for-project-type`** — Ensure done/cancelled project headings are skipped from stuck-project and active-project views.

### No Changes to `org-gtd-projects.el`

Reuse `org-gtd-project--active-p` as-is.

---

## Data Flow

```
Heading at point
    │
    ▼
Has ORG_GTD_PROJECT_IDS?
    │           │
    no          yes
    │           │
    ▼           ▼
 Include     For each project ID:
 (standalone    call org-gtd-project--active-p
  action)       │
                ▼
             Any active?  ──yes──►  Include
                │                   (task is in a live project)
                no
                │
                ▼
             Any ID not found?  ──yes──►  Include
                │                         (fail open)
                no
                │
                ▼
             SKIP (all projects inactive)
```

**Fail-open rule:** If `org-id-find` returns nil for a project ID (broken reference), treat it as "can't confirm inactive" and include the task.

---

## API Surface

**New:**
- `org-gtd-pred--task-has-active-project` — No-arg function returning a closure. Follows the pattern of `org-gtd-pred--not-done`, `org-gtd-pred--project-has-active-tasks`, etc.

**Reused:**
- `org-gtd-project--active-p` (org-gtd-projects.el) — checks ORG_GTD=Projects AND not done

---

## Error Handling

| Scenario | Handling |
|----------|----------|
| `ORG_GTD_PROJECT_IDS` empty/missing | Include (standalone action) |
| Project ID → `org-id-find` returns nil | Include (fail open) |
| Project heading has no `ORG_GTD` property | `org-gtd-project--active-p` → nil → inactive |
| Project heading has no TODO state | `org-entry-is-done-p` → nil → active (not done) |

---

## Testing Strategy

### Unit Tests (predicate)

- No `ORG_GTD_PROJECT_IDS` → include
- One active project → include
- One CNCL project → skip
- One DONE project → skip
- Multi-project: one CNCL + one active → include
- Multi-project: both CNCL → skip
- Unresolvable project ID → include (fail open)

### Integration Tests (issue #283 reproduction)

- Cancelled project with TODO child tasks → stuck single action review excludes them
- Active project with TODO child tasks → stuck single action review includes them
- Task in two projects (one CNCL, one active) → engage view includes it
- Task in two projects (both CNCL) → engage view excludes it
