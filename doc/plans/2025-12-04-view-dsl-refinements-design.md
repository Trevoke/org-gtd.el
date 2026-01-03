# View DSL Refinements Design

Date: 2025-12-04

## Summary

Refine the GTD View Language DSL to remove redundancy, improve naming, and update documentation.

## Changes

### 1. Remove `category` filter

The `category` filter is redundant with `type` - both filter on the ORG_GTD property. The name is also confusing since org-mode's CATEGORY property is used for area-of-focus.

**Action:** Remove `org-gtd-view-lang--translate-category-filter` and the `category` case from the dispatcher.

### 2. Consolidate `done`/`not-done` with `closed`

Currently `done` is boolean and `closed` handles time-based filtering. Consolidate into extended `done` filter:

| Old | New |
|-----|-----|
| `(done . t)` | `(done . t)` - any done item |
| `(closed . recent)` | `(done . recent)` - last 7 days |
| `(closed . today)` | `(done . today)` - today |
| `(closed . past-day)` | `(done . past-day)` - last day |
| `(closed . past-week)` | `(done . past-week)` - last week |
| `(closed . past-month)` | `(done . past-month)` - last month |
| `(closed . past-year)` | `(done . past-year)` - last year |

**Action:**
- Extend `org-gtd-view-lang--translate-done-filter` to handle time specs
- Remove `org-gtd-view-lang--translate-closed-filter` and `closed` case from dispatcher

### 3. Rename `tickler-project` to `incubated-project`

The name `tickler-project` is too specific. Projects can be incubated in either Tickler (time-triggered) or Someday/Maybe (motivation-triggered).

**New semantics:** `(type . incubated-project)` matches projects where:
- Current ORG_GTD is Tickler OR Someday/Maybe
- PREVIOUS_ORG_GTD is Projects

**Action:** Replace `tickler-project` case in `org-gtd-view-lang--translate-type-filter` with `incubated-project`.

### 4. Documentation Updates

The documentation has stale references to `has-active-tasks` which was removed. Also missing complete View DSL reference.

**Action:**
- Remove all `has-active-tasks` references from doc/org-gtd.org
- Add complete View DSL reference section
- Link reference from custom view tutorial section

## Files to Modify

- `org-gtd-view-language.el` - DSL implementation
- `test/unit/gtd-view-language-test.el` - unit tests
- `test/integration/gtd-view-language-test.el` - integration tests
- `doc/org-gtd.org` - main documentation

## Testing Strategy

Use TDD:
1. Write failing tests for new `done` time specs
2. Write failing test for `incubated-project`
3. Implement changes
4. Verify `category` removal doesn't break anything (search for usages first)
