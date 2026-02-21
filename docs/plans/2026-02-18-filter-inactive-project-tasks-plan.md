# Filter Inactive Project Tasks — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task.

**Goal:** Implicitly exclude tasks from fully inactive projects in all active-task views.

**Architecture:** New predicate in org-gtd-skip.el + wired into four skip function builder paths in org-gtd-view-language.el.

**Base SHA:** bece290

---

### Task 1: Add predicate org-gtd-pred--task-has-active-project

**Goal**: Create the predicate that checks whether a task belongs to at least one active project.

**Test**: Write unit tests in `test/unit/skip-predicates-test.el` covering:
- Heading with no ORG_GTD_PROJECT_IDS → predicate returns t (standalone action)
- Heading with one active project ID → returns t
- Heading with one CNCL project ID → returns nil
- Heading with one DONE project ID → returns nil
- Heading with two project IDs (one CNCL, one active) → returns t
- Heading with two project IDs (both CNCL) → returns nil
- Heading with unresolvable project ID → returns t (fail open)

**Implementation**: Add `org-gtd-pred--task-has-active-project` to `org-gtd-skip.el` in the "Project Predicates" section (after `org-gtd-pred--project-is-stuck`). Add `declare-function` for `org-gtd-project--active-p`. The predicate returns a closure that:
1. Gets `ORG_GTD_PROJECT_IDS` via `org-entry-get-multivalued-property`
2. If nil/empty → return t (standalone action)
3. For each ID: call `org-gtd-project--active-p` — if any returns t, return t
4. For each ID: if `org-id-find` returns nil (in active-p), return t (fail open — can't confirm inactive)
5. Only return nil if ALL IDs resolve to inactive projects

**Files**: `org-gtd-skip.el`, `test/unit/skip-predicates-test.el`

**Acceptance Criteria**: All 7 unit tests pass. Predicate follows the closure pattern of existing predicates in the file.

---

### Task 2: Wire predicate into stuck-single-action builder

**Goal**: Tasks from fully inactive projects should be skipped by the stuck-single-action view.

**Test**: Write integration test in `test/integration/view-filters-test.el` (or new file):
- Create a cancelled project (heading CNCL) with child tasks (ORG_GTD=Actions, TODO state TODO)
- Build the stuck-single-action skip function
- Position at a child task heading, call skip function → should return non-nil (skip)
- Also test: active project with TODO child task → skip function returns nil (include, it's stuck)

**Implementation**: In `org-gtd-view-lang--build-skip-function-for-stuck-single-action` (org-gtd-view-language.el line 594), add an early check: after verifying ORG_GTD=Actions and before checking done/stuck, call the predicate. If it returns nil (all projects inactive), return `end` (skip).

**Files**: `org-gtd-view-language.el`, test file

**Acceptance Criteria**: Cancelled project tasks no longer appear in stuck single action review. Active project stuck tasks still appear.

---

### Task 3: Wire predicate into stuck-type and generic builders

**Goal**: Complete the coverage across all remaining builder paths.

**Test**: Unit test verifying the predicate is included in:
- stuck-type builder (stuck-calendar, stuck-delegated, etc.)
- generic predicate-composition path

**Implementation**:
1. In `org-gtd-view-lang--build-skip-function-for-stuck-type` (line 517): after verifying ORG_GTD matches the base type, add the active-project check before the stuck logic.
2. In `org-gtd-view-lang--build-skip-function` generic path (line 774): push `(org-gtd-pred--task-has-active-project)` into the predicates list, right before the `(org-gtd-pred--not-done)` push.

**Files**: `org-gtd-view-language.el`, test file

**Acceptance Criteria**: All three builder paths (stuck-single-action from Task 2, stuck-type, generic) filter inactive project tasks. Existing tests still pass.

---

### Task 4: Wire predicate into project-type builder

**Goal**: Ensure cancelled/done project headings don't appear in stuck-project or active-project views.

**Test**:
- Create a CNCL project heading → stuck-project skip function should skip it
- Create a DONE project heading → active-project skip function should skip it
- Verify existing stuck-project behavior still works for active projects with TODO tasks

**Implementation**: In `org-gtd-view-lang--build-skip-function-for-project-type` (line 548): after the ORG_GTD=Projects check, add `(if (org-entry-is-done-p) end ...)` before the type-specific cond branches. This ensures done/cancelled projects are always skipped regardless of project-type.

**Files**: `org-gtd-view-language.el`, test file

**Acceptance Criteria**: CNCL and DONE projects don't appear in stuck-project or active-project views. Existing project view tests still pass.
