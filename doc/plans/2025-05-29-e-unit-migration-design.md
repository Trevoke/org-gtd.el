# E-Unit Test Migration Design

## Overview

Migrate org-gtd.el tests from buttercup to e-unit with mock-fs, starting with acceptance tests for basic GTD workflows.

## Directory Structure

```
test-eunit/
├── unit/
│   ├── capture/
│   ├── process/
│   ├── organize/
│   ├── engage/
│   └── review/
├── integration/
│   ├── capture/
│   ├── process/
│   ├── organize/
│   ├── engage/
│   └── review/
├── acceptance/
│   ├── basic-workflows-test.el      # First migration target
│   ├── cancel-archive-test.el
│   ├── review-flows-test.el
│   ├── multi-file-dag-test.el
│   ├── shared-tasks-test.el
│   └── ...
└── helpers/
    ├── prelude.el
    ├── setup.el
    └── builders.el
```

**Organization principle:**
- Top level: test type (unit/integration/acceptance)
- Second level (unit/integration): GTD workflow stages
- Acceptance tests: grouped by feature complexity (full-cycle tests by nature)

## Mock-FS Integration

Use `with-mock-fs` via `around-each` hook for complete test isolation:

```elisp
(defun ogt-eunit--mock-fs-spec ()
  "Return the mock filesystem spec for GTD tests."
  '(("/gtd/" . nil)
    ("/gtd/inbox.org" . "")
    ("/gtd/org-gtd-tasks.org" . "")
    ("/gtd/org-gtd-calendar.org" . "")
    ("/gtd/org-gtd-incubate.org" . "")))

(around-each (proceed context)
  (with-mock-fs (ogt-eunit--mock-fs-spec)
    (ogt-eunit--configure-emacs)
    (unwind-protect
        (funcall proceed context)
      (ogt-eunit--cleanup))))
```

**Rationale:** mock-fs uses `file-name-handler-alist` like TRAMP - file operations should be transparent. If something doesn't work, that's a bug in mock-fs worth fixing.

## Syntax Translation

| Buttercup | E-Unit |
|-----------|--------|
| `(describe "..." (it "..." ...))` | `(deftest name () "..." ...)` |
| `(expect x :to-equal y)` | `(assert-equal y x)` |
| `(expect x :to-match "pat")` | `(assert-match "pat" x)` |
| `(expect x :not :to-match "pat")` | `(assert-not-match "pat" x)` |
| `(before-each ...)` | `(before-each ...)` |
| `(after-each ...)` | `(after-each ...)` |

## First Migration: Basic Workflows

**Source:** `test/end-to-end-test.el` lines 12-290

**Target:** `test-eunit/acceptance/basic-workflows-test.el`

**Tests to migrate:**
1. `single-action-workflow` - capture → process → organize → engage → archive
2. `project-workflow` - with subtasks
3. `calendar-workflow` - scheduled items
4. `delegated-workflow` - waiting for others
5. `tickler-workflow` - someday/maybe items
6. `knowledge-workflow` - reference material
7. `multiple-item-processing` - sequence of different types
8. `complete-gtd-workflow-integration` - full cycle test

## Implementation Tasks

1. Create directory structure (`test-eunit/acceptance/`, `test-eunit/helpers/`)
2. Create `test-eunit/helpers/prelude.el` - e-unit equivalent of buttercup prelude
3. Create `test-eunit/helpers/setup.el` - mock-fs configuration and test setup
4. Create `test-eunit/helpers/builders.el` - test helper functions (adapted for mock-fs paths)
5. Create `test-eunit/acceptance/basic-workflows-test.el` - migrate 8 tests
6. Verify tests pass with `eldev etest`
7. Update Eldev to run e-unit tests from subdirectories

## Future Migration Phases

After basic workflows are stable:
- Cancel/Archive tests
- Review flow tests
- Habit tests
- Multi-file DAG tests
- Shared task tests
- Graph validation tests
