# Plan: Fix TODO Keywords After Graph Operations

## Problem

Graph transient operations that modify project dependencies don't call `org-gtd-projects-fix-todo-keywords` afterward. This means tasks may have stale TODO states:
- A task with new blockers might still be NEXT when it should be TODO
- A task whose blockers were removed might still be TODO when it should be NEXT

## Solution

Add `org-gtd-projects-fix-todo-keywords` call before `org-gtd-graph-view-refresh` in each graph operation that modifies dependencies.

## Operations to Modify

| Function | Line | Modifies Dependencies? |
|----------|------|----------------------|
| `org-gtd-graph-transient-add-root` | 240 | YES (adds to FIRST_TASKS) |
| `org-gtd-graph--modify-blockers-apply` | 559 | YES |
| `org-gtd-graph--modify-successors-apply` | 680 | YES |
| `org-gtd-graph-remove-task` | 762 | YES (rewires) |
| `org-gtd-graph-trash-task` | 780 | YES |
| `org-gtd-graph-change-state` | 798 | NO (just org-todo) |
| `org-gtd-graph--add-successor-apply` | 945 | YES |
| `org-gtd-graph--add-blocker-apply` | 1068 | YES |

Total: 7 functions need modification.

## Implementation Steps

### Step 1: Write failing tests

Create test file `test/integration/graph-fix-keywords-test.el` with tests for:

1. **Add successor fixes keywords**: When adding a successor that blocks an existing task, the blocked task should change from NEXT to TODO
2. **Add blocker fixes keywords**: When adding a blocker to a NEXT task, that task should change to TODO
3. **Modify blockers fixes keywords**: When removing all blockers from a TODO task, it should become NEXT
4. **Remove task fixes keywords**: When removing a blocker task, tasks it was blocking should become NEXT if they have no other blockers
5. **Add root fixes keywords**: New root task should be NEXT (no blockers)

### Step 2: Add require statement

Add `(require 'org-gtd-projects)` to `org-gtd-graph-transient.el` if not already present.

### Step 3: Add fix-keywords calls

In each of the 7 functions, add before `org-gtd-graph-view-refresh`:
```elisp
(org-gtd-projects-fix-todo-keywords org-gtd-graph-view--project-marker)
```

### Step 4: Run tests

Verify all new tests pass and no existing tests regress.

## Test Design

Each test should:
1. Create a project with tasks in specific TODO states
2. Perform a graph operation
3. Verify the TODO states are correct after the operation

Example test structure:
```elisp
(ert-deftest graph-add-blocker-fixes-keywords ()
  "Adding a blocker to a NEXT task should change it to TODO."
  (with-org-gtd-test-environment
    ;; Setup: Create project with task A (NEXT, no blockers)
    ;; Action: Add task B as blocker to task A
    ;; Assert: Task A is now TODO (has blocker)
    ;; Assert: Task B is NEXT (no blockers)
    ))
```
