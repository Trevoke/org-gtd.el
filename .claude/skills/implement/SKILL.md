# /implement — Plan + Execute

Use when a design exists and it's time to break it into tasks, then execute them with subagent dispatch and review.

## Behavior

### 1. Find Design

Look for the most recent design doc in `docs/plans/`:
```
docs/plans/*-design.md
```

If multiple exist, ask the user which one. If none exist, suggest running `/architect` first.

### 2. Break Into Tasks

Read the design and break it into bite-sized tasks. Each task should be:
- **2-5 minutes** of focused work
- **One TDD cycle**: one test + its implementation
- **Independent** where possible (note dependencies where not)
- **Self-contained**: includes all context needed

For each task, write:
```
### Task N: [Title]

**Goal**: [one sentence]

**Steps**:
1. Write test: [describe the test]
2. Implement: [describe the code]
3. Verify: [what to check]

**Files**: [which files to create/modify]

**Acceptance Criteria**:
- [specific, testable criterion]
```

### 3. Save the Plan

Derive name from design doc.

Save to: `docs/plans/YYYY-MM-DD-<name>-plan.md`

### 4. Execute Each Task

Record the base SHA before starting: `git rev-parse HEAD`

For each task:

#### a. Dispatch Implementer
Use `Task` tool with `subagent_type: "tdd"` to dispatch the `implementer` agent. Include in the prompt:
- Full task text (copy it inline — the subagent can't read plan files)
- Relevant code context (file contents, patterns to follow)
- Acceptance criteria

#### b. Handle Questions
If the implementer asks questions, answer from context (design doc, codebase knowledge).

#### c. Review: Spec Compliance
When implementer completes, dispatch `spec-reviewer` agent via `Task` tool with `subagent_type: "superpowers:code-reviewer"`. Include:
- The task spec (acceptance criteria)
- List of files changed

If FAIL: resume the implementer to fix the issues, then re-review.

#### d. Review: Code Quality
Dispatch `quality-reviewer` agent via `Task` tool with `subagent_type: "superpowers:code-reviewer"`. Include:
- Base SHA and current HEAD
- Feature context

If critical issues: resume implementer to fix, then re-review.

#### e. Mark Complete
Note the task as done. Move to the next task.

### 5. After All Tasks

Tell the user:
> Implementation complete. When ready, use `/qa` for adversarial testing, or `/refactor` for a quality pass.

## Rules

- **Context inline.** Subagents receive everything they need in their prompt. They never read plan files.
- **Fresh subagent per task.** Don't reuse subagents across tasks — fresh context prevents confusion.
- **Review every task.** No task is "too simple" to review.
- **Fix before continuing.** Don't move to the next task if reviews fail.
- **Small tasks.** If a task takes more than 10 minutes, it's too big — split it.
