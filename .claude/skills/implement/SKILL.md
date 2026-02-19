---
name: implement
description: Use when a design exists and it's time to break it into tasks and execute with subagent dispatch and review
---

# /implement — Plan + Execute

## Overview

**Break a design into bite-sized TDD tasks, execute each with a fresh subagent, review every task before moving on.**

**Core principle:** Small tasks + fresh subagents + two-stage review = high quality.

## When to Use

- Design exists (from `/architect` or provided by user)
- Ready to write code
- Work can be broken into independent TDD steps

## The Process

### 1. Find Design

Look for the most recent `*-design.md` in `docs/plans/`. If none found or multiple exist, ask the user which to use. If no design exists, suggest `/architect` first.

### 2. Break Into Tasks

Each task must be:
- **One TDD cycle**: one test + its implementation
- **2-5 minutes** of focused work
- **Self-contained**: all context provided inline

Template per task:
```markdown
### Task N: [Title]

**Goal**: [one sentence]
**Test**: [describe the test to write first]
**Implementation**: [describe the code to make it pass]
**Files**: [which files to create/modify]
**Acceptance Criteria**: [specific, testable]
```

### 3. Save the Plan

```
docs/plans/YYYY-MM-DD-<name>-plan.md
```

### 4. Execute Each Task

Record base SHA: `git rev-parse HEAD`

For each task, in order:

#### a. Dispatch Implementer

Use `Task` tool with `subagent_type: "tdd"`. In the prompt, include **inline**:
- Full task text (don't reference plan files — subagent can't read them)
- Relevant code snippets (read and paste the actual file content)
- Patterns to follow (paste examples from existing code)
- Acceptance criteria

**Context goes inline.** The subagent receives everything it needs in the prompt.

#### b. Spec Review

When implementer completes, dispatch `spec-reviewer` via `Task` tool with `subagent_type: "superpowers:code-reviewer"`. Include:
- The task's acceptance criteria (copy inline)
- List of changed files

If FAIL: resume implementer to fix, then re-review.

#### c. Quality Review

Dispatch `quality-reviewer` via `Task` tool with `subagent_type: "superpowers:code-reviewer"`. Include:
- Base SHA and current HEAD
- Feature context

If critical issues: resume implementer to fix, then re-review.

#### d. Move On

Only proceed to next task when both reviews pass.

### 5. After All Tasks

> Implementation complete. Use `/qa` for adversarial testing, or `/refactor` for a quality pass.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Tasks too large (whole features) | One TDD cycle per task. If it takes >10 min, split it. |
| Expecting subagent to read plan files | Provide ALL context inline in the dispatch prompt. |
| Skipping review for "simple" tasks | Every task gets spec review + quality review. No exceptions. |
| Phase-based grouping instead of task list | Flat numbered list. Dependencies noted, not phase boundaries. |
| No plan file saved | Always save to `docs/plans/YYYY-MM-DD-<name>-plan.md`. |
| Moving on despite failed review | Fix first. Never proceed with review failures. |
| Not referencing next step | End with: "use `/qa` or `/refactor` when ready" |
