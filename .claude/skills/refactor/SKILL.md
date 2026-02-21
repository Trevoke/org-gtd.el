---
name: refactor
description: Use when code quality needs improvement after implementation, without changing behavior
---

# /refactor — Code Quality Pass

## Overview

**Improve code structure without changing behavior.** Propose changes, get approval, make one change at a time with tests between each.

**Core principle:** Never change behavior. Tests must stay green after every change.

## When to Use

- After implementation and QA pass
- User requests a quality check on recent work
- You notice structural issues while working

## The Process

### 1. Establish Baseline

**Run the test suite first. Actually run it.**

```bash
~/bin/eldev etest -r dot
```

Record the result. This is your safety net. If tests aren't green before you start, fix that first.

### 2. Identify What Changed

```bash
git diff main...HEAD    # if on a branch
git log --oneline -N    # user specifies range
```

Read all changed files **in full** — not just the diff. Context matters.

### 3. Find Issues

Look for:

| Issue | Signal |
|-------|--------|
| **DRY violations** | Same logic in two places |
| **SRP violations** | Function hard to name (doing too much) |
| **Naming** | Names don't match what the code does |
| **Unnecessary complexity** | Abstraction not earning its keep |
| **Dead code** | Unreachable branches, unused functions |

**Don't look for:**
- Style preferences (tab vs space, quote style)
- Missing documentation on unchanged code
- "Improvements" to code you didn't change

### 4. Propose Changes — Get Approval

Present refactorings ordered by **highest impact, lowest risk** first:

```markdown
### 1. [Title] — Impact: High, Risk: Low

**What**: [what to change]
**Why**: [why it improves the code]
**Risk**: [what could go wrong]
```

**Ask the user which to do.** Don't start changing code without approval.

### 5. Make Changes — One at a Time

For each approved refactoring:

1. **Run tests** — confirm green before starting
2. **Make one change** — a single, atomic refactoring
3. **Run tests** — confirm still green
4. **Commit** — describe the refactoring, not the code

If tests fail after a change: **revert immediately.** You introduced a bug.

### 6. Final Verification

```bash
~/bin/eldev etest -r dot
```

All tests must pass. Report the final state.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Changing behavior ("while I'm here") | Refactoring changes structure ONLY. If tests break, revert. |
| Not running tests before starting | Run tests FIRST. No green baseline = no refactoring. |
| Making changes without user approval | Propose, get approval, then act. |
| Multiple changes at once | One change, one test run, one commit. Atomic. |
| Renaming public API | That's a breaking change, not a refactoring. |
| Proposing 10+ changes | Focus on 3-5 highest-impact items. Less is more. |
| Describing changes without making them | Actually edit the files. Actually run the tests. |
