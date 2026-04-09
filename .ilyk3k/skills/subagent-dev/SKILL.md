---
name: subagent-dev
description: Use when executing a single bead/task with test-driven development approach
---

# Subagent Development

Execute a single bead using test-driven development. Fresh context, focused work, clean commits.

## Process

### 1. Read Bead Requirements

Understand what needs to be built:
- Read the bead title and description
- Check linked plan or design docs if referenced
- Identify acceptance criteria

### 2. Ask Questions First

Before writing any code, clarify ambiguities:
- Ask about unclear requirements
- Confirm expected behavior
- Identify edge cases

**Don't proceed with assumptions** - get clarity upfront.

### 3. Write Failing Test

```bash
# Create test file if needed
# Write test that captures the requirement
# Run to verify it fails
```

**Test must fail for the right reason** - "function not defined" or "assertion failed", not syntax error.

### 4. Implement Minimal Code

Write the simplest code that makes the test pass:
- No extra features
- No premature optimization
- Just enough to pass the test

### 5. Verify Test Passes

```bash
# Run the specific test
# Verify it passes
# Run related tests to check for regressions
```

### 6. Self-Review

Before committing, check:
- [ ] Code matches requirements (not more, not less)
- [ ] Tests are meaningful (not just passing)
- [ ] No obvious issues or shortcuts
- [ ] Code is clean and readable

### 7. Commit Changes

```bash
git add -A
git commit -m "feat: <what was implemented>

- <key detail 1>
- <key detail 2>"
```

## TDD Red Flags

**Never:**
- Write implementation before test
- Skip the "verify test fails" step
- Add features not in requirements
- Commit with failing tests

**If stuck:**
- Ask for clarification
- Break into smaller steps
- Check if requirements are clear

## Output

- Implemented feature with passing tests
- Clean commit with descriptive message
- Ready for code review
