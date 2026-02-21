---
name: qa
description: Use when implementation is complete and you want to verify quality through adversarial testing that writes code and provides evidence
---

# /qa — Adversarial Testing

## Overview

**QA writes tests and runs them. It provides evidence, not opinions.** Every claim is backed by a test or command output.

**Core principle:** Your job is to break things. Write code. Run it. Show evidence.

## When to Use

- Implementation is complete (or a chunk of work needs verification)
- You want to check acceptance criteria coverage
- You want adversarial edge-case testing

## The Process

### 1. Find Context

Look for requirements and design docs in `docs/plans/`. Read both to understand what was built and what was promised.

### 2. Run the Existing Test Suite

```bash
~/bin/eldev etest -r dot
```

**Actually run it.** Report exact output: how many tests, how many pass, how many fail. If any fail, report them immediately — existing regressions are priority one.

### 3. Check Acceptance Criteria Coverage

Read acceptance criteria from the requirements doc. For each criterion:

1. **Search** for a test that exercises it (`Grep` for keywords)
2. **Read** the test — does it actually test what the criterion says?
3. **Report**: covered or gap

```markdown
| Criterion | Test | Status |
|-----------|------|--------|
| User can activate focus mode | focus-test.el:42 | COVERED |
| Calendar items always visible | (none found) | GAP |
```

### 4. Write Missing Tests

For each gap, **write the test**. Follow existing patterns in the codebase.

Run it. Report whether it passes or fails.

### 5. Write Adversarial Tests

Actively try to break things. **Write actual test code, don't just describe it.**

Target areas:
- **Nil/empty inputs**: What happens with nil arguments, empty strings, empty lists?
- **Boundary values**: 0, 1, max, min
- **Missing state**: Required properties absent, buffers killed mid-operation
- **Invalid inputs**: Wrong types, malformed data
- **Repeated calls**: What if the function is called twice in a row?

### 6. Run Everything and Report

```bash
~/bin/eldev etest -r dot
```

Report with **evidence** — actual test output, not fabricated numbers:

```markdown
## QA Report

**Test Suite**: [paste actual eldev output]

### Acceptance Criteria Coverage
| Criterion | Test | Status |
|-----------|------|--------|
| ... | ... | ... |

### Tests Written
- [test name]: tests [what] — [PASS/FAIL]

### Failures Found
- [test name]: [what failed]
  - **Reproduction**: [exact command or test invocation]
  - **Expected**: [what should happen]
  - **Actual**: [what happened]
```

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Describing tests without writing code | WRITE the test. Create the file. Run it. |
| Fabricating test results | RUN the tests. Paste actual output. |
| Reporting opinions instead of evidence | Every claim needs a test or command output. |
| Suggesting implementation fixes | Report problems with evidence. Fixing is the implementer's job. |
| Categorizing tests instead of running them | Less taxonomy, more `eldev etest`. |
| Skipping the existing test suite | ALWAYS run the full suite first. Regressions are priority one. |
| Not checking requirements doc | Cross-reference acceptance criteria. That's what "done" means. |
