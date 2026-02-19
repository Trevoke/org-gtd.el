# /qa — Adversarial Testing

Use when implementation is complete and you want to verify quality through adversarial testing. QA actively tries to break things.

## Behavior

### 1. Find Context

Look for requirements and design docs in `docs/plans/`:
```
docs/plans/*-requirements.md
docs/plans/*-design.md
```

Read both to understand what was built and what was promised.

### 2. Run Existing Tests

```bash
~/bin/eldev etest -r dot
```

Report results: how many tests, how many pass, how many fail. If any fail, report them immediately — existing regressions are priority one.

### 3. Check Acceptance Criteria Coverage

Read the acceptance criteria from the requirements doc. For each criterion:

1. **Find the test**: Search for a test that exercises this criterion
2. **Verify the test**: Read it — does it actually test what the criterion says?
3. **Report gaps**: List any criteria without adequate test coverage

### 4. Write Missing Tests

For any uncovered acceptance criterion, write a test. Follow existing test patterns in the codebase.

### 5. Write Adversarial Tests

Actively try to break things:

#### Edge Cases
- Empty inputs (nil, empty string, empty list)
- Boundary values (0, 1, max, min)
- Single-element collections
- Very large inputs

#### Error Paths
- What happens when a file doesn't exist?
- What happens when a buffer is killed mid-operation?
- What happens with malformed org content?
- What happens when required properties are missing?

#### Invalid Inputs
- Wrong types (string where number expected, etc.)
- Malformed data
- Missing required arguments

#### State Issues
- What if called twice in a row?
- What if called with no GTD directory configured?
- What if the org file is read-only?

### 6. Run All Tests

```bash
~/bin/eldev etest -r dot
```

Report with evidence:
```
## QA Report

**Test Suite**: [N] tests, [M] passing, [K] failing

### Acceptance Criteria Coverage
- [criterion 1]: COVERED by test_xyz
- [criterion 2]: COVERED by test_abc
- [criterion 3]: GAP — wrote test_def to cover

### New Tests Written
- [test name]: tests [what it tests]

### Adversarial Tests
- [test name]: [what it tries to break] — [PASS/FAIL]

### Failures
- [test name]: [what failed, how to reproduce]
```

### 7. Report Failures Clearly

For each failure, provide:
- **What**: which test failed
- **Why**: root cause if identifiable
- **Reproduction**: exact steps or test invocation
- **Suggestion**: likely fix if obvious

## Rules

- **Write code.** QA doesn't just report — it writes tests and provides evidence.
- **Be adversarial.** Your job is to find problems, not confirm things work.
- **Provide evidence.** Every claim needs a test or command output backing it up.
- **Don't fix bugs.** Report them with evidence. Fixing is the implementer's job.
- **Run tests, don't imagine them.** Execute every test you write.
