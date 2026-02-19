# /refactor — Code Quality Pass

Use after implementation and QA pass, or anytime you want to improve code quality without changing behavior.

## Behavior

### 1. Identify What Changed

Get the recent changes:
```bash
git diff main...HEAD    # if on a branch
git diff HEAD~N         # if on main, user specifies N
```

Read all changed files in full — not just the diff.

### 2. Check for Issues

Review the code for:

#### DRY Violations
- Duplicated logic across functions or files
- Copy-pasted code with minor variations
- Same concept expressed in multiple places

#### SRP Violations
- Functions doing more than one thing
- Modules with mixed responsibilities
- Functions that are hard to name (sign of doing too much)

#### Naming Clarity
- Does the code read like prose?
- Are variable names descriptive?
- Do function names say what they do?
- Are naming conventions consistent with the codebase?

#### Unnecessary Complexity
- Abstractions that aren't earning their keep
- Indirection that adds confusion without value
- Over-engineered solutions to simple problems

#### Dead Code
- Unused functions, variables, or imports
- Commented-out code
- Unreachable branches

### 3. Propose Changes

Present refactoring suggestions in order of impact (highest first):

```
## Refactoring Proposals

### 1. [Title] — [Impact: High/Medium/Low]
**What**: [what to change]
**Why**: [why it improves the code]
**Risk**: [what could go wrong]

### 2. ...
```

Get user approval before making changes.

### 4. Make Changes

For each approved refactoring:
1. Run tests — confirm they pass before starting
2. Make one refactoring change
3. Run tests — confirm they still pass
4. Commit with message describing the refactoring

**Never change behavior.** If a test fails after refactoring, you introduced a bug — revert and try again.

### 5. Final Verification

```bash
~/bin/eldev etest -r dot
```

All tests must pass. Report the final state.

## Rules

- **Never change behavior.** Refactoring changes structure, not behavior. Tests must stay green.
- **One change at a time.** Each refactoring is atomic and committed separately.
- **Tests between every change.** If tests break, revert immediately.
- **Ask before acting.** Present proposals and get approval — don't surprise the user.
- **Impact order.** Highest-impact, lowest-risk changes first.
- **Know when to stop.** Not every suggestion needs to be acted on.
