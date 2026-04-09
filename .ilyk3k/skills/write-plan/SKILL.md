---
name: write-plan
description: Use when a design document exists and needs to be broken into detailed implementation tasks
---

# Write Implementation Plan

Create comprehensive implementation plans assuming the engineer has zero context. Document everything: which files to touch, code snippets, how to test. Break into bite-sized tasks. DRY. YAGNI. TDD. Frequent commits.

## Plan Document Header

Every plan MUST start with:

```markdown
# [Feature Name] Implementation Plan

**Goal:** [One sentence describing what this builds]

**Architecture:** [2-3 sentences about approach]

**Tech Stack:** [Key technologies/libraries]

---
```

## Task Structure

Each task follows this format:

```markdown
## Task N: [Component Name]

**Files:**
- Create: `exact/path/to/file.py`
- Modify: `exact/path/to/existing.py:123-145`
- Test: `tests/exact/path/to/test.py`

**Step 1: Write the failing test**

[Complete test code]

**Step 2: Run test to verify it fails**

Run: `pytest tests/path/test.py::test_name -v`
Expected: FAIL with "function not defined"

**Step 3: Write minimal implementation**

[Complete implementation code]

**Step 4: Run test to verify it passes**

Run: `pytest tests/path/test.py::test_name -v`
Expected: PASS

**Step 5: Commit**

```bash
git add tests/path/test.py src/path/file.py
git commit -m "feat: add specific feature"
```
```

## Bite-Sized Granularity

Each step is one action (2-5 minutes):
- "Write the failing test" - step
- "Run it to verify it fails" - step
- "Implement minimal code" - step
- "Run tests to verify pass" - step
- "Commit" - step

## Key Requirements

- **Exact file paths** - Always include full paths
- **Complete code** - No "add validation here" placeholders
- **Exact commands** - Include expected output
- **TDD flow** - Test first, then implement
- **Frequent commits** - After each task

## Output

Implementation plan saved to `docs/plans/YYYY-MM-DD-<feature>-implementation.md`

After plan is written:
1. Commit the plan document
2. Proceed to create-beads to break plan into parallelizable units
