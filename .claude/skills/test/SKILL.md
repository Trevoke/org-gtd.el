---
name: test
description: Use EVERY TIME you need to run tests, verify tests pass, or check for regressions. ALWAYS use this instead of running eldev directly.
---

# /test — Run Tests with Clean Output

Run `./run-tests.sh` (in this skill's directory) to execute tests. The script handles all noise filtering and outputs only a clean summary.

## How to Run

```bash
.claude/skills/test/run-tests.sh [ARGUMENT]
```

Parse the ARGUMENTS line and pass it through:

| Argument | What it does |
|----------|-------------|
| (none) / `all` | Full suite |
| `test/unit/foo-test.el` | Single file |
| `unit` / `integration` / `acceptance` | Category |
| `--seed=N` | Pin e-unit's shuffle order (reproduce a flake); combine with any target |

### Reproducing order-dependent flakes

The suite shuffles file/test order each run; the summary always prints the
seed used. If a run fails, re-run with that exact seed to reproduce:

```bash
.claude/skills/test/run-tests.sh --seed=1783402152928884
```

`--seed=N` can be combined with a file or category, e.g.
`run-tests.sh --seed=42 test/unit/accessors-test.el`.

### Resolving test names

If the argument contains `/` but no `.el` (looks like a test name, e.g. `stuck-sa-filter/cancelled-project-task-skipped`):
1. Grep for `deftest <name>` in `test/` to find the file
2. Pass the file path to `run-tests.sh`

## Output Format

The script returns one of:

**Success:** `PASS: 1195 tests in 18.4s` followed by `Seed: <N>`

**Failure:** Summary line + full failure/error details (test name, expected/actual, file:line)

**Unexpected:** Last 30 lines of raw output

Report the script output verbatim. Do not add commentary unless the user asks.
