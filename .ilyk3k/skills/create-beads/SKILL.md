---
name: create-beads
description: Use when an implementation plan exists and needs to be broken into parallelizable work units with dependencies
---

# Create Beads

Convert implementation plan into beads with dependencies so workers can claim and execute them.

## Prerequisites

- Implementation plan exists at `docs/plans/*-implementation.md`
- `bd` CLI is available (check with `which bd`)
- If bd not initialized: `bd init --prefix <project-name> --quiet`

## Process

### 1. Read the Implementation Plan

Find and read the most recent implementation plan:

```bash
ls -t docs/plans/*-implementation.md | head -1
```

Identify each `### Task N:` section as a distinct unit of work.

### 2. Assess Scaffold Opportunities

Before creating task beads, analyze whether a foundational structure would unlock parallelism.

**Scaffolds are:**
- Module files with type definitions and function signatures (empty implementations)
- Directory structure and basic interfaces
- Test file skeletons with pending/skip markers
- Data schemas/structs that multiple tasks will use

**Benefits:**
- Workers can implement different functions independently once scaffold exists
- Easier merges (workers fill in pre-defined slots)
- Guidance (skeleton shows exactly where code goes)

**Create scaffold bead if:** Multiple tasks modify the same module, or tasks create interdependent types.

### 3. Assess Dependencies

Analyze the plan to determine task dependencies:

- **File references:** If Task 3 modifies a file Task 2 creates, Task 3 depends on Task 2
- **Logical ordering:** Tests for a feature depend on the feature existing
- **Parallelization:** Independent components can run simultaneously

**Default to sequential if uncertain.**

### 4. Create Beads

For scaffold (if identified):

```bash
bd create "Create module skeletons for <feature>" -d "Set up file structure and interfaces" -t task -p 1
# Note the returned bead ID (e.g., proj-abc1)
```

For each task:

```bash
bd create "Task N: <title from plan>" -d "<brief description>" -t task -p 2
# Note each returned bead ID
```

### 5. Wire Dependencies

```bash
# If scaffold exists, it blocks all first-level tasks
bd dep add <task1-id> <scaffold-id>
bd dep add <task2-id> <scaffold-id>

# Tasks block their dependents
bd dep add <task3-id> <task2-id>
```

### 6. Report the Dependency Graph

Output the created structure:

```
Created N beads from implementation plan:

Scaffold: proj-a1b2 - Create module skeletons
  |-- Task 1: proj-c3d4 - Implement user module
  |-- Task 2: proj-e5f6 - Implement auth module (parallel with Task 1)
      |-- Task 3: proj-g7h8 - Wire integration
          |-- Task 4: proj-i9j0 - Final verification

Ready to start: proj-a1b2 (Scaffold)
```

Verify with: `bd ready`

## Error Handling

**bd not initialized:**
Run `bd init --prefix <project-name> --quiet` first. Detect project name from directory.

**bd routes to different database:**
If `bd --verbose` shows "Routing to target repo: <other-path>", the CLI has multi-repo routing configured. Either:
- Use `--db .beads/beads.db` to force local database
- Check that the target database has `issue_prefix` configured
- Or work with the routing as intended (beads go to planning repo)

**Beads already exist for this plan:**
Check `bd list` for matching titles. Warn and ask whether to skip, update, or abort.

**Plan has no clear task structure:**
Fall back to single bead for entire plan. Flag: "Could not identify distinct tasks."

**Dependency analysis uncertain:**
Default to sequential. Add note: "Dependencies assumed sequential - review for parallelization."

## Output

- Beads appear in `bd ready` for workers to claim
- BeadsWatcher detects `.beads/issues.jsonl` changes
- BeadsCoordinator spawns workers for ready beads
