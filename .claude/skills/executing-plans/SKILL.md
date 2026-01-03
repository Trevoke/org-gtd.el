---
name: executing-plans
description: Use when you have a written implementation plan to execute in a separate session with review checkpoints
---

# Executing Plans

## Overview

Load plan, review critically, execute tasks in batches, report for review between batches.

**Core principle:** Batch execution with checkpoints for architect review.

**Announce at start:** "I'm using the executing-plans skill to implement this plan."

## The Process (Enhanced with BD Features)

### Step 1: Load and Review Plan
1. Read plan file
2. Search for similar work: `bd search "$PLAN_KEYWORDS"`
3. Review critically - identify any questions or concerns about the plan
4. If concerns: Raise them with your human partner before starting
5. If no concerns: Create epic with all tasks and proceed

### Step 2: Create Epic and Tasks
```bash
# Create epic for the entire plan
bd create "$PLAN_TITLE" --type epic --estimate $TOTAL_MINUTES
EPIC_ID=<created-id>

# Pin it for session visibility
bd pin $EPIC_ID

# Extract tasks and create with estimates
bd create "Task 1" --parent $EPIC_ID --estimate 30 --labels "batch-1,implementation"
bd create "Task 2" --parent $EPIC_ID --estimate 25 --labels "batch-1,implementation" --deps blocks:<task1>
bd create "Task 3" --parent $EPIC_ID --estimate 40 --labels "batch-2,implementation"
# ... continue for all tasks
```

### Step 3: Execute Batch
**Default: First 3 ready tasks**

```bash
# Check for stale work from previous sessions
bd stale --days 1
bd list --status in_progress

# Get ready tasks (respects dependencies)
bd ready

# Start batch execution
bd update <id1> <id2> <id3> --status in_progress
```

For each task:
1. Follow each step exactly (plan has bite-sized steps)
2. Run verifications as specified
3. Add audit trail: `bd audit "Task X: <key decisions/findings>"`
4. Update labels: `bd update <id> --add-label "tested"`
5. Mark as completed: `bd close <id>`

### Step 4: Report
When batch complete:
```bash
# Show completed work with time spent
bd list --status closed --parent $EPIC_ID --created-after today

# Check epic progress
bd show $EPIC_ID

# Show remaining work with estimates
bd ready
```
- Show what was implemented
- Show verification output
- Report time spent vs estimates
- Say: "Ready for feedback."

### Step 5: Continue
Based on feedback:
- Apply changes if needed
- Check next batch: `bd ready` (automatically respects dependencies)
- Update estimates if needed: `bd update <id> --estimate 60`
- Repeat until complete

### Step 6: Complete Development

After all tasks complete and verified:
```bash
# Check for any forgotten work
bd stale --days 0
bd list --status open --parent $EPIC_ID
# Should return: "No open issues"

# Unpin the epic
bd unpin $EPIC_ID

# Close the epic
bd close $EPIC_ID
```
- Announce: "I'm using the finishing-a-development-branch skill to complete this work."
- **REQUIRED SUB-SKILL:** Use superpowers:finishing-a-development-branch
- Follow that skill to verify tests, present options, execute choice

## BD-Enhanced Workflow

### Creating Tasks with Dependencies and Estimates
```bash
# Always search first
bd search "$KEYWORDS_FROM_PLAN"

# Create epic with total estimate
bd create "Plan: $TITLE" --type epic --estimate 240  # 4 hours total
bd pin $EPIC_ID  # Keep visible

# Extract tasks with realistic estimates
# Guidelines:
# - Simple task: 15-30 min
# - Medium complexity: 30-60 min
# - Complex with tests: 60-90 min

bd create "Setup database" --parent $EPIC_ID --type task --priority 1 \
  --estimate 30 --labels "infrastructure,batch-1"

bd create "Create API" --parent $EPIC_ID --type task --priority 2 \
  --estimate 60 --labels "backend,batch-1" \
  --deps blocks:setup-db

bd create "Build UI" --parent $EPIC_ID --type task --priority 2 \
  --estimate 45 --labels "frontend,batch-2" \
  --deps blocks:setup-db

bd create "Integration tests" --parent $EPIC_ID --type task --priority 3 \
  --estimate 30 --labels "testing,batch-3" \
  --deps blocks:create-api,blocks:build-ui

# Visualize the plan
bd graph --epic $EPIC_ID
```

### Tracking Progress with Labels
```bash
# Semantic labels for filtering
--labels "batch-1,backend"      # Batch assignment
--labels "tested,reviewed"       # Status tracking
--labels "blocked,needs-input"  # Issues

# Filter by batch
bd list --label batch-1 --parent $EPIC_ID
bd list --label batch-2 --parent $EPIC_ID

# Find what needs attention
bd list --label needs-input
bd list --label blocked
```

### Handling Blockers and Context
```bash
# Mark task as blocked with reason
bd update <id> --status blocked
bd comment <id> "Blocked: Missing API credentials from client"
bd audit "Blocker: Awaiting client credentials for payment gateway"

# When unblocked
bd update <id> --status open
bd comment <id> "Unblocked: Received credentials"
```

### Session Recovery After Interruption
```bash
# After interruption, quick recovery:
bd list --status in_progress     # What was being worked on
bd stale --hours 4               # Recent but stale work
bd list --parent $PINNED_EPIC    # All tasks in current plan
bd show $PINNED_EPIC             # Overall progress and estimates
bd graph --epic $PINNED_EPIC     # Visual state of dependencies

# Resume from exact point
bd ready  # Shows next unblocked tasks
```

## When to Stop and Ask for Help

**STOP executing immediately when:**
- Hit a blocker mid-batch (missing dependency, test fails, instruction unclear)
  - Mark as blocked: `bd update <id> --status blocked`
  - Add audit: `bd audit "Blocked: <detailed reason>"`
- Plan has critical gaps preventing starting
- You don't understand an instruction
- Verification fails repeatedly
- Estimates way off (task taking 3x longer than estimated)

**Ask for clarification rather than guessing.**

## When to Revisit Earlier Steps

**Return to Review (Step 1) when:**
- Partner updates the plan based on your feedback
- Fundamental approach needs rethinking
- Dependencies need restructuring: `bd dep remove <id> blocks <other>`

**Don't force through blockers** - stop and ask.

## Advantages of BD Integration

**Persistence:**
- Plan survives session restarts
- Can resume work exactly where left off
- Full history of attempts and decisions

**Visibility:**
- Partner can run `bd status` to see progress
- `bd show $EPIC_ID` shows time spent vs estimates
- `bd sync` pushes updates to git for team visibility
- Audit trail provides decision history

**Dependencies:**
- Automatic task ordering based on blocks/blocked-by
- Can't accidentally work on blocked tasks
- Clear view of what unblocks what

**Time Management:**
- Estimates help set expectations
- Can report accurate progress percentages
- Identify which tasks consistently run over

**Flexibility:**
- Can defer tasks: `bd defer <id> --comment "Descoped for MVP"`
- Can reprioritize: `bd update <id> --priority 0`
- Can add new tasks mid-execution

## Red Flags

**Never:**
- Skip search before creating tasks
- Create tasks without estimates
- Forget to pin the epic for visibility
- Execute blocked tasks
- Skip audit trail for decisions
- Ignore stale task warnings
- Close epic with open child tasks

**Always:**
- Search for similar work first
- Create epic for multi-task plans
- Add realistic time estimates
- Use semantic labels for organization
- Check stale tasks when resuming
- Audit important decisions
- Unpin and close epic when done

## Remember
- Review plan critically first
- Search for similar work before starting
- Create epic and pin it
- Add estimates to set expectations
- Create all tasks with proper dependencies
- Follow plan steps exactly
- Don't skip verifications
- Reference skills when plan says to
- Between batches: report progress with time data
- Stop when blocked, don't guess
- Keep bd updated for session continuity
- Unpin and close epic when complete