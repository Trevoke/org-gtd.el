---
name: finish-dev
description: Use when bead implementation is complete and ready for integration
---

# Finish Development

Complete development work by verifying tests, presenting options, and cleaning up.

## Process

### 1. Verify Tests Pass

Before anything else, verify all tests pass:

```bash
mix test  # or appropriate test command
```

**If tests fail:** Stop. Fix failures before proceeding.

**If tests pass:** Continue to step 2.

### 2. Determine Base Branch

```bash
git merge-base HEAD main 2>/dev/null || git merge-base HEAD master
```

### 3. Present Options

Present exactly these 4 options:

```
Implementation complete. What would you like to do?

1. Merge back to <base-branch> locally
2. Push and create a Pull Request
3. Keep the branch as-is (I'll handle it later)
4. Discard this work

Which option?
```

### 4. Execute Choice

**Option 1: Merge Locally**
```bash
git checkout <base-branch>
git pull
git merge <feature-branch>
# Verify tests on merged result
git branch -d <feature-branch>
```

**Option 2: Push and Create PR**
```bash
git push -u origin <feature-branch>
gh pr create --title "<title>" --body "## Summary
- <bullet points>

## Test Plan
- [ ] <verification steps>"
```

**Option 3: Keep As-Is**
Report: "Keeping branch <name>. You can return to it later."

**Option 4: Discard**
Confirm first:
```
This will permanently delete:
- Branch <name>
- All commits: <list>

Type 'discard' to confirm.
```

Then if confirmed:
```bash
git checkout <base-branch>
git branch -D <feature-branch>
```

## Quick Reference

| Option | Merge | Push | Delete Branch |
|--------|-------|------|---------------|
| 1. Merge locally | ✓ | - | ✓ |
| 2. Create PR | - | ✓ | - |
| 3. Keep as-is | - | - | - |
| 4. Discard | - | - | ✓ (force) |

## Red Flags

**Never:**
- Proceed with failing tests
- Merge without verifying tests on result
- Delete work without confirmation
- Force-push without explicit request

**Always:**
- Verify tests before offering options
- Present exactly 4 options
- Get typed confirmation for discard
