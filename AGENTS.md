# Agent Instructions for org-gtd.el

This document provides guidance for AI agents working on the org-gtd.el project.

## Issue Tracking with yaks

**IMPORTANT**: This project uses **yaks** (`yx`) for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why yaks?

- Git-native: Stored as git objects, shares via `yx sync` / `git push`
- Simple states: todo → wip → blocked → done
- Hierarchical: Nest tasks under parents with `--under`
- Agent-friendly: JSON output with `--format json`

### Quick Start

Install with:
```bash
curl -fsSL https://raw.githubusercontent.com/mattwynne/yaks/main/install.sh | bash
```

**List work to do:**
```bash
yx list --only not-done --format json
```

**Create new task:**
```bash
yx add "Task title" --format json
yx add "Sub-task" --under "Parent task" --format json
```

**Start working:**
```bash
yx start "task name" --format json
```

**Complete work:**
```bash
yx done "task name" --format json
```

**Block a task:**
```bash
yx state "task name" blocked --format json
```

### States

- `todo` - Not started (default)
- `wip` - In progress
- `blocked` - Waiting on something
- `done` - Complete

### Workflow for AI Agents

1. **Check ready work**: `yx list --only not-done` shows what needs doing
2. **Start your task**: `yx start "task name"`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create a linked task:
   - `yx add "Found issue" --under "current task name"`
5. **Complete**: `yx done "task name"`
6. **Sync**: `yx sync` to share via git

### Important Rules

- ✅ Use yaks for ALL task tracking
- ✅ Use `--format json` for programmatic use
- ✅ Nest discovered work under the current task with `--under`
- ✅ Check `yx list --only not-done` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - Push your branch, but **never auto-push `master`** (it deploys to MELPA):
   ```bash
   git pull --rebase
   yx sync
   # Only push if NOT on master:
   [[ "$(git branch --show-current)" != "master" ]] && git push
   git status
   ```
   If on `master`, stop here and ask the user to review before pushing.
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- On any branch other than `master`: push automatically, work is NOT complete until `git push` succeeds
- On `master`: do NOT push — ask the user to review first, as `master` auto-deploys to MELPA
- NEVER say "ready to push when you are" on a feature branch — YOU must push
- If push fails, resolve and retry until it succeeds
