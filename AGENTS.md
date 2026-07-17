# Agent Instructions for org-gtd.el

This document provides guidance for AI agents working on the org-gtd.el project.

## Branching Model

This repo uses a light git-flow with **three** long-lived branches. The
guiding rule: **v5 work never lands on `develop`.**

- **`master`** — the current **4.x stable release** line. MELPA's *unstable*
  channel tracks it, and git tags (`4.6.1`, `5.0.0`, …) are the MELPA *stable*
  releases. **Pushing `master` triggers a MELPA deploy — treat it as a release
  action** (see "Landing the Plane"). Only bugfixes and docs land here.
- **`develop`** — the **4.x main line**. Based on `master`; it is where work
  happens more safely, and it feeds `master` before a tag. Carries 4.x bugfixes
  and genuinely-additive 4.x features *if we need them*. **No v5 work.** Pushing
  `develop` is safe (no deploy).
- **`org-gtd-5`** — the **v5 development trunk**. *All* v5 work lives here:
  breaking changes **and** new v5 features (View Manager, guided review,
  checklists, init, the unified type/`create-item` API, the v5 journal). Feature
  branches for v5 work branch **off `org-gtd-5`** and merge **back into
  `org-gtd-5`**. When 5.0 is ready, `org-gtd-5` merges into `develop` (→
  `master`) for release. Pushing `org-gtd-5` is safe (no deploy).

### Where does my work go?

- **4.x bugfix** → branch off `develop`, merge back into `develop` (it flows to
  `master` at release).
- **v5 feature or breaking change** → branch off `org-gtd-5`, merge back into
  `org-gtd-5`.
- About to put a new feature on `develop`? Stop and ask: is this 4.x or 5.0?
  **When in doubt, it's 5.0 → `org-gtd-5`.**

### Releasing

- **4.x patch/minor:** land on `develop`, merge `develop` → `master`, bump the
  version (see CLAUDE.md "Creating a new release"), tag, push `master` + tag.
- **5.0 (major):** merge `org-gtd-5` → `develop` → `master`, bump to `5.0.0`,
  tag, push `master` + tag.

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
