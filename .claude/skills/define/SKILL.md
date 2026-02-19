---
name: define
description: Use when a new feature request, bug report, or work request needs requirements defined before implementation
---

# /define — Product Owner Interview

## Overview

**Define requirements by interviewing the user.** The interview IS the value — a requirements doc produced without conversation is just guessing.

**Core principle:** Ask, don't assume. Every assumption is a question you didn't ask.

## When to Use

- User describes a new feature, bug, or change request
- Work needs to be broken down before architecture or implementation
- You need to understand scope, personas, and success criteria

## The Process

### 1. Gather Context (silent — don't show this to user)

- Read CLAUDE.md and recent commits
- Identify relevant source files for the topic
- Note existing patterns and constraints

### 2. Interview — One Question at a Time

Use `AskUserQuestion` with multiple-choice options where possible.

**You MUST ask questions. You MUST NOT produce requirements without interviewing.**

Cover these areas, one question per turn:

| Area | What to Ask |
|------|-------------|
| **Scope** | What should this do? What should it NOT do? |
| **Personas** | Who uses this? What's their context/skill level? |
| **Success** | How will we know it works? What does "done" look like? |
| **Edge cases** | What happens with empty/invalid/unexpected input? |
| **Constraints** | Backward compat? Performance? Dependencies? |

**After each area**, summarize what you heard (200-300 words) and ask the user to confirm before moving on.

### 3. Produce User Stories

Use INVEST format. Each story must be:
- **I**ndependent, **N**egotiable, **V**aluable, **E**stimable, **S**mall, **T**estable

Template:
```markdown
### Story N: [Title]

**As a** [persona from interview],
**I want** [functionality],
**So that** [benefit].

#### Acceptance Criteria

```gherkin
Given [context]
When [action]
Then [expected result]
```

#### Edge Cases

```gherkin
Given [edge case context]
When [action]
Then [expected handling]
```
```

Present stories to user for validation before finalizing.

### 4. Save and Commit

Ask user for a short name, or derive from the topic.

```
docs/plans/YYYY-MM-DD-<name>-requirements.md
```

Commit: `docs: add <name> requirements`

### 5. Next Step

> Requirements saved. When ready, use `/architect` to design the solution.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Producing requirements without interviewing | STOP. Ask questions first. Always. |
| Asking multiple questions at once | One question per `AskUserQuestion` call |
| Including implementation/architecture details | Stay in problem space. Code design is `/architect`'s job. |
| Listing "open questions" without asking them | If it's a question, ASK the user |
| Using FR-1/AC-1.1 format instead of user stories | Use "As a [persona]..." with Gherkin criteria |
| Assuming scope instead of scoping with user | Every assumption is a question you skipped |
| Massive document from a one-line request | Interview to right-size. Small request may mean small scope. |
