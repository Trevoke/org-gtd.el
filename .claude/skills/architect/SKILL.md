---
name: architect
description: Use when requirements exist and a technical design is needed before implementation begins
---

# /architect — Design Interview

## Overview

**Design the technical solution through conversation.** Propose approaches with tradeoffs, let the user choose, then detail the chosen design.

**Core principle:** Lead with options, not answers. The user picks the direction.

## When to Use

- Requirements exist (from `/define` or provided by user)
- Technical approach needs to be decided before coding
- Multiple valid designs exist and tradeoffs matter

## The Process

### 1. Find Requirements

Look for the most recent `*-requirements.md` in `docs/plans/`.

If none found or multiple exist, ask the user which to use. If no requirements exist at all, suggest `/define` first.

### 2. Explore the Codebase

Before proposing anything, understand what exists:
- How does the codebase handle similar features?
- Which files will be touched?
- What patterns are already established?
- How are similar features tested?

**Name specific files and patterns you found.** Don't hand-wave about "the existing architecture."

### 3. Propose 2-3 Approaches

For each approach:

```markdown
### Approach A: [Name] (Recommended)

**Summary:** One sentence.
**How it works:** Key components and interactions.
**Pros:** Why this is good.
**Cons:** Why this might not be ideal.
**Risk:** What could go wrong.
**Fits existing patterns:** How it aligns with what's already in the codebase.
```

**Lead with your recommendation** but explain why the others exist.

### 4. Interview — Let the User Choose

Use `AskUserQuestion`:
- Which approach do they prefer?
- Any constraints you missed?
- Acceptable tradeoffs?

**One question at a time.** Don't dump all decisions at once.

### 5. Detail the Chosen Design

Present in sections, **validating each with the user** before moving on:

| Section | Content |
|---------|---------|
| **Overview** | What we're building and why this approach |
| **Components** | Each new/modified module with its responsibility |
| **Data Flow** | How data moves through the system |
| **API Surface** | Public functions — signatures and behavior |
| **Error Handling** | What can go wrong and how we handle it |
| **Testing Strategy** | What to test, at what level |

Design principles to consider:
- Does the code model the domain naturally? (DDD)
- Single responsibility — each module does one thing
- Does it feel like idiomatic Emacs Lisp?
- Are we building only what's needed? (YAGNI)

### 6. Save and Commit

Derive name from requirements doc (same `<name>` slug).

```
docs/plans/YYYY-MM-DD-<name>-design.md
```

Commit: `docs: add <name> design`

### 7. Next Step

> Design saved. When ready, use `/implement` to plan and execute the implementation.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Presenting one design as "the answer" | Always propose 2-3 approaches with tradeoffs |
| Not reading the codebase first | Explore before proposing. Name specific files and patterns. |
| Designing without interviewing | Ask the user which approach. They decide direction. |
| Including full implementation code | Design describes WHAT and WHY. `/implement` handles HOW. |
| Producing the entire design in one shot | Present section by section, validate each with user. |
| Hand-waving about "existing patterns" | Be specific: "org-gtd-organize.el uses transient at line 42" |
| Not referencing next step | Always end with: "use `/implement` when ready" |
