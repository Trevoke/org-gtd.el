---
name: brainstorming
description: Use when starting new feature work to turn ideas into fully formed designs through collaborative dialogue
---

# Brainstorming

Help turn ideas into fully formed designs through collaborative dialogue.

## Process

### 1. Understand Context

Check out the current project state first:
- Read relevant files and docs
- Check recent commits
- Understand existing patterns

### 2. Ask Clarifying Questions

Ask questions one at a time to refine the idea:
- Prefer multiple choice questions when possible
- Only one question per message
- Focus on: purpose, constraints, success criteria

### 3. Explore Approaches

Propose 2-3 different approaches with trade-offs:
- Lead with your recommended option
- Explain reasoning for recommendation
- Be clear about trade-offs

### 4. Present Design

Once you understand what you're building:
- Break into sections of 200-300 words
- Ask after each section if it looks right
- Cover: architecture, components, data flow, error handling, testing
- Be ready to go back and clarify

## Key Principles

- **One question at a time** - Don't overwhelm with multiple questions
- **Multiple choice preferred** - Easier to answer than open-ended
- **YAGNI ruthlessly** - Remove unnecessary features from all designs
- **Explore alternatives** - Always propose 2-3 approaches before settling
- **Incremental validation** - Present design in sections, validate each

## Output

Design document saved to `docs/plans/YYYY-MM-DD-<topic>-design.md`

After design is approved:
1. Commit the design document
2. Ask if ready to proceed to implementation planning
