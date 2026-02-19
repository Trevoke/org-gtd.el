# /define — Product Owner Interview

Use when a new work request, feature request, or requirement needs to be broken down into development-ready user stories.

## Behavior

### 1. Gather Context
- Read recent commits (`git log --oneline -20`)
- Read CLAUDE.md for project context
- Identify relevant source files based on the user's topic

### 2. Interview the User

**This is the core of the skill.** Never produce requirements without interviewing first.

Ask questions **one at a time** using `AskUserQuestion` with multiple-choice options where possible. Cover:

- **Scope**: What exactly should this do? What should it NOT do?
- **Personas**: Who uses this? What's their context?
- **Success criteria**: How will we know it works?
- **Edge cases**: What happens with empty input? Concurrent use? Errors?
- **Constraints**: Performance requirements? Backward compatibility? Dependencies?

Present requirements in sections (200-300 words each), validating each section with the user before moving on.

### 3. Produce Requirements Document

Create INVEST user stories:
- **I**ndependent: each story stands alone
- **N**egotiable: room for implementation choices
- **V**aluable: delivers user value
- **E**stimable: small enough to estimate
- **S**mall: completable in one session
- **T**estable: has clear acceptance criteria

Each story follows:
```
### Story N: [Title]

**As a** [persona],
**I want** [functionality],
**So that** [benefit].

#### Acceptance Criteria

```gherkin
Given [context]
When [action]
Then [expected result]
```
```

### 4. Save and Commit

Ask the user for a short name for this work (or derive one from the topic).

Save to: `docs/plans/YYYY-MM-DD-<name>-requirements.md`

Commit the file with message: `docs: add <name> requirements`

### 5. Next Step

Tell the user:
> Requirements saved. When ready, use `/architect` to design the solution.

## Rules

- **Interview first.** The interview IS the value — don't skip it.
- **One question at a time.** Don't overwhelm with multi-part questions.
- **Validate sections.** Present each section and get user approval before continuing.
- **Be concrete.** Vague stories like "handle errors gracefully" are not testable.
- **Include edge cases.** Every story needs at least one edge case in its acceptance criteria.
