# /architect — Design Interview

Use when requirements exist and you need to design the technical solution before implementing.

## Behavior

### 1. Find Requirements

Look for the most recent requirements doc in `docs/plans/`:
```
docs/plans/*-requirements.md
```

If multiple exist, ask the user which one to use. If none exist, ask the user to provide requirements or suggest running `/define` first.

### 2. Explore the Codebase

Before proposing anything, understand:
- **Existing patterns**: How does the codebase handle similar features?
- **Relevant modules**: Which files will be touched?
- **Dependencies**: What does this interact with?
- **Test patterns**: How are similar features tested?

Use `Grep`, `Glob`, and `Read` to explore. Be thorough — bad designs come from not understanding the codebase.

### 3. Propose Approaches

Present 2-3 approaches with tradeoffs. Lead with your recommendation.

For each approach:
- **Summary**: One sentence
- **How it works**: Key components and their interactions
- **Pros**: Why this is good
- **Cons**: Why this might not be ideal
- **Risk**: What could go wrong

### 4. Interview the User

Use `AskUserQuestion` to discuss:
- Which approach do they prefer?
- Any constraints you should know about?
- Acceptable tradeoffs?

One question at a time. Validate understanding before moving on.

### 5. Produce Design Document

Present the design in sections, validating each with the user:

1. **Overview**: What we're building and why this approach
2. **Components**: Each new/modified module with its responsibility
3. **Data Flow**: How data moves through the system
4. **API Surface**: Public functions, their signatures and behavior
5. **Error Handling**: What can go wrong and how we handle it
6. **Testing Strategy**: What to test, how, at what level
7. **Migration**: Any backward compatibility concerns

Design principles to consider:
- **DDD alignment**: Does the code model the domain naturally?
- **SOLID**: Especially SRP and dependency inversion
- **Language idioms**: Does it feel like idiomatic Emacs Lisp?
- **YAGNI**: Are we building only what's needed?

### 6. Save and Commit

Derive the name from the requirements doc (same `<name>` slug).

Save to: `docs/plans/YYYY-MM-DD-<name>-design.md`

Commit with message: `docs: add <name> design`

### 7. Next Step

Tell the user:
> Design saved. When ready, use `/implement` to plan and execute the implementation.

## Rules

- **Explore first.** Never propose a design without reading the relevant code.
- **Lead with recommendation.** Don't present options equally — have an opinion.
- **Validate sections.** Get user approval on each section before moving on.
- **Be concrete.** Name actual files, functions, and data structures — not abstractions.
- **Consider testing.** If a design is hard to test, it's probably a bad design.
