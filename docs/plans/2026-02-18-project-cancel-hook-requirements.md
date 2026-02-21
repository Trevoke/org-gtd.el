# Project Cancel Hook Requirements

**Date:** 2026-02-18
**Context:** GitHub issue #283 — tasks from cancelled projects appear in stuck single action review because cancellation wasn't triggered automatically.

## Problem

When a user manually sets a project heading to CNCL, org-gtd does nothing — child tasks remain in their current state. The user must separately call `org-gtd-project-cancel` to cancel child tasks. If they forget, orphaned tasks pollute reviews.

Additionally, `org-gtd-project-cancel` itself doesn't set the project heading to CNCL, leading to inconsistent state.

## Stories

### Story 1: Confirm before cancelling a project

**As a** GTD practitioner,
**I want** org-gtd to always confirm before cancelling a project,
**So that** I don't accidentally cancel a project and all its tasks.

#### Acceptance Criteria

```gherkin
Given a project heading with ORG_GTD="Projects"
When the user changes the heading's TODO state to CNCL (manually via C-c C-t)
Then org-gtd prompts "Cancel project '<project heading>'?"
```

```gherkin
Given org-gtd-project-cancel is called from the agenda
Then org-gtd prompts "Cancel project '<project heading>'?"
```

```gherkin
Given org-gtd-project-cancel is called from the graph view
Then org-gtd prompts "Cancel project '<project heading>'?"
```

```gherkin
Given the confirmation prompt is showing
When the user answers yes
Then all incomplete child tasks are set to CNCL
And the project heading is set to CNCL
```

```gherkin
Given the confirmation prompt for a manual CNCL state change
When the user answers no
Then the project heading reverts to its previous TODO state
And no child tasks are modified
```

#### Edge Cases

```gherkin
Given the user answers no and the heading reverts via org-todo
When the revert triggers org-after-todo-state-change-hook
Then the hook does NOT re-prompt (guard suppresses only the revert)
```

### Story 2: org-gtd-project-cancel sets project heading to CNCL

**As a** GTD practitioner,
**I want** `org-gtd-project-cancel` to also cancel the project heading itself,
**So that** the project and its tasks are in a consistent state.

#### Acceptance Criteria

```gherkin
Given a project heading in NEXT state with child tasks
When org-gtd-project-cancel is called (and user confirms)
Then the project heading is set to CNCL
And all incomplete child tasks are set to CNCL
```

```gherkin
Given a project heading already in CNCL state
When org-gtd-project-cancel is called
Then only the confirmation is shown (heading already CNCL)
And incomplete child tasks are set to CNCL
```

## Technical Context

- `org-after-todo-state-change-hook` is the detection mechanism (3 existing handlers in `org-gtd-mode.el`)
- Hook provides `org-state` (new) and `org-last-state` (previous) as dynamic variables
- Detection condition: `org-state` = CNCL and `ORG_GTD` = "Projects"
- Revert uses `org-last-state` with a guard variable to prevent re-prompting
- Confirmation message always includes project heading name regardless of entry point
