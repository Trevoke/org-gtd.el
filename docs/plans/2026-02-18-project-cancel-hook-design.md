# Project Cancel Hook Design

**Date:** 2026-02-18
**Requirements:** `2026-02-18-project-cancel-hook-requirements.md`
**Approach:** A — Confirmation in `org-gtd-project-cancel` (single source of truth)

## Overview

Centralize project cancellation in `org-gtd-project-cancel` so it becomes the single source of truth. It gains two new responsibilities: (1) always prompt for confirmation with the project heading name, and (2) set the project heading itself to CNCL. A new hook function detects when a user manually sets CNCL on a project heading via `C-c C-t` and delegates to the same function.

Every entry point (manual state change, agenda command, graph view command) gets consistent behavior for free. The existing confirmation in `org-gtd-project-cancel-from-context` becomes redundant and is removed.

**Scope:** Two files modified (`org-gtd-projects.el`, `org-gtd-mode.el`). No new files. Issue #283 is resolved by ensuring cancellation always happens, not by changing the review filter.

## Components

### Modified: `org-gtd-project-cancel` (org-gtd-projects.el:342)

Currently: Cancels child tasks only, no confirmation, no heading state change.

After:
- Prompts `"Cancel project '<heading>'? "` using `yes-or-no-p`
- If confirmed: sets the project heading to CNCL (with guard), then cancels incomplete child tasks
- If declined: does nothing (no-op)
- Uses guard variable `org-gtd-project--cancel-in-progress` to suppress hook when it programmatically sets heading to CNCL

### Modified: `org-gtd-project-cancel-from-context` (org-gtd-projects.el:318)

Currently: Has its own `yes-or-no-p`, then delegates.

After: Removes its own confirmation. Navigates to project heading and calls `org-gtd-project-cancel` (which now handles confirmation).

### Modified: `org-gtd-project-cancel-from-agenda` (org-gtd-projects.el:358)

No changes needed — already delegates to `org-gtd-project-cancel`, which now handles confirmation.

### New: `org-gtd-project--maybe-cancel-from-hook` (org-gtd-projects.el)

Hook function for `org-after-todo-state-change-hook`. Follows the pattern of `org-gtd-single-action--maybe-convert-to-delegated` (org-gtd-single-action.el:77).

Detection: `org-state` = `(org-gtd-keywords--canceled)` AND `(org-entry-get (point) "ORG_GTD")` = `(org-gtd-type-org-gtd-value 'project)`

Behavior:
- If `org-gtd-project--cancel-in-progress` is non-nil, return immediately (guard)
- Prompt confirmation with project heading name
- If yes: call `org-gtd-project-cancel` (handles child tasks + heading state)
- If no: revert heading to `org-last-state` using guard to suppress re-triggering

### New: `org-gtd-project--cancel-in-progress` (org-gtd-projects.el)

A `defvar` guard variable, initially nil. Bound to `t` by `org-gtd-project-cancel` while it sets the heading state, and by the hook function when it reverts the heading state. Prevents the hook from re-triggering during programmatic state changes.

### Modified: `org-gtd-mode.el` hook registration

Add/remove `org-gtd-project--maybe-cancel-from-hook` in `org-gtd--enable-org-gtd-mode` and `org-gtd--disable-org-gtd-mode`, alongside the existing three hooks.

## Data Flow

### Path 1: User manually sets CNCL via C-c C-t

```
User sets heading to CNCL
  → org-after-todo-state-change-hook fires
  → org-gtd-project--maybe-cancel-from-hook runs
  → Detects: org-state=CNCL, ORG_GTD="Projects", guard=nil
  → Prompts "Cancel project '<heading>'?"
  → YES:
      → Calls org-gtd-project-cancel
        → Sets guard=t
        → Heading already CNCL, skips (org-todo) since state matches
        → Cancels incomplete child tasks
        → Clears guard
  → NO:
      → Sets guard=t
      → Reverts heading to org-last-state via (org-todo org-last-state)
      → Hook fires again but guard=t, so no-op
      → Clears guard
```

### Path 2: User calls org-gtd-project-cancel from agenda or graph view

```
User invokes command
  → Finds project marker (via agenda item or graph context)
  → Calls org-gtd-project-cancel at project heading
  → Prompts "Cancel project '<heading>'?"
  → YES:
      → Sets guard=t
      → Sets heading to CNCL via (org-todo)
      → Hook fires but guard=t, so no-op
      → Cancels incomplete child tasks
      → Clears guard
  → NO:
      → Does nothing
```

## API Surface

### Modified public function

```elisp
(defun org-gtd-project-cancel ()
  "Cancel the project at point, with confirmation.
Prompts the user to confirm, then sets the project heading to CNCL
and marks all incomplete child tasks as CNCL.")
```

Behavior change: Now prompts for confirmation, sets heading to CNCL, then cancels children. Previously was fire-and-forget for child tasks only.

### New private function

```elisp
(defun org-gtd-project--maybe-cancel-from-hook ()
  "Hook function for `org-after-todo-state-change-hook'.
When a project heading is changed to CNCL, prompts for confirmation
and cancels child tasks. If declined, reverts the heading state.")
```

### New variable

```elisp
(defvar org-gtd-project--cancel-in-progress nil
  "Guard variable to prevent re-entrant cancellation prompts.")
```

### Modified public function (simplified)

```elisp
(defun org-gtd-project-cancel-from-context ()
  "Cancel the current project from graph view or agenda context."
  ;; No longer has its own yes-or-no-p — delegates to org-gtd-project-cancel
  )
```

## Error Handling

| Scenario | Handling |
|----------|----------|
| Heading already CNCL when `org-gtd-project-cancel` called | Skip `(org-todo)` call (check current state first), still prompt, still cancel children |
| No incomplete child tasks | Loop over empty list — no error, no-op for children |
| Error mid-cancel leaves guard set | `unwind-protect` ensures guard is always cleared |
| Hook fires on non-project heading set to CNCL | Detection check (`ORG_GTD` = "Projects") filters it out |
| `org-last-state` is nil | Only revert if `org-last-state` is non-nil |

## Testing Strategy

### Unit tests for `org-gtd-project-cancel`

1. **Confirms and cancels**: Simulated "yes" → heading CNCL, child tasks CNCL
2. **Declines and does nothing**: Simulated "no" → heading unchanged, children unchanged
3. **Heading already CNCL**: Still prompts, still cancels children, no error
4. **No incomplete children**: Prompts, sets heading CNCL, no error

### Unit tests for hook function

5. **Manual CNCL on project — confirmed**: Hook fires, prompts, cancels children
6. **Manual CNCL on project — declined**: Hook fires, prompts, reverts heading, no child changes
7. **Guard prevents re-prompt on revert**: Revert's second hook fire is suppressed
8. **Guard prevents re-prompt on programmatic cancel**: `org-gtd-project-cancel` sets heading, hook suppressed
9. **Non-project heading set to CNCL**: Hook does nothing

### Integration tests

10. **Cancel from agenda**: Agenda item → project found → confirmed → all cancelled
11. **Cancel from graph view**: Context marker → confirmed → all cancelled

Test tooling: `with-simulated-input` for yes/no responses.
