# Unified Reactivation System Design

## Overview

Implement a unified system for saving and restoring GTD item state when moving items to/from someday/tickler. Currently only projects preserve state; this extends the pattern to all GTD types.

## Problem

When a delegated item goes to someday/maybe:
- Current: ORG_GTD becomes "Someday", DELEGATED_TO and ORG_GTD_TIMESTAMP are lost
- Desired: State is preserved and restored on reactivation

## Design

### Core Principle

The type system (`org-gtd-types`) defines what properties belong to each type via `:properties`. This is the source of truth for what needs to be preserved.

### State Preservation

**When someday-ing/tickler-ing an item:**

1. Check if current ORG_GTD exists and is not already someday/tickler
2. If so, save:
   - `PREVIOUS_ORG_GTD` ← current ORG_GTD value
   - `PREVIOUS_TODO` ← current TODO keyword
   - For each property in the type's `:properties`:
     - `PREVIOUS_<org-property>` ← current value

**When reactivating:**

1. If `PREVIOUS_ORG_GTD` is nil → call `org-gtd-clarify-item` (no previous state)
2. If `PREVIOUS_ORG_GTD` exists:
   - Clear someday/tickler properties (ORG_GTD_TIMESTAMP)
   - Restore ORG_GTD from PREVIOUS_ORG_GTD
   - Restore TODO from PREVIOUS_TODO
   - For each type-specific property, prompt user to confirm/update:
     - Show: "When to check in? [2024-01-15]: "
     - User can hit RET to keep or type new value
   - Delete all PREVIOUS_* properties

### API

**New module: org-gtd-reactivate.el**

```elisp
(defun org-gtd-save-state ()
  "Save current GTD state to PREVIOUS_* properties.
Only saves if item has ORG_GTD that is not someday/tickler.")

(defun org-gtd-restore-state ()
  "Restore GTD state from PREVIOUS_* properties.
Prompts user to confirm/update each type-specific property.")

(defun org-gtd-reactivate ()
  "Reactivate a someday/tickler item at point.
Interactive command that validates item type and calls restore-state.")
```

### Integration

**org-gtd-someday.el:**
- Call `org-gtd-save-state` in `org-gtd-someday--configure` before setting type
- Remove `org-gtd-someday-activate`

**org-gtd-tickler.el:**
- Call `org-gtd-save-state` in `org-gtd-tickler--configure` before setting type
- Remove `org-gtd-reactivate`

**org-gtd-projects.el:**
- `org-gtd-project--save-state` iterates tasks, calls `org-gtd-save-state` on each
- `org-gtd-project--restore-state` iterates tasks, calls `org-gtd-restore-state` on each
- Keep `org-gtd-project-reactivate` for project-specific iteration + dependency recalculation

### Properties by Type

| Type | Properties to Save |
|------|-------------------|
| delegated | DELEGATED_TO, ORG_GTD_TIMESTAMP |
| calendar | ORG_GTD_TIMESTAMP |
| habit | SCHEDULED, STYLE |
| next-action | (none) |
| project | (none - but tasks have their own) |
| reference | (none) |
| quick-action | (none) |

### Test Cases

1. Delegated → someday → reactivate: restores DELEGATED_TO and timestamp with prompts
2. Calendar → tickler → reactivate: restores timestamp with prompt
3. Inbox → someday → reactivate: calls clarify (no previous state)
4. Project → tickler → reactivate: all tasks restored (existing behavior)
5. Habit → someday → reactivate: restores SCHEDULED and STYLE with prompts
6. Single action → someday → reactivate: restores TODO state, no property prompts
