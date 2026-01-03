# Someday/Maybe Review Feature

**Date:** 2025-12-13
**Status:** Design approved

## Overview

A wizard-style review workflow for someday/maybe items during GTD reflect. Users iterate through items one-at-a-time in a read-only WIP buffer, choosing to clarify (reactivate) or defer (keep for later).

## Entry Point

```elisp
(org-gtd-reflect-someday-review &optional list)
```

- **Interactive**: Prompts for list selection (if `org-gtd-someday-lists` is populated)
- **Programmatic**: Pass LIST name directly to skip prompt
- If LIST is nil and config is nil: reviews all someday items

## Configuration

### Customization Variable

```elisp
(defcustom org-gtd-someday-lists nil
  "List of someday/maybe list names for review grouping.
When nil, all someday items are reviewed together without prompting.
When populated, user is prompted to select which list to review."
  :type '(repeat string)
  :group 'org-gtd)
```

### Item Property

`ORG_GTD_SOMEDAY_LIST` - Optional property on items to categorize them into lists.

## Selection Flow

1. If `org-gtd-someday-lists` is nil → review all someday items (no prompt)
2. If populated → completing-read with:
   - All configured list names
   - "Unassigned" option (items without `ORG_GTD_SOMEDAY_LIST` property)

## Review Flow (WIP Buffer)

### Buffer Characteristics

- Read-only (view-only, no editing)
- Header-line shows available actions: `[d] Defer  [c] Clarify  [q] Quit`
- Item content displayed for review

### Keybindings

| Key | Action |
|-----|--------|
| `d` | Defer - Add LOGBOOK entry, advance to next item |
| `c` | Clarify - Call `org-gtd-reactivate`, then advance to next |
| `q` | Quit - Exit review session |

### Defer Action

Adds silent timestamped entry to LOGBOOK drawer:

```org
* Some someday item
:LOGBOOK:
- Reviewed [2025-12-13 Fri 14:30]
- Reviewed [2025-11-15 Fri 10:00]
:END:
```

Preserves full review history for tracking stale items.

### Clarify Action

1. Calls `org-gtd-reactivate` which:
   - Restores previous state from `PREVIOUS_*` properties, OR
   - Triggers fresh `org-gtd-clarify-item` if no previous state
2. User completes the organize flow (picks new type, refiles)
3. Automatically returns to review and advances to next item

## Completion

When all items are processed:
- Display message: `"Review complete. 5 items reviewed, 2 clarified."`
- Kill WIP buffer
- Return to previous window

## Organize Flow Update

When organizing an item as someday/maybe via `org-gtd-someday`:

```elisp
;; Only prompt when lists are configured
(when org-gtd-someday-lists
  (let ((list (completing-read "Someday list: " org-gtd-someday-lists)))
    (org-entry-put nil "ORG_GTD_SOMEDAY_LIST" list)))
```

Zero friction for simple setups - only adds prompt when user has opted into the list system.

## GTD Flow Integration

| Step | Impact |
|------|--------|
| **Capture** | No change |
| **Clarify** | No change |
| **Organize** | Prompt for list when choosing Someday/Maybe (if lists configured) |
| **Engage** | N/A (someday items aren't actionable) |
| **Reflect** | New `org-gtd-reflect-someday-review` command |

## Implementation Architecture

### Review State Persistence

To support returning to review after an organize flow completes:

```elisp
(defvar org-gtd--someday-review-state nil
  "State for active someday review session.
Plist with :queue (list of org-ids), :position (current index),
:list-name (which list being reviewed), :stats (review statistics).")
```

The organize completion hooks check if review is active and resume.

### Files to Modify

1. `org-gtd-someday.el` - Add list prompt during organize
2. `org-gtd-reflect.el` - Add `org-gtd-reflect-someday-review` command
3. New: `org-gtd-someday-review.el` - Review minor mode, WIP buffer handling, state management
4. `org-gtd-organize-core.el` - Hook to resume review after organize
5. `test/unit/someday-test.el` - Unit tests for list property
6. `test/unit/someday-review-test.el` - Review flow tests
7. `doc/org-gtd.org` - Documentation

## Edge Cases

- **Empty list**: If selected list has no items, show message and return
- **Unassigned items**: Shown as separate "Unassigned" option in completing-read
- **Quit mid-review**: Clean exit, no partial state left behind
- **Item deleted during review**: Skip gracefully, continue with next

## Example Usage

### Simple (no lists configured)

```elisp
;; Review all someday items
(org-gtd-reflect-someday-review)
```

### With lists configured

```elisp
;; Configuration
(setq org-gtd-someday-lists '("Work Ideas" "Personal Projects" "Learning"))

;; Interactive - will prompt for list
(org-gtd-reflect-someday-review)

;; Programmatic - skip prompt
(org-gtd-reflect-someday-review "Work Ideas")
```

### Finding stale items

Future enhancement: view spec filter for items not reviewed in N days:

```elisp
(org-gtd-view-show
 '((name . "Stale Someday Items")
   (type . someday)
   (not-reviewed-since . 90)))  ;; days
```
