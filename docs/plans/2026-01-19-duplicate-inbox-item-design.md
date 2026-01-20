# Design: Duplicate Inbox Item During Clarify/Process

**Date:** 2026-01-19
**Issue:** #275
**Status:** Ready for implementation

## Overview

Allow users to duplicate an item during clarification when one inbox item needs to become multiple GTD outcomes. Duplicates form a queue that must be fully processed before returning to the previous context.

## Mental Model

When clarifying an item, the user may realize it represents multiple outcomes. They can "spawn" duplicates that form a temporary queue. All spawned items must be clarified and organized before returning to the previous context.

Duplicates are held in memory only—they never touch the inbox unless the user cancels and explicitly saves them.

## User Interface

### Keybindings

In `org-gtd-clarify-mode-map`:

| Key | Command | Behavior |
|-----|---------|----------|
| `d` | `org-gtd-clarify-duplicate` | Duplicate with rename prompt |
| `D` | `org-gtd-clarify-duplicate-exact` | Exact copy, no prompt |

### Rename Prompt

When user presses `d`:
- Minibuffer prompts: "Duplicate title: "
- Current item's title is the default value
- User edits and presses RET
- Standard Emacs `read-string` behavior (history with `M-p`, etc.)

### Queue Window

- **Position:** Configurable via `org-gtd-clarify-duplicate-queue-position`, default `bottom`
- **Appears:** Immediately on first duplicate
- **Content:** Numbered list with count header
- **Interactivity:** Read-only, informational only (FIFO processing)
- **Closes:** Automatically when queue empties

**Format:**
```
Pending (2):
  1. Book train tickets
  2. Prepare meeting notes
```

**Buffer name:** `*Org GTD Duplicate Queue*`

### Echo Messages

- After duplicate: "Duplicated: <title>" (or similar confirmation)
- When queue empties: "All duplicates processed"

### Cancel Behavior

When user presses `C-c C-k` with pending duplicates:

1. Display prompt showing queued items:
   ```
   2 pending duplicates:
     - Book train tickets
     - Prepare meeting notes
   [d]iscard all  [s]ave to inbox  [c]ancel
   ```

2. User chooses:
   - **Discard:** Queue cleared, nothing saved
   - **Save to inbox:** All duplicates appended to `org-gtd-inbox-path`
   - **Cancel:** Return to clarify buffer (abort the abort)

## Workflow

### Basic Flow

```
1. User clarifies item X (from anywhere)
2. User presses 'd' → prompted for title → duplicate X' queued, window appears
3. User presses 'd' again → duplicate X'' queued, window updates
4. User organizes X
5. Automatically: X' opens for clarification
6. User organizes X'
7. Automatically: X'' opens for clarification
8. User organizes X''
9. Queue window closes, "All duplicates processed" message
10. Return to original context (next inbox item or previous window config)
```

### During Inbox Processing

When duplicates are created during `org-gtd-process-inbox`:
- Duplicates are processed immediately after the current item
- Then inbox processing continues with the next inbox item
- Duplicates are "inserted" into the flow, not added to inbox for later

### Nested Duplicates

While clarifying a duplicate, user can create more duplicates:
- New duplicates append to end of queue
- Queue window updates to show new items
- All must be processed before returning

## Implementation

### New Customization

```elisp
(defcustom org-gtd-clarify-duplicate-queue-position 'bottom
  "Position for the duplicate queue window during clarification.
Values can be: top, right, left, bottom."
  :group 'org-gtd-clarify
  :package-version '(org-gtd . "4.4.0")
  :type '(choice (const top) (const right) (const left) (const bottom)))
```

### New Buffer-Local Variables

```elisp
(defvar-local org-gtd-clarify--duplicate-queue nil
  "List of pending duplicate items to clarify after current item.
Each element is a plist with :title and :content keys.")
```

### New Functions

| Function | Purpose |
|----------|---------|
| `org-gtd-clarify-duplicate` | Interactive: duplicate with rename prompt |
| `org-gtd-clarify-duplicate-exact` | Interactive: exact copy |
| `org-gtd-clarify--queue-add` | Add item to queue, update window |
| `org-gtd-clarify--queue-pop` | Remove and return next item from queue |
| `org-gtd-clarify--queue-display` | Create/refresh the queue window |
| `org-gtd-clarify--queue-cleanup` | Close window, clear state |
| `org-gtd-clarify--queue-empty-p` | Predicate: is queue empty? |
| `org-gtd-clarify--queue-save-to-inbox` | Save all queued items to inbox |

### Integration Points

1. **After `org-gtd-organize` completes:**
   - Check if `org-gtd-clarify--duplicate-queue` has items
   - If yes: pop next item, initialize new clarify buffer with it
   - If no: proceed with existing continuation logic

2. **In `org-gtd-clarify-stop`:**
   - Check if queue has items
   - If yes: show discard/save prompt
   - Handle user's choice before cleanup

### Validation

Before allowing duplicate:
- Check that WIP buffer has content (at least one heading with non-empty title)
- If empty, show message: "Nothing to duplicate" and abort

## Edge Cases

| Case | Behavior |
|------|----------|
| Empty WIP buffer | Disallow duplication, show message |
| Cancel with queue | Prompt: discard or save to inbox |
| Nested duplicates | Allowed, appended to queue |
| Window position conflict | Standard Emacs window stacking |
| Duplicate while not in clarify-mode | Command not available (keymap scoped) |

## Future Considerations

- Smarter window layout when multiple side windows are active
- Option to reorder queue (deliberately not included in v1)
- Visual indicator in header-line as alternative to side window

## Testing Checklist

- [ ] `d` prompts for title, creates duplicate with new title
- [ ] `D` creates exact duplicate without prompt
- [ ] Queue window appears on first duplicate
- [ ] Queue window shows correct count and numbered list
- [ ] Organizing current item auto-opens next queued item
- [ ] Queue window closes when last item organized
- [ ] Echo message appears when queue empties
- [ ] Cancel with queue shows discard/save prompt
- [ ] Discard clears queue, saves nothing
- [ ] Save to inbox appends all queued items
- [ ] Empty buffer prevents duplication
- [ ] Works during inbox processing (duplicates processed before next inbox item)
- [ ] Nested duplicates work (duplicate while clarifying a duplicate)
- [ ] Window position respects customization
