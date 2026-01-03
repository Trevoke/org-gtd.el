# Organize Help Buffer Design

**Date:** 2025-01-28
**Status:** Approved

## Overview

Add an optional helper buffer that displays GTD organization type descriptions during clarification. Follows the same pattern as the horizons window.

## Design Decisions

| Aspect | Decision |
|--------|----------|
| Content | Brief org headings with purpose + key properties |
| Display timing | User-toggled, off by default |
| Content source | Elisp constant (not a file) |
| Window position | Configurable via separate variable |
| File location | `org-gtd-clarify.el` |

## Public Interface

### Customization Variable

```elisp
(defcustom org-gtd-clarify-show-organize-help nil
  "If non-nil, position to display the organize types help buffer.
Values: nil, 'right, 'left, 'top, 'bottom."
  :group 'org-gtd-clarify
  :type 'symbol)
```

### Interactive Command

```elisp
(defun org-gtd-clarify-toggle-organize-help ()
  "Toggle the window showing GTD organization type descriptions."
  ...)
```

## Content

```elisp
(defconst org-gtd-clarify-organize-help-content
  "* Quick Action [q]
Do it now (< 2 minutes). Marks DONE and archives immediately.

* Single Action [s]
One-off task to do when ready.
- Marks as NEXT
- Shows in engage view

* Project [p]
Multi-step outcome requiring several tasks.
- Creates task dependencies
- First task(s) marked NEXT, rest TODO
- Auto-advances on completion

* Add to Project [a]
Attach this task to an existing project.

* Calendar [c]
Must happen at a specific date/time.
- Prompts for date
- Shows in agenda at that time

* Delegate [d]
Someone else will do it; you follow up.
- Prompts for person and follow-up date
- Marks as WAIT

* Habit [h]
Recurring action with org-habit tracking.

* Tickler [i]
Remind me to reconsider on a specific date.
- Prompts for review date
- Reappears in agenda then

* Someday/Maybe [y]
Might do eventually, no commitment or date.

* Knowledge [k]
Reference material to file away.

* Trash [t]
Not needed. Deletes the item.
")
```

## Implementation

### Constants

```elisp
(defconst org-gtd-clarify-organize-help-buffer-name "*Org GTD Organize Help*"
  "Buffer name for the organize types help window.")
```

### Functions

```elisp
(defun org-gtd-clarify--get-or-create-organize-help-buffer ()
  "Get or create the organize help buffer with type descriptions."
  (let ((buffer (get-buffer org-gtd-clarify-organize-help-buffer-name)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (with-current-buffer (get-buffer-create org-gtd-clarify-organize-help-buffer-name)
        (erase-buffer)
        (insert org-gtd-clarify-organize-help-content)
        (org-mode)
        (read-only-mode 1)
        (goto-char (point-min))
        (current-buffer)))))

(defun org-gtd-clarify--display-organize-help-window ()
  "Display organize help in a side window."
  (let ((side (or org-gtd-clarify-show-organize-help 'right))
        (buffer (org-gtd-clarify--get-or-create-organize-help-buffer)))
    (display-buffer buffer
                    `(display-buffer-in-side-window . ((side . ,side))))))

(defun org-gtd-clarify-toggle-organize-help ()
  "Toggle the organize types help window."
  (interactive)
  (let ((window (get-buffer-window org-gtd-clarify-organize-help-buffer-name)))
    (if window
        (quit-window nil window)
      (org-gtd-clarify--display-organize-help-window))))
```

## File Changes

All changes in `org-gtd-clarify.el`:

1. Add `defconst org-gtd-clarify-organize-help-content` (Constants section)
2. Add `defconst org-gtd-clarify-organize-help-buffer-name` (Constants section)
3. Add `defcustom org-gtd-clarify-show-organize-help` (Customization section)
4. Add three functions (Functions section, near horizons functions)

No changes to other files required.
