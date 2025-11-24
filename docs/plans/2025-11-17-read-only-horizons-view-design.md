# Read-Only Horizons View Design

**Date:** 2025-11-17
**Status:** Approved

## Problem Statement

When displaying the horizons file in a side window during clarification, users should see a read-only view. The actual horizons file should remain editable when opened directly, but in the clarification context, the side window should be read-only to prevent accidental edits during the GTD workflow.

## Requirements

1. Display horizons in a read-only buffer during clarification
2. Keep the actual horizons file editable when opened normally
3. Preserve view state (scroll position) when toggling the horizons window during clarification
4. Clean up the read-only view when clarification sessions end:
   - After one-off clarification completes
   - After inbox processing completes
5. Use standard Emacs read-only-mode messaging for edit attempts

## Design Overview

### Approach: Indirect Buffer Management

Use Emacs indirect buffers to create a read-only view of the horizons file. An indirect buffer shares the exact same text content as its base buffer but can have independent buffer-local properties (like read-only status, modes, etc.).

### Benefits

- Automatic content sync (indirect buffer reflects file changes instantly)
- No duplication of buffer content
- Clear separation between "view" and "edit" contexts
- Lightweight implementation following Emacs conventions

## Architecture

### Core Components

1. **Buffer creation function** - `org-gtd-clarify--get-or-create-horizons-view`
   - Creates or retrieves the indirect buffer named `*Org GTD Horizons View*`
   - Marks it read-only
   - Returns singleton buffer (shared across WIP buffers in same session)

2. **Display function modification** - `org-gtd-clarify--display-horizons-window`
   - Changed to display the view buffer instead of the file buffer

3. **Cleanup function** - `org-gtd-clarify--cleanup-horizons-view`
   - Kills the view buffer when clarification sessions end

### Session Tracking

The existing `org-gtd-clarify--inbox-p` buffer-local variable distinguishes:
- `nil` - One-off clarification
- `t` - Inbox processing session

This determines cleanup scope.

## Implementation Details

### New Function: `org-gtd-clarify--get-or-create-horizons-view`

```elisp
(defun org-gtd-clarify--get-or-create-horizons-view ()
  "Get or create read-only indirect buffer for horizons file."
  (let* ((horizons-buffer (org-gtd--horizons-file))
         (view-buffer-name "*Org GTD Horizons View*")
         (existing-view (get-buffer view-buffer-name)))
    (if (and existing-view (buffer-live-p existing-view))
        existing-view
      (with-current-buffer horizons-buffer
        (let ((view-buffer (make-indirect-buffer
                            horizons-buffer
                            view-buffer-name
                            t))) ; clone means copy local vars
          (with-current-buffer view-buffer
            (read-only-mode 1))
          view-buffer)))))
```

### Modified Function: `org-gtd-clarify--display-horizons-window`

```elisp
(defun org-gtd-clarify--display-horizons-window ()
  "Display horizons window."
  (let ((horizons-side (or org-gtd-clarify-show-horizons 'right))
        (view-buffer (org-gtd-clarify--get-or-create-horizons-view)))
    (display-buffer view-buffer
                    `(display-buffer-in-side-window . ((side . ,horizons-side))))))
```

### New Function: Cleanup

```elisp
(defun org-gtd-clarify--cleanup-horizons-view ()
  "Kill the horizons view buffer if it exists."
  (let ((view-buffer (get-buffer "*Org GTD Horizons View*")))
    (when (and view-buffer (buffer-live-p view-buffer))
      (kill-buffer view-buffer))))
```

### Cleanup Hook Points

**1. One-off clarification end** - In `org-gtd-organize--call` (org-gtd-organize.el:139)

After line 169 where the WIP buffer is killed:

```elisp
(when task-id
  (kill-buffer (org-gtd-wip--buffer-name task-id)))
(unless loop-p  ; Only for one-off clarification
  (org-gtd-clarify--cleanup-horizons-view))
```

**2. Inbox processing end** - In `org-gtd-process--stop` (org-gtd-process.el:58)

```elisp
(defun org-gtd-process--stop ()
  "Stop processing the inbox."
  (org-gtd-clarify--cleanup-horizons-view)
  (whitespace-cleanup))
```

## Edge Cases

1. **Horizons file doesn't exist yet** - Existing `org-gtd--horizons-file` handles this with `org-gtd--ensure-file-exists`

2. **Horizons file buffer gets killed** - `buffer-live-p` check handles recreation

3. **User manually kills view buffer** - Next toggle creates fresh buffer

4. **Multiple clarification sessions** - All WIP buffers share the same singleton view buffer. Cleanup happens only when entire session ends.

5. **Toggle horizons window off/on during clarification** - View buffer persists, window closes. Reopening shows same buffer with preserved scroll position.

## Testing Strategy

Test cases to add in `test/horizons-test.el`:

1. **View buffer creation** - Verify indirect buffer created with correct name and read-only mode
2. **Content sync** - Verify changes to horizons file appear in view buffer
3. **Cleanup on one-off clarification** - Verify view buffer killed after organizing single item
4. **Cleanup on inbox processing** - Verify view buffer killed when inbox empties
5. **Toggle preserves buffer** - Verify closing/reopening window reuses same buffer during session
6. **Read-only enforcement** - Verify edit attempts are blocked

## Files to Modify

- `org-gtd-clarify.el` - Add view creation, modify display, add cleanup
- `org-gtd-organize.el` - Add cleanup call for one-off clarification
- `org-gtd-process.el` - Add cleanup call for inbox processing end
- `test/horizons-test.el` - Add new test cases

## Open Questions

None - design approved.
