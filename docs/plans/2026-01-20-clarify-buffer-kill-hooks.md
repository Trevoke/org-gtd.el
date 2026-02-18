# Design: Clarify Buffer Kill Hooks

**Date:** 2026-01-20
**Status:** Ready for implementation

## Problem Statement

When a clarify buffer is killed directly (`C-x k`, `kill-buffer`) rather than via the proper cancel command (`C-c C-k`):
- Pending duplicates are silently lost
- Side windows (queue, horizons, organize help, dependencies helper) remain open

## Background

### Current State

We have three ways a clarify buffer can end:

1. **Normal organize flow** - User organizes item, `org-gtd-organize--call` kills buffer
2. **Intentional cancel** (`C-c C-k`) - `org-gtd-clarify-stop` prompts about duplicates, cleans up
3. **Direct kill** (`C-x k`, `kill-buffer`) - No handling currently

We also have `kill-emacs-query-functions` handling the Emacs exit case.

### Key Finding

`kill-emacs` does NOT run `kill-buffer-hook` for buffers. These are separate code paths:
- `kill-buffer` → runs `kill-buffer-query-functions`, then `kill-buffer-hook`
- `kill-emacs` → runs only `kill-emacs-hook` (which we already handle)

## Design

### Hook Registration

Register buffer-local hooks in `org-gtd-clarify-mode`:

```elisp
(define-derived-mode org-gtd-clarify-mode org-mode "GTD Clarify"
  "..."
  ;; ... existing setup ...

  ;; Kill buffer hooks for cleanup
  (add-hook 'kill-buffer-query-functions
            #'org-gtd-clarify--kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook
            #'org-gtd-clarify--kill-buffer-cleanup nil t))
```

### Query Function (Can Abort Kill)

```elisp
(defun org-gtd-clarify--kill-buffer-query ()
  "Query before killing clarify buffer if duplicates are pending.
Returns t to allow kill, nil to abort."
  (if (org-gtd-clarify--queue-empty-p)
      t  ; No duplicates, allow kill
    ;; Prompt user - reuse existing prompt logic
    (pcase (org-gtd-clarify--prompt-queue-action)
      ('save (org-gtd-clarify--queue-save-to-inbox) t)
      ('discard t)
      ('cancel nil))))
```

### Kill Hook (Cleanup Side Windows)

```elisp
(defun org-gtd-clarify--kill-buffer-cleanup ()
  "Clean up side windows when clarify buffer is killed.
Only cleans up global side windows if no other clarify buffers exist."
  (unless (org-gtd-clarify--other-clarify-buffers-exist-p)
    ;; Clean up all side windows
    (org-gtd-clarify--kill-side-window "*Org GTD Duplicate Queue*")
    (org-gtd-clarify--kill-side-window "*Org GTD Organize Help*")
    (org-gtd-clarify--kill-side-window org-gtd-horizons--buffer-name)
    (org-gtd-clarify--kill-side-window "*Org GTD Dependencies*"))
  ;; Always clean up WIP temp file for this buffer
  (when org-gtd-clarify--clarify-id
    (org-gtd-wip--cleanup-temp-file org-gtd-clarify--clarify-id)))
```

### Helper Functions

```elisp
(defun org-gtd-clarify--other-clarify-buffers-exist-p ()
  "Return t if other clarify buffers exist besides current one."
  (let ((current (current-buffer)))
    (cl-some (lambda (buf)
               (and (not (eq buf current))
                    (with-current-buffer buf
                      (derived-mode-p 'org-gtd-clarify-mode))))
             (buffer-list))))

(defun org-gtd-clarify--kill-side-window (buffer-name)
  "Kill side window buffer if it exists."
  (when-let ((buffer (get-buffer buffer-name)))
    (when-let ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer)))
```

### Modification to org-gtd-organize--call

Clear the queue after capturing it, before killing the buffer:

```elisp
(defun org-gtd-organize--call (...)
  ;; ... existing code ...
  (let ((duplicate-queue (copy-sequence org-gtd-clarify--duplicate-queue))
        ;; ... other captures ...
        )
    ;; Clear queue so kill hooks don't prompt
    (setq org-gtd-clarify--duplicate-queue nil)

    ;; ... rest of function including kill-buffer ...
    ))
```

This ensures:
- Query function sees empty queue → returns t (no prompt)
- State is preserved via the captured `duplicate-queue` variable
- Normal organize flow is unaffected

## Side Windows to Clean Up

| Buffer | Variable/Constant |
|--------|-------------------|
| `*Org GTD Duplicate Queue*` | `org-gtd-clarify--queue-buffer-name` |
| `*Org GTD Organize Help*` | hardcoded in `org-gtd-clarify--get-or-create-organize-help-buffer` |
| Horizons buffer | `org-gtd-horizons--buffer-name` |
| `*Org GTD Dependencies*` | hardcoded in dependencies helper |

## Flow Diagrams

### Normal Organize Flow
```
User organizes item
  → org-gtd-organize--call
    → capture queue with copy-sequence
    → set queue to nil
    → kill-buffer (hooks run but see empty queue)
    → if captured queue has items: process next
```

### Direct Kill Flow (C-x k)
```
User kills buffer
  → kill-buffer-query-functions
    → org-gtd-clarify--kill-buffer-query
      → queue empty? return t
      → queue has items? prompt discard/save/cancel
        → discard: return t
        → save: save to inbox, return t
        → cancel: return nil (abort kill)
  → kill-buffer-hook (if query allowed)
    → org-gtd-clarify--kill-buffer-cleanup
      → other clarify buffers? skip side window cleanup
      → no other buffers? clean up side windows
      → clean up WIP temp file
```

### Intentional Cancel (C-c C-k)
```
User presses C-c C-k
  → org-gtd-clarify-stop
    → prompt about duplicates (existing logic)
    → cleanup queue window
    → cleanup WIP temp file
    → restore window config
    → kill buffer (hooks run but queue already cleared)
```

## Edge Cases

| Case | Behavior |
|------|----------|
| Kill buffer with empty queue | Hooks pass through, side windows cleaned if no other clarify buffers |
| Kill buffer with pending duplicates | Query function prompts, user chooses |
| Kill one of multiple clarify buffers | Side windows stay open for remaining buffers |
| Emacs exit with pending duplicates | Handled by existing `kill-emacs-query-functions` |
| Normal organize flow | Queue cleared before kill, hooks see nothing |

## Implementation Tasks

1. Add `org-gtd-clarify--other-clarify-buffers-exist-p` helper
2. Add `org-gtd-clarify--kill-side-window` helper
3. Add `org-gtd-clarify--kill-buffer-query` function
4. Add `org-gtd-clarify--kill-buffer-cleanup` function
5. Register hooks in `org-gtd-clarify-mode`
6. Modify `org-gtd-organize--call` to clear queue before kill
7. Update `org-gtd-clarify-stop` to clear queue before any kill (if not already)
8. Add tests
9. Update documentation

## Testing Checklist

- [ ] Direct kill with empty queue cleans up side windows
- [ ] Direct kill with duplicates prompts user
- [ ] Discard option allows kill, loses duplicates
- [ ] Save option saves to inbox, allows kill
- [ ] Cancel option aborts kill
- [ ] Multiple clarify buffers: killing one doesn't close shared side windows
- [ ] Normal organize flow: no prompts, duplicates processed correctly
- [ ] `C-c C-k` still works as before
- [ ] Emacs exit still prompts about pending duplicates
