# Implementation Plan: Unified Organize Flow

## Goal

Unify programmatic and interactive flows so all item creation goes through `org-gtd-organize--call`.

## Problem

Programmatic `*-create` functions were bypassing `org-gtd-organize--call`:

```
Interactive:     org-gtd-calendar → organize--call → --apply
Programmatic:    org-gtd-calendar-create → --apply directly (bypassed organize--call)
```

Additionally, programmatic functions were calling `org-gtd-clarify-item` which created WIP buffers that were never used.

## Solution

The main organize commands (`org-gtd-calendar`, `org-gtd-delegate`, etc.) already accept optional parameters for non-interactive use. The fix was simply to:

1. Remove unnecessary `org-gtd-clarify-item` calls from `*-create` functions
2. Call the main command instead of `--apply` directly

## Changes Made

### Files Modified

- `org-gtd-calendar.el` - `org-gtd-calendar-create`
- `org-gtd-single-action.el` - `org-gtd-single-action-create`
- `org-gtd-delegate.el` - `org-gtd-delegate-create`
- `org-gtd-habit.el` - `org-gtd-habit-create`
- `org-gtd-tickler.el` - `org-gtd-tickler-create`
- `org-gtd-someday.el` - `org-gtd-someday-create`

### Before/After Example (calendar)

**Before:**
```elisp
(defun org-gtd-calendar-create (topic appointment-date)
  (let ((buffer ...)
        (config-override `(('active-timestamp . ,(lambda (_x) ...)))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)                    ;; unnecessary
      (org-gtd-calendar--apply config-override)) ;; bypassed organize--call
    (kill-buffer buffer)))
```

**After:**
```elisp
(defun org-gtd-calendar-create (topic appointment-date)
  (let ((buffer ...))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-calendar appointment-date))  ;; uses organize--call
    (kill-buffer buffer)))
```

## Why This Works

1. `org-gtd-organize--call` handles missing markers gracefully (just skips "cut original" step)
2. The main commands already build config-override from their parameters
3. Refile works from temp buffer - copies to GTD file, deletes from source (which we kill anyway)

## Result

- All 774 tests pass
- All flows now go through `org-gtd-organize--call`
- Code is simpler (removed duplicate config-override building)
- No new functions needed (no refile dispatcher, no cleanup function)

## Status: COMPLETE
