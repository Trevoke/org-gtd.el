# Skip Refile Design

## Problem

When clarifying items already in the agenda (not from inbox), the current flow always refiles the item to a target location. This is unnecessary churn when the item is already where it belongs and just needs reconfiguration (e.g., changing TODO state, updating properties).

## Solution

Add a "skip refile" option that updates the item in place instead of refiling it elsewhere.

## Design

### New Buffer-Local Variable

```elisp
(defvar-local org-gtd-clarify--skip-refile nil
  "When non-nil, update item in place instead of refiling.")
```

Set in the WIP buffer to indicate the item should be updated in place.

### Setting the Flag

**At clarify time (C-u prefix):**

Both `org-gtd-clarify-item` and `org-gtd-clarify-agenda-item` check `current-prefix-arg`. If non-nil, set `org-gtd-clarify--skip-refile` to t in the WIP buffer.

**At organize time (transient toggle):**

The `org-gtd-organize` transient includes a toggle suffix that reads/writes the same variable. The toggle is hidden when `org-gtd-clarify--inbox-p` is t (inbox processing must always refile).

```elisp
["Options"
 ("-n" "Update in place (no refile)" org-gtd-clarify--skip-refile
  :if (lambda () (not org-gtd-clarify--inbox-p)))]
```

### Behavior Change in Category Finalize Functions

Each category's `--finalize` function chooses between refile and update-in-place at the same logical point:

```elisp
(defun org-gtd-single-action--finalize ()
  "Finalize single action organization."
  (setq-local org-gtd--organize-type 'single-action)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-action org-gtd-action-template)))
```

### New Function: `org-gtd-organize--update-in-place`

Located in `org-gtd-organize.el`:

```elisp
(defun org-gtd-organize--update-in-place ()
  "Replace original heading with configured content from WIP buffer."
  (let ((new-content (save-excursion
                       (goto-char (point-min))
                       (org-back-to-heading t)
                       (org-copy-subtree)
                       (current-kill 0))))
    (with-current-buffer (marker-buffer org-gtd-clarify--source-heading-marker)
      (goto-char org-gtd-clarify--source-heading-marker)
      (org-back-to-heading t)
      (org-cut-subtree)
      (insert new-content)
      (save-buffer))))
```

### Behavior Change in `org-gtd-organize--call`

The cleanup section checks the flag to decide whether to cut the original:

- If refiled: cut original (it's been copied elsewhere)
- If updated in place: don't cut (we just replaced it)

```elisp
(unless org-gtd-clarify--skip-refile
  (when (and marker buffer position)
    (with-current-buffer buffer
      (goto-char position)
      (org-cut-subtree))))
```

## Files to Modify

1. **org-gtd-clarify.el**
   - Add `org-gtd-clarify--skip-refile` variable
   - Modify `org-gtd-clarify-item` to check prefix arg and set flag
   - Modify `org-gtd-clarify-agenda-item` to pass through prefix arg

2. **org-gtd-organize.el**
   - Add transient toggle (hidden during inbox processing)
   - Add `org-gtd-organize--update-in-place` function
   - Modify `org-gtd-organize--call` to conditionally skip cutting original

3. **Category modules** (each needs `--finalize` updated):
   - org-gtd-single-action.el
   - org-gtd-calendar.el
   - org-gtd-delegate.el
   - org-gtd-habit.el
   - org-gtd-incubate.el
   - org-gtd-knowledge.el
   - org-gtd-trash.el
   - org-gtd-projects.el

4. **org-gtd-delegate.el**
   - Remove `org-gtd-delegate-item-at-point` (replaced by this mechanism)

5. **doc/org-gtd.org**
   - Document C-u prefix behavior for clarify commands
   - Document transient toggle
   - Explain use case (reconfiguring existing agenda items)
   - Migration note for `org-gtd-delegate-item-at-point` removal

## Testing

- Test C-u prefix sets flag correctly
- Test transient toggle works and is hidden during inbox processing
- Test update-in-place replaces content correctly
- Test normal refile flow still works
- Test inbox processing ignores the flag/toggle
