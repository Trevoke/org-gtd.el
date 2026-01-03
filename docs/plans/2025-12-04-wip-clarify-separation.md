# WIP/Clarify Separation Design

## Goal

Make `org-gtd-wip.el` generic temp-file-backed buffer infrastructure with no GTD opinions. Move all clarification-specific behavior to `org-gtd-clarify.el`.

## Current State

### org-gtd-wip.el provides:
- `org-gtd-wip-mode` - major mode with header-line and auto-save
- `org-gtd-wip--get-buffer` - creates temp-file-backed buffer, applies mode
- `org-gtd-wip--maybe-initialize-buffer-contents` - copies subtree, deletes GTD properties
- `org-gtd-wip--get-buffers` - finds buffers by name prefix
- `org-gtd-wip--cleanup-temp-file` - cleans up temp file for ID
- Temp file hash table tracking

### org-gtd-clarify.el provides:
- `org-gtd-clarify-mode` - minor mode adding keybindings, overwrites header-line
- `org-gtd-clarify-item` - main entry point, calls WIP functions
- Window management, horizons display, dependency helpers

### Problems:
1. Two modes on same buffer (major + minor), both set header-line
2. WIP has GTD-specific logic (property deletion)
3. Unclear ownership of clarification behavior

## New Design

### org-gtd-wip.el (generic infrastructure)

**Remove:**
- `org-gtd-wip-mode` - gone entirely
- `org-gtd-wip--maybe-initialize-buffer-contents` - caller handles

**Keep/modify:**
- `org-gtd-wip--get-buffer(id, name-prefix)` - returns plain `org-mode` buffer, caller sets mode
- `org-gtd-wip--get-buffers(name-prefix)` - filters by prefix
- `org-gtd-wip--cleanup-temp-file(id)` - unchanged
- `org-gtd-wip--cleanup-all-temp-files` - unchanged
- `org-gtd-wip--buffer-name(id, prefix)` - add prefix parameter

### org-gtd-clarify.el (owns clarification)

**Change `org-gtd-clarify-mode` from minor to major mode:**
```elisp
(define-derived-mode org-gtd-clarify-mode org-mode "GTD-Clarify"
  "Major mode for GTD item clarification."
  (setq-local org-gtd--loading-p t)
  (setq-local header-line-format ...)
  (auto-save-mode 1))
```

**Add subtree copying and property cleanup:**
```elisp
(defun org-gtd-clarify--initialize-buffer (buffer marker)
  "Copy subtree at MARKER into BUFFER and clean GTD properties."
  ...)
```

**Update `org-gtd-clarify-item`:**
1. Get buffer via `org-gtd-wip--get-buffer`
2. Copy subtree and cleanup properties itself
3. Apply `org-gtd-clarify-mode`

### org-gtd-id-overlay.el

**Update hook:**
```elisp
;; Was: (add-hook 'org-gtd-wip-mode-hook ...)
(add-hook 'org-gtd-clarify-mode-hook #'org-gtd-id-overlay-maybe-enable)

;; Was: (eq major-mode 'org-gtd-wip-mode)
(eq major-mode 'org-gtd-clarify-mode)
```

## API Changes

| Old | New |
|-----|-----|
| `org-gtd-wip-mode` | removed (use `org-gtd-clarify-mode`) |
| `org-gtd-wip-mode-hook` | removed (use `org-gtd-clarify-mode-hook`) |
| `org-gtd-wip-mode-map` | removed (use `org-gtd-clarify-map`) |
| `org-gtd-wip--get-buffer(id)` | `org-gtd-wip--get-buffer(id, prefix)` |
| `org-gtd-wip--get-buffers()` | `org-gtd-wip--get-buffers(prefix)` |
| `org-gtd-wip--maybe-initialize-buffer-contents` | removed |

## Files to Update

1. `org-gtd-wip.el` - remove mode, update function signatures
2. `org-gtd-clarify.el` - major mode, own initialization
3. `org-gtd-id-overlay.el` - hook and mode check
4. `test/unit/wip-buffer-test.el` - update for new API
5. `test/unit/wip-temp-file-test.el` - update for new API
6. `test/unit/clarify-test.el` - update mode expectations
7. `test/unit/id-overlay-test.el` - update mode references
8. `test/helpers/wip.el` - update helpers
9. `CHANGELOG.org` - document breaking change
10. `doc/org-gtd.org` - update mode references

## Test Strategy (TDD)

### Phase 1: Update WIP tests for new API
- Test `org-gtd-wip--get-buffer` returns org-mode buffer (not wip-mode)
- Test prefix parameter for buffer naming
- Test `org-gtd-wip--get-buffers` filters by prefix

### Phase 2: Add clarify-mode tests
- Test `org-gtd-clarify-mode` is major mode derived from org-mode
- Test mode sets expected buffer-local variables
- Test keymap exists with expected bindings

### Phase 3: Update integration tests
- Test full clarify workflow uses new mode
- Test id-overlay enables in clarify-mode

## Backward Compatibility

Breaking change for anyone who:
- Hooks into `org-gtd-wip-mode-hook`
- Checks for `org-gtd-wip-mode`
- Uses `org-gtd-wip-mode-map`

Add obsolete aliases where possible, document in CHANGELOG.
