# GTD View Builder Design

Interactive transient menu for building org-agenda views with DSL filters, with save/load capability.

## Overview

Two new commands allow users to interactively construct GTD agenda views and save them for reuse:

- `org-gtd-view-builder` - Build filter specs interactively via transient menu
- `org-gtd-load-view` - Load, edit, or delete saved views

One new custom variable stores saved views:

- `org-gtd-saved-views` - Alist of `(name . view-spec)` pairs

## View Builder (`org-gtd-view-builder`)

### Initial State

- Shows all GTD items (baseline filter: `ORG_GTD<>""`)
- No type selected
- No secondary or tertiary filters active
- Agenda preview displays all items with ORG_GTD property

### Filter Hierarchy

Filters are organized in three tiers:

**Primary: GTD Type (optional, single-select)**
- next-action, delegated, calendar, tickler, project, habit, someday, reference, quick-action
- Selecting a type narrows the view and reveals type-specific secondary filters
- Selecting another type replaces the current selection

**Secondary: Type-Specific Filters (contextual)**

| Type | Secondary Filters |
|------|-------------------|
| delegated | `when` (timestamp), `who` (text) |
| calendar | `when` (timestamp) |
| tickler | `when` (timestamp) |
| habit | `when` (timestamp) |
| next-action | (none) |
| project | `state` (stuck/active/completed) |
| someday | (none) |
| reference | (none) |
| quick-action | (none) |

**Tertiary: General Org-mode Filters (always available)**
- Tags: multi-select via `completing-read-multiple` with `org-global-tags-completion-table`
- Priority: A, B, C or combinations
- Effort: comparison expressions like `< 30m`, `> 1h`

### Transient Menu Structure

```
GTD View Builder
================

Current: [All GTD Items] → 42 items

Type (pick one)
  [n] Next Action    [d] Delegated    [c] Calendar
  [t] Tickler        [p] Project      [h] Habit
  [s] Someday        [r] Reference    [q] Quick Action

[Secondary filters appear here when type selected]

General Filters
  [T] Tags: (none)
  [P] Priority: (none)
  [E] Effort: (none)

View
  [RET] Refresh preview
  [a]   Add another block (0 blocks so far)
  [S]   Save view...
  [Q]   Quit
```

When a type is selected (e.g., Delegated):

```
GTD View Builder
================

Current: Delegated → 12 items

Type (pick one)
  [n] Next Action    [d] Delegated *  [c] Calendar
  ...

Delegated Filters
  [w] When: (any)      → past | today | future | < Nd
  [W] Who: (any)       → completing-read from existing values

General Filters
  [T] Tags: (none)
  [P] Priority: (none)
  [E] Effort: (none)

View
  ...
```

### Multi-Block Support

Users can build composite views with multiple blocks (e.g., "Delegated past due" + "Next actions @work").

**Adding a block:**
- Press `a` to save current filters as a block
- UI resets for defining next block
- Block count updates in menu

**When blocks exist:**

```
GTD View Builder
================

Blocks (2 defined)
  [1] Edit: Delegated (when: past)
  [2] Edit: Next Action (tags: @work)
  [x] Delete block...
  [a] Add new block

Current: Adding new block...

Type (pick one)
  ...
```

**Editing a block:**
- Press number to select block for editing
- Filters populate with that block's current values
- Preview shows ALL blocks (complete view context)
- Changes update that block

**Deleting a block:**
- Press `x`, then select block number
- Confirm deletion
- Block removed, remaining blocks renumbered

### Time Filter Input

For `when` filters, users choose:
- Semantic: `past`, `today`, `future`
- Relative: prompted for duration like `7d`, `2w`, `1M`
- Produces DSL like `(when . past)` or `(when . (< "7d"))`

### Save Flow

When user presses `S`:

1. Prompt: `(read-string "View name: ")`
2. If name exists: confirm overwrite ("View 'X' exists. Overwrite?")
3. Save to `org-gtd-saved-views`
4. Display message:

```
View "My Work View" saved to org-gtd-saved-views.

To make this permanent, add to your init file:
  (customize-save-variable 'org-gtd-saved-views org-gtd-saved-views)

Or use M-x customize-variable RET org-gtd-saved-views RET to save interactively.
```

## Load View (`org-gtd-load-view`)

### Menu Structure

```
Load GTD View
=============

Saved Views
  [1] My Work View
  [2] Weekly Review
  [3] Delegated Follow-ups

Actions
  [e] Edit view...
  [d] Delete view...
  [q] Quit
```

### Actions

**Load (press number):**
- Display the view immediately via `org-gtd-view-show`

**Edit (`e` then number):**
- Open `org-gtd-view-builder` pre-populated with that view's spec
- User can modify and re-save (same name or new name)

**Delete (`d` then number):**
- Confirm: "Delete view 'X'?"
- Remove from `org-gtd-saved-views`

## Custom Variable

```elisp
(defcustom org-gtd-saved-views nil
  "Alist of saved GTD view specifications.
Each entry is (NAME . VIEW-SPEC) where VIEW-SPEC is a valid
org-gtd-view-language specification."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-gtd)
```

## DSL Integration

The view builder produces standard org-gtd-view-language specs:

**Single block:**
```elisp
'((name . "Overdue Delegated")
  (type . delegated)
  (when . past))
```

**Multi-block:**
```elisp
'((name . "Work Dashboard")
  (blocks . (((type . delegated)
              (when . past))
             ((type . next-action)
              (tags . ("@work"))))))
```

All specs are compatible with `org-gtd-view-show` and the existing DSL infrastructure.

## Implementation Notes

### Files to Create/Modify

- `org-gtd-view-builder.el` (new) - Transient menus and builder logic
- `org-gtd-core.el` - Add `org-gtd-saved-views` defcustom
- `org-gtd.el` - Require new module, add autoloads

### Key Dependencies

- `transient.el` - Menu infrastructure (already a dependency)
- `org-gtd-view-language.el` - DSL spec handling
- `org-gtd-types.el` - Type definitions and property lookups

### Transient Patterns to Follow

Reference existing patterns in:
- `org-gtd-organize.el` - Conditional sections with `:if`
- `org-gtd-agenda-transient.el` - Dynamic context display
- `org-gtd-graph-transient.el` - Sticky mode, multi-key sequences
