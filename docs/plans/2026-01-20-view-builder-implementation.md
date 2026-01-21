# GTD View Builder Implementation Plan

This plan implements the view builder design from `docs/plans/2026-01-19-view-builder-design.md`.

## Project Image Context

This section provides key code patterns and references from the existing codebase.

### Transient Menu Patterns

**From `org-gtd-organize.el:69-86`** - Basic transient structure:
```elisp
(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item."
  [:if (lambda () (not org-gtd-clarify--inbox-p))
   "Options"
   ("-n" org-gtd-organize--skip-refile-infix)]
  ["Actionable"
   [("q" "Quick action" org-gtd-quick-action)
    ("s" "Single action" org-gtd-single-action)]
   [("d" "Delegate" org-gtd-delegate)
    ("c" "Calendar" org-gtd-calendar)
    ("h" "Habit" org-gtd-habit)]]
  ...)
```

**From `org-gtd-agenda-transient.el:62-66`** - Dynamic context display:
```elisp
(defun org-gtd-agenda-transient--show-context ()
  "Show context about task at agenda point."
  (if-let ((info (org-gtd-agenda-transient--task-at-point)))
      (format "Selected: %s" (plist-get info :title))
    "No task at point"))

;; Used with:
[:description org-gtd-agenda-transient--show-context
 :class transient-row]
```

**From `org-gtd-graph-transient.el:567-570`** - Custom prefix class with state:
```elisp
(defclass org-gtd-graph-transient-prefix (transient-prefix)
  ((edge-selection :initarg :edge-selection :initform nil
                   :documentation "Alist of (TASK-ID . SELECTED-P)"))
  "Custom transient prefix for graph operations with edge selection state.")
```

**From `org-gtd-graph-transient.el:612-650`** - Dynamic suffix generation:
```elisp
(defun org-gtd-graph--modify-blockers-setup (_)
  "Setup children for modify-blockers transient."
  (let* ((scope (transient-scope))
         ...)
    (transient-parse-suffixes
     'org-gtd-graph-modify-blockers-menu
     (list
      (vconcat
       (mapcar (lambda (candidate-id)
                 (list key display toggle-fn :transient t))
               valid-candidates))))))
```

### Type System Reference (`org-gtd-types.el`)

| Type | ORG_GTD Value | State | Properties |
|------|---------------|-------|------------|
| next-action | "Actions" | :next | none |
| delegated | "Delegated" | :wait | :who, :when |
| calendar | "Calendar" | nil | :when |
| tickler | "Tickler" | nil | :when |
| someday | "Someday" | nil | none |
| project | "Projects" | nil | none |
| habit | "Habit" | nil | :when, :style |
| reference | "Reference" | :done | none |
| trash | "Trash" | :canceled | none |
| quick-action | "Quick" | :done | none |

**Type access functions:**
```elisp
(org-gtd-type-org-gtd-value 'delegated)  ; => "Delegated"
(org-gtd-type-property 'delegated :who)  ; => "DELEGATED_TO"
(org-gtd-type-property 'delegated :when) ; => "ORG_GTD_TIMESTAMP"
```

### View Language DSL (`org-gtd-view-language.el`)

**Filter keys (line 178-184):**
```elisp
'(name type when deadline scheduled todo done not-done
  area-of-focus who tags priority effort clocked last-clocked-out
  blocks prefix prefix-width view-type agenda-span show-habits
  additional-blocks filters not-habit property
  block-type group-contexts group-by todo-keyword prefix-format native)
```

**Simple types (line 215-218):**
```elisp
'(next-action delegated calendar tickler project someday habit reference trash quick-action)
```

**Complex/computed types (line 220-225):**
```elisp
'(stuck-project active-project completed-project tickler-project incubated-project
  stuck-delegated stuck-calendar stuck-tickler stuck-habit stuck-single-action)
```

**DSL example specs:**
```elisp
;; Single block
'((name . "Overdue Delegated")
  (type . delegated)
  (when . past))

;; Multi-block
'((name . "Work Dashboard")
  (blocks . (((type . delegated) (when . past))
             ((type . next-action) (tags . ("@work"))))))
```

### Autoload Pattern

From `org-gtd-agenda-transient.el:196`:
```elisp
;;;###autoload (autoload 'org-gtd-agenda-transient "org-gtd-agenda-transient" nil t)
(transient-define-prefix org-gtd-agenda-transient ()
  ...)
```

---

## Overview

Create an interactive transient-based UI for building org-agenda views with DSL filters, with save/load capability.

## Implementation Units

### Unit 1: Core Custom Variable and Utilities

**Goal**: Define `org-gtd-saved-views` and helper functions for view spec manipulation.

**Files**: `org-gtd-core.el`

**Changes**:
1. Add `org-gtd-saved-views` defcustom:
```elisp
(defcustom org-gtd-saved-views nil
  "Alist of saved GTD view specifications.
Each entry is (NAME . VIEW-SPEC) where VIEW-SPEC is a valid
org-gtd-view-language specification."
  :type '(alist :key-type string :value-type sexp)
  :group 'org-gtd)
```

**Dependencies**: None

**Tests**:
- Variable exists and has correct type
- Can save/retrieve view specs from the alist

---

### Unit 2: View Builder State Management

**Goal**: Create buffer-local state for the view builder transient.

**Files**: `org-gtd-view-builder.el` (new file)

**Changes**:
1. Define module structure with requires:
   - `transient`
   - `org-gtd-core`
   - `org-gtd-view-language`
   - `org-gtd-types`

2. Define state variables:
```elisp
(defvar-local org-gtd-view-builder--blocks nil
  "List of completed block specs in the current view builder session.")

(defvar-local org-gtd-view-builder--current-type nil
  "Currently selected GTD type filter (symbol or nil).")

(defvar-local org-gtd-view-builder--current-filters nil
  "Alist of current filter settings for the block being built.")

(defvar-local org-gtd-view-builder--editing-block-index nil
  "Index of block being edited, or nil if adding new block.")
```

3. Define filter reset function:
```elisp
(defun org-gtd-view-builder--reset-current ()
  "Reset current block filters to defaults.")
```

4. Define item counting function:
```elisp
(defun org-gtd-view-builder--count-items ()
  "Count items matching current filters using org-map-entries.")
```

**Dependencies**: Unit 1

**Tests**:
- State variables initialize correctly
- Reset function clears all filter state
- Item counting returns reasonable numbers for test data

---

### Unit 3: Type Filter Selection

**Goal**: Implement type selection row in transient.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Create type selection suffixes:
```elisp
(defun org-gtd-view-builder--select-type (type)
  "Select TYPE as the primary filter, clearing secondary filters.")
```

2. Create type display function:
```elisp
(defun org-gtd-view-builder--type-label (type)
  "Return display label for TYPE, with * if selected.")
```

3. Define type selection transient group using `transient-define-suffix` for each type:
   - next-action, delegated, calendar, tickler, project, habit, someday, reference, quick-action

**Dependencies**: Unit 2

**Tests**:
- Selecting a type updates `org-gtd-view-builder--current-type`
- Selecting a new type replaces the previous selection
- Type labels show asterisk when selected

---

### Unit 4: Secondary Filters (Type-Specific)

**Goal**: Implement conditional secondary filters that appear based on selected type.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Create predicate for showing secondary filters:
```elisp
(defun org-gtd-view-builder--show-delegated-filters-p ()
  "Return non-nil if delegated filters should be shown.")
```

2. Implement `when` filter input:
```elisp
(defun org-gtd-view-builder--set-when-filter ()
  "Prompt for when filter: past, today, future, or relative duration.")
```

3. Implement `who` filter input (for delegated):
```elisp
(defun org-gtd-view-builder--set-who-filter ()
  "Prompt for delegation recipient using completing-read from existing values.")
```

4. Extract existing delegation recipients:
```elisp
(defun org-gtd-view-builder--get-delegation-recipients ()
  "Return list of unique DELEGATED_TO values from agenda files.")
```

5. Implement project state filter (for projects):
```elisp
(defun org-gtd-view-builder--set-project-state-filter ()
  "Prompt for project state: stuck, active, or completed.")
```

**Dependencies**: Unit 3

**Tests**:
- Secondary filters only appear when appropriate type is selected
- When filter produces correct DSL (`past`, `today`, `future`, `(< "7d")`)
- Who filter uses completion from existing values
- Project state filter produces correct computed type

---

### Unit 5: Tertiary Filters (General)

**Goal**: Implement always-available general org-mode filters.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Implement tags filter:
```elisp
(defun org-gtd-view-builder--set-tags-filter ()
  "Prompt for tags using completing-read-multiple.")
```

2. Implement priority filter:
```elisp
(defun org-gtd-view-builder--set-priority-filter ()
  "Prompt for priority: A, B, C, or combinations.")
```

3. Implement effort filter:
```elisp
(defun org-gtd-view-builder--set-effort-filter ()
  "Prompt for effort comparison: < 30m, > 1h, etc.")
```

4. Display functions for current filter values:
```elisp
(defun org-gtd-view-builder--tags-label ()
  "Return display label for current tags filter.")
```

**Dependencies**: Unit 2

**Tests**:
- Tags filter uses `org-global-tags-completion-table`
- Priority filter accepts single or multiple priorities
- Effort filter validates comparison format
- Display labels show "(none)" when no filter set

---

### Unit 6: Block Management

**Goal**: Implement multi-block support for composite views.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Add current block to blocks list:
```elisp
(defun org-gtd-view-builder--add-block ()
  "Save current filters as a block and reset for next block.")
```

2. Edit existing block:
```elisp
(defun org-gtd-view-builder--edit-block (index)
  "Load block at INDEX into current filters for editing.")
```

3. Delete block:
```elisp
(defun org-gtd-view-builder--delete-block ()
  "Prompt for block to delete and remove it.")
```

4. Block display functions:
```elisp
(defun org-gtd-view-builder--blocks-description ()
  "Return formatted string describing all blocks.")
```

5. Dynamic block selection suffixes using `:setup-children` pattern from `org-gtd-graph-transient.el`

**Dependencies**: Unit 2, Unit 3

**Tests**:
- Adding a block moves current filters to blocks list
- Editing a block populates current filters
- Deleting a block renumbers remaining blocks
- Block count updates correctly in display

---

### Unit 7: View Spec Generation

**Goal**: Convert builder state to valid DSL view specs.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Generate single-block spec:
```elisp
(defun org-gtd-view-builder--generate-block-spec ()
  "Generate view spec alist from current filter state.")
```

2. Generate multi-block spec:
```elisp
(defun org-gtd-view-builder--generate-view-spec (name)
  "Generate complete view spec with NAME from all blocks.")
```

3. Handle type-to-DSL mapping:
   - Simple types: `(type . next-action)`
   - Project states: `(type . stuck-project)`
   - Type + when: `(type . delegated) (when . past)`

**Dependencies**: Unit 2, Unit 6

**Tests**:
- Single block without type produces `(ORG_GTD<>"")`
- Single block with type produces correct DSL
- Multi-block produces `(blocks . (...))` structure
- Generated specs are valid for `org-gtd-view-show`

---

### Unit 8: Preview Integration

**Goal**: Show live preview of current view in split window.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Refresh preview command:
```elisp
(defun org-gtd-view-builder--refresh-preview ()
  "Update preview buffer with current filters.")
```

2. Preview buffer management:
```elisp
(defun org-gtd-view-builder--show-preview ()
  "Display preview in side window.")
```

3. Use `org-gtd-view-show` with generated spec for preview

**Dependencies**: Unit 7, `org-gtd-view-language.el`

**Tests**:
- Preview shows correct item count
- Preview updates when filters change
- Preview displays all blocks in multi-block mode

---

### Unit 9: Save Workflow

**Goal**: Implement view saving to `org-gtd-saved-views`.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Save command:
```elisp
(defun org-gtd-view-builder--save ()
  "Prompt for name and save current view to org-gtd-saved-views.")
```

2. Overwrite confirmation:
```elisp
(defun org-gtd-view-builder--name-exists-p (name)
  "Return non-nil if NAME exists in saved views.")
```

3. Display save instructions message:
```elisp
(defun org-gtd-view-builder--show-save-message (name)
  "Display message with instructions for making save permanent.")
```

**Dependencies**: Unit 1, Unit 7

**Tests**:
- Saving adds entry to `org-gtd-saved-views`
- Overwrite prompts when name exists
- Save message includes customize-save-variable instruction

---

### Unit 10: Main Transient Definition

**Goal**: Define the main `org-gtd-view-builder` transient prefix.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Header display:
```elisp
(defun org-gtd-view-builder--header ()
  "Return header string with current state and item count.")
```

2. Define main transient using patterns from `org-gtd-organize.el`:
```elisp
(transient-define-prefix org-gtd-view-builder ()
  "Build GTD agenda views interactively."
  [:description org-gtd-view-builder--header]
  ;; Type selection group
  ;; Secondary filters group (conditional)
  ;; Tertiary filters group
  ;; Block management group
  ;; Actions group
  )
```

3. Add autoload cookie:
```elisp
;;;###autoload (autoload 'org-gtd-view-builder "org-gtd-view-builder" nil t)
```

**Dependencies**: Units 2-9

**Tests**:
- Transient displays correctly
- All keybindings work
- Conditional sections show/hide appropriately

---

### Unit 11: Load View Command

**Goal**: Implement `org-gtd-load-view` for loading saved views.

**Files**: `org-gtd-view-builder.el`

**Changes**:
1. Load view transient:
```elisp
(transient-define-prefix org-gtd-load-view ()
  "Load, edit, or delete saved GTD views.")
```

2. Load action:
```elisp
(defun org-gtd-view-builder--load (name)
  "Display saved view NAME using org-gtd-view-show.")
```

3. Edit action:
```elisp
(defun org-gtd-view-builder--edit (name)
  "Open view builder pre-populated with saved view NAME.")
```

4. Delete action:
```elisp
(defun org-gtd-view-builder--delete (name)
  "Remove saved view NAME after confirmation.")
```

5. Dynamic suffix generation for saved views using `:setup-children`

**Dependencies**: Unit 1, Unit 10

**Tests**:
- Load displays the correct view
- Edit pre-populates all filters correctly
- Delete removes entry and updates display

---

### Unit 12: Module Integration

**Goal**: Integrate view builder into org-gtd package.

**Files**: `org-gtd.el`, `org-gtd-view-builder.el`

**Changes**:
1. Add require to `org-gtd.el`:
```elisp
(require 'org-gtd-view-builder)
```

2. Add autoloads for both commands:
```elisp
;;;###autoload (autoload 'org-gtd-view-builder "org-gtd-view-builder" nil t)
;;;###autoload (autoload 'org-gtd-load-view "org-gtd-view-builder" nil t)
```

3. Add `provide` statement to `org-gtd-view-builder.el`

4. Update package documentation if needed

**Dependencies**: All units

**Tests**:
- Commands are available after loading org-gtd
- Autoloads work correctly
- No byte-compile warnings

---

## Testing Strategy

### Unit Tests (`test/unit/view-builder-test.el`)

1. **State management tests**
   - Filter state initialization
   - Filter state reset
   - Block list manipulation

2. **Spec generation tests**
   - Single block spec generation
   - Multi-block spec generation
   - All filter types produce correct DSL

3. **Saved views tests**
   - Save and retrieve round-trip
   - Overwrite behavior
   - Delete behavior

### Integration Tests

1. **End-to-end workflow test**
   - Build view interactively
   - Save view
   - Load and display view
   - Edit saved view

2. **Preview accuracy test**
   - Preview matches actual agenda output

## Implementation Order

Recommended order for minimal dependencies:

1. Unit 1 (Custom Variable)
2. Unit 2 (State Management)
3. Unit 7 (Spec Generation) - can test independently
4. Unit 3 (Type Selection)
5. Unit 5 (Tertiary Filters)
6. Unit 4 (Secondary Filters)
7. Unit 6 (Block Management)
8. Unit 9 (Save Workflow)
9. Unit 8 (Preview)
10. Unit 10 (Main Transient)
11. Unit 11 (Load View)
12. Unit 12 (Integration)

## Key Patterns from Existing Code

### Transient Patterns to Follow

From `org-gtd-organize.el`:
- Conditional sections with `:if`
- Simple suffix definitions

From `org-gtd-agenda-transient.el`:
- Dynamic header with `:description`
- Context detection functions

From `org-gtd-graph-transient.el`:
- Custom prefix class for mutable state
- `:setup-children` for dynamic suffix generation
- Sticky mode implementation
- `:scope` for passing data between transient and suffixes

### View Language Integration

Use existing `org-gtd-view-language.el` functions:
- `org-gtd-view-show` for preview and load
- `org-gtd-view-lang--create-custom-commands` for understanding spec format
- Type constants from `org-gtd-types.el`

### Type Information

From `org-gtd-types.el`:
- `org-gtd-type-property` for semantic property lookup
- `org-gtd-type-org-gtd-value` for ORG_GTD property values

## File Summary

| File | Action | Description |
|------|--------|-------------|
| `org-gtd-core.el` | Modify | Add `org-gtd-saved-views` defcustom |
| `org-gtd-view-builder.el` | Create | Main implementation (~500-700 lines) |
| `org-gtd.el` | Modify | Add require and autoloads |
| `test/unit/view-builder-test.el` | Create | Unit tests |

## Risk Assessment

**Low Risk**:
- Custom variable definition
- State management
- Spec generation
- Save/load workflow

**Medium Risk**:
- Dynamic transient suffix generation (complex but well-documented in graph-transient)
- Preview integration (depends on view-language stability)

**Mitigations**:
- Follow existing patterns from graph-transient exactly
- Test preview with various filter combinations
- Ensure spec generation matches view-language expectations
