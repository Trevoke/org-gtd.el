# View DSL Simplification Design

## Overview

Simplify the GTD view DSL to be more intuitive, consistent, and GTD-focused. Users should think in GTD terms, not org-mode implementation details.

## Key Changes

### 1. Remove `filters` wrapper

**Before:**
```elisp
'((name . "Stuck Projects")
  (filters . ((type . stuck-project))))
```

**After:**
```elisp
'((name . "Stuck Projects")
  (type . stuck-project))
```

### 2. Block-based multi-view structure

Single-block views use flat structure. Multi-block views use `blocks`:

```elisp
'((name . "GTD Engage")
  (blocks . (
    ((name . "Today's Calendar")
     (block-type . calendar-day))

    ((name . "Items to Review")
     (type . incubated)
     (when . today))

    ((name . "Delegated Check-ins")
     (type . delegated)
     (when . today))

    ((name . "Next Actions")
     (type . next-action)))))
```

### 3. Types include TODO keywords

| Type | ORG_GTD Value | TODO Keyword(s) |
|------|---------------|-----------------|
| `next-action` | "Actions" | NEXT |
| `delegated` | "Delegated" | WAIT |
| `calendar` | "Calendar" | (none) |
| `incubated` | "Incubated" | (none) |
| `habit` | "Habit" | (none) |
| `project` | "Projects" | (none) |

### 4. Computed project types (via DAG walk)

| Type | Query |
|------|-------|
| `stuck-project` | ORG_GTD="Projects" + has incomplete tasks + no NEXT tasks |
| `active-project` | ORG_GTD="Projects" + has active tasks |
| `completed-project` | ORG_GTD="Projects" + all tasks done/canceled |
| `incubated-project` | ORG_GTD="Incubated" + PREVIOUS_ORG_GTD="Projects" |

### 5. Semantic time filters

Use `:when` from org-gtd-types instead of `timestamp`:

```elisp
(when . past)    ; ORG_GTD_TIMESTAMP before today
(when . today)   ; ORG_GTD_TIMESTAMP equals today (any time)
(when . future)  ; ORG_GTD_TIMESTAMP after today
```

### 6. Special block type: `calendar-day`

Native org-agenda day view with time grid, filtered to:
- ORG_GTD=Calendar items
- ORG_GTD=Habit items

## Files to Change

1. **org-gtd-view-language.el** - Core DSL implementation
2. **org-gtd-engage.el** - Engage view specs
3. **org-gtd-reflect.el** - Reflect view specs
4. **test/gtd-view-language-test.el** - Tests
5. **doc/org-gtd.org** - Documentation

## Engage View Final Spec

```elisp
'((name . "GTD Engage")
  (blocks . (
    ((name . "Today's Calendar")
     (block-type . calendar-day))

    ((name . "Items to Review")
     (type . incubated)
     (when . today))

    ((name . "Delegated Check-ins")
     (type . delegated)
     (when . today))

    ((name . "Next Actions")
     (type . next-action)))))
```
