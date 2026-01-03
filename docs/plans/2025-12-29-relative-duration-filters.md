# Relative Duration Filters for View Language DSL

**Date:** 2025-12-29
**Status:** Approved

## Overview

Extend the org-gtd view language DSL to support relative duration comparisons in time-based filters. Currently `when`, `deadline`, and `scheduled` only support `past`, `today`, `future`. This adds syntax like `(< "14d")` for "within next 14 days".

## Duration Syntax

**Format:** `[+-]?[0-9]+[mhdwMy]`

| Unit | Meaning |
|------|---------|
| `m` | minutes |
| `h` | hours |
| `d` | days |
| `w` | weeks |
| `M` | months |
| `y` | years |

**Sign:**
- `+` or no sign = future (14 days from now)
- `-` = past (7 days ago)

**Examples:**
- `"14d"` or `"+14d"` = 14 days from now
- `"-7d"` = 7 days ago
- `"2w"` = 2 weeks from now
- `"-1M"` = 1 month ago

## Comparison Operators

| Operator | Meaning |
|----------|---------|
| `<` | less than (closer to now) |
| `>` | greater than (further from now) |
| `=` | equals (same day) |

## Filter Semantics

| Filter | Positive Duration | Negative Duration |
|--------|-------------------|-------------------|
| `when` | future timestamp | past timestamp |
| `deadline` | future deadline | past deadline |
| `scheduled` | future scheduled | past scheduled |
| `done` | error (implicit past) | past completion |
| `last-clocked-out` | error (implicit past) | past clock-out |

For `done` and `last-clocked-out`, durations are implicitly past since these filters only make sense looking backward. Positive durations produce a user-error.

## DSL Syntax

Each time-based filter accepts three forms:

```elisp
;; Form 1: Symbol (existing - unchanged)
(when . past)
(when . today)
(when . future)

;; Form 2: Comparison expression (new)
(when . (< "14d"))
(when . (> "-7d"))
(when . (= "today"))

;; Form 3: Friendly aliases (existing - unchanged)
(done . past-week)
(done . past-month)
```

## Example Specs

```elisp
;; Upcoming calendar (next 2 weeks)
((type . calendar)
 (when . (< "14d"))
 (name . "Upcoming Calendar"))

;; Overdue delegated items (past week)
((type . delegated)
 (when . (< "-7d"))
 (name . "Recently Overdue Delegated"))

;; Deadlines approaching
((type . next-action)
 (deadline . (< "3d"))
 (name . "Deadlines Soon"))

;; Recently completed
((type . next-action)
 (done . (< "7d"))
 (name . "Recently Completed"))

;; Stale tasks (not worked on in 2 weeks)
((type . next-action)
 (last-clocked-out . (> "14d"))
 (name . "Needs Attention"))
```

## Implementation

### Files to Modify

1. **org-gtd-skip.el**
   - Extend `org-gtd--parse-relative-time` to handle +/- signs
   - Add `org-gtd--duration-to-reference-time` helper

2. **org-gtd-view-language.el**
   - Update filter processing to handle comparison expressions
   - Add validation for comparison expression format

### Processing Flow

```
DSL spec: (when . (< "14d"))
    |
    v
Parse: op=<, duration="14d"
    |
    v
Calculate reference time: now + 14 days
    |
    v
Build predicate: (org-gtd-pred--property-ts< prop reference)
    |
    v
Compose into skip function
```

### Validation Rules

1. Comparison expression must be `(op duration)` where op is `<`, `>`, or `=`
2. Duration must match pattern `[+-]?[0-9]+[mhdwMy]` or be `"today"`
3. For `done`/`last-clocked-out`, positive durations produce user-error

## Testing Strategy

### Unit Tests (org-gtd-skip.el)

**Duration parsing:**
- `"14d"` -> 14 days from now
- `"+14d"` -> 14 days from now
- `"-7d"` -> 7 days ago
- `"2w"` -> 2 weeks from now
- Invalid formats -> error

**Predicate behavior:**
- `(< "14d")` matches timestamp 7 days from now
- `(< "14d")` rejects timestamp 20 days from now
- `(< "-7d")` matches timestamp 3 days ago
- `(< "-7d")` rejects timestamp 10 days ago

### Unit Tests (org-gtd-view-language.el)

**DSL validation:**
- Valid: `(when . (< "14d"))`
- Valid: `(deadline . (> "-1w"))`
- Invalid: `(done . (< "+7d"))` -> error
- Invalid: `(when . (? "14d"))` -> error

**Skip function generation:**
- Spec with `(when . (< "14d"))` produces correct skip function
- Combining filters works correctly

## Scope

**In Scope:**
- ~50-80 lines in `org-gtd-skip.el`
- ~30-50 lines in `org-gtd-view-language.el`
- ~100-150 lines of tests
- Documentation updates

**Not In Scope:**
- Combining multiple time conditions (e.g., "between 7d and 14d")
- Time-of-day comparisons

## Backward Compatibility

All existing specs continue to work unchanged. The new syntax is purely additive.
