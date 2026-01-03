# Priority, Effort, and Clocked Filters for View DSL

**Date:** 2025-12-11
**Status:** Design approved

## Overview

Add three new filters to the GTD view DSL: priority, effort, and clocked time. These enable "quick wins" views, "deep work" views, and triage views for items missing metadata.

## Filter Syntax

### Priority Filter

```elisp
(priority . A)              ; single value
(priority . (A B))          ; list - matches A OR B
(priority . (>= B))         ; comparison - B or higher (A, B)
(priority . nil)            ; no priority set
```

Comparison operators: `<`, `>`, `<=`, `>=`

Note: A is highest priority (A > B > C). Respects `org-priority-highest` and `org-priority-lowest` for custom ranges.

### Effort Filter

```elisp
(effort . (< "0:30"))              ; under 30 minutes
(effort . (> "1:00"))              ; over 1 hour
(effort . (between "0:15" "1:00")) ; range (inclusive)
(effort . nil)                     ; no effort set
```

Duration strings support all org-mode formats:
- `"3:12"` - H:MM
- `"1:23:45"` - H:MM:SS
- `"1d3h5min"` - units
- `"30"` - bare number (minutes)

### Clocked Filter

```elisp
(clocked . (< "0:30"))              ; less than 30 min clocked
(clocked . (> "2:00"))              ; more than 2 hours invested
(clocked . (between "0:30" "2:00")) ; range
(clocked . nil)                     ; no time clocked yet (0 minutes)
```

Same duration format as effort.

## Implementation Architecture

### Translation Functions (org-gtd-view-language.el)

Add to the filter router in `org-gtd-view-lang--translate-filter`:

```elisp
org-gtd-view-lang--translate-priority-filter
org-gtd-view-lang--translate-effort-filter
org-gtd-view-lang--translate-clocked-filter
```

### Skip Predicates (org-gtd-skip.el)

For native agenda blocks:

```elisp
org-gtd-pred--priority-matches
org-gtd-pred--effort-matches
org-gtd-pred--clocked-matches
```

### Data Sources

- **Priority:** `(org-entry-get nil "PRIORITY")` - returns "A", "B", "C", or nil
- **Effort:** `(org-entry-get nil "Effort")` - returns duration string or nil
- **Clocked:** `(org-clock-sum-current-item)` - returns minutes as integer

### Duration Parsing

Use org-mode's built-in functions (no custom parsing):

```elisp
(org-duration-to-minutes "1d3h5min")  ; => 1625.0
(org-duration-p "1d3h5min")           ; => t (validation)
```

### Priority Comparison

Convert to numeric for comparison (lower number = higher priority):

```elisp
;; A=1, B=2, C=3
(org-gtd-view-lang--priority-to-number "A") ; => 1

;; (priority . (>= B)) means priority-number <= 2
```

## Edge Cases

- Invalid duration in filter spec: error at view creation (fail fast)
- Invalid duration in item's Effort property: item is skipped
- `(clocked . nil)` matches items with 0 minutes clocked
- Custom priority ranges respected via `org-priority-highest/lowest`

## Example Views

### Quick Wins
```elisp
(org-gtd-view-show
 '((name . "Quick Wins")
   (type . next-action)
   (effort . (< "15min"))))
```

### Focus Work
```elisp
(org-gtd-view-show
 '((name . "Focus Work")
   (type . next-action)
   (priority . (>= B))
   (clocked . (> "0:30"))))
```

### Triage (Missing Metadata)
```elisp
(org-gtd-view-show
 '((name . "Needs Priority")
   (type . next-action)
   (priority . nil)))
```

### Combined Filters
```elisp
(org-gtd-view-show
 '((name . "Quick Work Tasks")
   (area-of-focus . "Work")
   (type . next-action)
   (effort . (< "30min"))
   (priority . (A B))))
```

## Testing Strategy

### Unit Tests (test/unit/gtd-view-language-test.el)
- Priority filter translation: single, list, comparison, nil
- Effort filter translation: <, >, between, nil
- Clocked filter translation: same patterns
- Duration parsing with various formats
- Priority comparison with custom ranges

### Skip Predicate Tests (test/unit/skip-test.el)
- `org-gtd-pred--priority-matches` - all value forms
- `org-gtd-pred--effort-matches` - comparisons + nil
- `org-gtd-pred--clocked-matches` - with clock data

### Integration Tests
- End-to-end view with each filter type
- Combined filters work together
- Items correctly included/excluded

## Files to Modify

1. `org-gtd-view-language.el` - translation functions
2. `org-gtd-skip.el` - skip predicates
3. `test/unit/gtd-view-language-test.el` - unit tests
4. `test/unit/skip-test.el` - predicate tests
5. `doc/org-gtd.org` - documentation
