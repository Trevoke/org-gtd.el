# Design: Upcoming Delegated Items Review View

**Date:** 2025-11-18
**Author:** Brainstorming session
**Status:** Approved

## Purpose

Add a review view for delegated items with upcoming check-in dates. This complements the existing "missed delegated" view by showing future check-ins instead of past ones.

## Use Cases

1. **Planning follow-ups** - Review upcoming delegated items during weekly planning
2. **Ad-hoc checks** - Find specific delegated items when someone reports early completion
3. **Proactive monitoring** - Regularly check upcoming items to catch early completions and adjust plans

## Design

### View Specification

Add to `org-gtd-review.el` after line 280:

```elisp
(defconst org-gtd-review-upcoming-delegated-view-spec
  '((name . "Upcoming check-ins on delegated items")
    (filters . ((category . delegated)
                (timestamp . future))))
  "GTD view specification for upcoming delegated item check-ins.")
```

- Uses existing view language infrastructure
- Mirrors `org-gtd-review-missed-delegated-view-spec` but with `(timestamp . future)`
- Translates to org-ql query checking `ORG_GTD_TIMESTAMP > today`

### Interactive Function

Add to `org-gtd-review.el`:

```elisp
;;;###autoload
(defun org-gtd-review-upcoming-delegated ()
  "Show delegated items with upcoming check-in dates.
Displays all delegated items where ORG_GTD_TIMESTAMP is in the future.
Useful for planning follow-ups and catching early completions."
  (interactive)
  (with-org-gtd-context
      (let ((org-agenda-custom-commands
             (org-gtd-view-lang--create-custom-commands
              (list org-gtd-review-upcoming-delegated-view-spec)
              "o"
              "Upcoming Delegated Check-ins")))
        (org-agenda nil "o")
        (goto-char (point-min)))))
```

- Follows same pattern as `org-gtd-review-missed-delegated`
- Autoloaded for immediate availability
- Uses "o" key like other review functions
- Positions point at top of agenda buffer

### Testing

Add to `test/org-gtd-review-test.el`:

```elisp
(it "can translate upcoming delegated view specification to org-ql"
  (let* ((upcoming-spec org-gtd-review-upcoming-delegated-view-spec)
         (query (org-gtd-view-lang--translate-to-org-ql upcoming-spec)))
    (expect query :to-equal '(and (property "DELEGATED_TO")
                                  (property-ts> "ORG_GTD_TIMESTAMP" "today")
                                  (not (done))))))

(it "provides the org-gtd-review-upcoming-delegated function"
  (expect (fboundp 'org-gtd-review-upcoming-delegated) :to-be t))
```

- First test verifies correct org-ql translation
- Second test verifies function is properly defined and autoloaded

### Implementation Notes

**Potential issue:** `property-ts>` may not exist in org-gtd-view-language.el

Current status of `(timestamp . future)` translation is unknown. Need to:
1. Check if `(timestamp . future)` already translates correctly
2. If not, implement `property-ts>` translator in `org-gtd-view-language.el`
3. Follow same pattern as `property-ts<` for past timestamps

## Files Modified

1. **org-gtd-review.el** - New constant and function
2. **test/org-gtd-review-test.el** - New tests
3. **org-gtd-view-language.el** - Possibly add `property-ts>` support

## Scope Decisions

**What's included:**
- Standalone review function for upcoming delegated items
- Shows ALL future delegated items (no date filtering)
- Simple, focused view following existing patterns

**What's NOT included (may add later):**
- Date range filtering (e.g., "next 2 weeks only")
- Integration with missed engagements view
- Combined "delegation review" showing both past and future
- Custom timeframe configuration

## Success Criteria

1. `M-x org-gtd-review-upcoming-delegated` shows delegated items with future timestamps
2. Tests pass
3. View follows same UX pattern as existing review functions
4. Function is autoloaded and immediately available
