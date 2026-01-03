# Plan: Remove `with-org-gtd-context` Macro

**Goal:** Eliminate the fundamental hack that makes org-gtd temporarily reconfigure org-mode for every operation. Users should configure org-mode directly.

**Impact:** This is a breaking change for users who relied on org-gtd to "just work" without org-mode configuration. Requires clear migration documentation.

---

## Implementation Status: ✅ COMPLETE (2025-11-25)

All production code has been updated to no longer use `with-org-gtd-context`:
- Removed property inheritance (`t` argument) from 4 call sites
- Implemented project CATEGORY lookup via ID for agenda display
- Updated archive functions to use local `org-archive-location` binding
- Removed macro wrapper from all org-gtd modules
- Upgrade functions explicitly bind what they need (org-agenda-files, property inheritance)
- Macro deprecated with warning, kept for backward compatibility
- All 601 tests passing

---

## Phase 1: Verify Obsolescence of Each Binding

### 1.1 `org-use-property-inheritance "ORG_GTD"` - REMOVE

**Status:** ✅ VERIFIED OBSOLETE (2025-11-25)

**Background:** The `t` (inherit) argument in `org-entry-get` is legacy from v2/v3 where items were nested under GTD category headings. In v4, each item has its own `ORG_GTD` property directly, and projects use ID-based links (`ORG_GTD_PROJECT_IDS`) instead of nesting.

#### Case-by-Case Verification

**Case 1: `org-gtd-areas-of-focus.el:71`**
```elisp
(unless (org-entry-get nil "ORG_GTD" t)
  (user-error "This item has no ORG_GTD property - cannot set area of focus"))
```
- **Purpose:** Verify item has ORG_GTD before setting area of focus
- **v4 Status:** Items have direct ORG_GTD property
- **Action:** Remove `t` argument

**Case 2: `org-gtd-agenda.el:116`**
```elisp
(category (org-entry-get (point) org-gtd-prop-area-of-focus t))
```
- **Purpose:** Get CATEGORY for agenda display (areas of focus)
- **v4 Problem:** Project tasks don't inherit CATEGORY because they're linked via IDs, not nested
- **Current gap:** `org-gtd-areas-of-focus--set` skips `project-task` types, so project tasks don't get CATEGORY during organize
- **Action:** Replace inheritance with explicit project lookup - at display time, if task has `ORG_GTD_PROJECT_IDS`, look up the project heading's CATEGORY
- **Note:** This requires new code to resolve project ID → project heading → CATEGORY

**Case 3: `org-gtd-refile.el:103`**
```elisp
(org-refile-target-verify-function
 (lambda () (string-equal org-gtd-projects
                          (org-entry-get nil "ORG_GTD" t))))
```
- **Purpose:** Verify refile target is a project heading
- **v4 Status:** Project headings have `ORG_GTD: Projects` directly
- **Action:** Remove `t` argument
- **Note:** Refiling project tasks needs product definition - in v4 they don't need to be under project heading

**Case 4: `org-gtd-skip.el:101`**
```elisp
(if (and (string-equal (org-entry-get (point) org-gtd-prop-category t)
                       org-gtd-calendar)
         (org-entry-get (point) org-gtd-timestamp))
```
- **Purpose:** Skip-function to filter for calendar items
- **v4 Status:** Calendar items have `ORG_GTD: Calendar` directly
- **Action:** Remove `t` argument

#### Summary Table

| Case | File:Line | Purpose | Action |
|------|-----------|---------|--------|
| 1 | `org-gtd-areas-of-focus.el:71` | Verify item has ORG_GTD | Remove `t` |
| 2 | `org-gtd-agenda.el:116` | Get CATEGORY for display | Replace with project lookup |
| 3 | `org-gtd-refile.el:103` | Verify refile target | Remove `t` |
| 4 | `org-gtd-skip.el:101` | Filter calendar items | Remove `t` |

#### Implementation Notes

Cases 1, 3, 4 are straightforward: just remove the `t` argument.

Case 2 requires new functionality:
```elisp
;; Pseudocode for project CATEGORY lookup
(defun org-gtd--get-category-for-task ()
  "Get CATEGORY, looking up project if this is a project task."
  (or (org-entry-get (point) "CATEGORY")
      (when-let* ((project-ids (org-entry-get (point) org-gtd-prop-project-ids))
                  (first-id (car (split-string project-ids)))
                  (project-marker (org-id-find first-id 'marker)))
        (org-with-point-at project-marker
          (org-entry-get (point) "CATEGORY")))))
```

### 1.2 `org-archive-location` - LOCALIZE

**Status:** Only needed in archive functions.

**Action:** Bind locally in:
- `org-gtd-archive-completed-items`
- `org-gtd-archive-item-at-point`
- `org-gtd--archive-complete-projects` (if it calls archive directly)

### 1.3 `org-stuck-projects` - REMOVE

**Status:** User confirmed obsolete. org-gtd has its own stuck projects detection via `org-gtd-review-stuck-projects`.

**Verification:** Confirm no code calls `org-agenda-list-stuck-projects` or uses the `org-stuck-projects` variable.

### 1.4 `org-odd-levels-only nil` - REMOVE

**Status:** User believes obsolete (was assumption about org levels).

**Verification:** Search for any code that assumes specific heading levels. The DAG-based project structure should not care about odd/even levels.

### 1.5 `org-agenda-files` - USER CONFIGURATION

**Status:** User says this is already shifted out.

**Current behavior:** Macro binds to `(org-gtd-core--agenda-files)` which merges user's `org-agenda-files` with `org-gtd-directory`.

**New behavior:** User must include GTD directory in their `org-agenda-files`:
```elisp
(setq org-agenda-files (list "~/gtd/" "~/other-org-files/"))
```

**Impact:** The `org-gtd-core--agenda-files` function can remain as a convenience helper, but won't be automatically applied.

### 1.6 `org-gtd-agenda-property-list` - USER CONFIGURATION

**Status:** Can be moved to configuration.

**Current behavior:** Macro sets to `(list org-gtd-delegate-property)` which is `"DELEGATED_TO"`.

**New behavior:** Document that users should set:
```elisp
(setq org-gtd-agenda-property-list '("DELEGATED_TO"))
```

### 1.7 Advice `org-gtd-core--uniq` - REMOVE

**Status:** Just deduplicates agenda files. Defensive measure that's unnecessary if user configures correctly.

---

## Phase 2: Update Call Sites

### 2.1 org-gtd-wip.el (2 calls) - REMOVE WRAPPER

**Lines 84, 102:** `(with-org-gtd-context (org-gtd-wip-mode))`

**Analysis:** `org-gtd-wip-mode` just sets header-line and enables auto-save. Needs NONE of the bindings.

**Change to:** `(org-gtd-wip-mode)`

### 2.2 org-gtd-archive.el (3 calls) - LOCALIZE ARCHIVE LOCATION

**Line 73:** `org-gtd-archive-completed-items`
- Uses `org-map-entries ... 'agenda` - needs user's `org-agenda-files`
- Does NOT directly call `org-archive-subtree`

**Change:** Remove wrapper. User must configure `org-agenda-files`.

**Line 109:** `org-gtd-archive-item-at-point`
- Calls `org-archive-subtree` - needs `org-archive-location`

**Change to:**
```elisp
(let ((org-archive-location (funcall org-gtd-archive-location)))
  (org-archive-subtree))
```

**Line 169:** Check what this does (in `org-gtd--archive-complete-projects`?)

### 2.3 org-gtd-projects.el (2 calls) - REMOVE WRAPPER

**Lines 99, 1015:** Likely use `org-map-entries ... 'agenda`

**Change:** Remove wrapper. User must configure `org-agenda-files`.

### 2.4 org-gtd-view-language.el (1 call) - REMOVE WRAPPER

**Line 454:** Calls `org-agenda nil "g"`

**Change:** Remove wrapper. User must configure `org-agenda-files`.

### 2.5 org-gtd-core.el (1 call) - REMOVE WRAPPER

**Line 486:** In `org-gtd-core-prepare-agenda-buffers`, uses `org-map-entries`

**Change:** Remove wrapper. User must configure `org-agenda-files`.

### 2.6 org-gtd-graph-transient.el (1 call) - REMOVE WRAPPER

**Line 117:** Graph operations

**Change:** Remove wrapper. User must configure `org-agenda-files`.

### 2.7 org-gtd-organize.el (1 call) - REMOVE WRAPPER

**Line 147:** Organization

**Change:** Remove wrapper. Analyze what it actually needs.

### 2.8 org-gtd-refile.el (3 calls) - REMOVE WRAPPER

**Lines 87, 89, 104:** Inside refile macros

**Analysis:** These macros already bind their own `org-refile-targets` to `org-agenda-files`. They just need `org-agenda-files` to be correct.

**Change:** Remove wrapper from macro definitions.

### 2.9 org-gtd-upgrades.el (6 calls) - SPECIAL HANDLING

**Migration code.** This runs on potentially misconfigured systems.

**Options:**
1. Keep wrapper only for upgrades (deprecated shim)
2. Require users to configure before upgrading (document clearly)
3. Have upgrade function temporarily set `org-agenda-files` itself

**Recommendation:** Option 3 - have upgrade functions explicitly set `org-agenda-files` temporarily, with clear messaging.

### 2.10 org-gtd-mode.el (1 call) - ALREADY OBSOLETE

**Line 109:** Already obsolete, can ignore or remove entirely.

### 2.11 Tests (5 calls) - UPDATE TEST SETUP

Test helpers need to ensure proper `org-agenda-files` configuration.

**Files:**
- test/core-test.el (3)
- test/helpers/utils.el (1)
- test/helpers/assertions.el (1)

---

## Phase 3: Documentation Updates

### 3.1 Update Required Configuration Section

**File:** doc/org-gtd.org - Configuration section

Add to required configuration:
```elisp
;; org-agenda-files MUST include your GTD directory
(setq org-agenda-files (list org-gtd-directory))
;; Or if you have other org files:
(setq org-agenda-files (list org-gtd-directory "~/other-org/"))

;; Show delegate property in agenda (optional but recommended)
(setq org-gtd-agenda-property-list '("DELEGATED_TO"))
```

### 3.2 Migration Guide

Create new section: "Upgrading to org-gtd 4.x"

**Key points:**
1. `org-agenda-files` must now include GTD directory
2. `with-org-gtd-context` is removed - if you used it in custom code, remove it
3. Custom agenda views no longer need the wrapper

### 3.3 Update CLAUDE.md

Remove reference to `with-org-gtd-context` as a key pattern.

### 3.4 Update Docstrings

Remove references to the macro from function docstrings.

---

## Phase 4: Implementation Order

### Step 1: Create Compatibility Warning (Optional)

Before removing, could add warning when macro is used:
```elisp
(defmacro with-org-gtd-context (&rest body)
  (display-warning 'org-gtd
    "with-org-gtd-context is deprecated and will be removed.
     Configure org-agenda-files directly." :warning)
  `(progn ,@body))
```

### Step 2: Update Archive Functions

Localize `org-archive-location` binding in archive functions only.

### Step 3: Remove Wrapper from Simple Call Sites

In order:
1. org-gtd-wip.el (simplest - just remove)
2. org-gtd-refile.el (already has own bindings)
3. org-gtd-projects.el
4. org-gtd-view-language.el
5. org-gtd-core.el
6. org-gtd-graph-transient.el
7. org-gtd-organize.el

### Step 4: Handle Upgrades

Update org-gtd-upgrades.el to explicitly configure what it needs.

### Step 5: Update Tests

Update test setup to configure `org-agenda-files` properly.

### Step 6: Remove Macro Definition

Delete the macro from org-gtd-core.el.

### Step 7: Clean Up Related Code

- Remove `org-gtd-core--uniq` function
- Remove `org-gtd-stuck-projects` function (if truly unused)
- Update `org-gtd-core--agenda-files` docstring (now a helper, not required)

### Step 8: Update Documentation

All documentation changes from Phase 3.

---

## Phase 5: Testing Strategy

### Unit Tests

For each modified file, verify:
1. Functions work when `org-agenda-files` includes GTD directory
2. Functions fail gracefully when `org-agenda-files` is misconfigured

### Integration Tests

1. Full workflow: capture → clarify → organize → engage → archive
2. Project operations: create, modify dependencies, complete
3. Views: engage, stuck projects, custom views

### Migration Test

1. Start with v3.x configuration (no explicit `org-agenda-files`)
2. Run upgrade
3. Verify warning/error guides user to configure
4. After configuration, verify everything works

---

## Risks and Mitigations

### Risk 1: Breaking Existing Users

**Mitigation:**
- Clear upgrade documentation
- Warning message if GTD directory not in `org-agenda-files`
- Consider a one-time auto-fix that adds GTD directory to `org-agenda-files`

### Risk 2: Edge Cases in Archive

**Mitigation:**
- Thorough testing of archive with different `org-archive-location` configurations
- Local binding ensures correct behavior

### Risk 3: Upgrade Code Fails

**Mitigation:**
- Upgrade functions explicitly set what they need
- Clear pre-upgrade checklist in documentation

---

## Success Criteria

1. `with-org-gtd-context` macro is deleted
2. All tests pass
3. Documentation clearly states required `org-agenda-files` configuration
4. No advice is added/removed during operations
5. Archive works correctly with local binding
6. Upgrade path is clear and tested
