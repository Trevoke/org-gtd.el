# Design: Upgrading org-gtd.el to Emacs 28.1 Minimum Version

**Date**: 2025-11-23
**Version**: 4.0.0
**Status**: Approved

## Overview

This design covers upgrading org-gtd.el from Emacs 27.2 to 28.1 as the minimum supported version. The primary driver is transient.el's updated minimum requirement, but we'll leverage this opportunity to modernize the codebase comprehensively.

### Versioning Strategy

This is a **breaking change** requiring version bump to **4.0.0**:
- Drops support for Emacs 27.2 (breaking)
- Removes 27.2-specific compatibility code (API change)
- Adopts 28.1+ features that may subtly change behavior
- Signals to users: "Major infrastructure upgrade"

### Implementation Approach

**Three-phase implementation on `org-gtd-4` branch:**

1. **Phase 1: Foundation** - Version bump + compatibility cleanup
2. **Phase 2: UX Improvements** - 28.1 feature adoption
3. **Phase 3: Performance** - Native compilation + optimizations

**Conservative testing**: Extensive testing at each phase, keep all existing behavior identical, only change internals.

## Phase 1: Foundation (Version Bump + Cleanup)

### Files Requiring Version Changes (27.2 → 28.1)

1. `org-gtd.el` (line 7): Package-Requires header
2. `org-gtd-pkg.el` (line 3): define-package emacs dependency
3. `README.org` (line 37): Requirements section
4. `CLAUDE.md` (line 173): Dependencies documentation
5. `doc/org-gtd.org` (line 273): Installation requirements
6. `doc/org-gtd.texi` (line 681): Generated texinfo (auto-updates on export)
7. `org-gtd.info`: Regenerate from .org file

### Compatibility Code to Remove

#### 1. `declare (modes ...)` Comments

These were needed for 27.2's byte-compiler but are unnecessary in 28.1:

- `org-gtd-projects.el:105` - `(declare (modes org-agenda-mode))`
- `org-gtd-delegate.el:67` - `(declare (modes org-agenda-mode))`
- `org-gtd-delegate.el:87` - `(declare (modes org-mode))`
- `org-gtd-areas-of-focus.el:48` - `(declare (modes org-mode))`
- `org-gtd-areas-of-focus.el:58` - `(declare (modes org-agenda-mode))`
- `org-gtd-clarify.el:123` - `(declare (modes org-agenda-mode))`
- `org-gtd-clarify.el:138` - `(declare (modes org-mode))`
- `org-gtd-clarify.el:154` - `(declare (modes org-gtd-clarify-mode))`

**Action**: Remove all `declare (modes ...)` lines and their comments mentioning "27.2 compatibility"

#### 2. Version Checks to Remove

- `org-gtd-projects.el:967` - `(if (version< emacs-version "28") ...)`
  - **Action**: Keep only the 28+ code path, remove the conditional

- `org-gtd-id.el:130` - `(if (version< emacs-version "25.0") ...)`
  - **Action**: Remove ancient compatibility check (25.0 is way below 28.1)

- `org-gtd-configure.el:122-124` - Comment about 27.2 eval compatibility
  - **Action**: Update or remove comment if the workaround is no longer needed

#### 3. CI/Testing Updates

**File**: `.github/workflows/test.yml`

```yaml
# Before
matrix:
  emacs_version:
    - 27.2
    - 28.2

# After
matrix:
  emacs_version:
    - 28.2
    - 29.1
    - 30.1
```

**Rationale**: Test on minimum (28.2), current stable (29.1), and latest (30.1)

### Phase 1 Validation

```bash
# Must pass all of these:
eldev compile --warnings-as-errors
eldev lint
eldev test -B

# Optional multi-version testing:
eldev docker 28.2 test -B
eldev docker 29.1 test -B
eldev docker 30.1 test -B
```

**Success criteria**: All tests pass, no compilation warnings, no lint errors.

## Phase 2: UX Improvements (28.1 Feature Adoption)

### 1. Prompt Standardization with `format-prompt`

Emacs 28.1's `format-prompt` provides consistent "default value" display.

**Current state**: Most prompts use `org-read-date` which has its own default system, so `format-prompt` gains are minimal for date inputs.

**Opportunities identified**:
- `org-gtd-incubate.el:79, 89` - "Review date: " prompts
- `org-gtd-configure.el:49` - Date prompts
- `org-gtd-core.el:187-193` - Keyword configuration prompts

**Assessment**: Limited benefit since org-read-date already handles defaults well. Consider for non-date prompts only if they show explicit default values.

**Action**: Review all `read-string` and `completing-read` calls. Where defaults are used, standardize with `format-prompt`:

```elisp
;; Before
(read-string (format "%s (default %s): " prompt default))

;; After
(read-string (format-prompt prompt default))
```

### 2. Enhanced Command Discoverability

**Goal**: Ensure commands work well with `M-S-x` (execute-extended-command-for-buffer) filtering.

**Actions**:
- Review all `interactive` commands
- Ensure docstrings clearly describe what command does
- Verify `interactive` forms use appropriate completion
- Commands properly categorized for mode-based filtering

**No code changes expected** - mostly documentation improvements.

### 3. Code Quality Improvements (28.1-specific)

#### a) String Search Optimization

Replace `string-match` with literal searches using `string-search` (28.1+, much faster):

```elisp
;; Before (27.2)
(string-match (regexp-quote "literal") str)

;; After (28.1+)
(string-search "literal" str)
```

**Action**: Grep for `(string-match (regexp-quote` patterns and replace.

#### b) Improved Loop Constructs

Use 28.1's improved `dolist` and `pcase` where beneficial.

**Action**: Review hot-path loops in:
- `org-gtd-projects.el`
- `org-gtd-dependencies.el`
- `org-gtd-task-management.el`
- `org-gtd-view-language.el`

Only change if it improves clarity or performance measurably.

### Phase 2 Testing

```bash
# Standard validation
eldev compile --warnings-as-errors
eldev lint
eldev test -B

# Manual smoke test checklist:
# □ Capture item to inbox
# □ Process inbox item
# □ Create project with dependencies
# □ View project graph
# □ Open engage view
# □ Incubate and reactivate project
# □ Complete and archive items
```

**Success criteria**: All tests pass, manual workflow unchanged from user perspective.

## Phase 3: Performance (Native Compilation + Optimizations)

### 1. Native Compilation Support

**Goal**: Ensure package works seamlessly with native compilation when users have it enabled.

**Actions**:
- Verify `org-gtd-pkg.el` doesn't block native compilation
- Ensure all `.el` files have proper headers (lexical-binding, etc.)
- Audit for eval/load patterns that prevent native compilation
- Add documentation about native compilation benefits

**Files to check**:
- All `.el` files should have: `; -*- lexical-binding: t -*-`
- No problematic `eval` or dynamic code generation in hot paths

### 2. Performance Optimizations

**Optimization opportunities**:

#### a) String Operations
Replace `string-match` + `regexp-quote` with `string-search` for literal searches:

```elisp
;; Before
(when (string-match (regexp-quote "Projects") (org-entry-get (point) "ORG_GTD"))
  ...)

;; After (28.1+, faster)
(when (string-search "Projects" (org-entry-get (point) "ORG_GTD"))
  ...)
```

#### b) Property Access Batching
Batch `org-entry-get` calls where possible to reduce overhead (not 28.1-specific, but good practice).

#### c) Loop Optimization
Review loops in hot paths - use appropriate constructs for clarity and performance.

**Hot paths identified**:
- `org-gtd-projects.el` - Project graph rendering
- `org-gtd-dependencies.el` - Dependency traversal
- `org-gtd-task-management.el` - Task operations
- `org-gtd-view-language.el` - View filtering

### 3. Profile-Guided Optimization (Optional)

**Baseline test cases**:
- Processing inbox with 50 items
- Rendering project graph with 20 projects, 100 tasks
- Opening engage view with 200 active tasks

**Measurement**:
- Run on Emacs 28.2 before optimizations
- Run on Emacs 28.2 after optimizations
- Compare CPU time and memory allocation

**Target**: 10-20% improvement in hot paths (or no regression)

### 4. Documentation Updates

Add performance section to README.org:

```org
** Performance

*** Native Compilation

org-gtd supports native compilation for improved performance. If you have
Emacs built with native compilation support (--with-native-compilation):

1. Enable automatic native compilation:
   (setq package-native-compile t)

2. Packages will be natively compiled on installation

3. Expected performance improvement: 10-20% in large GTD datasets

*** Performance Characteristics

org-gtd is optimized for Emacs 28.1+ primitives:
- Fast string operations (string-search)
- Efficient property access batching
- Optimized loop constructs

Tested with GTD datasets up to 500+ tasks.
```

### Phase 3 Testing

```bash
# Standard validation
eldev compile --warnings-as-errors
eldev lint
eldev test -B

# Performance validation (optional)
# Run profiler on test workload before/after
# Ensure no regressions
```

**Success criteria**: All tests pass, no performance regressions, documentation updated.

## Documentation Updates (All Phases)

### CHANGELOG.org

Add at the top:

```org
** 4.0.0 - YYYY-MM-DD

*** BREAKING CHANGES
**** Minimum Emacs version now 28.1
org-gtd now requires Emacs 28.1 or higher due to transient.el dependency.

*Users on Emacs 27.2 must remain on org-gtd 3.x.*

*** Removed
- Emacs 27.2 compatibility code
- Workarounds for pre-28.1 behavior

*** Improved
- Prompts use format-prompt for consistent defaults (28.1)
- Performance improvements from 28.1 optimizations
- Native compilation support

*** Technical
- CI tests on Emacs 28.2, 29.1, 30.1
- Codebase modernized to use 28.1+ primitives
```

### README.org

Update requirements section:

```org
** Requirements

- Emacs 28.1 or higher
- org-mode 9.6 or higher
```

### CLAUDE.md

Update dependencies section (line 173):

```markdown
- **Emacs 28.1+**: Minimum version
```

### doc/org-gtd.org

Update installation requirements (line 273):

```org
This package requires Emacs 28.1 or higher.
```

Then regenerate:

```bash
# Regenerate texinfo and info files
emacs --batch --eval "(require 'ox-texinfo)" doc/org-gtd.org -f org-texinfo-export-to-texinfo
makeinfo doc/org-gtd.texi -o doc/org-gtd.info
```

## Testing Strategy Summary

### Per-Phase Requirements

Each phase must pass before proceeding:

```bash
eldev compile --warnings-as-errors
eldev lint
eldev test -B
```

### Optional Multi-Version Testing

```bash
eldev docker 28.2 test -B
eldev docker 29.1 test -B
eldev docker 30.1 test -B
```

### Manual Smoke Test (After Each Phase)

- □ Capture item to inbox
- □ Process inbox item
- □ Create project with dependencies
- □ View project graph
- □ Open engage view
- □ Incubate and reactivate project
- □ Complete and archive items

## Work Plan

All work happens on `org-gtd-4` branch:

1. Implement Phase 1 → commit
2. Test Phase 1 → verify
3. Implement Phase 2 → commit
4. Test Phase 2 → verify
5. Implement Phase 3 → commit
6. Test Phase 3 → verify
7. Update all documentation → commit
8. Final validation across all Emacs versions

Branch owner handles: release tagging, announcements, MELPA submission, etc.

## Success Criteria

- ✓ All version numbers updated to 28.1
- ✓ All 27.2 compatibility code removed
- ✓ CI tests pass on 28.2, 29.1, 30.1
- ✓ No compilation warnings
- ✓ No lint errors
- ✓ All existing tests pass
- ✓ Manual smoke test passes
- ✓ Documentation updated
- ✓ CHANGELOG entry added

## Risks and Mitigations

**Risk**: Breaking changes affect users on 27.2
**Mitigation**: Clear version 4.0.0 signals breaking change, CHANGELOG prominent

**Risk**: Performance regressions from changes
**Mitigation**: Conservative testing approach, profile hot paths

**Risk**: New 28.1 features introduce subtle bugs
**Mitigation**: Comprehensive test suite, manual smoke testing, phased approach

**Risk**: Native compilation issues
**Mitigation**: Ensure clean code (lexical-binding, no dynamic eval), test with/without

## Conclusion

This three-phase approach safely upgrades org-gtd to Emacs 28.1+ while:
- Maintaining stability through conservative testing
- Modernizing the codebase for performance and clarity
- Providing clear migration path for users
- Leveraging 28.1 features for better UX

The phased implementation allows for validation at each step, minimizing risk while maximizing benefit.
