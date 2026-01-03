# Test Suite Streamlining Plan

## Executive Summary

The org-gtd test suite has grown to ~20,000 lines across 89 files. While well-structured with strong helpers and isolation, there are significant consolidation opportunities that could improve maintainability and reduce duplication without losing expressiveness.

### Key Metrics

| Metric | Value |
|--------|-------|
| Total test lines | 20,592 |
| Changes vs master | +19,466 / -730 lines |
| WIP buffer pattern duplications | 71+ occurrences |
| Inline insert statements | 631 occurrences |
| Autoload test files | 10 nearly-identical files |
| Largest file | end-to-end-test.el (3,186 lines) |

---

## Issue 1: WIP Buffer Access Pattern (Priority: HIGH)

### Problem

The pattern for accessing WIP buffers appears **71+ times** across test files:

```elisp
(let ((wip-buffers (seq-filter (lambda (buf)
                                 (string-search org-gtd-wip--prefix (buffer-name buf)))
                               (buffer-list))))
  (when wip-buffers
    (with-current-buffer (car wip-buffers)
      ;; ... do something ...
      )))
```

This appears in:
- end-to-end-test.el (45+ times)
- project-test.el (5 times)
- integration tests (10+ times)
- keyboard-integration.el (4 times)
- processing-test.el, areas-of-focus.el, etc.

### Solution

Create a helper macro in `test/helpers/wip.el`:

```elisp
(defmacro with-wip-buffer (&rest body)
  "Execute BODY in the current WIP buffer if one exists.
Returns nil if no WIP buffer is found."
  `(when-let ((wip-buf (org-gtd-test--get-wip-buffer)))
     (with-current-buffer wip-buf
       ,@body)))

(defun org-gtd-test--get-wip-buffer ()
  "Return the current WIP buffer, or nil if none exists."
  (car (seq-filter (lambda (buf)
                     (string-search org-gtd-wip--prefix (buffer-name buf)))
                   (buffer-list))))
```

### Before/After Example

**Before (5 lines per use):**
```elisp
(let ((wip-buffers (seq-filter (lambda (buf)
                                 (string-search org-gtd-wip--prefix (buffer-name buf)))
                               (buffer-list))))
  (when wip-buffers
    (with-current-buffer (car wip-buffers)
      (goto-char (point-max))
      (make-task "Task 1" :level 2))))
```

**After (3 lines per use):**
```elisp
(with-wip-buffer
  (goto-char (point-max))
  (make-task "Task 1" :level 2))
```

### Estimated Impact
- **Lines saved:** ~350 lines (71 occurrences × 5 lines → 2 lines each)
- **Readability:** Significant improvement - intent is now clear
- **Maintenance:** Single point of change if WIP buffer naming changes

---

## Issue 2: Inline Org String Inserts (Priority: MEDIUM)

### Problem

There are **631 inline insert statements** like:
```elisp
(insert "** TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n...")
```

While builders.el provides `make-task`, `make-project`, etc., many tests still use inline strings, especially in:
- org-gtd-graph-*.el files (graph operations)
- migration-unit-test.el
- dependencies-test.el
- accessors-test.el

### Analysis

Not all inline inserts are bad. Some legitimate uses:
1. **Migration tests** - Testing legacy formats requires exact legacy strings
2. **Edge case tests** - Testing malformed input
3. **Simple single-property tests** - When only testing one aspect

**Problem cases:**
1. Tests that insert full property drawers manually
2. Tests that create project structures by hand instead of using `make-project`
3. Tests that duplicate the same structure in multiple places

### Solution: Selective Migration

#### Phase 1: Identify Non-Builder-Compatible Tests
```bash
# Find tests using inline org strings that should use builders
grep -l 'insert.*TODO' test/*.el | \
  xargs grep -L 'require.*builders'
```

#### Phase 2: Extend Builders for Edge Cases

Add helpers for testing-specific structures:

```elisp
;; In builders.el

(cl-defun make-task-with-legacy-trigger (description &rest args)
  "Create a task with legacy TRIGGER property for migration tests."
  (let ((marker (apply #'make-task description args)))
    (org-entry-put marker "TRIGGER" "org-gtd-next-task-action!")
    marker))

(cl-defun make-malformed-task (description &key missing-id missing-type)
  "Create a deliberately malformed task for edge case testing."
  ...)
```

#### Phase 3: Create Graph-Specific Builders

The graph tests have custom setup functions that could be builders:

```elisp
;; Current pattern in org-gtd-graph-remove-task-test.el:
(defun org-gtd-graph-remove-test--create-project-with-diamond (project-title)
  ;; 80 lines of inline inserts
  )

;; Proposed builder:
(make-diamond-project "Test Project"
  :tasks '("Task A" "Task B" "Task C" "Task D")
  :edges '((a . c) (b . c) (c . d)))
```

### Estimated Impact
- **Lines reduced:** ~500-800 lines
- **Files affected:** 15-20 test files
- **Risk:** Medium - need to ensure builders produce identical structures

---

## Issue 3: Autoload Tests (Priority: MEDIUM)

### Problem

10 nearly-identical autoload test files, each ~14 lines:

```elisp
;; test/autoload-tests/org-gtd-capture-test.el
(require 'buttercup)
(setq org-gtd-update-ack "3.0.0")
(load "org-gtd-autoloads")
(load "test/helpers/autoload-setup.el")

(describe "autoload management (recursive)"
  (before-each (ogt--prepare-gtd-directory))
  (after-each (ogt--clear-gtd-directory))

  (it "org-gtd-capture"
    (org-gtd-capture nil "i")))
```

### Solution: Parametric Autoload Tests

Consolidate into single file with data-driven tests:

```elisp
;; test/autoload-test.el
(require 'buttercup)
(setq org-gtd-update-ack "3.0.0")
(load "org-gtd-autoloads")
(load "test/helpers/autoload-setup.el")
(load "test/helpers/utils.el")

(defconst ogt-autoload-tests
  '(("org-gtd-capture" . (lambda () (org-gtd-capture nil "i")))
    ("org-gtd-engage" . #'org-gtd-engage)
    ("org-gtd-process-inbox" . #'org-gtd-process-inbox)
    ("org-gtd-clarify-item" . (lambda ()
                                (ogt--with-temp-org-buffer
                                  "* Heading"
                                  (org-gtd-clarify-item))))
    ("org-gtd-clarify-mode" . #'org-gtd-clarify-mode)
    ("org-gtd-inbox-path" . #'org-gtd-inbox-path)
    ("org-gtd-mode" . #'org-gtd-mode)
    ("org-gtd-archive-completed-items" . #'org-gtd-archive-completed-items)
    ("org-gtd-reflect-stuck-projects" . #'org-gtd-reflect-stuck-projects)
    ("org-gtd-show-all-next" . #'org-gtd-show-all-next)))

(describe "autoload management"
  (before-each (ogt--prepare-gtd-directory))
  (after-each (ogt--clear-gtd-directory))

  (dolist (test ogt-autoload-tests)
    (it (format "autoloads %s" (car test))
      (expect (funcall (cdr test)) :not :to-throw))))
```

### Estimated Impact
- **Files reduced:** 10 → 1
- **Lines reduced:** ~140 → ~40
- **Maintenance:** Add new autoload = add one cons cell

---

## Issue 4: Static Fixtures vs Builders (Priority: LOW)

### Problem

Two competing fixture systems:
1. `project-fixtures.el` - Static defconst strings
2. `builders.el` - Dynamic builder functions

Tests inconsistently use both, creating confusion about which to use when.

### Current Static Fixtures

```elisp
(defconst ogt--project-to-cancel "** [1/3] cancel me ...")
(defconst ogt--completed-project "** [3/3] completed ...")
(defconst ogt--canceled-project "** [3/3] canceled ...")
```

### Solution: Deprecate Static Fixtures

1. **Migrate uses to builder equivalents:**
   - `ogt--completed-project` → `(make-completed-project "completed")`
   - `ogt--canceled-project` → `(make-canceled-project "canceled")`

2. **Keep static fixtures only for:**
   - Legacy format testing (migration tests)
   - Specific edge cases that require exact formatting

3. **Add migration note to project-fixtures.el:**
   ```elisp
   ;;; DEPRECATION NOTICE
   ;; Prefer builders.el functions over these static fixtures.
   ;; These remain only for migration tests requiring exact legacy format.
   ```

### Estimated Impact
- **Lines reduced:** ~50-100 (mostly in test files using fixtures)
- **Consistency:** High - single approach to test data

---

## Issue 5: Graph Test Local Helpers (Priority: MEDIUM)

### Problem

Graph test files define local setup functions that duplicate builder functionality:

```elisp
;; org-gtd-graph-remove-task-test.el
(defun org-gtd-graph-remove-test--create-project-with-diamond ...)  ; 40 lines
(defun org-gtd-graph-remove-test--create-project-with-chain ...)    ; 50 lines

;; org-gtd-graph-navigation-test.el
(defun create-test-project-structure ...)  ; Similar patterns
```

### Solution: Extend Core Builders

Add topology-aware builders to builders.el:

```elisp
(cl-defun make-chain-project (title &key tasks (start-status 'next))
  "Create a project with linear task chain A → B → C → ...
TASKS is a list of task descriptions.
Returns plist with :project-id and :task-ids."
  ...)

(cl-defun make-diamond-project (title &key top-tasks middle-task bottom-tasks)
  "Create a project with diamond topology.
TOP-TASKS flow into MIDDLE-TASK which flows into BOTTOM-TASKS.
Returns plist with task IDs by role."
  ...)

(cl-defun make-parallel-project (title &key tasks)
  "Create a project with all tasks independent (parallel execution).
Returns plist with :project-id and :task-ids."
  ...)
```

### Estimated Impact
- **Lines reduced:** ~300 lines across graph tests
- **Reusability:** These topologies are common in testing

---

## Issue 6: Test Header Boilerplate (Priority: LOW)

### Problem

Every test file repeats:

```elisp
(require 'compat)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
```

### Solution: Test Prelude

Create `test/helpers/prelude.el` that loads common requirements:

```elisp
;;; prelude.el --- Standard test requirements -*- lexical-binding: t; -*-

(require 'compat)
(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd-test-helper-processing (file-name-concat default-directory "test/helpers/processing.el"))

(provide 'org-gtd-test-prelude)
```

Then tests become:

```elisp
(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe "Feature"
  (before-each (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))
  ...)
```

### Estimated Impact
- **Lines per file:** Reduced by 4-6
- **Total lines:** ~350 across all test files
- **Consistency:** All tests use same setup

---

## Implementation Phases

### Phase 1: WIP Buffer Helper (Estimated: 2-3 hours)

1. Create `test/helpers/wip.el` with `with-wip-buffer` macro
2. Update `test/helpers/setup.el` to require it
3. Search-replace pattern across all test files
4. Run full test suite to verify

**Files to modify:** ~15 test files
**Risk:** Low - mechanical replacement

### Phase 2: Autoload Test Consolidation (Estimated: 1 hour)

1. Create new parametric test file
2. Verify all commands are tested
3. Delete individual autoload test files
4. Update eldev test configuration if needed

**Files to delete:** 10
**Risk:** Low - straightforward consolidation

### Phase 3: Graph Builders (Estimated: 3-4 hours)

1. Add topology builders to builders.el:
   - `make-chain-project`
   - `make-diamond-project`
   - `make-parallel-project`
2. Migrate graph test local functions
3. Update graph tests to use new builders
4. Verify all graph tests pass

**Files to modify:** 7 graph test files
**Risk:** Medium - need to ensure identical structures

### Phase 4: Selective Inline Insert Migration (Estimated: 4-6 hours)

1. Audit each test file with inline inserts
2. Categorize as:
   - Migration/legacy tests (keep inline)
   - Normal tests (migrate to builder)
3. Create any missing builder variants
4. Migrate one file at a time, running tests after each

**Files to modify:** ~20 test files
**Risk:** Medium - some subtle differences possible

### Phase 5: Test Prelude (Estimated: 1 hour)

1. Create `test/helpers/prelude.el`
2. Update all test files to use it
3. Verify test isolation still works

**Files to modify:** All test files
**Risk:** Low - only changes require statements

---

## Decision Points for Discussion

1. **WIP buffer helper naming:** `with-wip-buffer` or `ogt-with-wip-buffer`?

2. **Autoload test strategy:** Keep as separate file or integrate into existing test suite?

3. **Builder vs fixture selection criteria:** When should static fixtures be preferred?

4. **Graph builder complexity:** Should topology builders support arbitrary DAGs or just common patterns?

5. **Prelude adoption:** All-or-nothing or gradual migration?

---

## Success Metrics

| Metric | Current | Target | Notes |
|--------|---------|--------|-------|
| Total test lines | 20,592 | ~18,000 | ~12% reduction |
| WIP pattern occurrences | 71+ | 0 | All via helper |
| Autoload test files | 10 | 1 | Parametric |
| Static fixture uses | ~10 | ~3 | Legacy only |
| Average test setup lines | 6 | 2 | Via prelude |

---

## Non-Goals

These are explicitly NOT part of this effort:

1. **Reducing test coverage** - All existing tests remain
2. **Changing test behavior** - Only structure, not logic
3. **Rewriting large test files** - Focus on patterns, not rewrites
4. **Changing helper public APIs** - Only adding new helpers
