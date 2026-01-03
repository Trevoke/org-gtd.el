# DSL org-ql Removal Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan.

**Goal:** Remove all org-ql translation code from the view DSL, keeping only the native agenda implementation. Add missing native test coverage and error handling.

**Architecture:** The view DSL currently has two code paths - org-ql translation (used only in tests) and native agenda blocks (used at runtime). We're deleting the org-ql path entirely and ensuring the native path has equivalent test coverage and error handling.

**Bead:** TBD (create when starting implementation)

---

## Phase 1: Add Native Test Coverage

Before deleting org-ql tests, ensure equivalent native coverage exists.

### Task 1.1: Done Filter Native Tests

**Files:**
- Create: `test/unit/done-filter-test.el`

**Tests to add:**
```elisp
;; Test org-gtd-view-lang--build-skip-function-for-done-filter
(deftest done-filter/t-includes-any-done-item ())
(deftest done-filter/t-skips-not-done-item ())
(deftest done-filter/recent-includes-within-7-days ())
(deftest done-filter/recent-skips-older-than-7-days ())
(deftest done-filter/today-includes-closed-today ())
(deftest done-filter/today-skips-closed-yesterday ())
(deftest done-filter/past-week-includes-within-range ())
(deftest done-filter/numeric-uses-custom-days ())
```

**Commit:** `test: add native tests for done filter skip function`

---

### Task 1.2: Area-of-Focus Native Test

**Files:**
- Modify: `test/unit/gtd-view-language-test.el` (or create dedicated file)

**Test to add:**
```elisp
(deftest view-lang/skip-function-filters-by-area-of-focus ()
  "Skip function includes items matching area-of-focus filter."
  (with-temp-buffer
    (org-mode)
    (insert "* Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:CATEGORY: Work\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((spec '((type . next-action) (area-of-focus . "Work")))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      (assert-nil (funcall skip-fn)))))  ; nil = include

(deftest view-lang/skip-function-excludes-wrong-area-of-focus ()
  "Skip function excludes items not matching area-of-focus filter."
  ...)
```

**Commit:** `test: add native test for area-of-focus skip function`

---

### Task 1.3: When Filter Tests and Error Handling

**Files:**
- Modify: `org-gtd-view-language.el` (add error handling)
- Modify: `test/unit/gtd-view-language-test.el` (add tests)

**Implementation - add to `org-gtd-view-lang--build-skip-function`:**
```elisp
;; At start of function, validate when filter
(when when-filter
  (unless type-filter
    (user-error "The 'when' filter requires a 'type' filter"))
  (unless (org-gtd-type-property type-filter :when)
    (user-error "Type %s does not support the 'when' filter" type-filter)))
```

**Tests to add:**
```elisp
(deftest view-lang/skip-function-when-today-includes-match ())
(deftest view-lang/skip-function-when-today-excludes-past ())
(deftest view-lang/skip-function-when-future-includes-match ())
(deftest view-lang/skip-function-when-without-type-errors ()
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function '((when . past)))))
(deftest view-lang/skip-function-when-unsupported-type-errors ()
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function '((type . next-action) (when . past)))))
```

**Commit:** `feat: add error handling for when filter in native skip function`

---

### Task 1.4: Unknown Filter Error Handling

**Files:**
- Modify: `org-gtd-view-language.el`
- Modify: `test/unit/gtd-view-language-test.el`

**Implementation - add validation to `org-gtd-view-lang--build-skip-function`:**
```elisp
(defconst org-gtd-view-lang--known-filter-keys
  '(name type when deadline scheduled todo done not-done
    area-of-focus who tags priority effort clocked last-clocked-out
    blocks prefix prefix-format view-type)
  "Known filter keys in view specs.")

;; At start of build-skip-function:
(let ((unknown-keys (cl-set-difference
                     (mapcar #'car gtd-view-spec)
                     org-gtd-view-lang--known-filter-keys)))
  (when unknown-keys
    (user-error "Unknown filter keys: %s" unknown-keys)))
```

**Tests:**
```elisp
(deftest view-lang/skip-function-errors-on-unknown-filter ()
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (invalid-filter . "bad")))))
```

**Commit:** `feat: add error handling for unknown filter keys`

---

## Phase 2: Delete Dead Code

### Task 2.1: Delete previous-type Filter

**Files:**
- Modify: `org-gtd-view-language.el`

**Delete:**
- `org-gtd-view-lang--translate-previous-type-filter` function
- Case for `previous-type` in `org-gtd-view-lang--translate-filter`

**Commit:** `refactor: remove unused previous-type filter`

---

### Task 2.2: Delete org-ql Translation Layer

**Files:**
- Modify: `org-gtd-view-language.el`

**Delete these functions:**
- `org-gtd-view-lang--translate-to-org-ql`
- `org-gtd-view-lang--translate-filter`
- `org-gtd-view-lang--translate-when-filter`
- `org-gtd-view-lang--translate-priority-filter`
- `org-gtd-view-lang--translate-effort-filter`
- `org-gtd-view-lang--translate-clocked-filter`
- `org-gtd-view-lang--translate-last-clocked-out-filter`
- `org-gtd-view-lang--translate-deadline-filter`
- `org-gtd-view-lang--translate-scheduled-filter`
- `org-gtd-view-lang--translate-done-filter`
- `org-gtd-view-lang--translate-not-done-filter`
- `org-gtd-view-lang--translate-who-filter`
- `org-gtd-view-lang--translate-not-habit-filter`
- `org-gtd-view-lang--translate-area-of-focus-filter`
- `org-gtd-view-lang--translate-todo-filter`
- `org-gtd-view-lang--translate-tags-filter`
- `org-gtd-view-lang--translate-property-filter`
- `org-gtd-view-lang--translate-type-filter`
- `org-gtd-view-lang--generate-stuck-query`
- `org-gtd-view-lang--translate-semantic-property-filter`

**Also update:**
- Commentary at top of file (remove "translate to performant org-ql queries")

**Commit:** `refactor: remove org-ql translation layer`

---

### Task 2.3: Delete org-ql Tests

**Files to modify:**

1. `test/unit/gtd-view-language-test.el` - Delete ~78 tests calling `translate-to-org-ql`
2. `test/unit/view-language-last-clocked-out-test.el` - Delete 4 org-ql tests
3. `test/unit/reflect-test.el` - Delete 6 org-ql tests
4. `test/unit/property-based-system-test.el` - Delete 1 org-ql test

**Tests to delete (pattern):** Any test that calls `org-gtd-view-lang--translate-to-org-ql`

**Commit:** `test: remove org-ql translation tests`

---

## Phase 3: Cleanup

### Task 3.1: Update Documentation

**Files:**
- Modify: `org-gtd-view-language.el` commentary

**Changes:**
- Remove any mention of org-ql
- Update description to say "native org-agenda blocks"

**Commit:** `docs: update view language commentary to reflect native-only implementation`

---

### Task 3.2: Run Full Test Suite

```bash
~/bin/eldev -p -dtT etest
```

Expected: All tests pass, count should be lower (removed ~89 org-ql tests, added ~15 native tests).

---

## Summary

| Metric | Before | After |
|--------|--------|-------|
| org-ql translation functions | ~20 | 0 |
| org-ql tests | ~89 | 0 |
| Native tests | ~73 | ~88 |
| Error handling coverage | Partial | Complete |

**Key improvements:**
1. No false implication that org-ql is used
2. Error on invalid filter specs (catch mistakes early)
3. All DSL behavior tested via native implementation
4. Smaller, cleaner codebase
