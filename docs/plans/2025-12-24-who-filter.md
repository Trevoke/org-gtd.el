# Who Filter Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `who` filter for native agenda skip functions so users can filter delegated items by recipient.

**Architecture:** The `who` filter requires a `type` filter to be present because `:who` is a semantic property that maps to different org properties per type (e.g., "DELEGATED_TO" for delegated items). Use existing `org-gtd-pred--property-equals` predicate with dynamic property lookup via `org-gtd-type-property`.

**Tech Stack:** Emacs Lisp, org-gtd-types semantic property system, org-gtd predicate composition

**Bead:** orggtd-5pa

---

### Task 1: Unit Tests for Who Filter

**Files:**
- Create: `test/unit/who-filter-test.el`

**Step 1: Create the test file**

```elisp
;;; who-filter-test.el --- Tests for who filter -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Skip Function with Who Filter

(deftest who-filter/matches-delegated-to-person ()
  "Skip function keeps items delegated to specified person."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Alice
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . "Alice")))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should NOT skip (nil means include)
      (assert-nil (funcall skip-fn)))))

(deftest who-filter/skips-delegated-to-other-person ()
  "Skip function excludes items delegated to different person."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . "Alice")))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should skip (returns end position)
      (assert-true (funcall skip-fn)))))

(deftest who-filter/nil-matches-missing-delegated-to ()
  "Who filter with nil matches items without DELEGATED_TO."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should NOT skip - item is missing who
      (assert-nil (funcall skip-fn)))))

(provide 'who-filter-test)
;;; who-filter-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev -p -dtT etest test/unit/who-filter-test.el`
Expected: Tests fail because who filter not yet handled in skip function

**Step 3: Commit**

```bash
git add test/unit/who-filter-test.el
git commit -m "test: add failing tests for who filter"
```

---

### Task 2: Implement Who Filter in Skip Function Builder

**Files:**
- Modify: `org-gtd-view-language.el` (in `org-gtd-view-lang--build-skip-function`)

**Step 1: Add who filter handling**

In `org-gtd-view-lang--build-skip-function`, after the tags predicate handling, add:

```elisp
        ;; Add who predicate (requires type for property lookup)
        (when (assq 'who gtd-view-spec)
          (let ((who-filter (alist-get 'who gtd-view-spec)))
            (when type-filter
              (when-let ((who-prop (org-gtd-type-property type-filter :who)))
                (if (or (null who-filter) (and (stringp who-filter) (string-empty-p who-filter)))
                    ;; nil or "" means find items with missing/empty :who
                    (push (org-gtd-pred--property-empty-or-missing who-prop) predicates)
                  ;; Otherwise filter by specific value
                  (push (org-gtd-pred--property-equals who-prop who-filter) predicates))))))
```

**Step 2: Run tests**

Run: `~/bin/eldev -p -dtT etest test/unit/who-filter-test.el`
Expected: All 3 tests PASS

**Step 3: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 4: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "feat: add who filter to skip function builder"
```

---

### Task 3: End-to-End Integration Test

**Files:**
- Modify: `test/integration/gtd-view-language-test.el`

**Step 1: Add integration tests**

Add before the `(provide ...)` line:

```elisp
;;; Who Filter Integration Tests

(deftest view-lang-int/who-filter-shows-delegated-to-person ()
  "View with who filter shows items delegated to specified person."
  (with-current-buffer (org-gtd--default-file)
    (insert "* WAIT Task for Alice
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Alice
:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>
:END:
* WAIT Task for Bob
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Alice's Tasks")
     (type . delegated)
     (who . "Alice")))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include Alice's task
        (assert-match "Task for Alice" content)
        ;; Should NOT include Bob's task
        (assert-no-match "Task for Bob" content)))))
```

**Step 2: Run integration tests**

Run: `~/bin/eldev -p -dtT etest test/integration/gtd-view-language-test.el`
Expected: All tests PASS

**Step 3: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 4: Commit**

```bash
git add test/integration/gtd-view-language-test.el
git commit -m "test: add end-to-end test for who filter"
```

---

### Task 4: Close the Bead

**Step 1: Verify all tests pass**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 2: Close the bead**

```bash
bd close orggtd-5pa --reason="Implemented who filter in skip function builder using semantic property lookup. Includes unit and integration tests."
bd sync
```

---

## Summary

After completing all tasks:

1. **Skip function integration**: Who filter handled via `org-gtd-type-property` lookup
2. **Unit tests**: 3 tests covering match, no-match, and nil cases
3. **Integration test**: 1 end-to-end test verifying delegated filtering

**Usage example:**
```elisp
(org-gtd-view-show
 '((name . "Alice's Delegated Tasks")
   (type . delegated)
   (who . "Alice")))
```

This shows only delegated items where DELEGATED_TO = "Alice".
