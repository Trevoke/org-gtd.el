# Property Filter Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `property` filter for the View DSL, enabling users to filter agenda items by arbitrary org properties with exact string matching. This implements the syntax already documented in `doc/org-gtd.org`.

**Architecture:** The `property` filter extracts a list of `(PROP . VALUE)` cons cells from the view spec and pushes one `org-gtd-pred--property-equals` predicate per pair. All pairs use AND logic (all must match). The existing predicate in `org-gtd-skip.el` handles the actual property comparison — no new predicates are needed.

**Tech Stack:** Emacs Lisp, existing `org-gtd-pred--property-equals` predicate, org-gtd view DSL predicate composition

---

### Task 1: Unit Tests for Property Filter

**Files:**
- Modify: `test/unit/gtd-view-language-test.el`

**Step 1: Add property filter tests**

Insert the following test section after the "Clocked Skip Predicate Tests" block (after line 1068, before the "Unknown Filter Key Error Handling Tests" section at line 1070):

```elisp
;;; Property Filter Skip Predicate Tests

(deftest view-lang/skip-property-includes-match ()
  "Skip function includes items with matching property value."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ENERGY: high\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (property . (("ENERGY" . "high")))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-property-skips-non-match ()
  "Skip function skips items with non-matching property value."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ENERGY: low\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (property . (("ENERGY" . "high")))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-property-skips-missing ()
  "Skip function skips items where the property is not set."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (property . (("ENERGY" . "high")))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-property-multiple-all-match ()
  "Skip function includes items where all property pairs match."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ENERGY: high\n:CONTEXT: office\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (property . (("ENERGY" . "high")
                                     ("CONTEXT" . "office")))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-property-multiple-partial-skips ()
  "Skip function skips items where only some property pairs match."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ENERGY: high\n:CONTEXT: home\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (property . (("ENERGY" . "high")
                                     ("CONTEXT" . "office")))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-property-invalid-non-list-errors ()
  "Property filter with non-list value signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . "bad")))))

(deftest view-lang/skip-property-invalid-entry-errors ()
  "Property filter with non-cons entry signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . ("not-a-pair"))))))

(deftest view-lang/skip-property-invalid-name-errors ()
  "Property filter with non-string property name signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . ((123 . "val")))))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: The 5 behavior tests fail (property filter not yet handled), and the 3 validation tests also fail (no validation yet).

**Step 3: Commit**

```bash
git add test/unit/gtd-view-language-test.el
git commit -m "test: add failing tests for property filter in view DSL"
```

---

### Task 2: Add Property Filter Validation

**Files:**
- Modify: `org-gtd-view-language.el`

**Step 1: Add input validation for property filter**

In `org-gtd-view-lang--build-skip-function` (`org-gtd-view-language.el`), after the unknown-keys check block (line 741) and before the `let*` binding of `type-filter` (line 742), add the property filter validation:

```elisp
  ;; Validate property filter shape
  (when-let ((property-filter (alist-get 'property gtd-view-spec)))
    (unless (listp property-filter)
      (user-error "Property filter must be a list of (PROP . VALUE) pairs"))
    (dolist (pair property-filter)
      (unless (consp pair)
        (user-error "Each property filter entry must be a cons cell (PROP . VALUE), got: %s" pair))
      (unless (stringp (car pair))
        (user-error "Property name must be a string, got: %s" (car pair)))
      (unless (stringp (cdr pair))
        (user-error "Property value must be a string, got: %s" (cdr pair)))))
```

The insertion point is between line 741 (end of `(user-error "Unknown filter key(s): %s" unknown-keys)))`) and line 742 (start of `(let* ((type-filter ...)`).

**Step 2: Run validation tests**

Run: `~/bin/eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: The 3 validation tests pass (`skip-property-invalid-non-list-errors`, `skip-property-invalid-entry-errors`, `skip-property-invalid-name-errors`). The 5 behavior tests still fail.

**Step 3: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "feat: add property filter validation in skip function builder"
```

---

### Task 3: Implement Property Filter in Skip Function Builder

**Files:**
- Modify: `org-gtd-view-language.el`

**Step 1: Add property predicate block**

In `org-gtd-view-lang--build-skip-function`, in the `t` branch (simple types), after the who predicate block (line 845) and before the deadline predicate block (line 846), add:

```elisp
        ;; Add property predicates (arbitrary property matching)
        (when-let ((property-filter (alist-get 'property gtd-view-spec)))
          (dolist (prop-pair property-filter)
            (push (org-gtd-pred--property-equals (car prop-pair) (cdr prop-pair))
                  predicates)))
```

The insertion point is between line 845 (end of `(push (org-gtd-pred--property-equals who-prop who-filter) predicates))))))`) and line 846 (start of `        ;; Add deadline predicate`).

**Step 2: Run tests**

Run: `~/bin/eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: All 8 property filter tests PASS.

**Step 3: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS.

**Step 4: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "feat: add property filter to skip function builder"
```

---

### Task 4: Update Commentary Documentation

**Files:**
- Modify: `org-gtd-view-language.el`

**Step 1: Add property filter to Commentary block**

In the `;;; Commentary:` section, after the "Metadata Filters" block (after line 110 — `;;   (who . nil)                - Missing delegation recipient`) and before the "Clock Time Filters" block (line 112 — `;; Clock Time Filters:`), add:

```elisp
;;
;; Property Filters:
;;   (property . (("PROP" . "VALUE")))          - Single property match
;;   (property . (("P1" . "V1") ("P2" . "V2"))) - Multiple (AND logic)
```

**Step 2: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "docs: add property filter to view DSL commentary"
```

---

### Task 5: Update Info Manual Documentation

**Files:**
- Modify: `doc/org-gtd.org`

**Step 1: Update property filter section**

Replace the property filter documentation section (lines 7733-7748) with an expanded version that documents multi-property AND logic and provides better examples.

Replace the current content:
```org
*** Property Filters

Filter by arbitrary org properties:

**** ~(property . (("PROP" . "VALUE")))~

*Matches*: Items with specified property value

*Use case*: Filter by custom properties

*Example*:
#+begin_src emacs-lisp
'((name . "High Priority Actions")
  (property . (("PRIORITY" . "A")))
  (type . next-action))
#+end_src
```

With:
```org
*** Property Filters

Filter by arbitrary org properties:

**** ~(property . (("PROP" . "VALUE") ...))~

*Matches*: Items where all specified properties have the given values

*Use case*: Filter by custom properties you've added via organize hooks

*Single property*:
#+begin_src emacs-lisp
'((name . "High Energy Actions")
  (type . next-action)
  (property . (("ENERGY" . "high"))))
#+end_src

*Multiple properties (AND logic)*:
#+begin_src emacs-lisp
'((name . "Office Quick Wins")
  (type . next-action)
  (property . (("CONTEXT" . "office")
               ("DIFFICULTY" . "easy"))))
#+end_src
```

**Step 2: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: expand property filter manual with multi-property examples"
```

---

### Task 6: Final Verification

**Step 1: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS.

**Step 2: Compile with warnings**

Run: `~/bin/eldev clean && ~/bin/eldev compile`
Expected: Clean compilation, no warnings.

---

## Summary

After completing all tasks:

1. **Validation**: Property filter input shape validated (list of string cons cells)
2. **Skip function integration**: Property filter handled via existing `org-gtd-pred--property-equals`
3. **Unit tests**: 8 tests (5 behavior + 3 validation)
4. **Commentary docs**: Property filter section added to Commentary block
5. **Manual docs**: Expanded property filter section with multi-property examples

| File | Change |
|------|--------|
| `org-gtd-view-language.el` | Add property predicate block in skip function builder (~5 lines) |
| `org-gtd-view-language.el` | Add property filter validation (~8 lines) |
| `org-gtd-view-language.el` | Add Commentary documentation (~3 lines) |
| `doc/org-gtd.org` | Update property filter section with multi-property docs and examples |
| `test/unit/gtd-view-language-test.el` | Add 8 tests (5 behavior + 3 validation) |
| `org-gtd-skip.el` | No changes — existing predicate is sufficient |

**Usage example:**
```elisp
;; High-energy next actions
(org-gtd-view-show
 '((name . "High Energy Actions")
   (type . next-action)
   (property . (("ENERGY" . "high")))))

;; Multiple property constraints
(org-gtd-view-show
 '((name . "Office Quick Wins")
   (type . next-action)
   (property . (("CONTEXT" . "office")
                ("DIFFICULTY" . "easy")))))
```
