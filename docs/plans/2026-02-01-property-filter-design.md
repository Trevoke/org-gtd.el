# Property Filter in View DSL — Design

## Summary

Add the `property` filter to the View DSL, enabling users to filter
agenda items by arbitrary org properties with exact string matching.
This implements the syntax already documented in `doc/org-gtd.org`.

## Syntax

```elisp
;; Single property match
(property . (("ENERGY" . "high")))

;; Multiple property match (AND logic)
(property . (("ENERGY" . "high")
             ("CONTEXT" . "office")))
```

The value is a list of cons cells `(PROPERTY-NAME . EXPECTED-VALUE)`.
All pairs must match (AND logic), consistent with how all DSL filters
compose.

This matches the syntax already documented at `doc/org-gtd.org:7661`.

## Example Usage

```elisp
;; High-energy next actions
'((name . "High Energy Actions")
  (type . next-action)
  (property . (("ENERGY" . "high"))))

;; Work tasks assigned to a specific client
'((name . "Acme Tasks")
  (type . next-action)
  (area-of-focus . "Work")
  (property . (("CLIENT" . "Acme"))))

;; Multiple property constraints
'((name . "Office Quick Wins")
  (type . next-action)
  (property . (("CONTEXT" . "office")
               ("DIFFICULTY" . "easy"))))
```

## Implementation

### 1. Skip function builder (`org-gtd-view-language.el`)

**Where**: Inside `org-gtd-view-lang--build-skip-function`, in the `t`
branch (simple types), after the existing `who` predicate block (around
line 845) and before the `deadline` predicate block.

**What**: Extract the `property` filter from the spec. For each cons
cell in the list, push a `org-gtd-pred--property-equals` predicate.

```elisp
;; Add property predicates
(when-let ((property-filter (alist-get 'property gtd-view-spec)))
  (dolist (prop-pair property-filter)
    (push (org-gtd-pred--property-equals (car prop-pair) (cdr prop-pair))
          predicates)))
```

**No new predicates needed** — `org-gtd-pred--property-equals` in
`org-gtd-skip.el` already does exact string matching on any named
property.

### 2. Validation (`org-gtd-view-language.el`)

The `property` key is already in `org-gtd-view-lang--known-filter-keys`,
so the unknown-key validation passes. No changes needed there.

**Add input validation** for the property filter value. After the
unknown-keys check (around line 741), validate the shape:

```elisp
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

### 3. Documentation — Commentary section (`org-gtd-view-language.el`)

**Where**: In the `;;; Commentary:` block, after the "Metadata Filters"
section (around line 110), add:

```elisp
;; Property Filters:
;;   (property . (("PROP" . "VALUE")))          - Single property match
;;   (property . (("P1" . "V1") ("P2" . "V2"))) - Multiple (AND logic)
```

### 4. Documentation — Info manual (`doc/org-gtd.org`)

The property filter section already exists at line 7657-7672. Update it
to document multi-property support and improve the example:

**Current** (line 7661):
```
**** ~(property . (("PROP" . "VALUE")))~
```

**Updated**:
```org
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

### 5. Tests (`test/unit/gtd-view-language-test.el`)

Add to the unit test file, following the existing test patterns
(with-temp-buffer, org-mode, insert heading with properties, build skip
function, funcall, assert):

**Test 1: Single property match includes matching item**
```elisp
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
```

**Test 2: Single property match skips non-matching item**
```elisp
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
```

**Test 3: Single property match skips item with missing property**
```elisp
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
```

**Test 4: Multiple properties — all match**
```elisp
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
```

**Test 5: Multiple properties — partial match skips**
```elisp
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
```

**Test 6: Validation — non-list value errors**
```elisp
(deftest view-lang/skip-property-invalid-non-list-errors ()
  "Property filter with non-list value signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . "bad")))))
```

**Test 7: Validation — non-cons entry errors**
```elisp
(deftest view-lang/skip-property-invalid-entry-errors ()
  "Property filter with non-cons entry signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . ("not-a-pair"))))))
```

**Test 8: Validation — non-string property name errors**
```elisp
(deftest view-lang/skip-property-invalid-name-errors ()
  "Property filter with non-string property name signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (property . ((123 . "val")))))))
```

## Changes Summary

| File | Change |
|------|--------|
| `org-gtd-view-language.el` | Add property predicate block in skip function builder (~5 lines) |
| `org-gtd-view-language.el` | Add property filter validation (~8 lines) |
| `org-gtd-view-language.el` | Add Commentary documentation (~3 lines) |
| `doc/org-gtd.org` | Update property filter section with multi-property docs and examples |
| `test/unit/gtd-view-language-test.el` | Add 8 tests (5 behavior + 3 validation) |
| `org-gtd-skip.el` | No changes — existing predicate is sufficient |

## What This Design Does NOT Include

- No negation (`!=`) operator
- No regex/substring matching
- No numeric comparison
- No timestamp comparison on arbitrary properties
- No `nil` check for missing properties (could be added later)

These are intentionally deferred per the brainstorming session.
