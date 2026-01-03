# Consolidate GTD Type System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Consolidate `org-gtd-items` and `org-gtd-types` into a single type system where `org-gtd-types` is the single source of truth.

**Architecture:** Replace all `org-gtd-configure-item` calls with `org-gtd-configure-as-type`, update `org-gtd-configure-as-type` to support programmatic values, then delete the old system. Project tasks use `'next-action` type plus separate structural setup (TRIGGER, DAG properties).

**Tech Stack:** Emacs Lisp, Buttercup testing framework, eldev build tool

---

## Task 1: Add `values` parameter to `org-gtd-configure-as-type`

**Files:**
- Modify: `org-gtd-configure.el:145-177`
- Test: `test/configure-test.el`

**Step 1: Write failing test for programmatic values**

Add to `test/configure-test.el`:

```elisp
(describe "org-gtd-configure-as-type with values parameter"

  (it "uses provided values instead of prompting for delegated type"
    (ogt--with-temp-org-buffer
     "* Test task"
     (org-gtd-configure-as-type 'delegated
                                '((:who . "John Doe")
                                  (:when . "<2025-12-01>")))
     (expect (org-entry-get nil "ORG_GTD") :to-equal "Delegated")
     (expect (org-entry-get nil "DELEGATED_TO") :to-equal "John Doe")
     (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>")))

  (it "uses provided values for calendar type"
    (ogt--with-temp-org-buffer
     "* Test task"
     (org-gtd-configure-as-type 'calendar
                                '((:when . "<2025-06-15>")))
     (expect (org-entry-get nil "ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-06-15>")))

  (it "uses provided values for incubated type"
    (ogt--with-temp-org-buffer
     "* Test task"
     (org-gtd-configure-as-type 'incubated
                                '((:when . "<2025-03-01>")))
     (expect (org-entry-get nil "ORG_GTD") :to-equal "Incubated")
     (expect (org-entry-get nil "ORG_GTD_TIMESTAMP") :to-equal "<2025-03-01>")))

  (it "uses provided values for habit type"
    (ogt--with-temp-org-buffer
     "* Test task"
     (org-gtd-configure-as-type 'habit
                                '((:when . "<2025-01-01 +1d>")))
     (expect (org-entry-get nil "ORG_GTD") :to-equal "Habit")
     ;; SCHEDULED is set via org-schedule, check scheduled time exists
     (expect (org-get-scheduled-time (point)) :not :to-be nil))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "org-gtd-configure-as-type with values parameter"`
Expected: FAIL - wrong number of arguments or properties not set

**Step 3: Update `org-gtd-configure-as-type` to accept values**

Replace the function in `org-gtd-configure.el`:

```elisp
(defun org-gtd-configure-as-type (type-name &optional values)
  "Configure item at point as TYPE-NAME using the GTD type system.

TYPE-NAME must be a symbol from `org-gtd-types' (e.g., \\='next-action,
\\='delegated, \\='calendar, \\='project).  This function:

1. Sets the ORG_GTD property to the type's defined value
2. Sets the TODO state based on the type's :state semantic
3. Sets all required semantic properties (prompted or from VALUES)

VALUES is an optional alist mapping semantic property keywords to values,
for non-interactive use. Example: \\='((:who . \"John\") (:when . \"<2025-01-15>\"))

When VALUES is provided, properties are set directly without prompting.
When VALUES is nil, required properties are prompted interactively."
  (let ((type-def (org-gtd-type-get type-name)))
    (unless type-def
      (user-error "Unknown GTD type: %s" type-name))
    (let ((org-gtd-val (plist-get (cdr type-def) :org-gtd))
          (state (plist-get (cdr type-def) :state))
          (props (plist-get (cdr type-def) :properties)))
      ;; Set ORG_GTD property
      (org-entry-put nil "ORG_GTD" org-gtd-val)
      ;; Set TODO state if defined
      (when state
        (org-todo (org-gtd--state-to-keyword state)))
      ;; Set each required property
      (dolist (prop props)
        (when (plist-get (cdr prop) :required)
          (let* ((semantic-name (car prop))
                 (prompt (plist-get (cdr prop) :prompt))
                 (prop-type (plist-get (cdr prop) :type))
                 (org-prop (plist-get (cdr prop) :org-property))
                 ;; Look up value in VALUES alist, or prompt
                 (value (or (alist-get semantic-name values)
                            (org-gtd--prompt-for-property-type prop-type prompt))))
            (if (string-equal org-prop "SCHEDULED")
                (org-schedule nil value)
              (org-entry-put nil org-prop value)))))
      ;; Ensure ID exists
      (org-gtd-id-get-create))))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "org-gtd-configure-as-type with values parameter"`
Expected: PASS

**Step 5: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 6: Commit**

```bash
git add org-gtd-configure.el test/configure-test.el
git commit -m "feat: add values parameter to org-gtd-configure-as-type"
```

---

## Task 2: Update `org-gtd-single-action.el`

**Files:**
- Modify: `org-gtd-single-action.el:78`

**Step 1: Run existing tests to establish baseline**

Run: `eldev test -B "single action"`
Expected: PASS

**Step 2: Replace `org-gtd-configure-item` with `org-gtd-configure-as-type`**

In `org-gtd-single-action.el`, change line 78 from:
```elisp
  (org-gtd-configure-item (point) :next))
```
to:
```elisp
  (org-gtd-configure-as-type 'next-action))
```

**Step 3: Add require for org-gtd-configure if not present**

Ensure `(require 'org-gtd-configure)` is in the Requirements section.

**Step 4: Run tests to verify nothing broke**

Run: `eldev test -B "single action"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-single-action.el
git commit -m "refactor: use org-gtd-configure-as-type in single-action"
```

---

## Task 3: Update `org-gtd-calendar.el`

**Files:**
- Modify: `org-gtd-calendar.el:87`

**Step 1: Run existing tests**

Run: `eldev test -B "calendar"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-calendar.el`, find `org-gtd-calendar--configure` and change:
```elisp
  (org-gtd-configure-item (point) :calendar nil config-override))
```
to:
```elisp
  (org-gtd-configure-as-type 'calendar
                             (when config-override
                               `((:when . ,(funcall (alist-get ''active-timestamp config-override) nil))))))
```

**Step 3: Run tests**

Run: `eldev test -B "calendar"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-calendar.el
git commit -m "refactor: use org-gtd-configure-as-type in calendar"
```

---

## Task 4: Update `org-gtd-delegate.el`

**Files:**
- Modify: `org-gtd-delegate.el:114`

**Step 1: Run existing tests**

Run: `eldev test -B "delegat"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-delegate.el`, find `org-gtd-delegate--configure` and change:
```elisp
  (org-gtd-configure-item (point) :delegated nil config-override))
```
to:
```elisp
  (org-gtd-configure-as-type 'delegated
                             (when config-override
                               (let ((who-fn (alist-get ''text config-override))
                                     (when-fn (alist-get ''active-timestamp config-override)))
                                 `(,@(when who-fn `((:who . ,(funcall who-fn nil))))
                                   ,@(when when-fn `((:when . ,(funcall when-fn nil)))))))))
```

**Step 3: Run tests**

Run: `eldev test -B "delegat"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-delegate.el
git commit -m "refactor: use org-gtd-configure-as-type in delegate"
```

---

## Task 5: Update `org-gtd-incubate.el`

**Files:**
- Modify: `org-gtd-incubate.el:156`

**Step 1: Run existing tests**

Run: `eldev test -B "incubat"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-incubate.el`, find `org-gtd-incubate--configure` and change:
```elisp
  (org-gtd-configure-item (point) :incubate nil config-override))
```
to:
```elisp
  (org-gtd-configure-as-type 'incubated
                             (when config-override
                               `((:when . ,(funcall (alist-get ''active-timestamp config-override) nil))))))
```

**Step 3: Run tests**

Run: `eldev test -B "incubat"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-incubate.el
git commit -m "refactor: use org-gtd-configure-as-type in incubate"
```

---

## Task 6: Update `org-gtd-habit.el`

**Files:**
- Modify: `org-gtd-habit.el:91`

**Step 1: Run existing tests**

Run: `eldev test -B "habit"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-habit.el`, find `org-gtd-habit--configure` and change:
```elisp
  (org-gtd-configure-item (point) :habit nil config-override))
```
to:
```elisp
  (org-gtd-configure-as-type 'habit
                             (when config-override
                               `((:when . ,(funcall (alist-get ''active-timestamp-with-repeater config-override) nil))))))
```

**Step 3: Run tests**

Run: `eldev test -B "habit"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-habit.el
git commit -m "refactor: use org-gtd-configure-as-type in habit"
```

---

## Task 7: Update `org-gtd-knowledge.el`

**Files:**
- Modify: `org-gtd-knowledge.el:60`

**Step 1: Run existing tests**

Run: `eldev test -B "knowledge\|reference"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-knowledge.el`, change:
```elisp
  (org-gtd-configure-item (point) :knowledge))
```
to:
```elisp
  (org-gtd-configure-as-type 'reference))
```

**Step 3: Run tests**

Run: `eldev test -B "knowledge\|reference"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-knowledge.el
git commit -m "refactor: use org-gtd-configure-as-type in knowledge"
```

---

## Task 8: Update `org-gtd-trash.el`

**Files:**
- Modify: `org-gtd-trash.el:54`

**Step 1: Run existing tests**

Run: `eldev test -B "trash"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-trash.el`, change:
```elisp
  (org-gtd-configure-item (point) :trash)
```
to:
```elisp
  (org-gtd-configure-as-type 'trash)
```

**Step 3: Run tests**

Run: `eldev test -B "trash"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-trash.el
git commit -m "refactor: use org-gtd-configure-as-type in trash"
```

---

## Task 9: Update `org-gtd-quick-action.el`

**Files:**
- Modify: `org-gtd-quick-action.el:54`

**Step 1: Run existing tests**

Run: `eldev test -B "quick"`
Expected: PASS

**Step 2: Replace the call**

In `org-gtd-quick-action.el`, change:
```elisp
  (org-gtd-configure-item (point) :quick-action))
```
to:
```elisp
  (org-gtd-configure-as-type 'quick-action))
```

**Step 3: Run tests**

Run: `eldev test -B "quick"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-quick-action.el
git commit -m "refactor: use org-gtd-configure-as-type in quick-action"
```

---

## Task 10: Update `org-gtd-task-management.el`

**Files:**
- Modify: `org-gtd-task-management.el:865`

**Step 1: Run existing tests**

Run: `eldev test -B`
Expected: PASS (baseline)

**Step 2: Replace the call**

In `org-gtd-task-management.el`, around line 865, change:
```elisp
          (org-gtd-configure-item (point) :next)))
```
to:
```elisp
          (org-gtd-configure-as-type 'next-action)))
```

**Step 3: Run tests**

Run: `eldev test -B`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-task-management.el
git commit -m "refactor: use org-gtd-configure-as-type in task-management"
```

---

## Task 11: Update `org-gtd-projects.el` - project heading

**Files:**
- Modify: `org-gtd-projects.el:326`

**Step 1: Run existing tests**

Run: `eldev test -B "project"`
Expected: PASS

**Step 2: Replace project-heading configuration**

In `org-gtd-projects.el`, in `org-gtd-project--transform-heading`, change:
```elisp
  (org-gtd-configure-item (point) :project-heading)
```
to:
```elisp
  (org-gtd-configure-as-type 'project)
```

**Step 3: Run tests**

Run: `eldev test -B "project"`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd-projects.el
git commit -m "refactor: use org-gtd-configure-as-type for project heading"
```

---

## Task 12: Update `org-gtd-projects.el` - project tasks

**Files:**
- Modify: `org-gtd-projects.el:439` and `org-gtd-projects.el:953`

**Step 1: Understand current behavior**

The `:project-task` config sets:
- `ORG_GTD = "Actions"`
- `TODO` keyword
- `ID`
- `TRIGGER`

We need to split this: type config (`'next-action`) + TRIGGER setup.

**Step 2: Replace first project-task call (line 439)**

In `org-gtd-projects--configure-all-tasks`, change:
```elisp
         (org-gtd-configure-item (point) :project-task)
```
to:
```elisp
         (org-gtd-configure-as-type 'next-action)
         (org-entry-put (point) "TRIGGER" "org-gtd-update-project-after-task-done!")
```

**Step 3: Replace second project-task call (line 953)**

In `org-gtd-project--configure-single-task`, change:
```elisp
  (org-gtd-configure-item (point) :project-task)
```
to:
```elisp
  (org-gtd-configure-as-type 'next-action)
  (org-entry-put (point) "TRIGGER" "org-gtd-update-project-after-task-done!")
```

**Step 4: Run tests**

Run: `eldev test -B "project"`
Expected: PASS

**Step 5: Run full test suite**

Run: `eldev test -B`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-projects.el
git commit -m "refactor: use org-gtd-configure-as-type for project tasks"
```

---

## Task 13: Delete `org-gtd-items` and `org-gtd-user-item-config`

**Files:**
- Modify: `org-gtd.el:86-151`

**Step 1: Remove `org-gtd-user-item-config` defcustom**

Delete lines 86-92 in `org-gtd.el`:
```elisp
(defcustom org-gtd-user-item-config '()
  "Overrides for `org-gtd-items' with your usual config.
Note that if `org-gtd' finds an item type in here, it will use this
information instead of the built-in one."
  :group 'org-gtd
  :type 'alist
  :package-version '(org-gtd . "3.1"))
```

**Step 2: Remove `org-gtd-items` function**

Delete lines 94-150 (the entire `org-gtd-items` function).

**Step 3: Run tests**

Run: `eldev test -B`
Expected: PASS

**Step 4: Commit**

```bash
git add org-gtd.el
git commit -m "refactor: remove org-gtd-items and org-gtd-user-item-config"
```

---

## Task 14: Delete `org-gtd-configure-item` and helpers

**Files:**
- Modify: `org-gtd-configure.el`

**Step 1: Identify functions to delete**

Delete these functions from `org-gtd-configure.el`:
- `org-gtd-configure-item`
- `org-gtd--merge-inputs`
- `org-gtd--pcase-inputs`
- `org-gtd-default-input-config`
- `org-gtd--build-prompt-form`

Keep:
- `org-gtd-prompt-for-active-date`
- `org-gtd-prompt-for-active-date-with-repeater`
- `org-gtd-configure-as-type`
- `org-gtd--state-to-keyword`
- `org-gtd--prompt-for-property-type`

**Step 2: Delete the old functions**

Remove `org-gtd-configure-item` (lines 74-107).
Remove `org-gtd--merge-inputs` (lines 109-114).
Remove `org-gtd--pcase-inputs` (lines 116-118).
Remove `org-gtd-default-input-config` (lines 120-123).
Remove `org-gtd--build-prompt-form` (lines 125-141).

**Step 3: Remove forward declarations that are no longer needed**

Remove from forward declarations:
```elisp
(defvar org-gtd-user-item-config)
(defvar org-gtd-default-input-config)
(declare-function org-gtd-items "org-gtd")
```

**Step 4: Run tests**

Run: `eldev test -B`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-configure.el
git commit -m "refactor: remove org-gtd-configure-item and helpers"
```

---

## Task 15: Update tests that reference old system

**Files:**
- Check: `test/configure-test.el`
- Check: All test files for references to `:next`, `:delegated`, etc.

**Step 1: Search for old references**

Run: `grep -r "org-gtd-configure-item\|org-gtd-items\|:project-task\|:next\|:delegated\|:calendar\|:incubate\|:habit\|:knowledge\|:trash\|:quick-action" test/`

**Step 2: Update any test helpers or fixtures**

If any tests directly call `org-gtd-configure-item`, update them to use `org-gtd-configure-as-type`.

**Step 3: Run full test suite**

Run: `eldev test -B`
Expected: All 680+ tests pass

**Step 4: Commit if changes made**

```bash
git add test/
git commit -m "test: update tests for new type system"
```

---

## Task 16: Final verification and cleanup

**Step 1: Search for any remaining references**

Run: `grep -r "org-gtd-configure-item\|org-gtd-items" --include="*.el" .`
Expected: No results (except possibly in comments or doc strings)

**Step 2: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 3: Compile to check for warnings**

Run: `eldev clean && eldev compile`
Expected: No errors, minimal warnings

**Step 4: Final commit**

```bash
git add -A
git commit -m "chore: consolidate type system complete"
```

---

## Summary

After completing all tasks:
- `org-gtd-types` is the single source of truth for GTD types
- `org-gtd-configure-as-type` is the only way to configure item types
- Project tasks use `'next-action` type + separate TRIGGER setup
- Old `org-gtd-items`, `org-gtd-user-item-config`, and `org-gtd-configure-item` are deleted
- All tests pass
