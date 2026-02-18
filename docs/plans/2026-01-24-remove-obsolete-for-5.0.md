# Remove Obsolete Items for 5.0.0 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all items deprecated in 4.0.0 to clean up the codebase for the 5.0.0 major release.

**Architecture:** Systematic removal of obsolete function/variable aliases, simplification of backward-compatibility shims, test updates (delete alias-verification tests, update tests using old names), and documentation updates. Each removal is independent and can be committed separately.

**Tech Stack:** Emacs Lisp, e-unit testing framework, Org-mode documentation

---

## Task 1: Remove Reflect/Review/Oops Function Aliases

**Files:**
- Modify: `org-gtd-reflect.el:316-397`
- Delete tests in: `test/unit/reflect-test.el:70-95`

**Step 1: Delete the 21 obsolete function aliases in org-gtd-reflect.el**

Remove these lines (316-397):

```elisp
;; DELETE ALL OF THESE:
(define-obsolete-function-alias 'org-gtd-oops
  #'org-gtd-reflect-missed-engagements "4.0")

(define-obsolete-function-alias 'org-gtd-oops-delegated
  #'org-gtd-reflect-missed-delegated "4.0")

(define-obsolete-function-alias 'org-gtd-oops-calendar
  #'org-gtd-reflect-missed-calendar "4.0")

(define-obsolete-function-alias 'org-gtd-oops-projects
  #'org-gtd-reflect-missed-projects "4.0")

(define-obsolete-function-alias 'org-gtd-oops-with-custom
  #'org-gtd-reflect-missed-with-custom "4.0")

(define-obsolete-function-alias 'org-gtd-review-area-of-focus
  #'org-gtd-reflect-area-of-focus "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-items
  #'org-gtd-reflect-missed-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-calendar-items
  #'org-gtd-reflect-stuck-calendar-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-delegated-items
  #'org-gtd-reflect-stuck-delegated-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-habit-items
  #'org-gtd-reflect-stuck-habit-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-incubated-items
  #'org-gtd-reflect-stuck-tickler-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-projects
  #'org-gtd-reflect-stuck-projects "4.0")

(define-obsolete-function-alias 'org-gtd-review-stuck-single-action-items
  #'org-gtd-reflect-stuck-single-action-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-completed-items
  #'org-gtd-reflect-completed-items "4.0")

(define-obsolete-function-alias 'org-gtd-review-completed-projects
  #'org-gtd-reflect-completed-projects "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-engagements
  #'org-gtd-reflect-missed-engagements "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-delegated
  #'org-gtd-reflect-missed-delegated "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-calendar
  #'org-gtd-reflect-missed-calendar "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-projects
  #'org-gtd-reflect-missed-projects "4.0")

(define-obsolete-function-alias 'org-gtd-review-upcoming-delegated
  #'org-gtd-reflect-upcoming-delegated "4.0")

(define-obsolete-function-alias 'org-gtd-review-missed-with-custom
  #'org-gtd-reflect-missed-with-custom "4.0")
```

**Step 2: Delete the 5 obsolete variable aliases in org-gtd-reflect.el**

Remove these lines (88-91, 218-221, 285-288):

```elisp
;; DELETE ALL OF THESE:
(define-obsolete-variable-alias 'org-gtd-review-missed-items-view-specs
  'org-gtd-reflect-missed-items-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-oops-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-engagements-view-specs
  'org-gtd-reflect-missed-engagements-view-specs "4.0")

(define-obsolete-variable-alias 'org-gtd-oops-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")

(define-obsolete-variable-alias 'org-gtd-review-missed-custom-views
  'org-gtd-reflect-missed-custom-views "4.0")
```

**Step 3: Delete alias verification tests in reflect-test.el**

Remove these tests (lines 70-95):

```elisp
;; DELETE ALL OF THESE TESTS:
(deftest reflect/provides-oops-alias ()
  "Provides org-gtd-oops as an alias."
  (assert-true (fboundp 'org-gtd-oops)))

(deftest reflect/provides-oops-delegated-alias ()
  "Provides org-gtd-oops-delegated as an alias."
  (assert-true (fboundp 'org-gtd-oops-delegated)))

(deftest reflect/provides-oops-calendar-alias ()
  "Provides org-gtd-oops-calendar as an alias."
  (assert-true (fboundp 'org-gtd-oops-calendar)))

(deftest reflect/provides-oops-projects-alias ()
  "Provides org-gtd-oops-projects as an alias."
  (assert-true (fboundp 'org-gtd-oops-projects)))

(deftest reflect/provides-oops-with-custom-alias ()
  "Provides org-gtd-oops-with-custom as an alias."
  (assert-true (fboundp 'org-gtd-oops-with-custom)))

(deftest reflect/provides-oops-custom-views-alias ()
  "Provides org-gtd-oops-custom-views as a variable alias."
  (assert-true (boundp 'org-gtd-oops-custom-views)))

(deftest reflect/provides-oops-view-specs-alias ()
  "org-gtd-oops-view-specs aliases to org-gtd-reflect-missed-view-specs."
  (assert-true (boundp 'org-gtd-oops-view-specs)))
```

**Step 4: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-reflect.el test/unit/reflect-test.el
git commit -m "$(cat <<'EOF'
refactor!: remove reflect/review/oops obsolete aliases

BREAKING CHANGE: Remove function and variable aliases deprecated in 4.0:
- org-gtd-oops -> org-gtd-reflect-missed-engagements
- org-gtd-oops-* -> org-gtd-reflect-missed-*
- org-gtd-review-* -> org-gtd-reflect-*
- org-gtd-oops-view-specs -> org-gtd-reflect-missed-engagements-view-specs
- org-gtd-oops-custom-views -> org-gtd-reflect-missed-custom-views

Users must update their configs to use the new names.
EOF
)"
```

---

## Task 2: Remove Tickler/Incubate Aliases

**Files:**
- Modify: `org-gtd-tickler.el:154-161`
- Modify: `org-gtd-core.el:110`
- Delete: `test/unit/backward-compatibility-test.el` (entire file)

**Step 1: Delete function aliases in org-gtd-tickler.el**

Remove lines 154-161:

```elisp
;; DELETE:
(define-obsolete-function-alias 'org-gtd-incubate
  #'org-gtd-tickler "4.0")

(define-obsolete-function-alias 'org-gtd-incubate-create
  #'org-gtd-tickler-create "4.0")
```

**Step 2: Delete variable alias in org-gtd-core.el**

Remove line 110:

```elisp
;; DELETE:
(define-obsolete-variable-alias 'org-gtd-incubate 'org-gtd-tickler "4.0")
```

**Step 3: Delete the entire backward-compatibility-test.el file**

```bash
rm test/unit/backward-compatibility-test.el
```

This file only contains tests verifying the incubate aliases exist.

**Step 4: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass (fewer tests now)

**Step 5: Commit**

```bash
git add org-gtd-tickler.el org-gtd-core.el
git add -u test/unit/backward-compatibility-test.el
git commit -m "$(cat <<'EOF'
refactor!: remove incubate->tickler obsolete aliases

BREAKING CHANGE: Remove aliases deprecated in 4.0:
- org-gtd-incubate -> org-gtd-tickler
- org-gtd-incubate-create -> org-gtd-tickler-create
- org-gtd-incubate variable -> org-gtd-tickler

Users must update their configs to use tickler instead of incubate.
EOF
)"
```

---

## Task 3: Remove Engage Alias

**Files:**
- Modify: `org-gtd-engage.el:42-43, 108-111`

**Step 1: Delete the variable alias**

Remove lines 42-43:

```elisp
;; DELETE:
(define-obsolete-variable-alias 'org-gtd-engage-prefix-width
  'org-gtd-prefix-width "4.0")
```

**Step 2: Delete the function alias**

Remove lines 108-111:

```elisp
;; DELETE:
(define-obsolete-function-alias 'org-gtd-engage-grouped-by-context
  #'org-gtd-engage-tagged "4.0"
  "Use `org-gtd-engage-tagged' instead.
The new function prompts for any tag, not just @-prefixed context tags.")
```

**Step 3: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 4: Commit**

```bash
git add org-gtd-engage.el
git commit -m "$(cat <<'EOF'
refactor!: remove engage obsolete aliases

BREAKING CHANGE: Remove aliases deprecated in 4.0:
- org-gtd-engage-prefix-width -> org-gtd-prefix-width
- org-gtd-engage-grouped-by-context -> org-gtd-engage-tagged
EOF
)"
```

---

## Task 4: Remove Clarify Map Alias

**Files:**
- Modify: `org-gtd-clarify.el:188`

**Step 1: Delete the variable alias**

Remove line 188:

```elisp
;; DELETE:
(make-obsolete-variable 'org-gtd-clarify-map 'org-gtd-clarify-mode-map "4.0")
```

**Step 2: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass (the alias is still defined elsewhere, tests use it)

**Step 3: Commit**

```bash
git add org-gtd-clarify.el
git commit -m "$(cat <<'EOF'
refactor!: remove org-gtd-clarify-map obsolete alias

BREAKING CHANGE: org-gtd-clarify-map is removed.
Use org-gtd-clarify-mode-map instead.
EOF
)"
```

---

## Task 5: Remove with-org-gtd-context Macro

**Files:**
- Modify: `org-gtd-core.el:465-484`

**Step 1: Delete the macro and its obsolete declaration**

Remove lines 465-484:

```elisp
;; DELETE ALL OF THIS:
(defmacro with-org-gtd-context (&rest body)
  "Execute BODY in the context of org-gtd.

DEPRECATED: This macro is no longer needed as of org-gtd 4.0.
Users should configure `org-agenda-files' directly.
This macro now simply executes BODY with a deprecation warning."
  (declare (indent 2))
  (let ((warning-shown (make-symbol "warning-shown")))
    `(progn
       (unless (bound-and-true-p org-gtd--context-warning-shown)
         (display-warning 'org-gtd
                          "with-org-gtd-context is deprecated and is now a no-op.
Configure `org-agenda-files' directly instead."
                          :warning)
         (setq org-gtd--context-warning-shown t))
       ,@body)))

(make-obsolete 'with-org-gtd-context
               "Configure `org-agenda-files' directly"
               "4.0")
```

**Step 2: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-core.el
git commit -m "$(cat <<'EOF'
refactor!: remove with-org-gtd-context macro

BREAKING CHANGE: The with-org-gtd-context macro is removed.
Users should configure org-agenda-files directly:

  (setq org-agenda-files (list org-gtd-directory))
EOF
)"
```

---

## Task 6: Remove Keyword Variables

**Files:**
- Modify: `org-gtd-core.el:411-437, 502-512`

**Step 1: Delete the 4 obsolete keyword variables**

Remove lines 411-437:

```elisp
;; DELETE ALL OF THESE:
(defcustom org-gtd-todo-keyword nil
  "..."
  ...)

(make-obsolete-variable 'org-gtd-todo-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-next-keyword nil
  "..."
  ...)

(make-obsolete-variable 'org-gtd-next-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-wait-keyword nil
  "..."
  ...)

(make-obsolete-variable 'org-gtd-wait-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-canceled-keyword nil
  "..."
  ...)

(make-obsolete-variable 'org-gtd-canceled-keyword 'org-gtd-keyword-mapping "4.0")
```

**Step 2: Simplify org-gtd-keywords--get-effective-mapping**

Replace the function (around lines 491-514) with:

```elisp
(defun org-gtd-keywords--get-effective-mapping ()
  "Get the effective keyword mapping."
  org-gtd-keyword-mapping)
```

This removes the backward-compatibility shim that checked for old variables.

**Step 3: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 4: Commit**

```bash
git add org-gtd-core.el
git commit -m "$(cat <<'EOF'
refactor!: remove individual keyword variables

BREAKING CHANGE: Remove deprecated keyword variables:
- org-gtd-todo-keyword
- org-gtd-next-keyword
- org-gtd-wait-keyword
- org-gtd-canceled-keyword

Use org-gtd-keyword-mapping instead:

  (setq org-gtd-keyword-mapping
        '((todo . "TODO")
          (next . "NEXT")
          (wait . "WAIT")
          (done . "DONE")
          (canceled . "CNCL")))
EOF
)"
```

---

## Task 7: Remove org-gtd-refile-to-any-target

**Files:**
- Modify: `org-gtd-refile.el:38-55, 79-84, 128-141`

**Step 1: Delete the deprecated variable and warning tracker**

Remove lines 38-55:

```elisp
;; DELETE:
(defcustom org-gtd-refile-to-any-target t
  "..."
  ...)

(make-obsolete-variable 'org-gtd-refile-to-any-target
                        'org-gtd-refile-prompt-for-types
                        "4.0.0")
```

Remove lines 83-84:

```elisp
;; DELETE:
(defvar org-gtd-refile--deprecated-warning-shown nil
  "Non-nil if deprecation warning for `org-gtd-refile-to-any-target' was shown.")
```

**Step 2: Update org-gtd-refile-prompt-for-types default and docstring**

Replace the defcustom (lines 57-77) with:

```elisp
(defcustom org-gtd-refile-prompt-for-types nil
  "List of GTD item types that should prompt for refile target selection.

By default this is nil, meaning all items auto-refile to the first
available target.  This provides a turnkey experience where organizing
is fast and frictionless.

To control where specific item types are filed, add them to this list.
When an item's type is in the list, org-gtd prompts you to choose from
available refile targets.

Example - prompt only for projects (most common customization):

  (setq org-gtd-refile-prompt-for-types
        \\='(project-heading project-task))

Example - prompt for everything (maximum control):

  (setq org-gtd-refile-prompt-for-types
        \\='(single-action project-heading project-task calendar
          someday delegated tickler habit knowledge quick-action trash))

Valid type symbols:
  single-action   - One-off tasks
  project-heading - New project containers
  project-task    - Tasks added to existing projects
  calendar        - Date/time specific items
  someday         - Someday/maybe items
  delegated       - Items waiting on others
  tickler         - Items to resurface later
  habit           - Recurring habits
  knowledge       - Reference material
  quick-action    - <2 minute tasks (done immediately)
  trash           - Items to discard"
  :group 'org-gtd-organize
  :package-version '(org-gtd . "5.0.0")
  :type '(repeat symbol))
```

**Step 3: Simplify org-gtd-refile--should-prompt-p**

Replace the function (lines 128-141) with:

```elisp
(defun org-gtd-refile--should-prompt-p (type)
  "Return non-nil if refiling TYPE should prompt for target selection.
Checks if TYPE is in `org-gtd-refile-prompt-for-types'."
  (memq type org-gtd-refile-prompt-for-types))
```

**Step 4: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: Tests FAIL (tests still reference old variable)

**Step 5: Commit work in progress**

```bash
git add org-gtd-refile.el
git commit -m "wip: remove org-gtd-refile-to-any-target (tests need update)"
```

---

## Task 8: Update Refile Tests

**Files:**
- Modify: `test/unit/refiling-test.el`
- Modify: `test/helpers/setup.el`

**Step 1: Delete deprecated-behavior tests in refiling-test.el**

Remove these tests (lines 176-199):

```elisp
;; DELETE ALL OF THESE:
(deftest refile/should-prompt-returns-nil-when-deprecated-var-set ()
  "Returns nil when org-gtd-refile-to-any-target is t (deprecated path)."
  (let ((org-gtd-refile-to-any-target t)
        (org-gtd-refile--deprecated-warning-shown t))
    (assert-nil (org-gtd-refile--should-prompt-p 'single-action))))

(deftest refile/should-prompt-checks-list-when-deprecated-var-nil ()
  "Checks org-gtd-refile-prompt-for-types when deprecated var is nil."
  (let ((org-gtd-refile-to-any-target nil)
        (org-gtd-refile-prompt-for-types '(single-action calendar)))
    (assert-true (org-gtd-refile--should-prompt-p 'single-action))
    (assert-true (org-gtd-refile--should-prompt-p 'calendar))
    (assert-nil (org-gtd-refile--should-prompt-p 'trash))))

(deftest refile/deprecated-var-shows-warning-once ()
  "Shows deprecation warning only once per session."
  (let ((org-gtd-refile-to-any-target t)
        (org-gtd-refile--deprecated-warning-shown nil)
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (org-gtd-refile--should-prompt-p 'single-action)
      (org-gtd-refile--should-prompt-p 'calendar)
      (assert-equal 1 (length warnings)))))
```

**Step 2: Add a new test for the simplified behavior**

Add this test in place of the deleted ones:

```elisp
(deftest refile/should-prompt-checks-list ()
  "Checks org-gtd-refile-prompt-for-types for prompting decision."
  (let ((org-gtd-refile-prompt-for-types '(single-action calendar)))
    (assert-true (org-gtd-refile--should-prompt-p 'single-action))
    (assert-true (org-gtd-refile--should-prompt-p 'calendar))
    (assert-nil (org-gtd-refile--should-prompt-p 'trash))))

(deftest refile/should-prompt-nil-means-no-prompts ()
  "When org-gtd-refile-prompt-for-types is nil, never prompt."
  (let ((org-gtd-refile-prompt-for-types nil))
    (assert-nil (org-gtd-refile--should-prompt-p 'single-action))
    (assert-nil (org-gtd-refile--should-prompt-p 'project-heading))
    (assert-nil (org-gtd-refile--should-prompt-p 'calendar))))
```

**Step 3: Remove org-gtd-refile-to-any-target bindings throughout refiling-test.el**

Search and remove all `(org-gtd-refile-to-any-target ...)` bindings from let forms. There are approximately 10 occurrences. For each one, simply delete the binding line.

Example - change:

```elisp
(let* ((org-gtd-refile-to-any-target nil)
       (targets (org-gtd-refile--get-targets org-gtd-projects))
```

To:

```elisp
(let* ((targets (org-gtd-refile--get-targets org-gtd-projects))
```

**Step 4: Update test/helpers/setup.el**

Remove line 92:

```elisp
;; DELETE:
org-gtd-refile-to-any-target t
```

Also update line 109 - ensure `org-gtd-refile-prompt-for-types` is set appropriately for tests (likely keep it as `nil` for auto-refile behavior in tests).

**Step 5: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 6: Commit**

```bash
git add test/unit/refiling-test.el test/helpers/setup.el
git commit -m "test: update refile tests for 5.0 variable removal"
```

**Step 7: Amend previous WIP commit**

```bash
git rebase -i HEAD~2
# Mark the WIP commit as "fixup" to squash into the test commit
# Or use: git reset --soft HEAD~2 && git commit -m "..."
```

Final commit message:

```bash
git commit --amend -m "$(cat <<'EOF'
refactor!: remove org-gtd-refile-to-any-target

BREAKING CHANGE: org-gtd-refile-to-any-target is removed.

The new behavior uses only org-gtd-refile-prompt-for-types:
- Default is nil (auto-refile everything, turnkey experience)
- Add types to the list to prompt for those types

Example - prompt for projects only:

  (setq org-gtd-refile-prompt-for-types
        '(project-heading project-task))
EOF
)"
```

---

## Task 9: Update Tests Using Old Names

**Files:**
- Modify: `test/unit/true-end-to-end-test.el`
- Modify: `test/unit/keymap-transient-test.el`
- Modify: `test/unit/horizons-test.el`
- Modify: `test/helpers/setup.el`
- Modify: `test/helpers/keyboard-integration.el`
- Modify: `test/integration/full-user-experience-test.el`
- Modify: `test/acceptance/review-flow-test.el`
- Modify: `test/acceptance/project-cancellation-test.el`
- Modify: `test/integration/end-to-end-test.el`

**Step 1: Replace org-gtd-clarify-map with org-gtd-clarify-mode-map**

Use search-and-replace across all test files:

```bash
# Find all occurrences first
grep -r "org-gtd-clarify-map[^-]" test/

# Replace (be careful - map not mode-map)
sed -i 's/org-gtd-clarify-map\([^-]\)/org-gtd-clarify-mode-map\1/g' \
  test/unit/true-end-to-end-test.el \
  test/unit/keymap-transient-test.el \
  test/unit/horizons-test.el \
  test/helpers/setup.el \
  test/helpers/keyboard-integration.el \
  test/integration/full-user-experience-test.el
```

**Step 2: Replace org-gtd-review-stuck-projects with org-gtd-reflect-stuck-projects**

```bash
sed -i 's/org-gtd-review-stuck-projects/org-gtd-reflect-stuck-projects/g' \
  test/acceptance/review-flow-test.el \
  test/acceptance/project-cancellation-test.el \
  test/integration/end-to-end-test.el \
  test/unit/true-end-to-end-test.el
```

**Step 3: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 4: Commit**

```bash
git add test/
git commit -m "test: update tests to use new function/variable names"
```

---

## Task 10: Update org-gtd-upgrades.el

**Files:**
- Modify: `org-gtd-upgrades.el:98-99`

**Step 1: Remove references to deprecated variable**

Find and update lines 98-99 that reference `org-gtd-refile-to-any-target`:

```elisp
;; FIND THIS:
(with-suppressed-warnings ((obsolete org-gtd-refile-to-any-target))
  (let ((org-gtd-refile-to-any-target t))
```

Replace with simply using `org-gtd-refile-prompt-for-types`:

```elisp
;; REPLACE WITH:
(let ((org-gtd-refile-prompt-for-types nil))  ; auto-refile during upgrade
```

**Step 2: Run tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-upgrades.el
git commit -m "fix: update upgrades to use new refile variable"
```

---

## Task 11: Update Documentation

**Files:**
- Modify: `doc/org-gtd.org`

**Step 1: Update the "What's New in 4.0" section (lines 145-180)**

Remove references to deprecated aliases and update the refile section. The key changes:

- Remove "Deprecates org-gtd-refile-to-any-target" language
- Update any mentions of old alias names to note they're now removed in 5.0

**Step 2: Update the quick start config example (lines 461-466)**

Remove these lines:

```org
;; Enable per-type refile prompting (recommended)
;; Without this, all items auto-refile to first target without prompting
(setq org-gtd-refile-to-any-target nil)
```

Update the comment about `org-gtd-refile-prompt-for-types` to reflect new default.

**Step 3: Update variable list entry (line 922)**

Change from:

```org
- ~org-gtd-refile-prompt-for-types~ :: list of GTD item types that should prompt for refile target selection. Types not in the list auto-refile to first target. Requires ~org-gtd-refile-to-any-target~ to be ~nil~.
```

To:

```org
- ~org-gtd-refile-prompt-for-types~ :: list of GTD item types that should prompt for refile target selection. Default is nil (auto-refile everything). Add types to prompt for specific items.
```

**Step 4: Rewrite "Controlling Refile Prompts" section (lines 1851-1885)**

Replace with new content that doesn't mention the old variable:

```org
**** Controlling Refile Prompts by Item Type
:PROPERTIES:
:CUSTOM_ID: controlling-refile-prompts
:END:

By default, org-gtd auto-refiles all items to the first available target without prompting. This provides a fast, turnkey experience.

To control where specific item types are filed, add them to ~org-gtd-refile-prompt-for-types~:

#+begin_src elisp
;; Prompt only for projects (most common customization)
(setq org-gtd-refile-prompt-for-types '(project-heading project-task))
#+end_src

When an item's type is in the list, org-gtd prompts you to choose from available refile targets.

*Example: Prompt for everything*

#+begin_src elisp
(setq org-gtd-refile-prompt-for-types
      '(single-action project-heading project-task calendar
        someday delegated tickler habit knowledge quick-action trash))
#+end_src

*Valid type symbols:* ~single-action~, ~project-heading~, ~project-task~, ~calendar~, ~someday~, ~delegated~, ~tickler~, ~habit~, ~knowledge~, ~quick-action~, ~trash~
```

**Step 5: Delete org-gtd-refile-to-any-target reference entry (lines 4161-4186)**

Remove the entire reference entry for the deprecated variable.

**Step 6: Update org-gtd-refile-prompt-for-types reference entry (lines 4188-4216)**

Update the default value and description:

```org
**** ~org-gtd-refile-prompt-for-types~

*Type*: List of symbols

*Default*: ~nil~

*Description*: List of GTD item types that should prompt for refile target selection. By default this is nil, meaning all items auto-refile to the first available target. Add types to this list to prompt for those specific item types.
```

**Step 7: Update "Backward compatibility" note (line 4510)**

Remove or update the note about `org-gtd-oops-custom-views` since it's now removed.

**Step 8: Build documentation to verify**

Run: `~/bin/eldev compile`
Expected: No errors

**Step 9: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: update documentation for 5.0 obsolete removal"
```

---

## Task 12: Add CHANGELOG Migration Section

**Files:**
- Modify: `CHANGELOG.org`

**Step 1: Add 5.0.0 section at the top of CHANGELOG.org**

Add after the header:

```org
* 5.0.0 - Unreleased

** BREAKING CHANGES

*** Removed deprecated aliases and variables

The following items deprecated in 4.0 have been removed:

**** Functions renamed (use new names):
- ~org-gtd-oops~ → ~org-gtd-reflect-missed-engagements~
- ~org-gtd-oops-delegated~ → ~org-gtd-reflect-missed-delegated~
- ~org-gtd-oops-calendar~ → ~org-gtd-reflect-missed-calendar~
- ~org-gtd-oops-projects~ → ~org-gtd-reflect-missed-projects~
- ~org-gtd-oops-with-custom~ → ~org-gtd-reflect-missed-with-custom~
- ~org-gtd-review-*~ → ~org-gtd-reflect-*~ (all review functions)
- ~org-gtd-incubate~ → ~org-gtd-tickler~
- ~org-gtd-incubate-create~ → ~org-gtd-tickler-create~
- ~org-gtd-engage-grouped-by-context~ → ~org-gtd-engage-tagged~

**** Variables renamed (use new names):
- ~org-gtd-incubate~ → ~org-gtd-tickler~
- ~org-gtd-clarify-map~ → ~org-gtd-clarify-mode-map~
- ~org-gtd-engage-prefix-width~ → ~org-gtd-prefix-width~
- ~org-gtd-oops-view-specs~ → ~org-gtd-reflect-missed-engagements-view-specs~
- ~org-gtd-oops-custom-views~ → ~org-gtd-reflect-missed-custom-views~
- ~org-gtd-review-missed-*-view-specs~ → ~org-gtd-reflect-*-view-specs~

**** Variables removed entirely:
- ~org-gtd-refile-to-any-target~ - Use ~org-gtd-refile-prompt-for-types~ instead
- ~org-gtd-todo-keyword~ - Use ~org-gtd-keyword-mapping~ instead
- ~org-gtd-next-keyword~ - Use ~org-gtd-keyword-mapping~ instead
- ~org-gtd-wait-keyword~ - Use ~org-gtd-keyword-mapping~ instead
- ~org-gtd-canceled-keyword~ - Use ~org-gtd-keyword-mapping~ instead

**** Macros removed:
- ~with-org-gtd-context~ - No longer needed; configure ~org-agenda-files~ directly

*** Default behavior change for refile prompting

~org-gtd-refile-prompt-for-types~ now defaults to ~nil~ instead of a list of types. This means:

- *Old behavior*: Most item types prompted for refile target selection
- *New behavior*: All items auto-refile to first target (turnkey experience)

To restore prompting for specific types:

#+begin_src elisp
;; Prompt for projects only
(setq org-gtd-refile-prompt-for-types '(project-heading project-task))

;; Prompt for everything
(setq org-gtd-refile-prompt-for-types
      '(single-action project-heading project-task calendar
        someday delegated tickler habit knowledge quick-action trash))
#+end_src
```

**Step 2: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs: add 5.0.0 CHANGELOG with migration guide"
```

---

## Task 13: Final Verification

**Step 1: Clean and recompile**

```bash
~/bin/eldev clean && ~/bin/eldev compile
```

Expected: No errors or warnings about obsolete items

**Step 2: Run full test suite**

```bash
~/bin/eldev -p -dtT etest -r dot
```

Expected: All tests pass

**Step 3: Run linter**

```bash
~/bin/eldev lint
```

Expected: No new warnings

**Step 4: Verify autoloads are regenerated**

Check that `org-gtd-autoloads.el` no longer contains the removed aliases:

```bash
grep -E "org-gtd-oops|org-gtd-incubate|org-gtd-review-|with-org-gtd-context" org-gtd-autoloads.el
```

Expected: No matches (or only in comments)

**Step 5: Final commit if any cleanup needed**

```bash
git status
# If autoloads need committing:
git add org-gtd-autoloads.el
git commit -m "chore: regenerate autoloads after obsolete removal"
```

---

## Summary

| Task | Description | Files Changed |
|------|-------------|---------------|
| 1 | Remove reflect/review/oops aliases | org-gtd-reflect.el, reflect-test.el |
| 2 | Remove incubate aliases | org-gtd-tickler.el, org-gtd-core.el, backward-compatibility-test.el |
| 3 | Remove engage aliases | org-gtd-engage.el |
| 4 | Remove clarify-map alias | org-gtd-clarify.el |
| 5 | Remove with-org-gtd-context | org-gtd-core.el |
| 6 | Remove keyword variables | org-gtd-core.el |
| 7 | Remove refile-to-any-target | org-gtd-refile.el |
| 8 | Update refile tests | refiling-test.el, setup.el |
| 9 | Update tests using old names | ~9 test files |
| 10 | Update upgrades.el | org-gtd-upgrades.el |
| 11 | Update documentation | doc/org-gtd.org |
| 12 | Add CHANGELOG migration | CHANGELOG.org |
| 13 | Final verification | - |
