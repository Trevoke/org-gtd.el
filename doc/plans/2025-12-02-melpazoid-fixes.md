# Melpazoid Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix all melpazoid/package-lint warnings to ensure org-gtd.el meets MELPA quality standards.

**Architecture:** This is a cleanup task with many small fixes across multiple files. We prioritize critical package-lint errors first, then warnings, then style issues.

**Tech Stack:** Emacs Lisp, melpazoid, package-lint, checkdoc

---

## Issue Categories

### Critical Errors (Must Fix for MELPA)
1. **org-edna aliases**: Non-standard separator `/` in function names (org-gtd-projects.el)
2. **False positive project-name**: Local variables triggering Emacs 29.1 dependency warning
3. **org-fold-show-entry**: Requires Emacs 29.1 (org-gtd-id-overlay.el)
4. **compat dependency**: In org-gtd-backward-compatibility.el
5. **org-gtd-pkg.el**: Missing Package-Requires, missing license
6. **org-gtd-graph-debug.el**: Missing license boilerplate
7. **org-gtd-agenda-property.el**: Missing lexical-binding, broken URL

### Warnings (Should Fix)
- Closing parens on new lines
- Sharp-quote function names
- Docstrings wider than 80 chars
- Aliases declared before referent
- eval-after-load usage

### Checkdoc (Nice to Have)
- Lisp symbols should appear in quotes
- Missing documentation strings
- Argument names in docstrings
- Various formatting issues

---

## Task 1: Fix org-edna Aliases (Critical)

**Files:**
- Modify: `org-gtd-projects.el:1016-1048`

**Context:** org-edna REQUIRES functions to be named `org-edna-finder/NAME` and `org-edna-action/NAME`. This is a design constraint of org-edna that we cannot change. The solution is to add package-lint directives to suppress these specific warnings.

**Step 1: Read the current code**

Read lines 1010-1050 of org-gtd-projects.el to understand the exact structure.

**Step 2: Add package-lint suppress comments**

Add `; package-lint-suppress(non-standard-symbol)` comments above each defalias:

```elisp
;; These aliases use org-edna's required naming convention:
;; org-edna-finder/NAME and org-edna-action/NAME
;; The slashes are required by org-edna's design.
;; package-lint-suppress(name-not-standard-symbol)
(defalias 'org-edna-finder/org-gtd-next-project-action
  #'org-gtd-projects--edna-next-project-action)

;; package-lint-suppress(name-not-standard-symbol)
(defalias 'org-edna-action/org-gtd-update-project-task!
  #'org-gtd-projects--edna-update-project-task)

;; package-lint-suppress(name-not-standard-symbol)
(defalias 'org-edna-action/org-gtd-update-project-after-task-done!
  #'org-gtd-projects--edna-update-project-after-task-done)
```

Also change from `'function` to `#'function` for sharp-quoting.

**Step 3: Run melpazoid and verify**

Run: `./run-melpazoid.sh`
Verify: org-gtd-projects.el package-lint section shows no more errors.

---

## Task 2: Rename project-name Local Variables (Critical)

**Files:**
- Modify: `org-gtd-clarify.el` (lines 316, 405, 424)
- Modify: `org-gtd-graph-view.el` (line 82)
- Modify: `org-gtd-agenda.el` (lines 57, 64)

**Context:** package-lint incorrectly flags local variables named `project-name` as calls to the Emacs 29.1 `project-name` function. Rename to `proj-name` to avoid false positive.

**Step 1: Fix org-gtd-clarify.el**

Find and replace in this file only:
- `project-name` -> `proj-name` (in function parameters and let bindings)
- Update docstrings: `PROJECT-NAME` -> `PROJ-NAME`

**Step 2: Fix org-gtd-graph-view.el**

Same replacement pattern.

**Step 3: Fix org-gtd-agenda.el**

Same replacement pattern.

**Step 4: Run tests**

Run: `~/.local/bin/eldev gtd-etest`
Expected: All tests pass

**Step 5: Run melpazoid**

Run: `./run-melpazoid.sh`
Verify: No more "project-name" Emacs 29.1 warnings

---

## Task 3: Fix org-fold-show-entry Dependency (Critical)

**Files:**
- Modify: `org-gtd-id-overlay.el` (lines 299, 307)

**Context:** `org-fold-show-entry` is from Emacs 29.1. Since org-gtd requires Emacs 28.1, we need to use the compat library or a compatibility shim.

**Step 1: Read the affected code**

Read lines 295-315 of org-gtd-id-overlay.el.

**Step 2: Use compat-function or conditional**

Option A: Use `(compat-function org-fold-show-entry)` if compat provides it
Option B: Use conditional: `(if (fboundp 'org-fold-show-entry) (org-fold-show-entry) (outline-show-entry))`

The compat library should provide this. Add require for compat if needed.

**Step 3: Verify**

Run: `~/.local/bin/eldev compile`
Expected: No byte-compile warnings about org-fold-show-entry

---

## Task 4: Fix org-gtd-agenda-property.el (Critical)

**Files:**
- Modify: `org-gtd-agenda-property.el` (lines 1, 6, 97)

**Step 1: Add lexical-binding**

Change line 1 from:
```elisp
;;; org-gtd-agenda-property.el --- Display org properties in the agenda buffer.
```
to:
```elisp
;;; org-gtd-agenda-property.el --- Display org properties in the agenda buffer. -*- lexical-binding: t; -*-
```

**Step 2: Fix URL**

Line 6: Change from `http://github.com/Bruce-Connor/org-gtd-agenda-property` to the org-gtd repository URL or comment noting this is vendored code.

**Step 3: Fix quote escaping**

Line 97: Change unescaped single quotes to `\\='` or use backticks.

---

## Task 5: Update org-gtd-pkg.el (Critical)

**Files:**
- Modify: `org-gtd-pkg.el`

**Step 1: Add missing dependencies**

Update to match org-gtd.el Package-Requires:

```elisp
(define-package "org-gtd" "3.0.0"
  "An implementation of GTD."
  '((emacs "28.1")
    (compat "30.0.0.0")
    (org-edna "1.1.2")
    (f "0.20.0")
    (org "9.6")
    (transient "0.3.7")
    (org-ql "0.8.10")
    (dag-draw "1.0.0")))
```

**Step 2: Add license boilerplate**

Add GPL license header at top of file.

---

## Task 6: Fix org-gtd-graph-debug.el (Critical)

**Files:**
- Modify: `org-gtd-graph-debug.el`

**Step 1: Add proper file header**

```elisp
;;; org-gtd-graph-debug.el --- Debug helpers for graph visualization -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;;; Commentary:

;; Debug helpers for visualizing org-gtd project graphs.
;; These are development utilities, not for end-user use.

;;; Code:

(require 'org-gtd-graph-data)
```

---

## Task 7: Fix Sharp-Quote Warnings (Warning)

**Files to modify (add #' before function symbols):**
- `org-gtd-tickler.el` (lines 160, 164)
- `org-gtd-task-management.el` (lines 651, 657)
- `org-gtd-reflect.el` (lines 300-387, many instances)
- `org-gtd-dependencies.el` (line 105)
- `org-gtd-areas-of-focus.el` (line 87)
- `org-gtd-agenda-property.el` (lines 181, 182, 186, 188)
- `org-gtd-graph-transient.el` (line 1031)

**Pattern:** Change `'function-name` to `#'function-name` where the value is a function reference.

---

## Task 8: Fix Closing Parens on New Lines (Warning)

**Files:**
- Modify: `org-gtd-ql.el` (line 118)
- Modify: `org-gtd-core.el` (line 129)

Move closing parens to same line as last element.

---

## Task 9: Fix Docstrings Wider Than 80 Chars (Warning)

**Files:**
- `org-gtd-view-language.el` (lines 112, 154)
- `org-gtd-upgrades.el` (line 150)
- `org-gtd-projects.el` (line 674)
- `org-gtd-core.el` (line 487)
- `org-gtd-archive.el` (line 45)
- `org-gtd-agenda-property.el` (lines 66, 69)

Wrap long docstrings to stay under 80 characters.

---

## Task 10: Fix Aliases Before Referent (Warning)

**Files:**
- `org-gtd-reflect.el` (lines 320, 323, 391, 394, 397)
- `org-gtd-core.el` (line 540)

Move the `defvar` or `defcustom` declarations BEFORE the `defalias` statements.

---

## Task 11: Fix Double Negation (Warning)

**Files:**
- `org-gtd-graph-transient.el` (lines 821, 940)

Change `(not (null x))` to just `x` or `(not (not x))` to `x`.

---

## Task 12: Fix Checkdoc Issues (Nice to Have)

These are lower priority but should be fixed for polish:

1. **Lisp symbols in quotes**: Add backticks around symbol names in docstrings
2. **Missing docstrings**: Add documentation for interactive functions
3. **Argument names**: Ensure arguments appear as UPPERCASE in docstrings
4. **Trailing whitespace**: Remove whitespace at end of lines
5. **y-or-n-p ending**: Ensure prompts end with "?"

---

## Verification

After all tasks complete:

1. Run full test suite:
   ```bash
   ~/.local/bin/eldev gtd-etest
   ```

2. Run melpazoid:
   ```bash
   ./run-melpazoid.sh
   ```

3. Check for remaining issues:
   ```bash
   grep -E "error:|warning:" org-gtd.log
   ```

---

## Notes

### Issues NOT to Fix

1. **org-gtd-autoloads.el**: This is GENERATED. Do not modify directly. Fix source files instead.

2. **eval-after-load warnings**: These are acceptable in org-gtd-agenda-property.el (vendored code) and may be necessary for the package functionality.

3. **Loading adds hooks warnings**: These are intentional for packages that need to set up hooks when loaded.

### org-edna Naming Convention

The `org-edna-finder/NAME` and `org-edna-action/NAME` naming is REQUIRED by org-edna's architecture. The `/` separator is part of org-edna's plugin discovery mechanism. We cannot change this without breaking org-edna integration.
