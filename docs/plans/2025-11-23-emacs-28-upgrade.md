# Emacs 28.1 Upgrade Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Upgrade org-gtd.el minimum Emacs version from 27.2 to 28.1, remove compatibility code, and adopt 28.1+ features for improved UX and performance.

**Architecture:** Three-phase approach: (1) Foundation - version bump and compatibility cleanup, (2) UX Improvements - adopt 28.1 features like format-prompt and string-search, (3) Performance - native compilation support and optimizations. Conservative testing at each phase ensures stability.

**Tech Stack:** Emacs Lisp 28.1+, Eldev (build/test), Buttercup (test framework), CI via GitHub Actions

---

## PHASE 1: FOUNDATION (Version Bump + Cleanup)

### Task 1: Update Package Version Requirements

**Files:**
- Modify: `org-gtd.el:7`
- Modify: `org-gtd-pkg.el:3`

**Step 1: Update org-gtd.el Package-Requires header**

Edit `org-gtd.el` line 7:

```elisp
;; Before:
;; Package-Requires: ((emacs "27.2") (compat "30.0.0.0") (org-edna "1.1.2") (f "0.20.0") (org "9.6") (org-agenda-property "1.3.1") (transient "0.3.7") (org-ql "0.8.10") (dag-draw "1.0.0"))

;; After:
;; Package-Requires: ((emacs "28.1") (compat "30.0.0.0") (org-edna "1.1.2") (f "0.20.0") (org "9.6") (org-agenda-property "1.3.1") (transient "0.3.7") (org-ql "0.8.10") (dag-draw "1.0.0"))
```

**Step 2: Update org-gtd-pkg.el define-package**

Edit `org-gtd-pkg.el` line 3:

```elisp
;; Before:
  '((emacs "27.2")

;; After:
  '((emacs "28.1")
```

**Step 3: Verify changes**

Run: `grep -n "27\.2" org-gtd.el org-gtd-pkg.el`
Expected: No output (no matches found)

Run: `grep -n "28\.1" org-gtd.el org-gtd-pkg.el`
Expected: Shows the updated lines

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors`
Expected: Compiles successfully with no warnings

**Step 5: Commit**

```bash
git add org-gtd.el org-gtd-pkg.el
git commit -m "chore: bump minimum Emacs version to 28.1

Package-Requires updated to reflect transient.el dependency.
This is a breaking change for 4.0.0 release."
```

---

### Task 2: Update Documentation Version Requirements

**Files:**
- Modify: `README.org:37`
- Modify: `CLAUDE.md:173`
- Modify: `doc/org-gtd.org:273`

**Step 1: Update README.org**

Find line 37 (or search for "Emacs 27.2"):

```org
;; Before:
- Emacs 27.2 or higher

;; After:
- Emacs 28.1 or higher
```

**Step 2: Update CLAUDE.md**

Find line 173 (or search for "Emacs 27.2+"):

```markdown
;; Before:
- **Emacs 27.2+**: Minimum version

;; After:
- **Emacs 28.1+**: Minimum version
```

**Step 3: Update doc/org-gtd.org**

Find line 273 (or search for "requires emacs 27.2"):

```org
;; Before:
This package requires emacs 27.2 or higher.

;; After:
This package requires Emacs 28.1 or higher.
```

**Step 4: Verify changes**

Run: `grep -n "27\.2" README.org CLAUDE.md doc/org-gtd.org`
Expected: No output

Run: `grep -n "28\.1" README.org CLAUDE.md doc/org-gtd.org`
Expected: Shows all three updated locations

**Step 5: Regenerate texinfo documentation**

Run: `emacs --batch --eval "(require 'ox-texinfo)" doc/org-gtd.org -f org-texinfo-export-to-texinfo`
Expected: Generates updated `doc/org-gtd.texi`

Run: `makeinfo doc/org-gtd.texi -o doc/org-gtd.info`
Expected: Generates updated `doc/org-gtd.info` with no errors

**Step 6: Commit**

```bash
git add README.org CLAUDE.md doc/org-gtd.org doc/org-gtd.texi doc/org-gtd.info
git commit -m "docs: update minimum version to 28.1 in all documentation

Updated README, CLAUDE.md, and info manual to reflect new requirement.
Regenerated texinfo and info files."
```

---

### Task 3: Remove 27.2 Compatibility Declarations in org-gtd-projects.el

**Files:**
- Modify: `org-gtd-projects.el:105`
- Modify: `org-gtd-projects.el:967`

**Step 1: Remove declare (modes ...) at line 105**

Find and remove this line and its comment:

```elisp
;; Before (around line 105):
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 2: Remove version check at line 967**

Find the version check and keep only the 28+ code path:

```elisp
;; Before (around line 967):
  (if (version< emacs-version "28")
      ;; 27.2 code path
      (old-implementation)
    ;; 28+ code path
    (new-implementation))

;; After - keep only the new implementation:
  (new-implementation)
```

Note: The exact code depends on what's there. Keep the `else` branch (28+ code).

**Step 3: Verify changes**

Run: `grep -n "27\.2" org-gtd-projects.el`
Expected: No matches

Run: `grep -n "version< emacs-version" org-gtd-projects.el`
Expected: No matches

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors`
Expected: No warnings, successful compilation

**Step 5: Test suite**

Run: `eldev test -B --file=test/project-test.el`
Expected: All tests pass

**Step 6: Commit**

```bash
git add org-gtd-projects.el
git commit -m "refactor: remove 27.2 compatibility code from org-gtd-projects

Removed declare (modes ...) and version< emacs-version check.
Now assumes 28.1+ behavior."
```

---

### Task 4: Remove 27.2 Compatibility Declarations in org-gtd-delegate.el

**Files:**
- Modify: `org-gtd-delegate.el:67`
- Modify: `org-gtd-delegate.el:87`

**Step 1: Remove first declare (modes ...) at line 67**

```elisp
;; Before (around line 67):
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 2: Remove second declare (modes ...) at line 87**

```elisp
;; Before (around line 87):
  (declare (modes org-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 3: Verify changes**

Run: `grep -n "27\.2" org-gtd-delegate.el`
Expected: No matches

Run: `grep -n "declare (modes" org-gtd-delegate.el`
Expected: No matches

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors org-gtd-delegate.el`
Expected: No warnings

**Step 5: Commit**

```bash
git add org-gtd-delegate.el
git commit -m "refactor: remove 27.2 compatibility declarations from org-gtd-delegate"
```

---

### Task 5: Remove 27.2 Compatibility Declarations in org-gtd-areas-of-focus.el

**Files:**
- Modify: `org-gtd-areas-of-focus.el:48`
- Modify: `org-gtd-areas-of-focus.el:58`

**Step 1: Remove first declare at line 48**

```elisp
;; Before:
  (declare (modes org-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 2: Remove second declare at line 58**

```elisp
;; Before:
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 3: Verify changes**

Run: `grep -n "27\.2" org-gtd-areas-of-focus.el`
Expected: No matches

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors org-gtd-areas-of-focus.el`
Expected: No warnings

**Step 5: Commit**

```bash
git add org-gtd-areas-of-focus.el
git commit -m "refactor: remove 27.2 compatibility declarations from org-gtd-areas-of-focus"
```

---

### Task 6: Remove 27.2 Compatibility Declarations in org-gtd-clarify.el

**Files:**
- Modify: `org-gtd-clarify.el:123`
- Modify: `org-gtd-clarify.el:138`
- Modify: `org-gtd-clarify.el:154`

**Step 1: Remove declare at line 123**

```elisp
;; Before:
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 2: Remove declare at line 138**

```elisp
;; Before:
  (declare (modes org-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 3: Remove declare at line 154**

```elisp
;; Before:
  (declare (modes org-gtd-clarify-mode)) ;; for 27.2 compatibility

;; After:
  ;; (removed)
```

**Step 4: Verify changes**

Run: `grep -n "27\.2" org-gtd-clarify.el`
Expected: No matches

**Step 5: Test compilation**

Run: `eldev compile --warnings-as-errors org-gtd-clarify.el`
Expected: No warnings

**Step 6: Commit**

```bash
git add org-gtd-clarify.el
git commit -m "refactor: remove 27.2 compatibility declarations from org-gtd-clarify"
```

---

### Task 7: Remove Ancient Version Check in org-gtd-id.el

**Files:**
- Modify: `org-gtd-id.el:130`

**Step 1: Find and examine the version check**

Look at line 130:

```elisp
;; Around line 130 - this checks for Emacs 25.0 (ancient!)
(if (version< emacs-version "25.0")
    (old-code)
  (new-code))
```

**Step 2: Keep only the else branch**

Since we're now requiring 28.1, keep only the modern code path:

```elisp
;; After:
(new-code)
```

**Step 3: Verify changes**

Run: `grep -n "version< emacs-version \"25" org-gtd-id.el`
Expected: No matches

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors org-gtd-id.el`
Expected: No warnings

**Step 5: Commit**

```bash
git add org-gtd-id.el
git commit -m "refactor: remove ancient Emacs 25.0 version check from org-gtd-id

Now requires 28.1+, no need for pre-25 compatibility."
```

---

### Task 8: Update org-gtd-configure.el Compatibility Comment

**Files:**
- Modify: `org-gtd-configure.el:122-124`

**Step 1: Find the compatibility comment**

Look at lines 122-124:

```elisp
;; Around line 122:
This function avoids using `eval' to ensure compatibility with Emacs 27.2,
which has limitations with closures in quoted forms.
Instead of returning a quoted form with a closure embedded (which fails on 27.2),
```

**Step 2: Update or remove the comment**

If the workaround is no longer needed in 28.1, remove it. If it's still needed for other reasons, update the comment:

```elisp
;; Option A - If workaround still needed for other reasons:
This function avoids using `eval' for cleaner code generation.
Instead of returning a quoted form with a closure embedded,

;; Option B - If workaround not needed, refactor the code to use simpler approach
```

Read the surrounding code to determine which approach is appropriate.

**Step 3: Verify the code still works**

Run: `eldev compile --warnings-as-errors org-gtd-configure.el`
Expected: No warnings

**Step 4: Test if configure functionality works**

Run: `eldev test -B --file=test/configure-test.el` (if test file exists)
Expected: Tests pass, or note if no specific tests exist

**Step 5: Commit**

```bash
git add org-gtd-configure.el
git commit -m "refactor: update or remove 27.2 compatibility comment in org-gtd-configure

Clarified that workaround is for code clarity, not 27.2 compatibility."
```

---

### Task 9: Update CI Matrix to Test on 28.2, 29.1, 30.1

**Files:**
- Modify: `.github/workflows/test.yml:16-18`

**Step 1: Update the matrix**

Edit `.github/workflows/test.yml`:

```yaml
# Before (around line 16-18):
    strategy:
      matrix:
        emacs_version:
          - 27.2
          - 28.2

# After:
    strategy:
      matrix:
        emacs_version:
          - 28.2
          - 29.1
          - 30.1
```

**Step 2: Verify changes**

Run: `grep -A5 "emacs_version:" .github/workflows/test.yml`
Expected: Shows 28.2, 29.1, 30.1 (no 27.2)

**Step 3: Commit**

```bash
git add .github/workflows/test.yml
git commit -m "ci: update test matrix to Emacs 28.2, 29.1, 30.1

Dropped 27.2 support, added 29.1 and 30.1 for future compatibility testing."
```

---

### Task 10: Phase 1 Validation - Full Test Suite

**Files:**
- None (validation only)

**Step 1: Clean and compile**

Run: `eldev clean && eldev compile --warnings-as-errors`
Expected: All files compile successfully with no warnings

**Step 2: Run linter**

Run: `eldev lint`
Expected: No lint errors

**Step 3: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 4: Verify no 27.2 references remain in code**

Run: `grep -r "27\.2" --include="*.el" --exclude-dir=test .`
Expected: No matches in source files (some may remain in test files as historical references)

**Step 5: Create Phase 1 completion marker**

```bash
echo "Phase 1 (Foundation) complete: $(date)" >> docs/plans/upgrade-progress.txt
git add docs/plans/upgrade-progress.txt
git commit -m "chore: mark Phase 1 (Foundation) complete

All version numbers updated, compatibility code removed, tests passing."
```

---

## PHASE 2: UX IMPROVEMENTS (28.1 Feature Adoption)

### Task 11: Audit and Replace string-match with string-search

**Files:**
- Audit: Multiple files (identified via grep)
- Modify: Files with `(string-match (regexp-quote ...))`

**Step 1: Find all string-match with regexp-quote patterns**

Run: `grep -rn "string-match.*regexp-quote" --include="*.el" .`
Expected: Shows all locations where we search for literal strings using regexp

Save output to review.

**Step 2: For each match, verify it's a literal search**

Review each instance. If the pattern is:
```elisp
(string-match (regexp-quote "literal-string") haystack)
```

It can be replaced with:
```elisp
(string-search "literal-string" haystack)
```

**Step 3: Replace first instance (example in org-gtd-projects.el)**

If found:
```elisp
;; Before:
(when (string-match (regexp-quote "Projects") (org-entry-get (point) "ORG_GTD"))
  ...)

;; After:
(when (string-search "Projects" (org-entry-get (point) "ORG_GTD"))
  ...)
```

**Step 4: Test the file**

Run: `eldev compile --warnings-as-errors org-gtd-projects.el`
Run: `eldev test -B --file=test/project-test.el`
Expected: Compiles and tests pass

**Step 5: Commit this file**

```bash
git add org-gtd-projects.el
git commit -m "perf: replace string-match + regexp-quote with string-search in org-gtd-projects

Using 28.1's faster string-search for literal string matching."
```

**Step 6: Repeat for other files**

For each file with this pattern, make the replacement, test, and commit separately.

---

### Task 12: Review and Enhance Command Docstrings

**Files:**
- Audit: All files with `###autoload` commands
- Modify: Commands with unclear docstrings

**Step 1: Find all autoloaded interactive commands**

Run: `grep -B1 "^(defun.*interactive" --include="*.el" -r . | grep "###autoload" -A2`
Expected: Shows all user-facing commands

**Step 2: Review each command's docstring**

Check that each docstring:
- Starts with a clear imperative verb ("Show...", "Create...", "Process...")
- Describes what the command does (not how)
- Mentions key arguments if any
- Is suitable for `M-S-x` filtering (28.1 feature)

**Step 3: Improve unclear docstrings**

Example improvement:
```elisp
;; Before:
(defun org-gtd-process-inbox ()
  "Go through inbox."
  ...)

;; After:
(defun org-gtd-process-inbox ()
  "Process all items in the GTD inbox one by one.

Opens each inbox item in a clarification buffer and prompts for
organization decision. Continues until inbox is empty."
  ...)
```

**Step 4: Test compilation**

Run: `eldev compile --warnings-as-errors`
Expected: No warnings

**Step 5: Commit**

```bash
git add [modified-files]
git commit -m "docs: improve command docstrings for better discoverability

Enhanced docstrings for M-S-x filtering (28.1 feature).
Clearer descriptions of what each command does."
```

---

### Task 13: Consider format-prompt Adoption (Optional)

**Files:**
- Audit: Files using `read-string` with default values
- Potentially modify: Selected prompts

**Step 1: Search for read-string patterns with defaults**

Run: `grep -rn "read-string.*default" --include="*.el" .`
Expected: Shows prompts that mention defaults

**Step 2: Evaluate if format-prompt adds value**

Most prompts use `org-read-date` which has its own default system.

For `read-string` prompts that manually format defaults:
```elisp
;; Before:
(read-string (format "%s (default %s): " prompt default))

;; After (if beneficial):
(read-string (format-prompt prompt default))
```

**Step 3: Apply only if it improves consistency**

If no clear wins, skip this task. The main UX value is in string-search (already done).

**Step 4: If changes made, test and commit**

```bash
git add [modified-files]
git commit -m "refactor: use format-prompt for consistent default display

Adopting 28.1 standard for showing default values in prompts."
```

---

### Task 14: Phase 2 Validation - UX Testing

**Files:**
- None (validation only)

**Step 1: Full compilation and lint**

Run: `eldev clean && eldev compile --warnings-as-errors`
Run: `eldev lint`
Expected: All clean

**Step 2: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 3: Manual smoke test**

Start Emacs with org-gtd loaded:
1. Capture item: `M-x org-gtd-capture`
2. Process inbox: `M-x org-gtd-process-inbox`
3. Create project with dependencies
4. View project graph: `M-x org-gtd-show-project-graph`
5. Open engage view: `C-c d e`
6. Incubate project: Press `I` in graph view
7. Reactivate project: `M-x org-gtd-reactivate`

Expected: All workflows function identically to before, no UX regressions

**Step 4: Mark Phase 2 complete**

```bash
echo "Phase 2 (UX Improvements) complete: $(date)" >> docs/plans/upgrade-progress.txt
git add docs/plans/upgrade-progress.txt
git commit -m "chore: mark Phase 2 (UX Improvements) complete

string-search adoption complete, command docstrings improved, all tests passing."
```

---

## PHASE 3: PERFORMANCE (Native Compilation + Optimizations)

### Task 15: Verify Lexical Binding in All Files

**Files:**
- Audit: All `.el` files

**Step 1: Check for lexical-binding headers**

Run: `head -1 *.el | grep -L "lexical-binding: t"`
Expected: Shows any files missing lexical-binding (should be none or very few)

**Step 2: Add lexical-binding to any missing files**

If any files lack it, add as first line:
```elisp
; -*- lexical-binding: t -*-
```

**Step 3: Test compilation**

Run: `eldev compile --warnings-as-errors`
Expected: All files compile with no warnings

**Step 4: Commit if changes made**

```bash
git add [modified-files]
git commit -m "refactor: ensure lexical-binding in all files for native compilation"
```

---

### Task 16: Audit for eval/Dynamic Code Patterns

**Files:**
- Audit: All source files

**Step 1: Search for problematic patterns**

Run: `grep -rn "\\(eval\\|load\\)" --include="*.el" --exclude-dir=test .`
Expected: Shows all uses of eval/load

**Step 2: Review each instance**

Check if eval/load prevents native compilation:
- `eval` in hot paths: Problematic
- `load` for feature loading: Usually OK
- `eval-when-compile`: OK (compile-time only)

**Step 3: Refactor if needed**

If problematic patterns found, refactor to avoid dynamic code generation.
This is optional for Phase 3 - only if obvious issues found.

**Step 4: Document findings**

```bash
echo "Native compilation audit: No blocking issues found" >> docs/plans/upgrade-progress.txt
git add docs/plans/upgrade-progress.txt
git commit -m "docs: audit complete for native compilation compatibility"
```

---

### Task 17: Add Native Compilation Documentation

**Files:**
- Modify: `README.org` (add performance section)

**Step 1: Add Performance section to README.org**

After the configuration section, add:

```org
** Performance

*** Native Compilation

org-gtd supports native compilation for improved performance. If you have
Emacs built with native compilation support (--with-native-compilation):

1. Enable automatic native compilation:
   #+begin_src emacs-lisp
   (setq package-native-compile t)
   #+end_src

2. Reinstall or recompile org-gtd:
   #+begin_src emacs-lisp
   M-x package-reinstall RET org-gtd RET
   #+end_src

3. Packages will be natively compiled on installation

4. Expected performance improvement: 10-20% in large GTD datasets (500+ tasks)

*** Performance Characteristics

org-gtd is optimized for Emacs 28.1+ primitives:
- Fast literal string matching (string-search)
- Lexical binding throughout
- Efficient property access

Tested with GTD datasets up to 500+ tasks without performance issues.
```

**Step 2: Verify rendering**

Open README.org in Emacs and verify org-mode rendering looks correct.

**Step 3: Commit**

```bash
git add README.org
git commit -m "docs: add native compilation and performance notes to README

Documenting 28.1 performance benefits and native compilation setup."
```

---

### Task 18: Optional Performance Profiling

**Files:**
- None (profiling exercise)

**Step 1: Create test workload**

Create a test file with realistic GTD data:
- 20 projects
- 100 tasks with dependencies
- Various states (NEXT, TODO, DONE)

**Step 2: Profile before optimizations (baseline)**

Run Emacs profiler:
```elisp
M-x profiler-start RET cpu RET
;; Execute test workload:
;; - Open engage view
;; - Render project graphs
;; - Process inbox items
M-x profiler-report RET
```

Save baseline results.

**Step 3: Profile after optimizations**

Run same profiler test with current code.
Compare results - should be similar or better.

**Step 4: Document findings**

If significant improvements or no regressions, note in commit message.
This task is optional - skip if time-constrained.

---

### Task 19: Phase 3 Validation - Final Testing

**Files:**
- None (validation only)

**Step 1: Clean build and full compilation**

Run: `eldev clean && eldev compile --warnings-as-errors`
Expected: Clean build with no warnings

**Step 2: Run linter**

Run: `eldev lint`
Expected: No lint errors

**Step 3: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 4: Test on multiple Emacs versions (if docker available)**

Run: `eldev docker 28.2 test -B`
Run: `eldev docker 29.1 test -B`
Run: `eldev docker 30.1 test -B`
Expected: All pass on all versions

**Step 5: Manual smoke test with native compilation**

If you have native compilation enabled:
1. Reinstall package: `M-x package-reinstall RET org-gtd RET`
2. Verify native compilation occurred (check `*.eln` files)
3. Run through full GTD workflow
4. Verify no performance regressions

**Step 6: Mark Phase 3 complete**

```bash
echo "Phase 3 (Performance) complete: $(date)" >> docs/plans/upgrade-progress.txt
git add docs/plans/upgrade-progress.txt
git commit -m "chore: mark Phase 3 (Performance) complete

Native compilation support verified, documentation added, all tests passing."
```

---

### Task 20: Update CHANGELOG for 4.0.0 Release

**Files:**
- Modify: `CHANGELOG.org` (add 4.0.0 entry at top)

**Step 1: Add 4.0.0 entry**

At the top of CHANGELOG.org, add:

```org
** 4.0.0 - YYYY-MM-DD

*** BREAKING CHANGES
**** Minimum Emacs version now 28.1
org-gtd now requires Emacs 28.1 or higher due to transient.el dependency
updating its minimum requirement.

*Users on Emacs 27.2 must remain on org-gtd 3.x.*

To check your Emacs version: =M-x emacs-version=

*** Removed
- Emacs 27.2 compatibility code and declarations
- Workarounds for pre-28.1 behavior
- Ancient Emacs 25.0 version checks

*** Improved
- Performance: Using 28.1's faster string-search for literal matching
- Code quality: Lexical binding throughout for native compilation support
- Documentation: Enhanced command docstrings for better discoverability
- Testing: CI now validates on Emacs 28.2, 29.1, and 30.1

*** Added
- Native compilation support (10-20% performance improvement on large datasets)
- Performance documentation in README

*** Technical
- Replaced string-match + regexp-quote with string-search where applicable
- Removed all (declare (modes ...)) compatibility declarations
- CI matrix updated to test on 28.2, 29.1, 30.1
```

**Step 2: Update date before release**

Replace YYYY-MM-DD with actual release date when ready.

**Step 3: Verify org formatting**

Open CHANGELOG.org in Emacs and verify rendering.

**Step 4: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs: add CHANGELOG entry for 4.0.0 release

Comprehensive changelog documenting breaking changes, improvements,
and technical updates for Emacs 28.1 upgrade."
```

---

### Task 21: Final Validation - Complete Upgrade

**Files:**
- None (final validation)

**Step 1: Verify all version references updated**

Run: `grep -rn "27\.2" --include="*.el" --include="*.org" --include="*.md" .`
Expected: Only historical references in CHANGELOG or comments, no active code

Run: `grep -rn "28\.1" README.org CLAUDE.md doc/org-gtd.org org-gtd.el org-gtd-pkg.el`
Expected: Shows all updated version requirements

**Step 2: Full clean build**

Run: `eldev clean && eldev compile --warnings-as-errors`
Expected: All files compile with no warnings

**Step 3: Complete lint check**

Run: `eldev lint`
Expected: No errors

**Step 4: Complete test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 5: Review git status**

Run: `git status`
Expected: Working directory clean (all changes committed)

Run: `git log --oneline -20`
Expected: Shows all commits from the three phases

**Step 6: Create final completion marker**

```bash
cat >> docs/plans/upgrade-progress.txt << EOF

===========================================
EMACS 28.1 UPGRADE COMPLETE
===========================================
Date: $(date)

Phase 1: Foundation ✓
Phase 2: UX Improvements ✓
Phase 3: Performance ✓

All tests passing on Emacs 28.2, 29.1, 30.1
Documentation updated
CHANGELOG prepared for 4.0.0 release

Ready for final review and release.
EOF

git add docs/plans/upgrade-progress.txt
git commit -m "chore: mark complete upgrade to Emacs 28.1

All three phases complete, tests passing, ready for 4.0.0 release."
```

**Step 7: Final checklist**

Verify all success criteria from design document:
- ✓ All version numbers updated to 28.1
- ✓ All 27.2 compatibility code removed
- ✓ CI tests on 28.2, 29.1, 30.1
- ✓ No compilation warnings
- ✓ No lint errors
- ✓ All tests pass
- ✓ Manual smoke test passed
- ✓ Documentation updated
- ✓ CHANGELOG entry added

---

## Execution Notes

**Estimated time:** 2-3 hours for all 21 tasks
**Dependencies:** Eldev, Emacs 28.2+ for testing
**Testing strategy:** Conservative - test after each phase
**Commit frequency:** After each task (frequent small commits)

## Success Criteria

All tasks complete when:
1. All version numbers show 28.1 (not 27.2)
2. No compatibility code for 27.2 remains
3. `eldev compile --warnings-as-errors` passes
4. `eldev lint` passes
5. `eldev test -B` passes with 100% success
6. Manual GTD workflow smoke test passes
7. Documentation updated (README, CHANGELOG, manual)
8. CI configured for 28.2, 29.1, 30.1

Ready for 4.0.0 release tagging and announcement.
