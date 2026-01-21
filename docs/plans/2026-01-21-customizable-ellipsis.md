# Customizable Truncation Ellipsis Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `org-gtd-agenda-truncate-ellipsis` defcustom so users with fonts where "…" doesn't take 1 character width can customize the truncation string. (Issue #200)

**Architecture:** Add defcustom in org-gtd-core.el near `org-gtd-prefix-width`, update `org-gtd-agenda--resolve-prefix-chain` to use it, remove dead code `org-gtd-agenda--prefix-format`, update tests and documentation.

**Tech Stack:** Emacs Lisp, defcustom, e-unit testing framework

---

### Task 1: Add defcustom for truncation ellipsis

**Files:**
- Modify: `org-gtd-core.el:238` (add after `org-gtd-prefix-width`)

**Step 1: Read the file to confirm insertion point**

Confirm `org-gtd-prefix-width` ends at line 237, insert after.

**Step 2: Add the defcustom**

Insert after line 237:

```elisp
(defcustom org-gtd-agenda-truncate-ellipsis "…"
  "String to append when truncating long text in agenda prefixes.

The default ellipsis character (…) doesn't have consistent width across
all fonts. If your agenda items don't align properly, try using a
different string like \"...\" or \"⣀\".

See also `org-gtd-prefix-width' for controlling prefix column width."
  :group 'org-gtd
  :package-version '(org-gtd . "4.5")
  :type 'string)
```

**Step 3: Run tests to verify no breakage**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass (no functional change yet)

**Step 4: Commit**

```bash
git add org-gtd-core.el
git commit -m "feat(core): add org-gtd-agenda-truncate-ellipsis defcustom"
```

---

### Task 2: Update org-gtd-agenda--resolve-prefix-chain to use the variable

**Files:**
- Modify: `org-gtd-agenda.el:131`

**Step 1: Write failing test**

Add to `test/unit/prefix-element-test.el` before `(provide 'prefix-element-test)`:

```elisp
(deftest prefix-chain/uses-custom-ellipsis ()
  "Uses org-gtd-agenda-truncate-ellipsis for truncation."
  (create-project "Very long project name that will be truncated")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "task")
    (org-back-to-heading t)
    (let ((org-gtd-agenda-truncate-ellipsis "..."))
      (assert-equal "Very lo..."
                    (org-gtd-agenda--resolve-prefix-chain '(project) 10)))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest test/unit/prefix-element-test.el::prefix-chain/uses-custom-ellipsis -r dot`
Expected: FAIL - still uses hardcoded "…"

**Step 3: Update the function**

Change line 131 in `org-gtd-agenda.el` from:

```elisp
    (truncate-string-to-width (or result "") width nil ?\s "…")))
```

To:

```elisp
    (truncate-string-to-width (or result "") width nil ?\s org-gtd-agenda-truncate-ellipsis)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest test/unit/prefix-element-test.el -r dot`
Expected: All tests pass

**Step 5: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 6: Commit**

```bash
git add org-gtd-agenda.el test/unit/prefix-element-test.el
git commit -m "feat(agenda): use org-gtd-agenda-truncate-ellipsis for truncation

Closes #200"
```

---

### Task 3: Remove dead code org-gtd-agenda--prefix-format

**Files:**
- Modify: `org-gtd-agenda.el:54-67` (remove function)
- Modify: `test/unit/gtd-view-language-test.el:386` (update test)

**Step 1: Verify function is not used in production**

Search for `org-gtd-agenda--prefix-format` - should only appear in its definition and one test.

**Step 2: Update the test that uses it as an example**

In `test/unit/gtd-view-language-test.el`, change line 386 from:

```elisp
         (prefix-format " %i %-12:(org-gtd-agenda--prefix-format 12) ")
```

To:

```elisp
         (prefix-format " %i %-12:(org-gtd-agenda--resolve-prefix-chain '(project) 12) ")
```

**Step 3: Remove the dead function**

Delete lines 54-67 in `org-gtd-agenda.el` (the entire `org-gtd-agenda--prefix-format` function).

**Step 4: Run tests to verify nothing breaks**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-agenda.el test/unit/gtd-view-language-test.el
git commit -m "refactor(agenda): remove dead code org-gtd-agenda--prefix-format"
```

---

### Task 4: Update documentation

**Files:**
- Modify: `doc/org-gtd.org:4416` (add after org-gtd-prefix-width documentation)

**Step 1: Add documentation for the new variable**

Insert after the `org-gtd-prefix-width` section (around line 4416, before "*** Archive Configuration"):

```org
**** ~org-gtd-agenda-truncate-ellipsis~

*Type*: String

*Default*: ~"…"~

*Description*: String appended when truncating long text in agenda prefixes.

The default ellipsis character (~…~) doesn't have consistent width across all fonts. Some fonts (like Iosevka) display it wider than one character, causing agenda items to misalign.

*Example*: Use ASCII dots for consistent alignment:
#+begin_src emacs-lisp
(setq org-gtd-agenda-truncate-ellipsis "...")
#+end_src

*Example*: Use a Braille character (works well in some fonts):
#+begin_src emacs-lisp
(setq org-gtd-agenda-truncate-ellipsis "⣀")
#+end_src

*See also*: ~org-gtd-prefix-width~ for controlling prefix column width.

```

**Step 2: Update the "truncated with" mentions**

Update line 4406 from:
```
Long values are truncated with "…". Short values are padded with spaces.
```

To:
```
Long values are truncated with ~org-gtd-agenda-truncate-ellipsis~ (default "…"). Short values are padded with spaces.
```

Update line 7884 similarly from:
```
*Note*: Long values are truncated with "…". Short values are padded with spaces.
```

To:
```
*Note*: Long values are truncated with ~org-gtd-agenda-truncate-ellipsis~ (default "…"). Short values are padded with spaces.
```

**Step 3: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: document org-gtd-agenda-truncate-ellipsis variable"
```

---

### Task 5: Update changelog and final verification

**Files:**
- Modify: `CHANGELOG.org:1` (add under Unreleased section)

**Step 1: Add changelog entry**

Add after existing Unreleased entries:

```org
*** Customizable truncation ellipsis (Issue #200)
~org-gtd-agenda-truncate-ellipsis~ controls the string used when truncating
long text in agenda prefixes. Users with fonts where "…" doesn't display
as one character width can now customize this (e.g., ~"..."~ or ~"⣀"~).
```

**Step 2: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 3: Compile with warnings**

Run: `~/bin/eldev clean && ~/bin/eldev compile`
Expected: No errors (minor warnings acceptable)

**Step 4: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs: add customizable truncation ellipsis to changelog"
```

---

### Summary

After completing all tasks:
- `org-gtd-agenda-truncate-ellipsis` defcustom available with default "…"
- Users can customize to "...", "⣀", or any string that aligns in their font
- Dead code removed (`org-gtd-agenda--prefix-format`)
- Single code path for truncation
- Documentation updated
- Issue #200 closed
