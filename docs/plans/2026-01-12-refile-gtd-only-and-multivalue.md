# Refile GTD-Only and Multivalue Property Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix auto-refile to always use GTD targets in org-gtd-tasks.org, and support multivalue ORG_GTD_REFILE properties.

**Architecture:** Add a GTD-only target lookup function that ignores user's `org-refile-targets`. Use this for auto-refile path and target existence checks. Change property comparisons from `string-equal` to `member` with `split-string` for multivalue support.

**Tech Stack:** Emacs Lisp, org-mode, org-refile

---

## Task 1: Add Test for GTD-Only Target Lookup

**Files:**
- Modify: `test/unit/refiling-test.el`

**Step 1: Write failing test for GTD-only targets**

Add after existing tests (around line 260):

```elisp
(ert-deftest refile/gtd-targets-ignores-user-refile-targets ()
  "GTD-only targets should not include user's org-refile-targets."
  (org-gtd-test-with-tmp-org-dir
    (let* ((user-file (expand-file-name "user-notes.org" org-gtd-directory))
           (org-refile-targets `(((,user-file) :maxlevel . 9))))
      ;; Create user file with a heading
      (with-current-buffer (find-file-noselect user-file)
        (insert "* User Target\n")
        (save-buffer))
      ;; Create GTD file with proper target
      (with-current-buffer (org-gtd--default-file)
        (erase-buffer)
        (insert (format "* Actions\n:PROPERTIES:\n:%s: %s\n:END:\n"
                        org-gtd-prop-refile org-gtd-action))
        (save-buffer))
      ;; GTD-only targets should NOT include user file
      (let ((targets (org-gtd-refile--get-gtd-targets org-gtd-action)))
        (should targets)
        (should-not (seq-find (lambda (t) (string-match-p "user-notes" (nth 1 t)))
                              targets))
        (should (seq-find (lambda (t) (string-match-p "org-gtd-tasks" (nth 1 t)))
                          targets))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev etest -r dot -p "gtd-targets-ignores"`
Expected: FAIL - function `org-gtd-refile--get-gtd-targets` not defined

**Step 3: Commit failing test**

```bash
git add test/unit/refiling-test.el
git commit -m "test: add failing test for GTD-only target lookup"
```

---

## Task 2: Implement GTD-Only Target Function

**Files:**
- Modify: `org-gtd-refile.el:175` (after `org-gtd-refile--get-targets`)

**Step 1: Add the new function**

Insert after `org-gtd-refile--get-targets` function:

```elisp
(defun org-gtd-refile--get-gtd-targets (type)
  "Get GTD-only refile targets for TYPE.
Only looks in `org-gtd-tasks.org', ignoring user's `org-refile-targets'
and other `org-agenda-files'.  This ensures auto-refile always goes to
the GTD file, regardless of user configuration."
  (let* ((gtd-file (org-gtd--path org-gtd-default-file-name))
         (org-refile-target-verify-function (org-gtd-refile--make-verify-function type))
         (org-refile-targets `(((,gtd-file) :maxlevel . 9)))
         (org-refile-use-outline-path t)
         (org-outline-path-complete-in-steps nil))
    (org-refile-get-targets)))
```

**Step 2: Run test to verify it passes**

Run: `eldev etest -r dot -p "gtd-targets-ignores"`
Expected: PASS

**Step 3: Run all refile tests to check for regressions**

Run: `eldev etest -r dot -p "refile"`
Expected: All PASS

**Step 4: Commit**

```bash
git add org-gtd-refile.el
git commit -m "feat(refile): add GTD-only target lookup function"
```

---

## Task 3: Add Test for Auto-Refile Using GTD-Only Targets

**Files:**
- Modify: `test/unit/refiling-test.el`

**Step 1: Write failing test**

Add after previous test:

```elisp
(ert-deftest refile/auto-refile-uses-gtd-targets-only ()
  "Auto-refile should use GTD targets, ignoring user's org-refile-targets."
  (org-gtd-test-with-tmp-org-dir
    (let* ((user-file (expand-file-name "user-notes.org" org-gtd-directory))
           (org-refile-targets `(((,user-file) :maxlevel . 9)))
           (org-gtd-refile-to-any-target t))
      ;; Create user file with a heading (would be first target in merged list)
      (with-current-buffer (find-file-noselect user-file)
        (insert "* User Target\n")
        (save-buffer))
      ;; Create GTD file with proper target
      (with-current-buffer (org-gtd--default-file)
        (erase-buffer)
        (insert (format "* Actions\n:PROPERTIES:\n:%s: %s\n:END:\n"
                        org-gtd-prop-refile org-gtd-action))
        (save-buffer))
      ;; Create inbox with item to process
      (with-current-buffer (org-gtd--inbox-file)
        (erase-buffer)
        (insert "* Test item\n")
        (save-buffer))
      ;; Process the item
      (org-gtd-process-inbox)
      (with-simulated-input "SPC"  ; select single-action
        (org-gtd-single-action))
      ;; Item should be in GTD file, not user file
      (with-current-buffer (find-file-noselect user-file)
        (goto-char (point-min))
        (should-not (search-forward "Test item" nil t)))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (should (search-forward "Test item" nil t))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev etest -r dot -p "auto-refile-uses-gtd"`
Expected: FAIL - item ends up in user file instead of GTD file

**Step 3: Commit failing test**

```bash
git add test/unit/refiling-test.el
git commit -m "test: add failing test for auto-refile using GTD-only targets"
```

---

## Task 4: Update org-gtd-refile--do to Use GTD-Only for Auto-Refile

**Files:**
- Modify: `org-gtd-refile.el:144-163`

**Step 1: Refactor org-gtd-refile--do**

Replace the entire function:

```elisp
(defun org-gtd-refile--do (type refile-target-element)
  "Refile an item to an appropriate GTD location.

TYPE is one of the org-gtd action types (e.g., `org-gtd-projects').
REFILE-TARGET-ELEMENT is a string template for creating a new target if needed.

When prompting (type in `org-gtd-refile-prompt-for-types'), shows merged
targets from user's `org-refile-targets' and GTD files.

When auto-refiling (type not in prompt list), only uses GTD targets in
`org-gtd-tasks.org', ignoring user configuration."
  ;; Check GTD-only targets for existence (not merged targets)
  (unless (org-gtd-refile--get-gtd-targets type)
    (org-gtd-refile--add-target refile-target-element))

  (if (org-gtd-refile--should-prompt-p org-gtd--organize-type)
      ;; Prompt mode: use merged targets (user + GTD)
      (let ((org-refile-target-verify-function (org-gtd-refile--make-verify-function type))
            (org-refile-targets (append org-refile-targets
                                        '((org-agenda-files :maxlevel . 9))))
            (org-refile-use-outline-path t)
            (org-outline-path-complete-in-steps nil))
        (org-refile nil nil nil "Finish organizing task under: "))
    ;; Auto-refile: GTD-only targets
    (org-refile nil nil (car (org-gtd-refile--get-gtd-targets type)))))
```

**Step 2: Run test to verify it passes**

Run: `eldev etest -r dot -p "auto-refile-uses-gtd"`
Expected: PASS

**Step 3: Run all refile tests**

Run: `eldev etest -r dot -p "refile"`
Expected: All PASS

**Step 4: Commit**

```bash
git add org-gtd-refile.el
git commit -m "feat(refile): auto-refile uses GTD-only targets"
```

---

## Task 5: Add Test for Multivalue ORG_GTD_REFILE Property

**Files:**
- Modify: `test/unit/refiling-test.el`

**Step 1: Write failing test**

```elisp
(ert-deftest refile/multivalue-property-matches-any-type ()
  "Headings with multiple ORG_GTD_REFILE values should match any of them."
  (org-gtd-test-with-tmp-org-dir
    (with-current-buffer (org-gtd--default-file)
      (erase-buffer)
      ;; Create heading that accepts both Actions and Projects
      (insert (format "* Work Tasks\n:PROPERTIES:\n:%s: %s %s\n:END:\n"
                      org-gtd-prop-refile org-gtd-action org-gtd-projects))
      (save-buffer))
    ;; Should find target for Actions
    (let ((action-targets (org-gtd-refile--get-gtd-targets org-gtd-action)))
      (should action-targets)
      (should (seq-find (lambda (t) (string-match-p "Work Tasks" (car t)))
                        action-targets)))
    ;; Should also find target for Projects
    (let ((project-targets (org-gtd-refile--get-gtd-targets org-gtd-projects)))
      (should project-targets)
      (should (seq-find (lambda (t) (string-match-p "Work Tasks" (car t)))
                        project-targets)))))
```

**Step 2: Run test to verify it fails**

Run: `eldev etest -r dot -p "multivalue-property"`
Expected: FAIL - only matches if property exactly equals type

**Step 3: Commit failing test**

```bash
git add test/unit/refiling-test.el
git commit -m "test: add failing test for multivalue ORG_GTD_REFILE"
```

---

## Task 6: Update Verify Function for Multivalue Property

**Files:**
- Modify: `org-gtd-refile.el:100-125`

**Step 1: Update org-gtd-refile--make-verify-function**

Change line 123 from:
```elisp
(in-gtd-dir (string-equal type refile-prop))
```

To:
```elisp
(in-gtd-dir (and refile-prop
                 (member type (split-string refile-prop))))
```

The full cond clause becomes:
```elisp
(cond
 ;; WIP temp files should never be refile targets
 (in-wip-dir nil)
 ;; Inbox file should never be a refile target
 (is-inbox nil)
 ;; Files in GTD dir must have matching ORG_GTD_REFILE property (multivalue)
 (in-gtd-dir (and refile-prop
                  (member type (split-string refile-prop))))
 ;; Other files (user's custom targets) are accepted
 (t t))
```

**Step 2: Run test to verify it passes**

Run: `eldev etest -r dot -p "multivalue-property"`
Expected: PASS

**Step 3: Run all refile tests**

Run: `eldev etest -r dot -p "refile"`
Expected: All PASS

**Step 4: Commit**

```bash
git add org-gtd-refile.el
git commit -m "feat(refile): support multivalue ORG_GTD_REFILE property"
```

---

## Task 7: Add Test for org-gtd-refile--group-p Multivalue

**Files:**
- Modify: `test/unit/refiling-test.el`

**Step 1: Write failing test**

```elisp
(ert-deftest refile/group-p-handles-multivalue ()
  "org-gtd-refile--group-p should handle multivalue ORG_GTD_REFILE."
  (org-gtd-test-with-tmp-org-dir
    (with-current-buffer (org-gtd--default-file)
      (erase-buffer)
      (insert (format "* Work Tasks\n:PROPERTIES:\n:%s: %s %s\n:END:\n"
                      org-gtd-prop-refile org-gtd-action org-gtd-projects))
      (save-buffer)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      ;; Should match both types
      (should (org-gtd-refile--group-p org-gtd-action))
      (should (org-gtd-refile--group-p org-gtd-projects))
      ;; Should not match unrelated types
      (should-not (org-gtd-refile--group-p org-gtd-calendar)))))
```

**Step 2: Run test to verify it fails**

Run: `eldev etest -r dot -p "group-p-handles"`
Expected: FAIL - uses string-equal, only matches first value

**Step 3: Commit failing test**

```bash
git add test/unit/refiling-test.el
git commit -m "test: add failing test for group-p multivalue support"
```

---

## Task 8: Update org-gtd-refile--group-p for Multivalue

**Files:**
- Modify: `org-gtd-refile.el:191-194`

**Step 1: Update the function**

Replace:
```elisp
(defun org-gtd-refile--group-p (type)
  "Determine whether the current heading is of a given gtd TYPE."
  (string-equal type
                (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
```

With:
```elisp
(defun org-gtd-refile--group-p (type)
  "Determine whether the current heading is of a given gtd TYPE.
TYPE can be one of multiple space-separated values in ORG_GTD_REFILE."
  (let ((refile-prop (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
    (and refile-prop
         (member type (split-string refile-prop)))))
```

**Step 2: Run test to verify it passes**

Run: `eldev etest -r dot -p "group-p-handles"`
Expected: PASS

**Step 3: Run all tests**

Run: `eldev etest -r dot`
Expected: All PASS

**Step 4: Commit**

```bash
git add org-gtd-refile.el
git commit -m "feat(refile): group-p supports multivalue ORG_GTD_REFILE"
```

---

## Task 9: Update CHANGELOG

**Files:**
- Modify: `CHANGELOG.org`

**Step 1: Add entry at top of changes**

Add under the next version section (or create one):

```org
** Unreleased

*** Changed
- Auto-refile now only uses targets in =org-gtd-tasks.org=, ignoring user's =org-refile-targets= and other =org-agenda-files=. This fixes issues where items would be refiled to unexpected locations.

*** Added
- =ORG_GTD_REFILE= property now supports multiple space-separated values. A heading with =:ORG_GTD_REFILE: Actions Projects:= will be a valid target for both single actions and projects.
```

**Step 2: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs: update CHANGELOG for refile improvements"
```

---

## Task 10: Final Verification

**Step 1: Run full test suite**

Run: `eldev etest -r dot`
Expected: All PASS

**Step 2: Run linting**

Run: `eldev lint`
Expected: No errors

**Step 3: Manual smoke test**

1. Start Emacs with org-gtd loaded
2. Set `org-refile-targets` to point to a non-GTD file
3. Capture an item to inbox
4. Process it as single-action
5. Verify it ends up in `org-gtd-tasks.org` under Actions heading

**Step 4: Compile to check for warnings**

Run: `eldev clean && eldev compile`
Expected: No warnings

---

## Summary of Changes

| File | Change |
|------|--------|
| `org-gtd-refile.el` | Add `org-gtd-refile--get-gtd-targets`, update `org-gtd-refile--do`, update verify function, update `group-p` |
| `test/unit/refiling-test.el` | Add 4 new tests |
| `CHANGELOG.org` | Document changes |
