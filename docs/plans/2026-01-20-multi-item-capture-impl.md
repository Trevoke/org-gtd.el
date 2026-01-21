# Multi-Item Capture Timestamps Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** When capturing multiple inbox items in one session, each level-1 heading gets the `ORG_GTD_CAPTURED_AT` timestamp automatically.

**Architecture:** Modify `org-gtd-capture--add-captured-at-timestamp` to use `org-map-entries` over all level-1 headings instead of just the current heading. All headings share the same timestamp (captured once at start).

**Tech Stack:** Emacs Lisp, org-map-entries, e-unit testing framework

---

### Task 1: Write failing test for multi-item capture

**Files:**
- Modify: `test/unit/capture-test.el:73` (add after existing tests)

**Step 1: Write the failing test**

Add this test at the end of `test/unit/capture-test.el`, before `(provide 'capture-test)`:

```elisp
(deftest capture/single-session-multi-item-timestamps-all ()
  "Multiple headings in one capture session all get timestamps."
  ;; Start capture
  (org-gtd-capture nil "i")
  ;; Add multiple headings in one session
  (insert "First item")
  (org-insert-heading)
  (insert "Second item")
  (org-insert-heading)
  (insert "Third item")
  ;; Finalize single capture
  (org-capture-finalize)
  ;; Verify all three have timestamps
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (let ((timestamps '()))
      (org-map-entries
       (lambda ()
         (push (org-entry-get nil "ORG_GTD_CAPTURED_AT") timestamps))
       "LEVEL=1" 'file)
      ;; All three have timestamps
      (assert-equal 3 (length (cl-remove-if-not #'identity timestamps)))
      ;; All timestamps are the same
      (assert-equal 1 (length (delete-dups (cl-remove-if-not #'identity timestamps)))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest test/unit/capture-test.el::capture/single-session-multi-item-timestamps-all`

Expected: FAIL - assertion `(assert-equal 3 ...)` fails because only 1 heading has timestamp

**Step 3: Commit**

```bash
git add test/unit/capture-test.el
git commit -m "test(capture): add failing test for multi-item capture timestamps"
```

---

### Task 2: Implement multi-item timestamp support

**Files:**
- Modify: `org-gtd-capture.el:96-101`

**Step 1: Read the current implementation**

Current code at lines 96-101:

```elisp
(defun org-gtd-capture--add-captured-at-timestamp ()
  "Add ORG_GTD_CAPTURED_AT property with inactive timestamp.
Used as :before-finalize hook in `org-gtd-capture-templates'."
  (org-back-to-heading t)
  (org-entry-put nil "ORG_GTD_CAPTURED_AT"
                 (format-time-string (org-time-stamp-format t t))))
```

**Step 2: Replace with new implementation**

Replace the function with:

```elisp
(defun org-gtd-capture--add-captured-at-timestamp ()
  "Add ORG_GTD_CAPTURED_AT property to all level-1 headings.
Used as :before-finalize hook in `org-gtd-capture-templates'.
All headings in a multi-item capture get the same timestamp."
  (let ((timestamp (format-time-string (org-time-stamp-format t t))))
    (org-map-entries
     (lambda ()
       (unless (org-entry-get nil "ORG_GTD_CAPTURED_AT")
         (org-entry-put nil "ORG_GTD_CAPTURED_AT" timestamp)))
     "LEVEL=1"
     'file)))
```

**Step 3: Run capture tests to verify they pass**

Run: `~/bin/eldev etest test/unit/capture-test.el -r dot`

Expected: All 4 tests pass (3 existing + 1 new)

**Step 4: Run full test suite**

Run: `~/bin/eldev etest -r dot`

Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-capture.el
git commit -m "feat(capture): timestamp all headings in multi-item capture

When capturing multiple items in one session, each level-1 heading
now gets the ORG_GTD_CAPTURED_AT property. All headings in the same
capture session share the same timestamp.

Closes #99"
```

---

### Task 3: Update changelog

**Files:**
- Modify: `CHANGELOG.org:1` (add under Unreleased section)

**Step 1: Add changelog entry**

Add after the existing Unreleased entries:

```org
*** Multi-item capture timestamps (Issue #99)
When capturing multiple inbox items in one session (by adding multiple
level-1 headings in the capture buffer), each heading now gets its own
~ORG_GTD_CAPTURED_AT~ timestamp. All items in the same capture session
share the same timestamp.
```

**Step 2: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs: add multi-item capture to changelog"
```

---

### Task 4: Run final verification

**Step 1: Run full test suite**

Run: `~/bin/eldev etest -r dot`

Expected: All tests pass

**Step 2: Compile with warnings**

Run: `~/bin/eldev clean && ~/bin/eldev compile`

Expected: No errors or warnings
