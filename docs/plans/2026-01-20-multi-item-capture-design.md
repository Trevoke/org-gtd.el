# Multi-Item Capture Timestamps Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** When capturing multiple inbox items in one session, each level-1 heading gets the `ORG_GTD_CAPTURED_AT` timestamp automatically. (Issue #99)

**Architecture:** Modify the existing `:before-finalize` hook to process all level-1 headings instead of just the current one.

**Tech Stack:** Emacs Lisp, org-map-entries

---

## Task 1: Write failing test for multi-item capture

**Files:**
- Modify: `test/unit/capture-test.el`

**Step 1: Add the failing test**

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

Run: `~/bin/eldev etest test/unit/capture-test.el -r dot`
Expected: FAIL - only first heading has timestamp

**Step 3: Commit**

```bash
git add test/unit/capture-test.el
git commit -m "test(capture): add failing test for multi-item capture timestamps"
```

---

## Task 2: Implement multi-item timestamp support

**Files:**
- Modify: `org-gtd-capture.el` (lines 96-101)

**Step 1: Update org-gtd-capture--add-captured-at-timestamp**

Change from:

```elisp
(defun org-gtd-capture--add-captured-at-timestamp ()
  "Add ORG_GTD_CAPTURED_AT property with inactive timestamp.
Used as :before-finalize hook in `org-gtd-capture-templates'."
  (org-back-to-heading t)
  (org-entry-put nil "ORG_GTD_CAPTURED_AT"
                 (format-time-string (org-time-stamp-format t t))))
```

To:

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

**Step 2: Run tests to verify they pass**

Run: `~/bin/eldev etest test/unit/capture-test.el -r dot`
Expected: All 4 tests pass

**Step 3: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 4: Commit**

```bash
git add org-gtd-capture.el
git commit -m "feat(capture): timestamp all headings in multi-item capture

When capturing multiple items in one session, each level-1 heading
now gets the ORG_GTD_CAPTURED_AT property. All headings in the same
capture session share the same timestamp.

Closes #99"
```

---

## Task 3: Update documentation

**Files:**
- Modify: `doc/org-gtd.org` (capture section)

**Step 1: Find capture documentation and add note about multi-item capture**

Add a note explaining that users can capture multiple items by adding headings in the capture buffer, and each will get a timestamp.

**Step 2: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: document multi-item capture behavior"
```

---

## Task 4: Update changelog

**Files:**
- Modify: `CHANGELOG.org`

**Step 1: Add entry under Unreleased**

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
