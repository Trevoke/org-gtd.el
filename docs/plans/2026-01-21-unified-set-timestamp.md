# Unified Set Timestamp Command Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create `org-gtd-set-timestamp` command that works from both org-mode headings and org-agenda items, then wire it into the agenda transient. (Issue #187)

**Architecture:** Add a unified command with context detection (org-mode vs agenda), validate GTD type (Calendar/Delegated/Tickler), delegate to shared core logic. Transient calls this same command.

**Tech Stack:** Emacs Lisp, org-mode, transient.el, e-unit testing framework

---

### Task 1: Add helper functions to org-gtd-core.el

**Files:**
- Modify: `org-gtd-core.el:45` (add after the timestamp constant)

**Step 1: Read the current file to confirm insertion point**

Run: Read `org-gtd-core.el` lines 40-50 to confirm the insertion point after `org-gtd-timestamp` constant.

**Step 2: Add the helper functions**

Insert after line 44 (`"Org property storing timestamps for `org-gtd' logic."`):

```elisp
(defconst org-gtd--timestamped-types '("Calendar" "Delegated" "Tickler")
  "GTD types that support ORG_GTD_TIMESTAMP operations.")

(defun org-gtd--timestamped-item-p ()
  "Return non-nil if heading at point is a timestamped GTD type.
Timestamped types are Calendar, Delegated, and Tickler items."
  (when-let ((org-gtd-type (org-entry-get nil "ORG_GTD")))
    (member org-gtd-type org-gtd--timestamped-types)))
```

**Step 3: Run tests to verify no breakage**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass (no functional change yet)

**Step 4: Commit**

```bash
git add org-gtd-core.el
git commit -m "refactor(core): add timestamped type helpers"
```

---

### Task 2: Write failing tests for org-gtd-set-timestamp

**Files:**
- Modify: `test/unit/agenda-transient-test.el:217` (add before `(provide 'agenda-transient-test)`)

**Step 1: Add tests for both org-mode and agenda contexts**

```elisp
;;; Set Timestamp Tests

(deftest time/set-timestamp-from-org-heading ()
  "Sets timestamp on Calendar item from org-mode heading."
  ;; Create a Calendar task
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* TODO Calendar item to reschedule\n")
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Calendar")
    (org-entry-put (point) org-gtd-timestamp "<2025-11-28>")
    (basic-save-buffer)

    ;; Call set-timestamp from org-mode
    (with-stub org-gtd-prompt-for-active-date "<2025-12-15>"
      (org-gtd-set-timestamp))

    ;; Verify timestamp was changed
    (assert-equal "<2025-12-15>" (org-entry-get (point) org-gtd-timestamp))))

(deftest time/set-timestamp-from-agenda ()
  "Sets timestamp on Delegated item from agenda buffer."
  ;; Create a Delegated task
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* WAIT Delegated item to reschedule\n")
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Delegated")
    (org-entry-put (point) org-gtd-timestamp "<2025-11-28>")
    (basic-save-buffer))

  ;; Generate agenda and call set-timestamp
  (org-agenda nil "t")
  (goto-char (point-min))
  (search-forward "Delegated item to reschedule")
  (beginning-of-line)

  (with-stub org-gtd-prompt-for-active-date "<2025-12-20>"
    (org-gtd-set-timestamp))

  ;; Verify timestamp was changed
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Delegated item to reschedule")
    (org-back-to-heading t)
    (assert-equal "<2025-12-20>" (org-entry-get (point) org-gtd-timestamp)))

  ;; Clean up agenda buffer
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

(deftest time/set-timestamp-rejects-non-timestamped-type ()
  "Refuses to set timestamp on non-timestamped GTD type."
  ;; Create an Actions task (not timestamped type)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* TODO Action item\n")
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Actions")
    (basic-save-buffer)

    ;; Call set-timestamp - should fail gracefully
    (org-gtd-set-timestamp)

    ;; Verify no timestamp was set
    (assert-nil (org-entry-get (point) org-gtd-timestamp))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest test/unit/agenda-transient-test.el -r dot`
Expected: FAIL - `org-gtd-set-timestamp` is void

**Step 3: Commit**

```bash
git add test/unit/agenda-transient-test.el
git commit -m "test(timestamp): add failing tests for org-gtd-set-timestamp"
```

---

### Task 3: Implement org-gtd-set-timestamp

**Files:**
- Modify: `org-gtd-core.el` (add after the helper functions from Task 1)

**Step 1: Add the context detection and main command**

Add after `org-gtd--timestamped-item-p`:

```elisp
(defun org-gtd--find-timestamped-item-at-point-or-agenda ()
  "Find timestamped GTD item from point in org-mode or agenda.
Returns marker if item is a Calendar, Delegated, or Tickler type.
Returns nil with message otherwise."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if-let ((marker (org-get-at-bol 'org-marker)))
        (org-with-point-at marker
          (if (org-gtd--timestamped-item-p)
              marker
            (message "Item is not a Calendar, Delegated, or Tickler type")
            nil))
      (message "No task at point")
      nil))

   ((derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (if (org-gtd--timestamped-item-p)
          (point-marker)
        (message "Item is not a Calendar, Delegated, or Tickler type")
        nil)))

   (t
    (message "Not in org-mode or org-agenda")
    nil)))

;;;###autoload
(defun org-gtd-set-timestamp ()
  "Set or change the timestamp for a Calendar, Delegated, or Tickler item.
Works from org-mode headings or org-agenda items.

Prompts for a new date and sets the ORG_GTD_TIMESTAMP property."
  (interactive)
  (when-let ((marker (org-gtd--find-timestamped-item-at-point-or-agenda)))
    (org-with-point-at marker
      (let ((new-date (org-gtd-prompt-for-active-date "New date")))
        (org-entry-put nil org-gtd-timestamp new-date)
        (save-buffer)))))
```

**Step 2: Add require for org-gtd-configure (for org-gtd-prompt-for-active-date)**

Check if `org-gtd-configure` is already required at top of file. If not, add:

```elisp
(require 'org-gtd-configure)
```

**Step 3: Run tests to verify they pass**

Run: `~/bin/eldev etest test/unit/agenda-transient-test.el -r dot`
Expected: All tests pass

**Step 4: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-core.el
git commit -m "feat(core): add org-gtd-set-timestamp command

Works from both org-mode headings and org-agenda items.
Validates that item is Calendar, Delegated, or Tickler type.

Closes #187"
```

---

### Task 4: Wire transient to use org-gtd-set-timestamp

**Files:**
- Modify: `org-gtd-agenda-transient.el:135-140`

**Step 1: Replace org-gtd-agenda-transient--set-date implementation**

Change from:

```elisp
(defun org-gtd-agenda-transient--set-date ()
  "Set new date for task's GTD timestamp."
  (interactive)
  (org-gtd-agenda-transient--with-task
   (let ((new-date (org-gtd-prompt-for-active-date "New date")))
     (org-entry-put (point) org-gtd-timestamp new-date))))
```

To:

```elisp
(defun org-gtd-agenda-transient--set-date ()
  "Set new date for task's GTD timestamp."
  (interactive)
  (org-gtd-set-timestamp))
```

**Step 2: Add require for org-gtd-core if not present**

Check requires at top of file. `org-gtd-core` should already be required.

**Step 3: Run tests to verify transient still works**

Run: `~/bin/eldev etest test/unit/agenda-transient-test.el -r dot`
Expected: All tests pass

**Step 4: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-agenda-transient.el
git commit -m "refactor(transient): use org-gtd-set-timestamp for set-date action"
```

---

### Task 5: Update changelog and final verification

**Files:**
- Modify: `CHANGELOG.org:1` (add under Unreleased section)

**Step 1: Add changelog entry**

Add after existing Unreleased entries:

```org
*** Unified set-timestamp command (Issue #187)
~org-gtd-set-timestamp~ now works from both org-mode headings and
org-agenda items. Use it to reschedule Calendar, Delegated, or Tickler
items. The agenda transient (~s~ key) uses this same command.
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
git commit -m "docs: add unified set-timestamp to changelog"
```

---

### Summary

After completing all tasks:
- `org-gtd-set-timestamp` is available as an interactive command
- Works from org-mode headings (Calendar/Delegated/Tickler items)
- Works from org-agenda items
- Transient uses the same code path (no duplication)
- Issue #187 is closed
